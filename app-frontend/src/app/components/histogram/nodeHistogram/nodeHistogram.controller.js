/* globals d3 _ */

export default class NodeHistogramController {
    constructor($log, $scope) {
        'ngInject';
        this.$log = $log;
        this.$scope = $scope;
    }

    $onInit() {
        this.setDefaults(this.options);
        this.$scope.$watch('$ctrl.minChecked', (minChecked) => {
            if (minChecked !== this._masks.min) {
                this._masks.min = minChecked;
                if (this.refreshHistogram) {
                    this.refreshHistogram();
                }
                this.onMasksChange({masks: Object.assign({}, this._masks)});
            }
        });
        this.$scope.$watch('$ctrl.maxChecked', (maxChecked) => {
            if (maxChecked !== this._masks.max) {
                this._masks.max = maxChecked;
                if (this.refreshHistogram) {
                    this.refreshHistogram();
                }
                this.onMasksChange({masks: Object.assign({}, this._masks)});
            }
        });
        this.$scope.$watch('$ctrl.options.discrete', () => {
            this.refreshHistogram();
        });
        this.api = {};
    }

    $onChanges(changes) {
        if (changes.options && changes.options.currentValue) {
            this.setDefaults(changes.options.currentValue);
        }

        if (changes.histogram && changes.histogram.currentValue) {
            this.processDataToPlot(changes.histogram.currentValue);
        }

        if (changes.masks && changes.masks.currentValue) {
            this._masks = Object.assign({min: false, max: false}, changes.masks.currentValue);
            this.minChecked = this._masks.min;
            this.maxChecked = this._masks.max;
        }

        if (changes.breakpoints && changes.breakpoints.currentValue) {
            this._breakpoints = Object.keys(changes.breakpoints.currentValue).map((key) => {
                return {key: key, breakpoint: changes.breakpoints.currentValue[key]};
            });
        }
    }

    updateHistogramColors() {
        let svg = d3.select('svg');
        let defs = svg.select('defs')[0].length ? svg.select('defs') : svg.append('defs');
        let linearGradient = defs.selectAll('linearGradient')[0].length ?
            defs.selectAll('linearGradient') : defs.append('linearGradient');
        let range = this.options.max - this.options.min;
        let data = this._breakpoints.map((bp) => {
            let offset = (bp.breakpoint.value - this.options.min) / range * 100;
            return {offset: `${offset}%`, color: bp.breakpoint.color};
        });
        if (this.options.discrete) {
            let offsetData = data.map((currentValue, index, array) => {
                if (index !== array.length - 1) {
                    return {offset: array[index + 1].offset, color: currentValue.color};
                }
                return currentValue;
            });
            data = _.flatten(_.zip(data, offsetData));
        }
        if (this._masks.min || this.options.discrete) {
            let last = _.last(data);
            if (last.color === 'NODATA' || !this.options.discrete) {
                data.splice(0, 0, {offset: data[0].offset, color: '#353C58'});
                data.splice(0, 0, {offset: data[0].offset, color: '#353C58'});
            } else {
                data.splice(0, 0, {offset: data[0].offset, color: _.first(data.color)});
                data.splice(0, 0, {offset: data[0].offset, color: last.color});
            }
        }
        if (this._masks.max || this.options.discrete) {
            let last = _.last(data);
            if (last.color === 'NODATA' || !this.options.discrete) {
                data.push({offset: _.last(data).offset, color: '#353C58'});
                data.push({offset: _.last(data).offset, color: '#353C58'});
            } else {
                data.push({offset: _.last(data).offset, color: last.color});
            }
        }
        linearGradient.attr('id', 'line-gradient')
            .attr('gradientUnits', 'userSpaceOnUse')
            .attr('x1', '0%').attr('y1', 0)
            .attr('x2', '100%').attr('y2', 0)
            .selectAll('stop')
            .data(data)
            .enter().append('stop')
            .attr('offset', (d) => d.offset)
            .attr('stop-color', (d) => d.color)
            .attr('stop-opacity', (d) => Number.isFinite(d.opacity) ? d.opacity : 1.0);
    }

    setDefaults(options) {
        this.options = Object.assign({
            min: 0,
            max: 255
        }, options ? options : {});

        this.histOptions = {
            chart: {
                type: 'lineChart',
                showLegend: false,
                showXAxis: false,
                showYAxis: false,
                yScale: d3.scale.log(),
                margin: {
                    top: 0,
                    right: 0,
                    bottom: 0,
                    left: 0
                },
                height: 100,
                xAxis: {
                    showLabel: false
                },
                yAxis: {
                    showLabel: false
                },
                tooltip: {
                    enabled: false
                },
                dispatch: {
                    renderEnd: () => {
                        this.updateHistogramColors();
                    }
                }
            }
        };

        if (!this._breakpoints) {
            this._breakpoints = {
                min: {
                    value: this.options.min,
                    checked: false
                },
                max: {
                    value: this.options.max,
                    checked: false
                }
            };
        }
    }

    processDataToPlot(data) {
        let plot = _.range(0, 256).map((pixelValue) => {
            let x = parseInt(pixelValue, 10);
            let y = parseInt(data[pixelValue.toString()] ? data[pixelValue] : 0, 10);
            return {x: x, y: y};
        });
        this.plot = [{
            values: plot,
            key: 'Value',
            area: true}];
    }

    onChange(bp, breakpoint) {
        let min = _.first(this._breakpoints).breakpoint.value;
        let max = _.last(this._breakpoints).breakpoint.value;
        let index = this._breakpoints.findIndex((b) => b.breakpoint === bp);
        if (index === 0) {
            let second = this._breakpoints[1].breakpoint.value;
            bp.value = breakpoint < second ? breakpoint : second;
        } else if (index === this._breakpoints.length - 1) {
            let secondToLast = this._breakpoints[this._breakpoints.length - 2].breakpoint.value;
            bp.value = breakpoint > secondToLast ? breakpoint : secondToLast;
        } else {
            bp.value = Math.min(Math.max(breakpoint, min), max);
            this._breakpoints.sort((a, b) => a.breakpoint.value - b.breakpoint.value);
        }

        //let breakpoints = Object.assign({min: {}, max: {}}, this._breakpoints);
        this.onBreakpointChange({breakpoints: this._breakpoints});
        if (!this.refreshHistogram && this.api.refresh) {
            this.refreshHistogram = _.throttle(this.api.refresh, 100);
        }
        if (this.refreshHistogram) {
            this.refreshHistogram();
        }
    }
}
