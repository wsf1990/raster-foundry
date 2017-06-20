/* global PIXI, $, requestAnimationFrame */

// const Map = require('es6-map');
// const SvgPanZoom = require('svg-pan-zoom');


export default class DiagramContainerController {
    constructor( // eslint-disable-line max-params
        $window, $element, $scope, $state, $timeout, $log,
        mousetipService, toolService
    ) {
        'ngInject';
        this.$log = $log;
        this.$element = $element;
        this.$window = $window;
        this.$scope = $scope;
        this.$state = $state;
        this.$timeout = $timeout;
        this.mousetipService = mousetipService;
        this.toolService = toolService;
    }

    $onInit() {
        const element = this.$element[0];
        const shadeNormal = 0x7B8391;
        const white = 0xffffff;

        const renderer = PIXI.autoDetectRenderer(
            element.offsetWidth,
            element.offsetHeight,
            {
                antialias: true, transparent: true, resolution: 1
            }
        );
        renderer.autoResize = true;

        this.$element[0].children[0].append(renderer.view);

        const stage = new PIXI.Container();
        stage.interactive = true;

        class NodeRect extends PIXI.Graphics {
            constructor(x, y, width, height) {
                super();
                this.beginFill(white);
                this.lineStyle(1.5, shadeNormal);
                this.drawRoundedRect(x, y, width, height, 5);
                this.endFill();
            }
        }

        class NodeShadow extends PIXI.Graphics {
            constructor(x, y, width, height) {
                super();
                this.beginFill(shadeNormal);
                this.drawRoundedRect(x + 4, y + 5, width - 4, height - 4, 5);
                this.endFill();
                this.filters = [new PIXI.filters.BlurFilter(2, 2, 3, 5)];
            }
        }

        class Node extends PIXI.Container {
            constructor(x, y, width, height) {
                super();
                this.x = x;
                this.y = y;
                this.width = width;
                this.height = height;

                this.shadow = new NodeShadow(0, 0, width, height);
                this.addChild(this.shadow);

                this.background = new NodeRect(0, 0, width, height);
                this.addChild(this.background);

                this.header = new NodeRect(0, 0, width, 50);
                this.addChild(this.header);

                this.headerText = new PIXI.Text('This is header text.', {
                    fontFamily: 'Arial',
                    fontSize: 14,
                    fill: shadeNormal
                });
                this.headerText.x = 5;
                this.headerText.y = 5;
                this.headerText.interactive = true;
                this.headerText.click = (e) => {
                    console.log('text clicked', e, this);
                    e.stopPropagation();
                };
                this.addChild(this.headerText);

                this.setPosition(x, y);
            }

            setPosition(x, y) {
                let newX = x - this.width / 2;
                this.x = newX > 0 ? newX : 0;
                this.y = y;
            }
        }

        this.node = new Node(10, 10, 200, 100);
        stage.addChild(this.node);

        angular.element(this.$window).on('resize', () => {
            renderer.resize(element.offsetWidth, element.offsetHeight);
            this.$log.log('resized to', element.offsetWidth, element.offsetHeight);
        });

        renderer.render(stage);

        // let velocity = [1, 0];
        let renderLoop = () => {
            requestAnimationFrame(renderLoop.bind());
            // if (this.node.x > 300) {
            //     this.node.setPosition(0, 0);
            // }
            // this.node.setPosition(this.node.x + velocity[0], this.node.y + velocity[1]);

            let mousePosition = renderer.plugins.interaction.mouse.global;
            if (mousePosition.x > 0 && mousePosition.y > 0) {
                this.node.setPosition(mousePosition.x, mousePosition.y);
            }

            renderer.render(stage);
        };

        renderLoop();
    }

    // $onInit() {
    //     this.workspaceElement = this.$element[0].children[0];
    //     this.comparison = [false, false];
    //     this.cellSize = [300, 75];
    //     this.paddingFactor = 0.8;
    //     this.nodeSeparationFactor = 0.25;
    //     this.panActive = false;
    //     this.initContextMenus();
    //     this.contextMenuTpl =
    //         `<div class="lab-contextmenu" ng-show="isShowingContextMenu">
    //             <div class="btn-group">
    //                 <button ng-repeat="item in currentContextMenu"
    //                     ng-click="item.callback()"
    //                     class="btn btn-default">
    //                     {{item.label}}
    //                 </button>
    //             </div>
    //         </div>`;
    //     this.extractInputs();
    //     this.extractShapes();
    //     this.initDiagram();
    // }

    // getToolLabel(json) {
    //     return json.metadata.label || json.apply;
    // }

    // initDiagram() {
    //     if (!this.graph) {
    //         this.graph = new joint.dia.Graph();
    //     } else {
    //         this.graph.clear();
    //     }

    //     if (!this.paper) {
    //         this.paper = new joint.dia.Paper({
    //             el: this.workspaceElement,
    //             height: $(this.workspaceElement).height(),
    //             width: $(this.workspaceElement).width(),
    //             gridSize: 25,
    //             drawGrid: true,
    //             model: this.graph,
    //             clickThreshold: 4
    //         });
    //         this.paper.drawGrid({
    //             color: '#aaa',
    //             thickness: 1
    //         });
    //         this.paper.on('blank:pointerclick', this.onPaperClick.bind(this));
    //         this.paper.on('cell:pointerclick', this.onCellClick.bind(this));
    //         this.paper.on('blank:pointerdown', () => {
    //             this.panActive = true;
    //             this.$scope.$evalAsync();
    //         });
    //         this.paper.on('blank:pointerup', () => {
    //             this.panActive = false;
    //             this.$scope.$evalAsync();
    //         });
    //     }

    //     if (this.shapes) {
    //         let padding = this.cellSize[0] * this.nodeSeparationFactor;
    //         this.shapes.forEach(s => this.graph.addCell(s));
    //         joint.layout.DirectedGraph.layout(this.graph, {
    //             setLinkVertices: false,
    //             rankDir: 'LR',
    //             nodeSep: padding,
    //             rankSep: padding * 2,
    //             marginX: padding,
    //             marginY: padding
    //         });

    //         this.paper.scaleContentToFit({
    //             padding: padding
    //         });
    //         if (!this.svgPanZoom) {
    //             this.svgPanZoom = SvgPanZoom(this.paper.svg, {
    //                 beforePan: () => {
    //                     return this.panActive;
    //                 },
    //                 beforeZoom: () => {
    //                     this.hideContextMenu();
    //                 },
    //                 dblClickZoomEnabled: false,
    //                 fit: false
    //             });
    //         }
    //     }
    // }

    // zoomIn() {
    //     if (this.svgPanZoom) {
    //         this.svgPanZoom.zoomIn();
    //     }
    // }

    // zoomOut() {
    //     if (this.svgPanZoom) {
    //         this.svgPanZoom.zoomOut();
    //     }
    // }

    // fitAndCenter() {
    //     if (this.svgPanZoom) {
    //         this.svgPanZoom.fit();
    //         this.svgPanZoom.center();
    //         this.svgPanZoom.zoomOut();
    //     }
    // }

    // initContextMenus() {
    //     this.contextMenus = new Map();
    //     this.defaultContextMenu = [{
    //         label: 'Compare to...',
    //         callback: () => {
    //             this.startComparison();
    //         }
    //     }, {
    //         label: 'View output',
    //         callback: () => {
    //             this.onPreview({data: this.selectedCellView.model.id});
    //         }
    //     }, {
    //         label: 'Share',
    //         callback: () => {
    //             this.onShare({data: this.selectedCellView.model.id});
    //         }
    //     }];
    //     this.cancelComparisonMenu = [{
    //         label: 'Cancel',
    //         callback: () => {
    //             this.cancelComparison();
    //         }
    //     }];
    // }

    // onPaperClick() {
    //     this.$scope.$evalAsync(() => {
    //         if (this.isComparing) {
    //             this.cancelComparison();
    //         } else {
    //             this.hideContextMenu();
    //             this.unselectCellView();
    //         }
    //     });
    // }

    // onCellClick(cv, evt) {
    //     this.$scope.$evalAsync(() => {
    //         if (this.isComparing) {
    //             this.continueComparison(cv);
    //         } else {
    //             this.selectCellView(cv, evt);
    //         }
    //     });
    // }

    // extractInputs() {
    //     this.inputsJson = [];

    //     let json = Object.assign({}, this.toolDefinition);
    //     let inputs = [json];
    //     while (inputs.length) {
    //         let input = inputs.pop();
    //         let args = input.args;
    //         if (args) {
    //             let tool = this.getToolLabel(input);
    //             if (!Array.isArray(args)) {
    //                 args = Object.values(args);
    //             }
    //             inputs = inputs.concat(args.map((a) => {
    //                 return Object.assign({parent: tool}, a);
    //             }));
    //         } else {
    //             this.inputsJson.push(input);
    //         }
    //     }
    // }

    // extractShapes() {
    //     let nextId = 0;
    //     let nodes = new Map();
    //     let shapes = [];
    //     let json = Object.assign({}, this.toolDefinition);
    //     let inputs = [json];

    //     while (inputs.length) {
    //         let input = inputs.pop();
    //         let rectangle;
    //         let args;

    //         // Args can be array or object, if object, convert to array
    //         if (input.args) {
    //             args = Array.isArray(input.args) ? input.args : Object.values(input.args);
    //         } else {
    //             args = [];
    //         }

    //         // Input nodes not of the layer type are not made into rectangles
    //         if (!input.type || input.type === 'layer') {
    //             let rectInputs = args.length;
    //             let rectOutputs = ['Output'];
    //             let ports = this.createPorts(rectInputs, rectOutputs);
    //             let currentId = nextId.toString();
    //             let rectAttrs = {
    //                 id: input.id,
    //                 label: this.getToolLabel(input),
    //                 inputs: rectInputs,
    //                 outputs: rectOutputs,
    //                 tag: input.tag,
    //                 ports: ports
    //             };

    //             rectangle = this.constructRect(rectAttrs);

    //             nodes.set(currentId, rectAttrs);

    //             shapes.push(rectangle);

    //             if (input.parent) {
    //                 let firstPort = input.parent.portData.ports.filter(i => {
    //                     return i.group === 'inputs' && !i.isConnected;
    //                 })[0];

    //                 firstPort.isConnected = true;

    //                 let link = new joint.dia.Link({
    //                     source: {id: rectangle.id, port: 'Output'},
    //                     target: {id: input.parent.id, port: firstPort.id},
    //                     attrs: {
    //                         '.marker-target': {
    //                             d: 'M 4 0 L 0 2 L 4 4 z'
    //                         }
    //                     }
    //                 });

    //                 shapes.push(link);
    //             }
    //             if (args) {
    //                 inputs = inputs.concat(args.map((a) => {
    //                     return Object.assign({
    //                         parent: rectangle
    //                     }, a);
    //                 }));
    //             }
    //             nextId += 1;
    //         }
    //     }
    //     this.shapes = shapes;
    //     this.nodes = nodes;
    // }

    // constructRect(config) {
    //     let label = joint.util.breakText(config.label || config.id.toString(), {
    //         width: this.cellSize[0] * this.paddingFactor,
    //         height: this.cellSize[1] * this.paddingFactor
    //     });

    //     return new joint.shapes.basic.Rect({
    //         id: config.id,
    //         size: {
    //             width: this.cellSize[0],
    //             height: this.cellSize[1]
    //         },
    //         attrs: {
    //             rect: {
    //                 fill: '#fff',
    //                 stroke: '#959cad',
    //                 'stroke-width': 0.5,
    //                 rx: 2,
    //                 ry: 4
    //             },
    //             text: {
    //                 fill: '#353b59',
    //                 text: label
    //             }
    //         },
    //         ports: {
    //             groups: {
    //                 inputs: {
    //                     position: {
    //                         name: 'left'
    //                     }
    //                 },
    //                 outputs: {
    //                     position: {
    //                         name: 'right'
    //                     }
    //                 }
    //             },
    //             items: config.ports
    //         }
    //     });
    // }

    // startComparison() {
    //     this.mousetipService.set('Select a node to compare');
    //     this.isComparing = true;
    //     this.comparison[0] = this.selectedCellView.model.id;
    //     this.showContextMenu(this.selectedCellView, this.cancelComparisonMenu);
    // }

    // continueComparison(cv) {
    //     this.mousetipService.remove();
    //     this.hideContextMenu();
    //     this.isComparing = false;
    //     this.comparison[1] = cv.model.id;
    //     this.onPreview({data: this.comparison});
    // }

    // cancelComparison() {
    //     this.hideContextMenu();
    //     this.isComparing = false;
    //     this.mousetipService.remove();
    // }

    // showContextMenu(cv, contextMenu) {
    //     this.hideContextMenu();

    //     let bounds = cv.getBBox() || this.selectedCellView.getBBox || false;
    //     let menuScope = this.$scope.$new();
    //     menuScope.currentContextMenu = contextMenu ||
    //                                    this.contextMenus.get(cv.model.id) ||
    //                                    this.defaultContextMenu;

    //     this.contextMenuEl = this.$compile(this.contextMenuTpl)(menuScope)[0];
    //     this.$element[0].appendChild(this.contextMenuEl);
    //     this.contextMenuEl = $(this.contextMenuEl).css({
    //         top: bounds.y,
    //         left: bounds.x + bounds.width / 2
    //     });

    //     menuScope.$evalAsync(() => {
    //         menuScope.isShowingContextMenu = true;
    //     });
    // }

    // hideContextMenu() {
    //     this.isShowingContextMenu = false;
    //     if (this.contextMenuEl) {
    //         this.contextMenuEl.remove();
    //     }
    // }

    // selectCellView(cellView) {
    //     this.unselectCellView();
    //     this.selectedCellView = cellView;
    //     cellView.model.attr({
    //         rect: {
    //             stroke: '#353b59',
    //             'stroke-width': '1'
    //         }
    //     });
    //     this.showContextMenu(cellView);
    // }

    // unselectCellView() {
    //     if (this.selectedCellView) {
    //         this.selectedCellView.model.attr({
    //             rect: {
    //                 stroke: '#959cad',
    //                 'stroke-width': 0.5
    //             }
    //         });
    //         this.selectedCellView = null;
    //     }
    // }


    // createPorts(inputs, outputs) {
    //     let ports = [];
    //     let inputList = Array.isArray(inputs) ?
    //         inputs : Array(inputs).fill();

    //     ports = inputList.map((_, idx) => {
    //         return {
    //             id: `input-${idx}`,
    //             label: `input-${idx}`,
    //             group: 'inputs'
    //         };
    //     });

    //     ports = ports.concat(outputs.map(o => {
    //         return {
    //             id: o,
    //             group: 'outputs'
    //         };
    //     }));

    //     return ports;
    // }

    // createLink(src, target) {
    //     let link = new joint.dia.Link({
    //         source: {id: src[0], port: src[1]},
    //         target: {id: target[0], port: target[1]},
    //         attrs: {
    //             '.marker-target': {
    //                 d: 'M 4 0 L 0 2 L 4 4 z'
    //             }
    //         }
    //     });
    //     return link;
    // }
}
