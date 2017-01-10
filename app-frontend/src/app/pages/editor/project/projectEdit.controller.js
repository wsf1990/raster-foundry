const Map = require('es6-map');

export default class ProjectEditController {
    constructor( // eslint-disable-line max-params
        $scope, $rootScope, $state, mapService, projectService, layerService, $uibModal
    ) {
        'ngInject';
        this.$state = $state;
        this.$scope = $scope;
        this.$rootScope = $rootScope;
        this.projectService = projectService;
        this.layerService = layerService;
        this.$uibModal = $uibModal;
        this.getMap = () => mapService.getMap('project');


        this.mapOptions = {
            static: false,
            fitToGeojson: false
        };
    }

    $onInit() {
        this.projectId = this.$state.params.projectid;
        this.selectedScenes = new Map();
        this.selectedLayers = new Map();
        this.sceneList = [];
        this.sceneLayers = new Map();
        this.layers = [];

        if (!this.project) {
            if (this.projectId) {
                this.loadingProject = true;
                this.projectService.query({id: this.projectId}).then(
                    (project) => {
                        this.project = project;
                        this.loadingProject = false;
                    },
                    () => {
                        this.loadingProject = false;
                        // @TODO: handle displaying an error message
                    }
                );
                this.getSceneList();
            } else {
                this.selectProjectModal();
            }
        }

        this.$scope.$watch('$ctrl.sceneList', this.fitAllScenes.bind(this));

        this.$scope.$on('$destroy', () => {
            if (this.activeModal) {
                this.activeModal.dismiss();
            }
        });
    }

    setHoveredScene(scene) {
        if (!scene) {
            return;
        }
        let styledGeojson = Object.assign({}, scene.dataFootprint, {
            properties: {
                options: {
                    weight: 2,
                    fillOpacity: 0
                }
            }
        });
        this.getMap().then((map) => {
            map.setGeojson('footprint', styledGeojson);
        });
    }

    removeHoveredScene() {
        this.getMap().then((map) => {
            map.deleteGeojson('footprint');
        });
    }

    applyCachedZOrder() {
        if (this.cachedZIndices) {
            for (const [id, l] of this.selectedLayers) {
                l.getTileLayer().then((tiles) => {
                    tiles.setZIndex(this.cachedZIndices.get(id));
                });
            }
        }
    }

    fitSelectedScenes() {
        this.fitScenes(Array.from(this.selectedScenes.values()));
    }

    bringSelectedScenesToFront() {
        this.cachedZIndices = new Map();
        for (const [id, l] of this.selectedLayers) {
            l.getTileLayer().then((tiles) => {
                this.cachedZIndices.set(id, tiles.options.zIndex);
                tiles.bringToFront();
            });
        }
    }

    fitAllScenes() {
        if (this.sceneList.length) {
            this.fitScenes(this.sceneList);
        }
    }

    fitScenes(scenes) {
        this.getMap().then((map) =>{
            let sceneFootprints = scenes.map((scene) => scene.dataFootprint);
            map.map.fitBounds(L.geoJSON(sceneFootprints).getBounds());
        });
    }

    getSceneList() {
        this.sceneRequestState = {loading: true};
        this.projectService.getAllProjectScenes(
            {projectId: this.projectId}
        ).then(
            (allScenes) => {
                this.sceneList = allScenes;
                for (const scene of this.sceneList) {
                    let scenelayer = this.layerService.layerFromScene(scene, this.projectId);
                    this.sceneLayers.set(scene.id, scenelayer);
                }
                this.layersFromScenes();
            },
            (error) => {
                this.sceneRequestState.errorMsg = error;
            }
        ).finally(() => {
            this.sceneRequestState.loading = false;
        });
    }

    layersFromScenes() {
        this.getMap().then((map) => {
            let layer = this.layerService.layerFromScene(this.sceneList, this.projectId, true);
            layer.getTileLayer().then((tiles) => {
                map.addLayer('project', tiles);
            });
        });
    }

    selectProjectModal() {
        if (this.activeModal) {
            this.activeModal.dismiss();
        }

        this.activeModal = this.$uibModal.open({
            component: 'rfSelectProjectModal',
            backdrop: 'static',
            keyboard: false,
            resolve: {
            }
        });
    }

    publishModal() {
        if (this.activeModal) {
            this.activeModal.dismiss();
        }

        this.activeModal = this.$uibModal.open({
            component: 'rfPublishModal',
            backdrop: 'static',
            keyboard: false,
            resolve: {
                project: () => this.project
            }
        });
    }
}
