import angular from 'angular';
import shapeItemTpl from './shapeItem.html';

const ShapeItemComponent = {
    templateUrl: shapeItemTpl,
    transclude: true,
    controller: 'ShapeItemController',
    bindings: {
        shape: '<'
    }
};

class ShapeItemController {
    constructor() {}
    $postLink() {
        this.shapeJson = JSON.stringify(this.shape);
    }
}

const ShapeItemModule = angular.module('components.vectors.shapeItem', []);

ShapeItemModule.component('rfShapeItem', ShapeItemComponent);
ShapeItemModule.controller('ShapeItemController', ShapeItemController);

export default ShapeItemModule;

