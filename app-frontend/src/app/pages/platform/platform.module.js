import angular from 'angular';

class PlatformController {
    constructor() {
    }
}

const PlatformModule = angular.module('pages.organization', []);
PlatformModule.controller('PlatformController', PlatformController);

export default PlatformModule;
