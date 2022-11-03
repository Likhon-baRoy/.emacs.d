/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.TypeHierarchyFeature = void 0;
const vscode_languageserver_protocol_1 = require("vscode-languageserver-protocol");
const TypeHierarchyFeature = (Base) => {
    return class extends Base {
        get typeHierarchy() {
            return {
                onPrepare: (handler) => {
                    this.connection.onRequest(vscode_languageserver_protocol_1.Proposed.TypeHierarchyPrepareRequest.type, (params, cancel) => {
                        return handler(params, cancel, this.attachWorkDoneProgress(params), undefined);
                    });
                },
                onSupertypes: (handler) => {
                    const type = vscode_languageserver_protocol_1.Proposed.TypeHierarchySupertypesRequest.type;
                    this.connection.onRequest(type, (params, cancel) => {
                        return handler(params, cancel, this.attachWorkDoneProgress(params), this.attachPartialResultProgress(type, params));
                    });
                },
                onSubtypes: (handler) => {
                    const type = vscode_languageserver_protocol_1.Proposed.TypeHierarchySubtypesRequest.type;
                    this.connection.onRequest(type, (params, cancel) => {
                        return handler(params, cancel, this.attachWorkDoneProgress(params), this.attachPartialResultProgress(type, params));
                    });
                }
            };
        }
    };
};
exports.TypeHierarchyFeature = TypeHierarchyFeature;
//# sourceMappingURL=proposed.typeHierarchy.js.map