import * as vscode from 'vscode';
import * as path from 'path';
import * as request from 'request';
import handleUrl from './HandleUrl';
export class PHPDetailPanel
{

	public static currentPanel: PHPDetailPanel | undefined;
	public static readonly viewType = 'PHPDocument';
	private readonly _panel: vscode.WebviewPanel;
    private  _url: string;
    private  _fun: string;
    private _disposables: vscode.Disposable[] = [];
    public static createOrShow(fun: string)
    {
        const column = vscode.window.activeTextEditor ? vscode.window.activeTextEditor.viewColumn : undefined;
        let config:vscode.WorkspaceConfiguration = vscode.workspace.getConfiguration("PHPDocument");
        let funUrl:string = fun.replace(/_/g,'-');
        let url:string = handleUrl(config.language,funUrl);
		if(PHPDetailPanel.currentPanel){
			PHPDetailPanel.currentPanel._panel.reveal(column);
			PHPDetailPanel.currentPanel._update(url,fun);
			return;
        }
		const panel = vscode.window.createWebviewPanel(PHPDetailPanel.viewType,"PHPDocument",column || vscode.ViewColumn.One,{
            enableScripts: true,
            retainContextWhenHidden:true,
            enableCommandUris:true
		});
		PHPDetailPanel.currentPanel = new PHPDetailPanel(panel,url,fun);
	}

/**
 *Creates an instance of PHPDetailPanel.
 * @param {vscode.WebviewPanel} panel 
 * @param {string} url
 * @param {string} fun 
 * @memberof PHPDetailPanel
 */

private constructor(panel: vscode.WebviewPanel,url:string,fun:string)
    {
		this._panel = panel;
        this._url = url;
        this._fun = fun;
        this._update();
        this._panel.onDidDispose(() => this.dispose(), null, this._disposables);
        this._panel.iconPath = {
            light:vscode.Uri.file(path.join(__filename,  '..', '..', 'resources', 'light', 'PHP.svg')) ,
            dark: vscode.Uri.file(path.join(__filename,  '..', '..', 'resources', 'dark',  'PHP.svg'))
        };
        this._panel.onDidChangeViewState(e => 
            {
            if (this._panel.visible) 
            {
                this._update()
            }
        }, null, this._disposables);

        /*this._panel.webview.onDidReceiveMessage(message => {
            switch (message.command) {
                case 'alert':
                    vscode.window.showErrorMessage(message.text);
                    return;
            }
        }, null, this._disposables);*/
    }

    public dispose() 
    {
        PHPDetailPanel.currentPanel = undefined;
        // Clean up our resources
        this._panel.dispose();

        while (this._disposables.length) 
        {
            const x = this._disposables.pop();
            if (x) 
            {
                x.dispose();
            }
        }
	}

    /**
     * @private
     * @param {string} [url] 访问的url
     * @param {string} [fun] php关键字
     * @returns
     * @memberof PHPDetailPanel
     */
    private _update(url?:string,fun?:string)
    {

        if(url) this._url = url;
        if(fun) this._fun = fun;
        this._panel.title = "php - " + this._fun;
        return this.getHtml();
    }
    
    async  getHtml()
    {
        let re = await this._getHtmlForWebview();
        let html = re.toString();
        let css = "<style>.navbar,.navbar-fixed-top,.layout-menu,#breadcrumbs-inner,.page-tools,.footer-content,.headsup{display:none;}a{pointer-events: none;}#toTop{pointer-events:auto;} </style></head>";
        html = html.replace(/<\/head>/,css);
        html = html.replace(/\/cached.php/g,'https://php.net/cached.php');
        this._panel.webview.html = html;
	}

    private _getHtmlForWebview ()
    {
        let url = this._url;
        return new Promise(function (resolve, reject) 
        {
            request(url, function (err, res) 
            {
                if (err) return vscode.window.showInformationMessage(err);
                resolve(res.body.toString());
            })
        })
    }
}