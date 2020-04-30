import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';

import { WelcomeComponent } from './welcome/welcome.component';
import { SportsComponent } from './sports/sports/sports.component';
import { PageNotFoundComponent } from './page-not-found/page-not-found.component';

const routes: Routes = 
[
  { path: 'welcome', component: WelcomeComponent },
  { path: 'sports', component: SportsComponent },
  { path: '', redirectTo: 'welcome', pathMatch: 'full' },
  { path: '**', component: PageNotFoundComponent },
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
