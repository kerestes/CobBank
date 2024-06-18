import { Component } from '@angular/core';
import { HeroHomeComponent } from './hero-home/hero-home.component';

@Component({
  selector: 'app-home',
  standalone: true,
  imports: [HeroHomeComponent],
  templateUrl: './home.component.html',
  styleUrl: './home.component.sass'
})
export class HomeComponent {

}
