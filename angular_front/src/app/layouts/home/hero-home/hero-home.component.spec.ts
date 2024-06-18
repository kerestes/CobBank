import { ComponentFixture, TestBed } from '@angular/core/testing';

import { HeroHomeComponent } from './hero-home.component';

describe('HeroHomeComponent', () => {
  let component: HeroHomeComponent;
  let fixture: ComponentFixture<HeroHomeComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [HeroHomeComponent]
    })
    .compileComponents();
    
    fixture = TestBed.createComponent(HeroHomeComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
