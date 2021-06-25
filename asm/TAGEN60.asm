*          DATA SET TAGEN60    AT LEVEL 003 AS OF 08/12/03                      
*PHASE T70260A,*                                                                
         TITLE 'T70260 - TABLES FOR NEW CANADIAN CONTRACT'                      
T70260   CSECT                                                                  
         DC    AL4(USETBLS-T70260)                                              
         DC    AL4(USELUT-T70260)                                               
         DC    AL4(MAJLUT-T70260)                                               
         DC    AL4(AFMCOLS-T70260)                                              
         DC    AL4(RADCOLS-T70260)                                              
         DC    AL4(OFFCOLS-T70260)                                              
         DC    AL4(ONCOLS-T70260)                                               
         DC    AL4(MSWEET-T70260)                                               
         DC    A(0)                                                             
         EJECT                                                                  
*              USE TYPE RATE TABLES                                             
         SPACE 3                                                                
USETBLS  DS    0F                                                               
BSCTTBL  DC    AL1(0,40,0,0)       NATIONAL TV                                  
         DC    F'54600'            PP,SD,CAR                                    
         DC    F'54600'            SOC,GD                                       
         DC    F'39850'            VO,SS                                        
         DC    F'17200'            GS                                           
         DC    F'81900'            SA = PP+50%                                  
         DC    F'54600'            DEM                                          
         DC    F'33400'            E                                            
         DC    F'22300'            GE                                           
         DC    F'54600'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(1,40,0,0)       SHORT LIFE TV 7                              
         DC    F'55600'                                                         
         DC    F'55600'                                                         
         DC    F'40600'                                                         
         DC    F'17500'                                                         
         DC    F'83400'                                                         
         DC    F'55600'                                                         
         DC    F'33400'                                                         
         DC    F'22300'                                                         
         DC    F'55600'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(2,40,0,0)       SHORT LIFE TV 14                             
         DC    F'55600'                                                         
         DC    F'55600'                                                         
         DC    F'40600'                                                         
         DC    F'17500'                                                         
         DC    F'83400'                                                         
         DC    F'55600'                                                         
         DC    F'33400'                                                         
         DC    F'22300'                                                         
         DC    F'55600'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(3,40,0,0)       SHORT LIFE TV 31                             
         DC    F'55600'                                                         
         DC    F'55600'                                                         
         DC    F'40600'                                                         
         DC    F'17500'                                                         
         DC    F'83400'                                                         
         DC    F'55600'                                                         
         DC    F'33400'                                                         
         DC    F'22300'                                                         
         DC    F'55600'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(4,40,0,0)       SHORT LIFE TV 45                             
         DC    F'55600'                                                         
         DC    F'55600'                                                         
         DC    F'40600'                                                         
         DC    F'17500'                                                         
         DC    F'83400'                                                         
         DC    F'55600'                                                         
         DC    F'33400'                                                         
         DC    F'22300'                                                         
         DC    F'55600'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(5,36,0,0)       LOCAL AND REGIONAL TV 1                      
         DC    F'38600'                                                         
         DC    F'37140'                                                         
         DC    F'19070'                                                         
         DC    F'14310'                                                         
         DC    F'57900'                                                         
         DC    F'0'                                                             
         DC    F'21810'                                                         
         DC    F'21810'                                                         
         SPACE 1                                                                
         DC    AL1(6,36,0,0)       LOCAL AND REGIONAL TV 2                      
         DC    F'31940'                                                         
         DC    F'30700'                                                         
         DC    F'11280'                                                         
         DC    F'6770'                                                          
         DC    F'47910'                                                         
         DC    F'0'                                                             
         DC    F'10980'                                                         
         DC    F'10980'                                                         
         SPACE 1                                                                
         DC    AL1(7,36,0,0)       LOCAL AND REGIONAL TV 3                      
         DC    F'27690'                                                         
         DC    F'26340'                                                         
         DC    F'8790'                                                          
         DC    F'5250'                                                          
         DC    F'41535'                                                         
         DC    F'0'                                                             
         DC    F'10980'                                                         
         DC    F'10980'                                                         
         EJECT                                                                  
DORTTBL  DC    AL1(12,40,0,0)      NATIONAL TV                                  
         DC    F'54600'            PP,SD,CAR                                    
         DC    F'54600'            SOC,GD                                       
         DC    F'39850'            VO,SS                                        
         DC    F'17200'            GS                                           
         DC    F'81900'            SA = PP+50%                                  
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'54600'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(13,40,0,0)      SHORT LIFE TV 7                              
         DC    F'55600'                                                         
         DC    F'55600'                                                         
         DC    F'40600'                                                         
         DC    F'17500'                                                         
         DC    F'83400'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'55600'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(14,40,0,0)      SHORT LIFE TV 14                             
         DC    F'55600'                                                         
         DC    F'55600'                                                         
         DC    F'40600'                                                         
         DC    F'17500'                                                         
         DC    F'83400'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'55600'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(15,40,0,0)      SHORT LIFE TV 31                             
         DC    F'55600'                                                         
         DC    F'55600'                                                         
         DC    F'40600'                                                         
         DC    F'17500'                                                         
         DC    F'83400'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'55600'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(16,40,0,0)      SHORT LIFE TV 45                             
         DC    F'55600'                                                         
         DC    F'55600'                                                         
         DC    F'40600'                                                         
         DC    F'17500'                                                         
         DC    F'83400'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'55600'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(17,24,0,0)      LOCAL AND REGIONAL TV 1                      
         DC    F'38600'                                                         
         DC    F'37140'                                                         
         DC    F'19070'                                                         
         DC    F'14310'                                                         
         DC    F'57900'                                                         
         SPACE 1                                                                
         DC    AL1(18,24,0,0)      LOCAL AND REGIONAL TV 2                      
         DC    F'31940'                                                         
         DC    F'30700'                                                         
         DC    F'11280'                                                         
         DC    F'6770'                                                          
         DC    F'47910'                                                         
         SPACE 1                                                                
         DC    AL1(19,24,0,0)      LOCAL AND REGIONAL TV 3                      
         DC    F'27690'                                                         
         DC    F'26340'                                                         
         DC    F'8790'                                                          
         DC    F'5250'                                                          
         DC    F'41535'                                                         
         EJECT                                                                  
BSCRTBL  DC    AL1(24,12,0,0)      NATIONAL RADIO                               
         DC    F'42550'            SV,SS                                        
         DC    F'31925'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(25,12,0,0)      SHORT LIFE RADIO 7                           
         DC    F'21675'                                                         
         DC    F'16275'                                                         
         SPACE 1                                                                
         DC    AL1(26,12,0,0)      SHORT LIFE RADIO 14                          
         DC    F'28650'                                                         
         DC    F'21475'                                                         
         SPACE 1                                                                
         DC    AL1(27,12,0,0)      SHORT LIFE RADIO 31                          
         DC    F'32550'                                                         
         DC    F'24450'                                                         
         SPACE 1                                                                
         DC    AL1(28,12,0,0)      SHORT LIFE RADIO 45                          
         DC    F'36850'                                                         
         DC    F'27675'                                                         
         SPACE 1                                                                
         DC    AL1(29,12,0,0)      LOCAL AND REGIONAL RADIO 1                   
         DC    F'31250'                                                         
         DC    F'23555'                                                         
         SPACE 1                                                                
         DC    AL1(30,12,0,0)      LOCAL AND REGIONAL RADIO 2                   
         DC    F'20295'                                                         
         DC    F'12205'                                                         
         SPACE 1                                                                
         DC    AL1(31,12,0,0)      LOCAL AND REGIONAL RADIO 3                   
         DC    F'18160'                                                         
         DC    F'10890'                                                         
         EJECT                                                                  
WSCTBL   DC    AL1(48,40,1,1)      TV WILDSPOT UNITS 1                          
         DC    F'42555'            PP,CAR,SD                                    
         DC    F'31175'            SOC,GD                                       
         DC    F'20825'            VO,SS                                        
         DC    F'12465'            GS                                           
         DC    F'42555'            SA                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'31175'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(48,40,2,2)      TV WILDSPOT UNITS 2                          
         DC    F'42555'            PP,CAR,SD                                    
         DC    F'31175'            SOC,GD                                       
         DC    F'20825'            VO,SS                                        
         DC    F'12465'            GS                                           
         DC    F'42555'            SA                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'31175'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(48,40,3,3)      TV WILDSPOT UNITS 4                          
         DC    F'42555'            PP,CAR,SD                                    
         DC    F'31175'            SOC,GD                                       
         DC    F'20825'            VO,SS                                        
         DC    F'12465'            GS                                           
         DC    F'42555'            SA                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'31175'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(48,40,4,4)      TV WILDSPOT UNITS 4                          
         DC    F'42555'            PP,CAR,SD                                    
         DC    F'31175'            SOC,GD                                       
         DC    F'20825'            VO,SS                                        
         DC    F'12465'            GS                                           
         DC    F'42555'            SA                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'31175'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(48,40,5,5)      TV WILDSPOT UNITS 5                          
         DC    F'42555'            PP,CAR,SD                                    
         DC    F'31175'            SOC,GD                                       
         DC    F'20825'            VO,SS                                        
         DC    F'12465'            GS                                           
         DC    F'42555'            SA                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'31175'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,6,6)      TV WILDSPOT UNIT 6                           
         DC    F'43650'                                                         
         DC    F'31890'                                                         
         DC    F'21595'                                                         
         DC    F'12860'                                                         
         DC    F'43650'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'31890'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,7,7)      TV WILDSPOT UNIT 7                           
         DC    F'44810'                                                         
         DC    F'32620'                                                         
         DC    F'22315'                                                         
         DC    F'13300'                                                         
         DC    F'44810'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'32620'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,8,8)      TV WILDSPOT UNIT 8                           
         DC    F'45950'                                                         
         DC    F'33335'                                                         
         DC    F'23065'                                                         
         DC    F'13720'                                                         
         DC    F'45950'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'33335'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,9,9)      TV WILDSPOT UNIT 9                           
         DC    F'47070'                                                         
         DC    F'34035'                                                         
         DC    F'23790'                                                         
         DC    F'14145'                                                         
         DC    F'47070'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'34035'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,10,10)    TV WILDSPOT UNIT 10                          
         DC    F'48215'                                                         
         DC    F'34740'                                                         
         DC    F'24530'                                                         
         DC    F'14570'                                                         
         DC    F'48215'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'34740'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,11,11)    TV WILDSPOT UNIT 11                          
         DC    F'49345'                                                         
         DC    F'35425'                                                         
         DC    F'25250'                                                         
         DC    F'14845'                                                         
         DC    F'49345'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'35425'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,12,12)    TV WILDSPOT UNIT 12                          
         DC    F'50470'                                                         
         DC    F'36150'                                                         
         DC    F'26005'                                                         
         DC    F'15190'                                                         
         DC    F'50470'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'36150'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,13,13)    TV WILDSPOT UNIT 13                          
         DC    F'51650'                                                         
         DC    F'36885'                                                         
         DC    F'26800'                                                         
         DC    F'15455'                                                         
         DC    F'51650'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'36885'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,14,14)    TV WILDSPOT UNIT 14                          
         DC    F'53135'                                                         
         DC    F'37535'                                                         
         DC    F'27525'                                                         
         DC    F'15780'                                                         
         DC    F'53135'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'37535'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,15,15)    TV WILDSPOT UNIT 15                          
         DC    F'53880'                                                         
         DC    F'38280'                                                         
         DC    F'28235'                                                         
         DC    F'16095'                                                         
         DC    F'53880'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'38280'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,16,16)    TV WILDSPOT UNIT 16                          
         DC    F'55015'                                                         
         DC    F'38985'                                                         
         DC    F'29010'                                                         
         DC    F'16350'                                                         
         DC    F'55015'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'38985'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,17,17)    TV WILDSPOT UNIT 17                          
         DC    F'56675'                                                         
         DC    F'39685'                                                         
         DC    F'29730'                                                         
         DC    F'16705'                                                         
         DC    F'56675'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'39685'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,18,18)    TV WILDSPOT UNIT 18                          
         DC    F'57780'                                                         
         DC    F'40380'                                                         
         DC    F'30455'                                                         
         DC    F'16995'                                                         
         DC    F'57780'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'40380'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,19,19)    TV WILDSPOT UNIT 19                          
         DC    F'58380'                                                         
         DC    F'41095'                                                         
         DC    F'31215'                                                         
         DC    F'17300'                                                         
         DC    F'58380'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'41095'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,20,20)    TV WILDSPOT UNIT 20                          
         DC    F'59565'                                                         
         DC    F'41805'                                                         
         DC    F'31945'                                                         
         DC    F'17585'                                                         
         DC    F'59565'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'41805'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,21,21)    TV WILDSPOT UNIT 21                          
         DC    F'60705'                                                         
         DC    F'42370'                                                         
         DC    F'32715'                                                         
         DC    F'17910'                                                         
         DC    F'60705'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'42370'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,22,22)    TV WILDSPOT UNIT 22                          
         DC    F'61845'                                                         
         DC    F'42970'                                                         
         DC    F'33475'                                                         
         DC    F'18205'                                                         
         DC    F'61845'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'42970'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,23,23)    TV WILDSPOT UNIT 23                          
         DC    F'62995'                                                         
         DC    F'43555'                                                         
         DC    F'34200'                                                         
         DC    F'18520'                                                         
         DC    F'62995'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'43555'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,24,24)    TV WILDSPOT UNIT 24                          
         DC    F'64105'                                                         
         DC    F'44055'                                                         
         DC    F'34910'                                                         
         DC    F'18865'                                                         
         DC    F'64105'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'44055'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,25,25)    TV WILDSPOT UNIT 25                          
         DC    F'65215'                                                         
         DC    F'44705'                                                         
         DC    F'35720'                                                         
         DC    F'19080'                                                         
         DC    F'65215'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'44705'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,26,26)    TV WILDSPOT UNIT 26                          
         DC    F'66365'                                                         
         DC    F'45330'                                                         
         DC    F'36130'                                                         
         DC    F'19440'                                                         
         DC    F'66365'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'45330'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,27,27)    TV WILDSPOT UNIT 27                          
         DC    F'67535'                                                         
         DC    F'45875'                                                         
         DC    F'36580'                                                         
         DC    F'19660'                                                         
         DC    F'67535'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'45875'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,28,28)    TV WILDSPOT UNIT 28                          
         DC    F'68640'                                                         
         DC    F'46425'                                                         
         DC    F'37025'                                                         
         DC    F'19915'                                                         
         DC    F'68640'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'46425'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,29,29)    TV WILDSPOT UNIT 29                          
         DC    F'69800'                                                         
         DC    F'47050'                                                         
         DC    F'37470'                                                         
         DC    F'20180'                                                         
         DC    F'69800'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'47050'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,30,30)    TV WILDSPOT UNIT 30                          
         DC    F'70945'                                                         
         DC    F'47610'                                                         
         DC    F'37935'                                                         
         DC    F'20460'                                                         
         DC    F'70945'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'47610'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,31,31)    TV WILDSPOT UNIT 31                          
         DC    F'71805'                                                         
         DC    F'48215'                                                         
         DC    F'38350'                                                         
         DC    F'20710'                                                         
         DC    F'71805'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'48215'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,32,32)    TV WILDSPOT UNIT 32                          
         DC    F'72605'                                                         
         DC    F'48780'                                                         
         DC    F'38835'                                                         
         DC    F'20975'                                                         
         DC    F'72605'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'48780'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,33,33)    TV WILDSPOT UNIT 33                          
         DC    F'73500'                                                         
         DC    F'49345'                                                         
         DC    F'39280'                                                         
         DC    F'21225'                                                         
         DC    F'73500'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'49345'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,34,34)    TV WILDSPOT UNIT 34                          
         DC    F'74365'                                                         
         DC    F'49935'                                                         
         DC    F'39720'                                                         
         DC    F'21540'                                                         
         DC    F'74365'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'49935'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,35,35)    TV WILDSPOT UNIT 35                          
         DC    F'75230'                                                         
         DC    F'50475'                                                         
         DC    F'40165'                                                         
         DC    F'21785'                                                         
         DC    F'75230'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'50475'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,36,36)    TV WILDSPOT UNIT 36                          
         DC    F'76090'                                                         
         DC    F'50920'                                                         
         DC    F'40500'                                                         
         DC    F'22035'                                                         
         DC    F'76090'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'50920'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,37,37)    TV WILDSPOT UNIT 37                          
         DC    F'76925'                                                         
         DC    F'51340'                                                         
         DC    F'40810'                                                         
         DC    F'22300'                                                         
         DC    F'76925'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'51340'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,38,38)    TV WILDSPOT UNIT 38                          
         DC    F'77800'                                                         
         DC    F'51825'                                                         
         DC    F'41175'                                                         
         DC    F'22555'                                                         
         DC    F'77800'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'51825'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,39,39)    TV WILDSPOT UNIT 39                          
         DC    F'78635'                                                         
         DC    F'52210'                                                         
         DC    F'41430'                                                         
         DC    F'22805'                                                         
         DC    F'78635'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'52210'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,40,40)    TV WILDSPOT UNIT 40                          
         DC    F'79505'                                                         
         DC    F'52650'                                                         
         DC    F'41790'                                                         
         DC    F'23095'                                                         
         DC    F'79505'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'52650'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,41,41)    TV WILDSPOT UNIT 41                          
         DC    F'80095'                                                         
         DC    F'52930'                                                         
         DC    F'42075'                                                         
         DC    F'23285'                                                         
         DC    F'80095'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'52930'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,42,42)    TV WILDSPOT UNIT 42                          
         DC    F'80655'                                                         
         DC    F'53510'                                                         
         DC    F'42415'                                                         
         DC    F'23530'                                                         
         DC    F'80655'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'53510'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,43,43)    TV WILDSPOT UNIT 43                          
         DC    F'81235'                                                         
         DC    F'53940'                                                         
         DC    F'42715'                                                         
         DC    F'23715'                                                         
         DC    F'81235'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'53940'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,44,44)    TV WILDSPOT UNIT 44                          
         DC    F'81830'                                                         
         DC    F'54375'                                                         
         DC    F'43010'                                                         
         DC    F'23980'                                                         
         DC    F'81830'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'54375'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,45,45)    TV WILDSPOT UNIT 45                          
         DC    F'82380'                                                         
         DC    F'54780'                                                         
         DC    F'43340'                                                         
         DC    F'24165'                                                         
         DC    F'82380'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'54780'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,46,46)    TV WILDSPOT UNIT 46                          
         DC    F'82990'                                                         
         DC    F'55220'                                                         
         DC    F'43675'                                                         
         DC    F'24435'                                                         
         DC    F'82990'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'55220'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,47,47)    TV WILDSPOT UNIT 47                          
         DC    F'83570'                                                         
         DC    F'55655'                                                         
         DC    F'43990'                                                         
         DC    F'24645'                                                         
         DC    F'83570'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'55655'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,48,48)    TV WILDSPOT UNIT 48                          
         DC    F'84150'                                                         
         DC    F'56085'                                                         
         DC    F'44310'                                                         
         DC    F'24855'                                                         
         DC    F'84150'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'56085'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,49,49)    TV WILDSPOT UNIT 49                          
         DC    F'84725'                                                         
         DC    F'56510'                                                         
         DC    F'44635'                                                         
         DC    F'25075'                                                         
         DC    F'84725'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'56510'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,50,50)    TV WILDSPOT UNIT 50                          
         DC    F'85310'                                                         
         DC    F'56930'                                                         
         DC    F'44965'                                                         
         DC    F'25295'                                                         
         DC    F'85310'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'56930'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,51,255)   TV WILDSPOT UNITS 51+                        
         DC    F'112'                                                           
         DC    F'85'                                                            
         DC    F'64'                                                            
         DC    F'33'                                                            
         DC    F'112'                                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'85'                ST, SAME AS SOC                             
                                                                                
                                                                                
                                                                                
         SPACE 1                                                                
         EJECT                                                                  
         DC    AL1(60,40,1,1)      TV WSP&NET SPC/NETWORK UNITS 1               
         DC    F'59500'            PP,CAR,SD                                    
         DC    F'43640'            SOC,GD                                       
         DC    F'27760'            VO,SS                                        
         DC    F'15885'            GS                                           
         DC    F'59500'            SA                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'43640'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,2,2)      TV WSP&NET SPC/NETWORK UNITS 2               
         DC    F'59500'                                                         
         DC    F'43640'                                                         
         DC    F'27760'                                                         
         DC    F'15885'                                                         
         DC    F'59500'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'43640'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,3,3)      TV WSP&NET SPC/NETWORK UNITS 3               
         DC    F'59500'                                                         
         DC    F'43640'                                                         
         DC    F'27760'                                                         
         DC    F'15885'                                                         
         DC    F'59500'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'43640'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,4,4)      TV WSP&NET SPC/NETWORK UNIT 4                
         DC    F'59500'                                                         
         DC    F'43640'                                                         
         DC    F'27760'                                                         
         DC    F'15885'                                                         
         DC    F'59500'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'43640'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,5,5)      TV WSP&NET SPC/NETWORK UNIT 5                
         DC    F'59500'                                                         
         DC    F'43640'                                                         
         DC    F'27760'                                                         
         DC    F'15885'                                                         
         DC    F'59500'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'43640'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,6,6)      TV WSP&NET SPC/NETWORK UNIT 6                
         DC    F'61080'                                                         
         DC    F'44615'                                                         
         DC    F'28765'                                                         
         DC    F'16375'                                                         
         DC    F'61080'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'44615'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,7,7)      TV WSP&NET SPC/NETWORK UNIT 7                
         DC    F'62665'                                                         
         DC    F'45605'                                                         
         DC    F'29785'                                                         
         DC    F'16915'                                                         
         DC    F'62665'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'45605'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,8,8)      TV WSP&NET SPC/NETWORK UNIT 8                
         DC    F'64300'                                                         
         DC    F'46585'                                                         
         DC    F'30760'                                                         
         DC    F'17440'                                                         
         DC    F'64300'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'46585'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,9,9)      TV WSP&NET SPC/NETWORK UNIT 9                
         DC    F'65900'                                                         
         DC    F'47600'                                                         
         DC    F'31845'                                                         
         DC    F'17990'                                                         
         DC    F'65900'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'47600'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,10,10)    TV WSP&NET SPC/NETWORK UNIT 10               
         DC    F'67440'                                                         
         DC    F'48570'                                                         
         DC    F'32800'                                                         
         DC    F'18490'                                                         
         DC    F'67440'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'48570'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,11,11)    TV WSP&NET SPC/NETWORK UNIT 11               
         DC    F'69080'                                                         
         DC    F'49555'                                                         
         DC    F'33815'                                                         
         DC    F'18895'                                                         
         DC    F'69080'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'49555'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,12,12)    TV WSP&NET SPC/NETWORK UNIT 12               
         DC    F'70640'                                                         
         DC    F'50535'                                                         
         DC    F'34825'                                                         
         DC    F'19310'                                                         
         DC    F'70640'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'50535'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,13,13)    TV WSP&NET SPC/NETWORK UNIT 13               
         DC    F'72215'                                                         
         DC    F'51490'                                                         
         DC    F'35830'                                                         
         DC    F'19720'                                                         
         DC    F'72215'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'51490'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,14,14)    TV WSP&NET SPC/NETWORK UNIT 14               
         DC    F'73850'                                                         
         DC    F'52520'                                                         
         DC    F'36885'                                                         
         DC    F'20150'                                                         
         DC    F'73850'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'52520'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,15,15)    TV WSP&NET SPC/NETWORK UNIT 15               
         DC    F'75435'                                                         
         DC    F'53505'                                                         
         DC    F'37855'                                                         
         DC    F'20535'                                                         
         DC    F'75435'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'53505'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,16,16)    TV WSP&NET SPC/NETWORK UNIT 16               
         DC    F'77015'                                                         
         DC    F'54435'                                                         
         DC    F'38835'                                                         
         DC    F'20920'                                                         
         DC    F'77015'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'54435'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,17,17)    TV WSP&NET SPC/NETWORK UNIT 17               
         DC    F'78585'                                                         
         DC    F'55445'                                                         
         DC    F'39830'                                                         
         DC    F'21365'                                                         
         DC    F'75885'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'55445'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,18,18)    TV WSP&NET SPC/NETWORK UNIT 18               
         DC    F'80205'                                                         
         DC    F'56430'                                                         
         DC    F'40810'                                                         
         DC    F'21750'                                                         
         DC    F'80205'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'56430'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,19,19)    TV WSP&NET SPC/NETWORK UNIT 19               
         DC    F'81750'                                                         
         DC    F'57435'                                                         
         DC    F'41800'                                                         
         DC    F'22120'                                                         
         DC    F'81750'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'57435'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,20,20)    TV WSP&NET SPC/NETWORK UNIT 20               
         DC    F'83365'                                                         
         DC    F'58380'                                                         
         DC    F'42815'                                                         
         DC    F'22685'                                                         
         DC    F'83365'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'58380'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,21,21)    TV WSP&NET SPC/NETWORK UNIT 21               
         DC    F'84980'                                                         
         DC    F'59230'                                                         
         DC    F'43725'                                                         
         DC    F'22910'                                                         
         DC    F'84980'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'59230'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,22,22)    TV WSP&NET SPC/NETWORK UNIT 22               
         DC    F'86565'                                                         
         DC    F'60060'                                                         
         DC    F'44715'                                                         
         DC    F'23245'                                                         
         DC    F'86565'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'60060'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,23,23)    TV WSP&NET SPC/NETWORK UNIT 23               
         DC    F'88150'                                                         
         DC    F'60895'                                                         
         DC    F'45730'                                                         
         DC    F'23590'                                                         
         DC    F'88150'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'60895'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,24,24)    TV WSP&NET SPC/NETWORK UNIT 24               
         DC    F'89705'                                                         
         DC    F'61760'                                                         
         DC    F'46690'                                                         
         DC    F'23955'                                                         
         DC    F'89705'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'61760'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,25,25)    TV WSP&NET SPC/NETWORK UNIT 25               
         DC    F'91345'                                                         
         DC    F'62585'                                                         
         DC    F'47670'                                                         
         DC    F'24320'                                                         
         DC    F'91345'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'62585'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,26,26)    TV WSP&NET SPC/NETWORK UNIT 26               
         DC    F'92895'                                                         
         DC    F'63380'                                                         
         DC    F'48300'                                                         
         DC    F'24670'                                                         
         DC    F'92895'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'63380'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,27,27)    TV WSP&NET SPC/NETWORK UNIT 27               
         DC    F'94490'                                                         
         DC    F'64235'                                                         
         DC    F'48925'                                                         
         DC    F'25020'                                                         
         DC    F'94490'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'64235'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,28,28)    TV WSP&NET SPC/NETWORK UNIT 28               
         DC    F'95985'                                                         
         DC    F'65055'                                                         
         DC    F'49520'                                                         
         DC    F'25365'                                                         
         DC    F'95985'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'65055'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,29,29)    TV WSP&NET SPC/NETWORK UNIT 29               
         DC    F'97585'                                                         
         DC    F'65920'                                                         
         DC    F'50130'                                                         
         DC    F'25685'                                                         
         DC    F'97585'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'65920'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,30,30)    TV WSP&NET SPC/NETWORK UNIT 30               
         DC    F'99140'                                                         
         DC    F'66725'                                                         
         DC    F'50750'                                                         
         DC    F'26070'                                                         
         DC    F'99140'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'66725'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,31,31)    TV WSP&NET SPC/NETWORK UNIT 31               
         DC    F'100395'                                                        
         DC    F'67555'                                                         
         DC    F'51335'                                                         
         DC    F'26425'                                                         
         DC    F'100395'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'67555'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,32,32)    TV WSP&NET SPC/NETWORK UNIT 32               
         DC    F'101565'                                                        
         DC    F'68330'                                                         
         DC    F'51910'                                                         
         DC    F'26725'                                                         
         DC    F'101565'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'68330'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,33,33)    TV WSP&NET SPC/NETWORK UNIT 33               
         DC    F'102760'                                                        
         DC    F'69140'                                                         
         DC    F'52550'                                                         
         DC    F'27055'                                                         
         DC    F'102760'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'69140'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,34,34)    TV WSP&NET SPC/NETWORK UNIT 34               
         DC    F'104010'                                                        
         DC    F'69685'                                                         
         DC    F'53130'                                                         
         DC    F'27365'                                                         
         DC    F'104010'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'69685'           ST, SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(60,40,35,35)    TV WSP&NET SPC/NETWORK UNIT 35               
         DC    F'105195'                                                        
         DC    F'70735'                                                         
         DC    F'53780'                                                         
         DC    F'27695'                                                         
         DC    F'105195'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'70735'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,36,36)    TV WSP&NET SPC/NETWORK UNIT 36               
         DC    F'106405'                                                        
         DC    F'71380'                                                         
         DC    F'54180'                                                         
         DC    F'28050'                                                         
         DC    F'106405'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'71380'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,37,37)    TV WSP&NET SPC/NETWORK UNIT 37               
         DC    F'107615'                                                        
         DC    F'71990'                                                         
         DC    F'54585'                                                         
         DC    F'28375'                                                         
         DC    F'107615'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'71990'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,38,38)    TV WSP&NET SPC/NETWORK UNIT 38               
         DC    F'108820'                                                        
         DC    F'72585'                                                         
         DC    F'54975'                                                         
         DC    F'28715'                                                         
         DC    F'108820'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'72585'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,39,39)    TV WSP&NET SPC/NETWORK UNIT 39               
         DC    F'110060'                                                        
         DC    F'73170'                                                         
         DC    F'55375'                                                         
         DC    F'29025'                                                         
         DC    F'110060'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'73170'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,40,40)    TV WSP&NET SPC/NETWORK UNIT 40               
         DC    F'111235'                                                        
         DC    F'73795'                                                         
         DC    F'55795'                                                         
         DC    F'29345'                                                         
         DC    F'111235'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'73795'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,41,41)    TV WSP&NET SPC/NETWORK UNIT 41               
         DC    F'112060'                                                        
         DC    F'74365'                                                         
         DC    F'56185'                                                         
         DC    F'29585'                                                         
         DC    F'112060'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'74365'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,42,42)    TV WSP&NET SPC/NETWORK UNIT 42               
         DC    F'112845'                                                        
         DC    F'74945'                                                         
         DC    F'56595'                                                         
         DC    F'29900'                                                         
         DC    F'112845'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'74945'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,43,43)    TV WSP&NET SPC/NETWORK UNIT 43               
         DC    F'113645'                                                        
         DC    F'75575'                                                         
         DC    F'56965'                                                         
         DC    F'30185'                                                         
         DC    F'113645'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'75575'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,44,44)    TV WSP&NET SPC/NETWORK UNIT 44               
         DC    F'114495'                                                        
         DC    F'76140'                                                         
         DC    F'57380'                                                         
         DC    F'30450'                                                         
         DC    F'114495'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'76140'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,45,45)    TV WSP&NET SPC/NETWORK UNIT 45               
         DC    F'116125'                                                        
         DC    F'77310'                                                         
         DC    F'58155'                                                         
         DC    F'30995'                                                         
         DC    F'116125'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'77310'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,46,46)    TV WSP&NET SPC/NETWORK UNIT 46               
         DC    F'116125'                                                        
         DC    F'77310'                                                         
         DC    F'58155'                                                         
         DC    F'30995'                                                         
         DC    F'116125'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'77310'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,47,47)    TV WSP&NET SPC/NETWORK UNIT 47               
         DC    F'116895'                                                        
         DC    F'77845'                                                         
         DC    F'58565'                                                         
         DC    F'31215'                                                         
         DC    F'116895'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'77845'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,48,48)    TV WSP&NET SPC/NETWORK UNIT 48               
         DC    F'117700'                                                        
         DC    F'78385'                                                         
         DC    F'58930'                                                         
         DC    F'31505'                                                         
         DC    F'117700'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'78385'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,49,49)    TV WSP&NET SPC/NETWORK UNIT 49               
         DC    F'118515'                                                        
         DC    F'79025'                                                         
         DC    F'59315'                                                         
         DC    F'31710'                                                         
         DC    F'118515'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'79025'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,50,50)    TV WSP&NET SPC/NETWORK UNIT 50               
         DC    F'119320'                                                        
         DC    F'79585'                                                         
         DC    F'59725'                                                         
         DC    F'32010'                                                         
         DC    F'119320'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'79585'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,51,255)   TV WSP&NET/NETWORK UNITS 51+                 
         DC    F'155'                                                           
         DC    F'118'                                                           
         DC    F'77'                                                            
         DC    F'43'                                                            
         DC    F'155'                                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'118'              ST, SAME AS SOC                              
         SPACE 1                                                                
CAUTTBL  DC    AL1(68,44,0,0)      AUDITION INTERVIEW - TV                      
         DC    F'2500'             PP,CAR,SD                                    
         DC    F'2500'             SOC,GD                                       
         DC    F'2500'             VO,SS                                        
         DC    F'2500'             GS                                           
         DC    F'2500'             SA                                           
         DC    F'2500'             DEM                                          
         DC    F'2500'             E                                            
         DC    F'2500'             GE                                           
         DC    F'2500'             ST                                           
         DC    F'2500'             EVERYBODY ELSE-US,SI,SB,PT                   
         SPACE 1                                                                
CAURTBL  DC    AL1(69,12,0,0)      AUDITION INTERVIEW - RADIO                   
         DC    F'2500'             SS,SV                                        
         DC    F'2500'             MV,GS                                        
         EJECT                                                                  
CDMTBL   DC    AL1(70,44,2,2)      TV PRESENTATION DEMO 2                       
         DC    F'21250'            PP,CAR,SD                                    
         DC    F'21250'            SOC,GD                                       
         DC    F'21250'            VO,SS                                        
         DC    F'21250'            GS                                           
         DC    F'21250'            SA                                           
         DC    F'21250'            DEM                                          
         DC    F'21250'            E                                            
         DC    F'21250'            GE                                           
         DC    F'21250'            ST                                           
         DC    F'21250'            ALL ELSE-US,SI,SB,PT                         
         SPACE 1                                                                
         DC    AL1(70,44,3,255)    TV PRESENTATION DEMO 3+                      
         DC    F'10650'            PP,CAR,SD                                    
         DC    F'10650'            SOC,GD                                       
         DC    F'10650'            VO,SS                                        
         DC    F'10650'            GS                                           
         DC    F'10650'            SA                                           
         DC    F'10650'            DEM                                          
         DC    F'10650'            E                                            
         DC    F'10650'            GE                                           
         DC    F'10650'            ST                                           
         DC    F'10650'            ALL ELSE-US,SI,SB,PT                         
         SPACE 1                                                                
         DC    X'FF'               *** END OF USE RATE TABLES ***               
         EJECT                                                                  
*              USE LOOK UP TABLES                                               
         SPACE 2                                                                
USELUT   DS    0CL5                                                             
         DC    AL1(0,UBSC,ALL,ACT,0,0,0,TV)     TV SESSION                      
         DC    AL1(24,UBSC,ALL,ACT,0,0,0,RADIO) RADIO SESSION                   
         DC    AL1(12,UDOR,ALL,ACT,0,0,0,TV)    TV DORMANCY                     
         DC    AL1(48,UWSC,ALL,ACT,0,0,0,TV)    TV WILDSPOT                     
         DC    AL1(60,UWSM,ALL,ACT,0,0,0,TV)    TV WILDSPOT COMBINED            
         DC    AL1(60,UNET,ALL,ACT,0,0,0,TV)    TV NETWORK                      
         DC    AL1(68,UCAU,ALL,ACT,0,0,0,TV)    AUDITION                        
         DC    AL1(69,UCAU,ALL,ACT,0,0,0,RADIO) AUDITION                        
         DC    AL1(70,UCDM,ALL,ACT,0,0,0,TV)    TV PRESENTATION DEMO            
         DC    X'FF'                                                            
         EJECT                                                                  
*              PERCENTAGE TABLES FOR MULTITRACKING & SWEETENING                 
         SPACE 3                                                                
MSWEET   DS    0CL3                                                             
         DC    X'FF',AL2(50)                            ALL OTHERS              
         SPACE 3                                                                
*              MAJORS TABLE                                                     
         SPACE 3                                                                
MAJLUT   DS    0CL2                                                             
         DC    AL1(0,0)                                                         
         DC    AL1(1,NY)                                                        
         DC    AL1(2,CHI)                                                       
         DC    AL1(2,LA)                                                        
         DC    AL1(3,NY+CHI)                                                    
         DC    AL1(3,NY+LA)                                                     
         DC    AL1(3,CHI+LA)                                                    
         DC    AL1(4,NY+CHI+LA)                                                 
         EJECT                                                                  
*              COLUMN TABLES - TV - ON CAMERA                                   
         SPACE 1                                                                
ONCOLS   DS    0CL2                                                             
         DC    AL1(1,CTPP)         PRINCIPAL PERFORMER                          
         DC    AL1(1,CTSD)         SOLO DANCER                                  
         DC    AL1(1,CTCAR)        CARTOONIST                                   
         DC    AL1(2,CTSOC)        SILENT ON-CAMERA                             
         DC    AL1(2,CTGD)         GROUP DANCER                                 
         DC    AL1(3,CTSS)         SOLO SINGER                                  
         DC    AL1(4,CTGS)         GROUP SINGER                                 
         DC    AL1(5,CTSA)         SPECIALTY ACT                                
         DC    AL1(6,CTDEM)        DEMONSTRATOR                                 
         DC    AL1(7,CTE)          EXTRA                                        
         DC    AL1(8,CTGE)         GROUP EXTRA                                  
         DC    AL1(9,CTST)         STUNT PERFORMER                              
         DC    AL1(10,CTUS)        UNDERSTUDY                                   
         DC    AL1(10,CTSI)        STANDIN                                      
         DC    AL1(10,CTSB)        STANDBY                                      
         DC    AL1(10,CTPT)        PUPPETEER                                    
*                                  IF DEFINE ROW >=18, CHANGE SYSCALC           
         DC    AL1(60,CTSSM)       SOLO/DUE WITH MULTI                          
         DC    AL1(60,CTSS1)       SOLO/DUO SWEETENING                          
         DC    AL1(60,CTSS2)       SOLO/DUO SWEETENING                          
         DC    AL1(60,CTSS3)       SOLO/DUO SWEETENING                          
         DC    AL1(60,CTSS4)       SOLO/DUO SWEETENING                          
         DC    AL1(60,CTGSM)       GROUP SINGERS WITH MULTI                     
         DC    AL1(60,CTGSS)       GROUP SINGERS SWEETENING                     
         DC    AL1(60,CTZZZ)       OTHER CATEGORY - ROW SHOULDN'T EXIST         
         DC    X'FF'                                                            
         EJECT                                                                  
*              COLUMN TABLES - TV - OFF CAMERA                                  
         SPACE 1                                                                
OFFCOLS  DS    0CL2                                                             
         DC    AL1(1,CTCAR)        CARTOONIST                                   
         DC    AL1(2,CTGD)         GROUP DANCER                                 
         DC    AL1(3,CTSS)         SOLO SINGER                                  
         DC    AL1(3,CTVO)         VOICE OVER                                   
         DC    AL1(4,CTGS)         GROUP SINGER                                 
         DC    AL1(5,CTSA)         SPECIALTY ACT                                
*                                  IF DEFINE ROW >=18, CHANGE SYSCALC           
         DC    AL1(60,CTSSM)       SOLO/DUE WITH MULTI                          
         DC    AL1(60,CTSS1)       SOLO/DUO SWEETENING                          
         DC    AL1(60,CTSS2)       SOLO/DUO SWEETENING                          
         DC    AL1(60,CTSS3)       SOLO/DUO SWEETENING                          
         DC    AL1(60,CTSS4)       SOLO/DUO SWEETENING                          
         DC    AL1(60,CTGSM)       GROUP SINGERS WITH MULTI                     
         DC    AL1(60,CTGSS)       GROUP SINGERS SWEETENING                     
         DC    AL1(60,CTSV)        SINGLE OR SINGER VOICE                       
         DC    AL1(60,CTMV)        MULTIPLE VOICE                               
         DC    AL1(60,CTZZZ)       OTHER CATEGORY - ROW SHOULDN'T EXIST         
         DC    X'FF'                                                            
         EJECT                                                                  
*              COLUMN TABLES - RADIO                                            
         SPACE 1                                                                
RADCOLS  DS    0CL2                                                             
         DC    AL1(1,CTSV)         SINGLE OR SINGER VOICE                       
         DC    AL1(1,CTSS)         SOLO SINGER                                  
         DC    AL1(2,CTGS)         GROUP SINGER                                 
         DC    AL1(2,CTMV)         MULTIPLE VOICE                               
*                                  IF DEFINE ROW >=9, CHANGE SYSCALC            
         DC    AL1(60,CTVO)        VOICE OVER                                   
         DC    AL1(60,CTSSM)       SOLO/DUE WITH MULTI                          
         DC    AL1(60,CTSS1)       SOLO/DUO SWEETENING                          
         DC    AL1(60,CTSS2)       SOLO/DUO SWEETENING                          
         DC    AL1(60,CTSS3)       SOLO/DUO SWEETENING                          
         DC    AL1(60,CTSS4)       SOLO/DUO SWEETENING                          
         DC    AL1(60,CTGSM)       GROUP SINGERS WITH MULTI                     
         DC    AL1(60,CTGSS)       GROUP SINGERS SWEETENING                     
         DC    AL1(60,CTGD)        GROUP DANCER                                 
         DC    AL1(60,CTSA)        SPECIALTY ACT                                
         DC    AL1(60,CTCAR)       CARTOONIST                                   
         DC    AL1(60,CTZZZ)       OTHER CATEGORY - ROW SHOULDN'T EXIST         
         DC    X'FF'                                                            
         EJECT                                                                  
*              COLUMN TABLES - AFM                                              
         SPACE 3                                                                
AFMCOLS  DS    0CL2                                                             
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE TASYSEQUS                                                      
         EJECT                                                                  
* TASYSDSECT                                                                    
* TAGENFILE                                                                     
         SPACE 1                                                                
         PRINT OFF                                                              
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003TAGEN60   08/12/03'                                      
         END                                                                    
