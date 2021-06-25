*          DATA SET TAGEN71    AT LEVEL 006 AS OF 02/12/09                      
*PHASE T70271A,*                                                                
         TITLE 'T70271 - TABLES FOR YEAR 2 CANADIAN CONTRACT'                   
T70271   CSECT                                                                  
         DC    AL4(USETBLS-T70271)                                              
         DC    AL4(USELUT-T70271)                                               
         DC    AL4(MAJLUT-T70271)                                               
         DC    AL4(AFMCOLS-T70271)                                              
         DC    AL4(RADCOLS-T70271)                                              
         DC    AL4(OFFCOLS-T70271)                                              
         DC    AL4(ONCOLS-T70271)                                               
         DC    AL4(MSWEET-T70271)                                               
         DC    A(0)                                                             
         EJECT                                                                  
*              USE TYPE RATE TABLES                                             
         SPACE 3                                                                
USETBLS  DS    0F                                                               
BSCTTBL  DC    AL1(0,40,0,0)       NATIONAL TV                                  
         DC    F'67200'            PP,SD,CAR                                    
         DC    F'67200'            SOC,GD                                       
         DC    F'49000'            VO,SS                                        
         DC    F'21200'            GS                                           
         DC    F'100800'           SA = PP+50%                                  
         DC    F'67200'            DEM                                          
         DC    F'41100'            E                                            
         DC    F'27450'            GE                                           
         DC    F'67200'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(1,40,0,0)       SHORT LIFE TV 7                              
         DC    F'67200'                                                         
         DC    F'67200'                                                         
         DC    F'49000'                                                         
         DC    F'21200'                                                         
         DC    F'100800'                                                        
         DC    F'67200'                                                         
         DC    F'41100'                                                         
         DC    F'27450'                                                         
         DC    F'67200'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(2,40,0,0)       SHORT LIFE TV 14                             
         DC    F'67200'                                                         
         DC    F'67200'                                                         
         DC    F'49000'                                                         
         DC    F'21200'                                                         
         DC    F'100800'                                                        
         DC    F'67200'                                                         
         DC    F'41100'                                                         
         DC    F'27450'                                                         
         DC    F'67200'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(3,40,0,0)       SHORT LIFE TV 31                             
         DC    F'67200'                                                         
         DC    F'67200'                                                         
         DC    F'49000'                                                         
         DC    F'21200'                                                         
         DC    F'100800'                                                        
         DC    F'67200'                                                         
         DC    F'41100'                                                         
         DC    F'27450'                                                         
         DC    F'67200'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(4,40,0,0)       SHORT LIFE TV 45                             
         DC    F'67200'                                                         
         DC    F'67200'                                                         
         DC    F'49000'                                                         
         DC    F'21200'                                                         
         DC    F'100800'                                                        
         DC    F'67200'                                                         
         DC    F'41100'                                                         
         DC    F'27450'                                                         
         DC    F'67200'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(5,36,0,0)       LOCAL AND REGIONAL TV 1                      
         DC    F'47400'                                                         
         DC    F'45720'                                                         
         DC    F'23480'                                                         
         DC    F'17620'                                                         
         DC    F'71100'                                                         
         DC    F'0'                                                             
         DC    F'26860'                                                         
         DC    F'26860'                                                         
         SPACE 1                                                                
         DC    AL1(6,36,0,0)       LOCAL AND REGIONAL TV 2                      
         DC    F'39330'                                                         
         DC    F'37770'                                                         
         DC    F'13900'                                                         
         DC    F'8340'                                                          
         DC    F'59000'                                                         
         DC    F'0'                                                             
         DC    F'13530'                                                         
         DC    F'13530'                                                         
         SPACE 1                                                                
         DC    AL1(7,36,0,0)       LOCAL AND REGIONAL TV 3                      
         DC    F'34070'                                                         
         DC    F'32430'                                                         
         DC    F'10820'                                                         
         DC    F'6470'                                                          
         DC    F'51110'                                                         
         DC    F'0'                                                             
         DC    F'13530'                                                         
         DC    F'13530'                                                         
         SPACE 1                                                                
         DC    AL1(8,40,0,0)       VIDEO                                        
         DC    F'32800'                                                         
         DC    F'32800'                                                         
         DC    F'24100'                                                         
         DC    F'10800'                                                         
         DC    F'49200'                                                         
         DC    F'32800'                                                         
         DC    F'20500'                                                         
         DC    F'13800'                                                         
         DC    F'32800'            ST, SAME AS SOC                              
         EJECT                                                                  
DORTTBL  DC    AL1(12,40,0,0)      NATIONAL TV                                  
         DC    F'67200'            PP,SD,CAR                                    
         DC    F'67200'            SOC,GD                                       
         DC    F'49000'            VO,SS                                        
         DC    F'21200'            GS                                           
         DC    F'100800'           SA = PP+50%                                  
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'67200'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(13,40,0,0)      SHORT LIFE TV 7                              
         DC    F'67200'                                                         
         DC    F'67200'                                                         
         DC    F'49000'                                                         
         DC    F'21200'                                                         
         DC    F'100800'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'67200'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(14,40,0,0)      SHORT LIFE TV 14                             
         DC    F'67200'                                                         
         DC    F'67200'                                                         
         DC    F'49000'                                                         
         DC    F'21200'                                                         
         DC    F'100800'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'67200'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(15,40,0,0)      SHORT LIFE TV 31                             
         DC    F'67200'                                                         
         DC    F'67200'                                                         
         DC    F'49000'                                                         
         DC    F'21200'                                                         
         DC    F'100800'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'67200'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(16,40,0,0)      SHORT LIFE TV 45                             
         DC    F'67200'                                                         
         DC    F'67200'                                                         
         DC    F'49000'                                                         
         DC    F'21200'                                                         
         DC    F'100800'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'67200'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(17,24,0,0)      LOCAL AND REGIONAL TV 1                      
         DC    F'47500'                                                         
         DC    F'45720'                                                         
         DC    F'23480'                                                         
         DC    F'17620'                                                         
         DC    F'71250'                                                         
         SPACE 1                                                                
         DC    AL1(18,24,0,0)      LOCAL AND REGIONAL TV 2                      
         DC    F'39330'                                                         
         DC    F'37770'                                                         
         DC    F'13900'                                                         
         DC    F'8340'                                                          
         DC    F'59000'                                                         
         SPACE 1                                                                
         DC    AL1(19,24,0,0)      LOCAL AND REGIONAL TV 3                      
         DC    F'34070'                                                         
         DC    F'32430'                                                         
         DC    F'10820'                                                         
         DC    F'6470'                                                          
         DC    F'51110'                                                         
         EJECT                                                                  
BSCRTBL  DC    AL1(24,12,0,0)      NATIONAL RADIO                               
         DC    F'52325'            SV,SS                                        
         DC    F'39300'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(25,12,0,0)      SHORT LIFE RADIO 7                           
         DC    F'26675'                                                         
         DC    F'20050'                                                         
         SPACE 1                                                                
         DC    AL1(26,12,0,0)      SHORT LIFE RADIO 14                          
         DC    F'35275'                                                         
         DC    F'26400'                                                         
         SPACE 1                                                                
         DC    AL1(27,12,0,0)      SHORT LIFE RADIO 31                          
         DC    F'40025'                                                         
         DC    F'30100'                                                         
         SPACE 1                                                                
         DC    AL1(28,12,0,0)      SHORT LIFE RADIO 45                          
         DC    F'45350'                                                         
         DC    F'34050'                                                         
         SPACE 1                                                                
         DC    AL1(29,12,0,0)      LOCAL AND REGIONAL RADIO 1                   
         DC    F'38450'                                                         
         DC    F'28990'                                                         
         SPACE 1                                                                
         DC    AL1(30,12,0,0)      LOCAL AND REGIONAL RADIO 2                   
         DC    F'24985'                                                         
         DC    F'15015'                                                         
         SPACE 1                                                                
         DC    AL1(31,12,0,0)      LOCAL AND REGIONAL RADIO 3                   
         DC    F'22355'                                                         
         DC    F'13410'                                                         
         SPACE 1                                                                
         DC    AL1(33,12,0,0)      AUDIO                                        
         DC    F'25600'                                                         
         DC    F'19500'                                                         
         EJECT                                                                  
WSCTBL   DC    AL1(48,40,1,1)      TV WILDSPOT UNITS 1                          
         DC    F'52360'            PP,CAR,SD                                    
         DC    F'38370'            SOC,GD                                       
         DC    F'25625'            VO,SS                                        
         DC    F'15345'            GS                                           
         DC    F'52360'            SA                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'38370'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(48,40,2,2)      TV WILDSPOT UNITS 2                          
         DC    F'52360'            PP,CAR,SD                                    
         DC    F'38370'            SOC,GD                                       
         DC    F'25625'            VO,SS                                        
         DC    F'15345'            GS                                           
         DC    F'52360'            SA                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'38370'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(48,40,3,3)      TV WILDSPOT UNITS 3                          
         DC    F'52360'            PP,CAR,SD                                    
         DC    F'38370'            SOC,GD                                       
         DC    F'25625'            VO,SS                                        
         DC    F'15345'            GS                                           
         DC    F'52360'            SA                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'38370'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(48,40,4,4)      TV WILDSPOT UNITS 4                          
         DC    F'52360'            PP,CAR,SD                                    
         DC    F'38370'            SOC,GD                                       
         DC    F'25625'            VO,SS                                        
         DC    F'15345'            GS                                           
         DC    F'52360'            SA                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'38370'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(48,40,5,5)      TV WILDSPOT UNITS 5                          
         DC    F'52360'            PP,CAR,SD                                    
         DC    F'38370'            SOC,GD                                       
         DC    F'25625'            VO,SS                                        
         DC    F'15345'            GS                                           
         DC    F'52360'            SA                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'38370'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,6,6)      TV WILDSPOT UNIT 6                           
         DC    F'53725'                                                         
         DC    F'39250'                                                         
         DC    F'26570'                                                         
         DC    F'15820'                                                         
         DC    F'53725'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'39250'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,7,7)      TV WILDSPOT UNIT 7                           
         DC    F'55145'                                                         
         DC    F'40145'                                                         
         DC    F'27470'                                                         
         DC    F'16370'                                                         
         DC    F'55145'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'40145'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,8,8)      TV WILDSPOT UNIT 8                           
         DC    F'56550'                                                         
         DC    F'41020'                                                         
         DC    F'28385'                                                         
         DC    F'16885'                                                         
         DC    F'56550'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'41020'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,9,9)      TV WILDSPOT UNIT 9                           
         DC    F'57930'                                                         
         DC    F'41885'                                                         
         DC    F'29270'                                                         
         DC    F'17415'                                                         
         DC    F'57930'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'41885'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,10,10)    TV WILDSPOT UNIT 10                          
         DC    F'59335'                                                         
         DC    F'42755'                                                         
         DC    F'30180'                                                         
         DC    F'17920'                                                         
         DC    F'59335'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'42755'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,11,11)    TV WILDSPOT UNIT 11                          
         DC    F'60730'                                                         
         DC    F'43600'                                                         
         DC    F'31075'                                                         
         DC    F'18270'                                                         
         DC    F'60730'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'43600'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,12,12)    TV WILDSPOT UNIT 12                          
         DC    F'62110'                                                         
         DC    F'44490'                                                         
         DC    F'32000'                                                         
         DC    F'18690'                                                         
         DC    F'62110'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'44490'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,13,13)    TV WILDSPOT UNIT 13                          
         DC    F'63555'                                                         
         DC    F'45395'                                                         
         DC    F'32985'                                                         
         DC    F'19025'                                                         
         DC    F'63555'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'45395'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,14,14)    TV WILDSPOT UNIT 14                          
         DC    F'65395'                                                         
         DC    F'46185'                                                         
         DC    F'33870'                                                         
         DC    F'19415'                                                         
         DC    F'65395'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'46185'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,15,15)    TV WILDSPOT UNIT 15                          
         DC    F'66315'                                                         
         DC    F'47105'                                                         
         DC    F'34745'                                                         
         DC    F'19805'                                                         
         DC    F'66315'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'47105'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,16,16)    TV WILDSPOT UNIT 16                          
         DC    F'67705'                                                         
         DC    F'47975'                                                         
         DC    F'35700'                                                         
         DC    F'20115'                                                         
         DC    F'67705'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'47975'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,17,17)    TV WILDSPOT UNIT 17                          
         DC    F'69750'                                                         
         DC    F'48835'                                                         
         DC    F'36585'                                                         
         DC    F'20560'                                                         
         DC    F'69750'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'48835'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,18,18)    TV WILDSPOT UNIT 18                          
         DC    F'71110'                                                         
         DC    F'49700'                                                         
         DC    F'37480'                                                         
         DC    F'20920'                                                         
         DC    F'71110'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'49700'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,19,19)    TV WILDSPOT UNIT 19                          
         DC    F'71840'                                                         
         DC    F'50575'                                                         
         DC    F'38410'                                                         
         DC    F'21285'                                                         
         DC    F'71840'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'50575'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,20,20)    TV WILDSPOT UNIT 20                          
         DC    F'73300'                                                         
         DC    F'51445'                                                         
         DC    F'39315'                                                         
         DC    F'21540'                                                         
         DC    F'73300'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'51445'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,21,21)    TV WILDSPOT UNIT 21                          
         DC    F'74705'                                                         
         DC    F'52135'                                                         
         DC    F'40260'                                                         
         DC    F'22045'                                                         
         DC    F'74705'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'52135'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,22,22)    TV WILDSPOT UNIT 22                          
         DC    F'76100'                                                         
         DC    F'52885'                                                         
         DC    F'41195'                                                         
         DC    F'22405'                                                         
         DC    F'76100'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'52885'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,23,23)    TV WILDSPOT UNIT 23                          
         DC    F'77520'                                                         
         DC    F'53595'                                                         
         DC    F'42090'                                                         
         DC    F'22785'                                                         
         DC    F'77520'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'53595'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,24,24)    TV WILDSPOT UNIT 24                          
         DC    F'78885'                                                         
         DC    F'54215'                                                         
         DC    F'42970'                                                         
         DC    F'23210'                                                         
         DC    F'78885'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'54215'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,25,25)    TV WILDSPOT UNIT 25                          
         DC    F'80265'                                                         
         DC    F'55015'                                                         
         DC    F'43955'                                                         
         DC    F'23480'                                                         
         DC    F'80265'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'55015'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,26,26)    TV WILDSPOT UNIT 26                          
         DC    F'81665'                                                         
         DC    F'55775'                                                         
         DC    F'44470'                                                         
         DC    F'23930'                                                         
         DC    F'81665'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'55775'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,27,27)    TV WILDSPOT UNIT 27                          
         DC    F'83100'                                                         
         DC    F'56465'                                                         
         DC    F'45020'                                                         
         DC    F'24195'                                                         
         DC    F'83100'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'56465'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,28,28)    TV WILDSPOT UNIT 28                          
         DC    F'84470'                                                         
         DC    F'57140'                                                         
         DC    F'45570'                                                         
         DC    F'24515'                                                         
         DC    F'84470'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'57140'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,29,29)    TV WILDSPOT UNIT 29                          
         DC    F'85895'                                                         
         DC    F'57905'                                                         
         DC    F'46115'                                                         
         DC    F'24840'                                                         
         DC    F'85895'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'57905'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,30,30)    TV WILDSPOT UNIT 30                          
         DC    F'87310'                                                         
         DC    F'58590'                                                         
         DC    F'46685'                                                         
         DC    F'25180'                                                         
         DC    F'87310'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'58590'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,31,31)    TV WILDSPOT UNIT 31                          
         DC    F'88365'                                                         
         DC    F'59335'                                                         
         DC    F'47195'                                                         
         DC    F'25485'                                                         
         DC    F'88365'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'59335'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,32,32)    TV WILDSPOT UNIT 32                          
         DC    F'89345'                                                         
         DC    F'60035'                                                         
         DC    F'47785'                                                         
         DC    F'25820'                                                         
         DC    F'89345'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'60035'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,33,33)    TV WILDSPOT UNIT 33                          
         DC    F'90450'                                                         
         DC    F'60730'                                                         
         DC    F'48335'                                                         
         DC    F'26120'                                                         
         DC    F'90450'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'60730'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,34,34)    TV WILDSPOT UNIT 34                          
         DC    F'91515'                                                         
         DC    F'61455'                                                         
         DC    F'48885'                                                         
         DC    F'26505'                                                         
         DC    F'91515'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'61455'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,35,35)    TV WILDSPOT UNIT 35                          
         DC    F'92585'                                                         
         DC    F'62115'                                                         
         DC    F'49430'                                                         
         DC    F'26810'                                                         
         DC    F'92585'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'62115'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,36,36)    TV WILDSPOT UNIT 36                          
         DC    F'93640'                                                         
         DC    F'62665'                                                         
         DC    F'49845'                                                         
         DC    F'27105'                                                         
         DC    F'93640'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'62665'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,37,37)    TV WILDSPOT UNIT 37                          
         DC    F'94670'                                                         
         DC    F'63175'                                                         
         DC    F'50215'                                                         
         DC    F'27450'                                                         
         DC    F'94670'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'63175'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,38,38)    TV WILDSPOT UNIT 38                          
         DC    F'95750'                                                         
         DC    F'63780'                                                         
         DC    F'50675'                                                         
         DC    F'27760'                                                         
         DC    F'95750'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'63780'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,39,39)    TV WILDSPOT UNIT 39                          
         DC    F'96780'                                                         
         DC    F'64250'                                                         
         DC    F'50995'                                                         
         DC    F'28070'                                                         
         DC    F'96780'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'64250'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,40,40)    TV WILDSPOT UNIT 40                          
         DC    F'97850'                                                         
         DC    F'64795'                                                         
         DC    F'51425'                                                         
         DC    F'28425'                                                         
         DC    F'97850'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'64795'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,41,41)    TV WILDSPOT UNIT 41                          
         DC    F'98560'                                                         
         DC    F'65140'                                                         
         DC    F'51780'                                                         
         DC    F'28660'                                                         
         DC    F'98560'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'65140'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,42,42)    TV WILDSPOT UNIT 42                          
         DC    F'99260'                                                         
         DC    F'65850'                                                         
         DC    F'52195'                                                         
         DC    F'28960'                                                         
         DC    F'99260'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'65850'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,43,43)    TV WILDSPOT UNIT 43                          
         DC    F'99970'                                                         
         DC    F'66385'                                                         
         DC    F'52565'                                                         
         DC    F'29190'                                                         
         DC    F'99970'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'66385'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,44,44)    TV WILDSPOT UNIT 44                          
         DC    F'100700'                                                        
         DC    F'66915'                                                         
         DC    F'52925'                                                         
         DC    F'29510'                                                         
         DC    F'100700'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'66915'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,45,45)    TV WILDSPOT UNIT 45                          
         DC    F'101380'                                                        
         DC    F'67415'                                                         
         DC    F'53340'                                                         
         DC    F'29745'                                                         
         DC    F'101380'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'67415'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,46,46)    TV WILDSPOT UNIT 46                          
         DC    F'102130'                                                        
         DC    F'67955'                                                         
         DC    F'53750'                                                         
         DC    F'30080'                                                         
         DC    F'102130'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'67955'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,47,47)    TV WILDSPOT UNIT 47                          
         DC    F'102840'                                                        
         DC    F'68495'                                                         
         DC    F'54135'                                                         
         DC    F'30335'                                                         
         DC    F'102840'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'68495'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,48,48)    TV WILDSPOT UNIT 48                          
         DC    F'103560'                                                        
         DC    F'69015'                                                         
         DC    F'54535'                                                         
         DC    F'30595'                                                         
         DC    F'103560'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'69015'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,49,49)    TV WILDSPOT UNIT 49                          
         DC    F'104265'                                                        
         DC    F'69540'                                                         
         DC    F'54930'                                                         
         DC    F'30860'                                                         
         DC    F'104265'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'69540'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,50,50)    TV WILDSPOT UNIT 50                          
         DC    F'104985'                                                        
         DC    F'70060'                                                         
         DC    F'55340'                                                         
         DC    F'31130'                                                         
         DC    F'104985'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'70060'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,51,255)   TV WILDSPOT UNITS 51+                        
         DC    F'136'                                                           
         DC    F'106'                                                           
         DC    F'78'                                                            
         DC    F'42'                                                            
         DC    F'136'                                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'106'               ST, SAME AS SOC                             
                                                                                
                                                                                
                                                                                
         SPACE 1                                                                
         EJECT                                                                  
         DC    AL1(60,40,1,1)      TV WSP&NET SPC/NETWORK UNITS 1               
         DC    F'73215'            PP,CAR,SD                                    
         DC    F'53715'            SOC,GD                                       
         DC    F'34165'            VO,SS                                        
         DC    F'19545'            GS                                           
         DC    F'73215'            SA                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'53715'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,2,2)      TV WSP&NET SPC/NETWORK UNITS 2               
         DC    F'73215'                                                         
         DC    F'53715'                                                         
         DC    F'34165'                                                         
         DC    F'19545'                                                         
         DC    F'73215'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'53715'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,3,3)      TV WSP&NET SPC/NETWORK UNITS 3               
         DC    F'73215'                                                         
         DC    F'53715'                                                         
         DC    F'34165'                                                         
         DC    F'19545'                                                         
         DC    F'73215'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'53715'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,4,4)      TV WSP&NET SPC/NETWORK UNIT 4                
         DC    F'73215'                                                         
         DC    F'53715'                                                         
         DC    F'34165'                                                         
         DC    F'19545'                                                         
         DC    F'73215'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'53715'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,5,5)      TV WSP&NET SPC/NETWORK UNIT 5                
         DC    F'73215'                                                         
         DC    F'53715'                                                         
         DC    F'34165'                                                         
         DC    F'19545'                                                         
         DC    F'73215'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'53715'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,6,6)      TV WSP&NET SPC/NETWORK UNIT 6                
         DC    F'75170'                                                         
         DC    F'54900'                                                         
         DC    F'35395'                                                         
         DC    F'20160'                                                         
         DC    F'75170'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'54900'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,7,7)      TV WSP&NET SPC/NETWORK UNIT 7                
         DC    F'77130'                                                         
         DC    F'56115'                                                         
         DC    F'36655'                                                         
         DC    F'20815'                                                         
         DC    F'77130'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'56115'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,8,8)      TV WSP&NET SPC/NETWORK UNIT 8                
         DC    F'79135'                                                         
         DC    F'57340'                                                         
         DC    F'37860'                                                         
         DC    F'21465'                                                         
         DC    F'79135'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'57340'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,9,9)      TV WSP&NET SPC/NETWORK UNIT 9                
         DC    F'81110'                                                         
         DC    F'58580'                                                         
         DC    F'39175'                                                         
         DC    F'22140'                                                         
         DC    F'81110'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'58580'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,10,10)    TV WSP&NET SPC/NETWORK UNIT 10               
         DC    F'83000'                                                         
         DC    F'59770'                                                         
         DC    F'40360'                                                         
         DC    F'22750'                                                         
         DC    F'83000'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'59770'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,11,11)    TV WSP&NET SPC/NETWORK UNIT 11               
         DC    F'85010'                                                         
         DC    F'60980'                                                         
         DC    F'41615'                                                         
         DC    F'23255'                                                         
         DC    F'85010'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'60980'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,12,12)    TV WSP&NET SPC/NETWORK UNIT 12               
         DC    F'86935'                                                         
         DC    F'62190'                                                         
         DC    F'42855'                                                         
         DC    F'23760'                                                         
         DC    F'86935'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'62190'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,13,13)    TV WSP&NET SPC/NETWORK UNIT 13               
         DC    F'88880'                                                         
         DC    F'63370'                                                         
         DC    F'44100'                                                         
         DC    F'24275'                                                         
         DC    F'88880'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'63370'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,14,14)    TV WSP&NET SPC/NETWORK UNIT 14               
         DC    F'90885'                                                         
         DC    F'64635'                                                         
         DC    F'45395'                                                         
         DC    F'24795'                                                         
         DC    F'90885'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'64635'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,15,15)    TV WSP&NET SPC/NETWORK UNIT 15               
         DC    F'92835'                                                         
         DC    F'65845'                                                         
         DC    F'46585'                                                         
         DC    F'25270'                                                         
         DC    F'92835'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'65845'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,16,16)    TV WSP&NET SPC/NETWORK UNIT 16               
         DC    F'94780'                                                         
         DC    F'66995'                                                         
         DC    F'47785'                                                         
         DC    F'25750'                                                         
         DC    F'94780'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'66995'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,17,17)    TV WSP&NET SPC/NETWORK UNIT 17               
         DC    F'96710'                                                         
         DC    F'68235'                                                         
         DC    F'49015'                                                         
         DC    F'26290'                                                         
         DC    F'96710'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'68235'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,18,18)    TV WSP&NET SPC/NETWORK UNIT 18               
         DC    F'98705'                                                         
         DC    F'69455'                                                         
         DC    F'50215'                                                         
         DC    F'26765'                                                         
         DC    F'98705'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'69455'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,19,19)    TV WSP&NET SPC/NETWORK UNIT 19               
         DC    F'100605'                                                        
         DC    F'70680'                                                         
         DC    F'51440'                                                         
         DC    F'27230'                                                         
         DC    F'100605'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'70680'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,20,20)    TV WSP&NET SPC/NETWORK UNIT 20               
         DC    F'102585'                                                        
         DC    F'71840'                                                         
         DC    F'52690'                                                         
         DC    F'27920'                                                         
         DC    F'102585'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'71840'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,21,21)    TV WSP&NET SPC/NETWORK UNIT 21               
         DC    F'104580'                                                        
         DC    F'72895'                                                         
         DC    F'53805'                                                         
         DC    F'28195'                                                         
         DC    F'104580'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'72895'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,22,22)    TV WSP&NET SPC/NETWORK UNIT 22               
         DC    F'106530'                                                        
         DC    F'73915'                                                         
         DC    F'55030'                                                         
         DC    F'28620'                                                         
         DC    F'106530'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'73915'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,23,23)    TV WSP&NET SPC/NETWORK UNIT 23               
         DC    F'108485'                                                        
         DC    F'74950'                                                         
         DC    F'56280'                                                         
         DC    F'29025'                                                         
         DC    F'108485'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'74950'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,24,24)    TV WSP&NET SPC/NETWORK UNIT 24               
         DC    F'110400'                                                        
         DC    F'76005'                                                         
         DC    F'57455'                                                         
         DC    F'29480'                                                         
         DC    F'110400'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'76005'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,25,25)    TV WSP&NET SPC/NETWORK UNIT 25               
         DC    F'112410'                                                        
         DC    F'77020'                                                         
         DC    F'58660'                                                         
         DC    F'29920'                                                         
         DC    F'112410'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'77020'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,26,26)    TV WSP&NET SPC/NETWORK UNIT 26               
         DC    F'114325'                                                        
         DC    F'77995'                                                         
         DC    F'59435'                                                         
         DC    F'30360'                                                         
         DC    F'114325'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'77995'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,27,27)    TV WSP&NET SPC/NETWORK UNIT 27               
         DC    F'116280'                                                        
         DC    F'79050'                                                         
         DC    F'60220'                                                         
         DC    F'30795'                                                         
         DC    F'116280'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'79050'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,28,28)    TV WSP&NET SPC/NETWORK UNIT 28               
         DC    F'118125'                                                        
         DC    F'80060'                                                         
         DC    F'60945'                                                         
         DC    F'31215'                                                         
         DC    F'118125'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'80060'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,29,29)    TV WSP&NET SPC/NETWORK UNIT 29               
         DC    F'120095'                                                        
         DC    F'81130'                                                         
         DC    F'61700'                                                         
         DC    F'31605'                                                         
         DC    F'120095'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'81130'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,30,30)    TV WSP&NET SPC/NETWORK UNIT 30               
         DC    F'122005'                                                        
         DC    F'82120'                                                         
         DC    F'62455'                                                         
         DC    F'32080'                                                         
         DC    F'122005'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'82120'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,31,31)    TV WSP&NET SPC/NETWORK UNIT 31               
         DC    F'123555'                                                        
         DC    F'83140'                                                         
         DC    F'63170'                                                         
         DC    F'32525'                                                         
         DC    F'123555'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'83140'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,32,32)    TV WSP&NET SPC/NETWORK UNIT 32               
         DC    F'124990'                                                        
         DC    F'84085'                                                         
         DC    F'63890'                                                         
         DC    F'32890'                                                         
         DC    F'124990'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'84085'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,33,33)    TV WSP&NET SPC/NETWORK UNIT 33               
         DC    F'126455'                                                        
         DC    F'85085'                                                         
         DC    F'64670'                                                         
         DC    F'33290'                                                         
         DC    F'126455'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'85085'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,34,34)    TV WSP&NET SPC/NETWORK UNIT 34               
         DC    F'128000'                                                        
         DC    F'85755'                                                         
         DC    F'65390'                                                         
         DC    F'33675'                                                         
         DC    F'128000'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'85755'           ST, SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(60,40,35,35)    TV WSP&NET SPC/NETWORK UNIT 35               
         DC    F'129465'                                                        
         DC    F'87050'                                                         
         DC    F'66185'                                                         
         DC    F'34085'                                                         
         DC    F'129465'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'87050'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,36,36)    TV WSP&NET SPC/NETWORK UNIT 36               
         DC    F'130950'                                                        
         DC    F'87840'                                                         
         DC    F'66675'                                                         
         DC    F'34515'                                                         
         DC    F'130950'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'87840'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,37,37)    TV WSP&NET SPC/NETWORK UNIT 37               
         DC    F'132435'                                                        
         DC    F'88595'                                                         
         DC    F'67070'                                                         
         DC    F'34925'                                                         
         DC    F'132435'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'88595'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,38,38)    TV WSP&NET SPC/NETWORK UNIT 38               
         DC    F'133915'                                                        
         DC    F'89320'                                                         
         DC    F'67650'                                                         
         DC    F'35335'                                                         
         DC    F'133915'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'89320'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,39,39)    TV WSP&NET SPC/NETWORK UNIT 39               
         DC    F'135445'                                                        
         DC    F'90050'                                                         
         DC    F'68140'                                                         
         DC    F'35715'                                                         
         DC    F'135445'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'90050'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,40,40)    TV WSP&NET SPC/NETWORK UNIT 40               
         DC    F'136885'                                                        
         DC    F'90815'                                                         
         DC    F'68670'                                                         
         DC    F'36110'                                                         
         DC    F'136885'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'90815'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,41,41)    TV WSP&NET SPC/NETWORK UNIT 41               
         DC    F'137910'                                                        
         DC    F'91515'                                                         
         DC    F'69150'                                                         
         DC    F'36410'                                                         
         DC    F'137910'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'91515'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,42,42)    TV WSP&NET SPC/NETWORK UNIT 42               
         DC    F'138870'                                                        
         DC    F'92230'                                                         
         DC    F'69645'                                                         
         DC    F'36795'                                                         
         DC    F'138870'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'92230'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,43,43)    TV WSP&NET SPC/NETWORK UNIT 43               
         DC    F'139860'                                                        
         DC    F'93005'                                                         
         DC    F'70110'                                                         
         DC    F'37150'                                                         
         DC    F'139860'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'93005'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,44,44)    TV WSP&NET SPC/NETWORK UNIT 44               
         DC    F'140910'                                                        
         DC    F'93705'                                                         
         DC    F'70620'                                                         
         DC    F'37475'                                                         
         DC    F'140910'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'93705'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,45,45)    TV WSP&NET SPC/NETWORK UNIT 45               
         DC    F'141865'                                                        
         DC    F'94365'                                                         
         DC    F'71145'                                                         
         DC    F'37845'                                                         
         DC    F'141865'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'94365'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,46,46)    TV WSP&NET SPC/NETWORK UNIT 46               
         DC    F'142910'                                                        
         DC    F'95135'                                                         
         DC    F'71570'                                                         
         DC    F'38145'                                                         
         DC    F'142910'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'95135'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,47,47)    TV WSP&NET SPC/NETWORK UNIT 47               
         DC    F'143860'                                                        
         DC    F'95795'                                                         
         DC    F'72075'                                                         
         DC    F'38410'                                                         
         DC    F'143860'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'95795'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,48,48)    TV WSP&NET SPC/NETWORK UNIT 48               
         DC    F'144825'                                                        
         DC    F'96475'                                                         
         DC    F'72530'                                                         
         DC    F'38780'                                                         
         DC    F'144825'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'96475'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,49,49)    TV WSP&NET SPC/NETWORK UNIT 49               
         DC    F'145850'                                                        
         DC    F'97250'                                                         
         DC    F'72995'                                                         
         DC    F'39025'                                                         
         DC    F'145850'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'97250'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,50,50)    TV WSP&NET SPC/NETWORK UNIT 50               
         DC    F'146840'                                                        
         DC    F'97940'                                                         
         DC    F'73505'                                                         
         DC    F'39385'                                                         
         DC    F'146840'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'97940'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,51,255)   TV WSP&NET/NETWORK UNITS 51+                 
         DC    F'190'                                                           
         DC    F'146'                                                           
         DC    F'96'                                                            
         DC    F'52'                                                            
         DC    F'190'                                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'146'              ST, SAME AS SOC                              
         SPACE 1                                                                
CAUTTBL  DC    AL1(68,44,0,0)      AUDITION INTERVIEW - TV                      
         DC    F'7050'             PP,CAR,SD                                    
         DC    F'7050'             SOC,GD                                       
         DC    F'7050'             VO,SS                                        
         DC    F'7050'             GS                                           
         DC    F'7050'             SA                                           
         DC    F'7050'             DEM                                          
         DC    F'7050'             E                                            
         DC    F'7050'             GE                                           
         DC    F'7050'             ST                                           
         DC    F'7050'             EVERYBODY ELSE-US,SI,SB,PT                   
         SPACE 1                                                                
CAURTBL  DC    AL1(69,12,0,0)      AUDITION INTERVIEW - RADIO                   
         DC    F'7050'             SS,SV                                        
         DC    F'7050'             MV,GS                                        
         EJECT                                                                  
CDMTBL   DC    AL1(70,44,2,2)      TV PRESENTATION DEMO 2                       
         DC    F'26100'            PP,CAR,SD                                    
         DC    F'26100'            SOC,GD                                       
         DC    F'26100'            VO,SS                                        
         DC    F'26100'            GS                                           
         DC    F'26100'            SA                                           
         DC    F'26100'            DEM                                          
         DC    F'26100'            E                                            
         DC    F'26100'            GE                                           
         DC    F'26100'            ST                                           
         DC    F'26100'            ALL ELSE-US,SI,SB,PT                         
         SPACE 1                                                                
         DC    AL1(70,44,3,255)    TV PRESENTATION DEMO 3+                      
         DC    F'13100'            PP,CAR,SD                                    
         DC    F'13100'            SOC,GD                                       
         DC    F'13100'            VO,SS                                        
         DC    F'13100'            GS                                           
         DC    F'13100'            SA                                           
         DC    F'13100'            DEM                                          
         DC    F'13100'            E                                            
         DC    F'13100'            GE                                           
         DC    F'13100'            ST                                           
         DC    F'13100'            ALL ELSE-US,SI,SB,PT                         
         SPACE 1                                                                
BSCHTBL  DC    AL1(80,40,0,0)      NATIONAL TV  - HOURLY RATE                   
         DC    F'8550'             PP,SD,CAR                                    
         DC    F'8550'             SOC,GD                                       
         DC    F'7050'             VO,SS                                        
         DC    F'7050'             GS                                           
         DC    F'12850'            SA = PP+50%                                  
         DC    F'8550'             DEM                                          
         DC    F'5250'             E                                            
         DC    F'3400'             GE                                           
         DC    F'8550'             ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(81,40,0,0)      NATIONAL TV  - OVERTIME HOURLY RATE          
         DC    F'11050'            PP,SD,CAR                                    
         DC    F'11050'            SOC,GD                                       
         DC    F'8900'             VO,SS                                        
         DC    F'8900'             GS                                           
         DC    F'16600'            SA = PP+50%                                  
         DC    F'11050'            DEM                                          
         DC    F'6350'             E                                            
         DC    F'4350'             GE                                           
         DC    F'11050'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(82,40,0,0)      NATIONAL TV  - DBLTIME HOURLY RATE           
         DC    F'12500'            PP,SD,CAR                                    
         DC    F'12500'            SOC,GD                                       
         DC    F'11050'            VO,SS                                        
         DC    F'11050'            GS                                           
         DC    F'18750'            SA = PP+50%                                  
         DC    F'12500'            DEM                                          
         DC    F'7900'             E                                            
         DC    F'5250'             GE                                           
         DC    F'12500'            ST, SAME AS SOC                              
         SPACE 1                                                                
CNMTBL   DC    AL1(100,40,0,0)     NEW MEDIA VIDEO 4 WEEKS                      
         DC    F'11475'            PP,SD,CAR                                    
         DC    F'11475'            SOC,GD                                       
         DC    F'8425'             VO,SS                                        
         DC    F'3775'             GS                                           
         DC    F'17225'            SA = PP+50%                                  
         DC    F'0'                DEM                                          
         DC    F'0'                E                                            
         DC    F'0'                GE                                           
         DC    F'11475'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(101,12,0,0)     NEW MEDIA AUDIO 4 WEEKS                      
         DC    F'8950'             SS,SV                                        
         DC    F'6850'             MV,GS                                        
         SPACE 1                                                                
         DC    AL1(110,40,0,0)     NEW MEDIA VIDEO 8 WEEKS                      
         DC    F'16400'            PP,SD,CAR                                    
         DC    F'16400'            SOC,GD                                       
         DC    F'12050'            VO,SS                                        
         DC    F'5400'             GS                                           
         DC    F'24600'            SA = PP+50%                                  
         DC    F'0'                DEM                                          
         DC    F'0'                E                                            
         DC    F'0'                GE                                           
         DC    F'16400'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(111,12,0,0)     NEW MEDIA AUDIO 8 WEEKS                      
         DC    F'12800'            SS,SV                                        
         DC    F'9750'             MV,GS                                        
         SPACE 1                                                                
         DC    AL1(120,40,0,0)     NEW MEDIA VIDEO 26 WEEKS                     
         DC    F'24600'            PP,SD,CAR                                    
         DC    F'24600'            SOC,GD                                       
         DC    F'18075'            VO,SS                                        
         DC    F'8100'             GS                                           
         DC    F'36900'            SA = PP+50%                                  
         DC    F'0'                DEM                                          
         DC    F'0'                E                                            
         DC    F'0'                GE                                           
         DC    F'24600'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(121,12,0,0)     NEW MEDIA AUDIO 26 WEEKS                     
         DC    F'19200'            SS,SV                                        
         DC    F'14650'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(130,40,0,0)     NEW MEDIA VIDEO 1 YEAR                       
         DC    F'32800'            PP,SD,CAR                                    
         DC    F'32800'            SOC,GD                                       
         DC    F'24100'            VO,SS                                        
         DC    F'10800'            GS                                           
         DC    F'49200'            SA = PP+50%                                  
         DC    F'0'                DEM                                          
         DC    F'0'                E                                            
         DC    F'0'                GE                                           
         DC    F'32800'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(131,12,0,0)     NEW MEDIA AUDIO 1 YEAR                       
         DC    F'25600'            SS,SV                                        
         DC    F'19500'            MV,GS                                        
         SPACE 1                                                                
CNMATBL  DC    AL1(140,12,0,0)     NEW MEDIA AUDIO 4 WEEKS ADDTL CUTS           
         DC    F'4475'             SS,SV                                        
         DC    F'3425'             MV,GS                                        
         SPACE 1                                                                
         DC    AL1(141,12,0,0)     NEW MEDIA AUDIO 8 WEEKS ADDTL CUTS           
         DC    F'6400'             SS,SV                                        
         DC    F'4875'             MV,GS                                        
         SPACE 1                                                                
         DC    AL1(142,12,0,0)     NEW MEDIA AUDIO 26 WEEKS ADDTL CUTS          
         DC    F'9600'             SS,SV                                        
         DC    F'7325'             MV,GS                                        
         SPACE 1                                                                
         DC    AL1(143,12,0,0)     NEW MEDIA AUDIO 1 YEAR ADDTL CUTS            
         DC    F'12800'            SS,SV                                        
         DC    F'9750'             MV,GS                                        
         SPACE 1                                                                
         DC    AL1(144,12,0,0)     BSC NEW MEDIA AUDIO ADDTL CUTS               
         DC    F'12800'            SS,SV                                        
         DC    F'9750'             MV,GS                                        
         SPACE 1                                                                
         DC    X'FF'               *** END OF USE RATE TABLES ***               
         EJECT                                                                  
*              USE LOOK UP TABLES                                               
         SPACE 2                                                                
USELUT   DS    0CL5                                                             
         DC    AL1(00,UBSC,ALL,ACT,0,0,0,TV+NEWMEDIA) TV SESSION                
         DC    AL1(80,UBSC,ALL,ACT,0,0,0,TV)    TV SESSION HOURLY RATE          
         DC    AL1(81,UBSC,ALL,ACT,0,0,0,TV)    TV SESSION HOURLY OT            
         DC    AL1(82,UBSC,ALL,ACT,0,0,0,TV)    TV SESSION HOURLY DT            
         DC    AL1(24,UBSC,ALL,ACT,0,0,0,RADIO+NEWMEDIA) RADIO SESSION          
         DC    AL1(12,UDOR,ALL,ACT,0,0,0,TV)    TV DORMANCY                     
         DC    AL1(48,UWSC,ALL,ACT,0,0,0,TV)    TV WILDSPOT                     
         DC    AL1(60,UWSM,ALL,ACT,0,0,0,TV)    TV WILDSPOT COMBINED            
         DC    AL1(60,UNET,ALL,ACT,0,0,0,TV)    TV NETWORK                      
         DC    AL1(68,UCAU,ALL,ACT,0,0,0,TV)    AUDITION                        
         DC    AL1(69,UCAU,ALL,ACT,0,0,0,RADIO) AUDITION                        
         DC    AL1(70,UCDM,ALL,ACT,0,0,0,TV)    TV PRESENTATION DEMO            
         DC    AL1(100,UCNM,UCNM4W,ACT,0,0,0,NEWMEDIA) NEW MEDIA 4 WK           
         DC    AL1(110,UCNM,UCNM8W,ACT,0,0,0,NEWMEDIA) NEW MEDIA 8 WK           
         DC    AL1(120,UCNM,UCNM26W,ACT,0,0,0,NEWMEDIA) NEW MEDIA 26 WK         
         DC    AL1(130,UCNM,UCNM1Y,ACT,0,0,0,NEWMEDIA) NEW MEDIA 1 YR           
         DC    AL1(140,UCNM,UCNM4W,ACT,0,0,0,NEWMEDIA) 4 WK ADDTL CUTS          
         DC    AL1(141,UCNM,UCNM8W,ACT,0,0,0,NEWMEDIA) 8 WK ADDTL CUTS          
         DC    AL1(142,UCNM,UCNM26W,ACT,0,0,0,NEWMEDIA) 26 WK ADDT CUTS         
         DC    AL1(143,UCNM,UCNM1Y,ACT,0,0,0,NEWMEDIA) 1 YR ADDTL CUTS          
         DC    AL1(144,UBSC,ALL,ACT,0,0,0,NEWMEDIA) AUDIO ADDTL CUTS            
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
         DC    AL1(60,CTZZ)        OTHER CATEGORY - ROW SHOULDN'T EXIST         
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
         DC    AL1(60,CTZZ)        OTHER CATEGORY - ROW SHOULDN'T EXIST         
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
         DC    AL1(60,CTZZ)        OTHER CATEGORY - ROW SHOULDN'T EXIST         
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
**PAN#1  DC    CL21'006TAGEN71   02/12/09'                                      
         END                                                                    
