*          DATA SET TAGEN68    AT LEVEL 061 AS OF 08/12/03                      
*PHASE T70268A,*                                                                
         TITLE 'T70268 - TABLES FOR CANADIAN CONTRACTS'                         
T70268   CSECT                                                                  
         DC    AL4(USETBLS-T70268)                                              
         DC    AL4(USELUT-T70268)                                               
         DC    AL4(MAJLUT-T70268)                                               
         DC    AL4(AFMCOLS-T70268)                                              
         DC    AL4(RADCOLS-T70268)                                              
         DC    AL4(OFFCOLS-T70268)                                              
         DC    AL4(ONCOLS-T70268)                                               
         DC    AL4(MSWEET-T70268)                                               
         DC    A(0)                                                             
         EJECT                                                                  
*              USE TYPE RATE TABLES                                             
         SPACE 3                                                                
USETBLS  DS    0F                                                               
BSCTTBL  DC    AL1(0,40,0,0)       NATIONAL TV                                  
         DC    F'53550'            PP,SD,CAR                                    
         DC    F'53550'            SOC,GD                                       
         DC    F'39050'            VO,SS                                        
         DC    F'16850'            GS                                           
         DC    F'80350'            SA = PP+50%                                  
         DC    F'53550'            DEM                                          
         DC    F'32750'            E                                            
         DC    F'21850'            GE                                           
         DC    F'53550'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(1,40,0,0)       SHORT LIFE TV 7                              
         DC    F'54500'                                                         
         DC    F'54500'                                                         
         DC    F'39800'                                                         
         DC    F'17150'                                                         
         DC    F'81750'                                                         
         DC    F'54500'                                                         
         DC    F'32750'                                                         
         DC    F'21850'                                                         
         DC    F'54500'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(2,40,0,0)       SHORT LIFE TV 14                             
         DC    F'54500'                                                         
         DC    F'54500'                                                         
         DC    F'39800'                                                         
         DC    F'17150'                                                         
         DC    F'81750'                                                         
         DC    F'54500'                                                         
         DC    F'32750'                                                         
         DC    F'21850'                                                         
         DC    F'54500'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(3,40,0,0)       SHORT LIFE TV 31                             
         DC    F'54500'                                                         
         DC    F'54500'                                                         
         DC    F'39800'                                                         
         DC    F'17150'                                                         
         DC    F'81750'                                                         
         DC    F'54500'                                                         
         DC    F'32750'                                                         
         DC    F'21850'                                                         
         DC    F'54500'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(4,40,0,0)       SHORT LIFE TV 45                             
         DC    F'54500'                                                         
         DC    F'54500'                                                         
         DC    F'39800'                                                         
         DC    F'17150'                                                         
         DC    F'81750'                                                         
         DC    F'54500'                                                         
         DC    F'32750'                                                         
         DC    F'21850'                                                         
         DC    F'54500'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(5,36,0,0)       LOCAL AND REGIONAL TV 1                      
         DC    F'37840'                                                         
         DC    F'36410'                                                         
         DC    F'18700'                                                         
         DC    F'14030'                                                         
         DC    F'56760'                                                         
         DC    F'0'                                                             
         DC    F'21380'                                                         
         DC    F'21380'                                                         
         SPACE 1                                                                
         DC    AL1(6,36,0,0)       LOCAL AND REGIONAL TV 2                      
         DC    F'31310'                                                         
         DC    F'30100'                                                         
         DC    F'11060'                                                         
         DC    F'6640'                                                          
         DC    F'46965'                                                         
         DC    F'0'                                                             
         DC    F'10760'                                                         
         DC    F'10760'                                                         
         SPACE 1                                                                
         DC    AL1(7,36,0,0)       LOCAL AND REGIONAL TV 3                      
         DC    F'27150'                                                         
         DC    F'25820'                                                         
         DC    F'8620'                                                          
         DC    F'5150'                                                          
         DC    F'40725'                                                         
         DC    F'0'                                                             
         DC    F'10760'                                                         
         DC    F'10760'                                                         
         EJECT                                                                  
DORTTBL  DC    AL1(12,40,0,0)      NATIONAL TV                                  
         DC    F'53550'            PP,SD,CAR                                    
         DC    F'53550'            SOC,GD                                       
         DC    F'39050'            VO,SS                                        
         DC    F'16850'            GS                                           
         DC    F'80350'            SA = PP+50%                                  
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'53550'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(13,40,0,0)      SHORT LIFE TV 7                              
         DC    F'54500'                                                         
         DC    F'54500'                                                         
         DC    F'39800'                                                         
         DC    F'17150'                                                         
         DC    F'81750'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'54500'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(14,40,0,0)      SHORT LIFE TV 14                             
         DC    F'54500'                                                         
         DC    F'54500'                                                         
         DC    F'39800'                                                         
         DC    F'17150'                                                         
         DC    F'81750'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'54500'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(15,40,0,0)      SHORT LIFE TV 31                             
         DC    F'54500'                                                         
         DC    F'54500'                                                         
         DC    F'39800'                                                         
         DC    F'17150'                                                         
         DC    F'81750'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'54500'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(16,40,0,0)      SHORT LIFE TV 45                             
         DC    F'54500'                                                         
         DC    F'54500'                                                         
         DC    F'39800'                                                         
         DC    F'17150'                                                         
         DC    F'81750'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'54500'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(17,24,0,0)      LOCAL AND REGIONAL TV 1                      
         DC    F'37840'                                                         
         DC    F'36410'                                                         
         DC    F'18700'                                                         
         DC    F'14030'                                                         
         DC    F'56760'                                                         
         SPACE 1                                                                
         DC    AL1(18,24,0,0)      LOCAL AND REGIONAL TV 2                      
         DC    F'31310'                                                         
         DC    F'30100'                                                         
         DC    F'11060'                                                         
         DC    F'6640'                                                          
         DC    F'46965'                                                         
         SPACE 1                                                                
         DC    AL1(19,24,0,0)      LOCAL AND REGIONAL TV 3                      
         DC    F'27150'                                                         
         DC    F'25820'                                                         
         DC    F'8620'                                                          
         DC    F'5150'                                                          
         DC    F'40725'                                                         
         EJECT                                                                  
BSCRTBL  DC    AL1(24,12,0,0)      NATIONAL RADIO                               
         DC    F'41725'            SV,SS                                        
         DC    F'31300'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(25,12,0,0)      SHORT LIFE RADIO 7                           
         DC    F'21250'                                                         
         DC    F'15950'                                                         
         SPACE 1                                                                
         DC    AL1(26,12,0,0)      SHORT LIFE RADIO 14                          
         DC    F'28100'                                                         
         DC    F'21050'                                                         
         SPACE 1                                                                
         DC    AL1(27,12,0,0)      SHORT LIFE RADIO 31                          
         DC    F'31900'                                                         
         DC    F'23975'                                                         
         SPACE 1                                                                
         DC    AL1(28,12,0,0)      SHORT LIFE RADIO 45                          
         DC    F'36125'                                                         
         DC    F'27125'                                                         
         SPACE 1                                                                
         DC    AL1(29,12,0,0)      LOCAL AND REGIONAL RADIO 1                   
         DC    F'30635'                                                         
         DC    F'23095'                                                         
         SPACE 1                                                                
         DC    AL1(30,12,0,0)      LOCAL AND REGIONAL RADIO 2                   
         DC    F'19895'                                                         
         DC    F'11965'                                                         
         SPACE 1                                                                
         DC    AL1(31,12,0,0)      LOCAL AND REGIONAL RADIO 3                   
         DC    F'17805'                                                         
         DC    F'10675'                                                         
         EJECT                                                                  
WSCTBL   DC    AL1(48,40,1,1)      TV WILDSPOT UNITS 1                          
         DC    F'41720'            PP,CAR,SD                                    
         DC    F'30565'            SOC,GD                                       
         DC    F'20415'            VO,SS                                        
         DC    F'12220'            GS                                           
         DC    F'41720'            SA                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'30565'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(48,40,2,2)      TV WILDSPOT UNITS 2                          
         DC    F'41720'            PP,CAR,SD                                    
         DC    F'30565'            SOC,GD                                       
         DC    F'20415'            VO,SS                                        
         DC    F'12220'            GS                                           
         DC    F'41720'            SA                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'30565'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(48,40,3,3)      TV WILDSPOT UNITS 4                          
         DC    F'41720'            PP,CAR,SD                                    
         DC    F'30565'            SOC,GD                                       
         DC    F'20415'            VO,SS                                        
         DC    F'12220'            GS                                           
         DC    F'41720'            SA                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'30565'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(48,40,4,4)      TV WILDSPOT UNITS 4                          
         DC    F'41720'            PP,CAR,SD                                    
         DC    F'30565'            SOC,GD                                       
         DC    F'20415'            VO,SS                                        
         DC    F'12220'            GS                                           
         DC    F'41720'            SA                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'30565'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(48,40,5,5)      TV WILDSPOT UNITS 5                          
         DC    F'41720'            PP,CAR,SD                                    
         DC    F'30565'            SOC,GD                                       
         DC    F'20415'            VO,SS                                        
         DC    F'12220'            GS                                           
         DC    F'41720'            SA                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'30565'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,6,6)      TV WILDSPOT UNIT 6                           
         DC    F'42795'                                                         
         DC    F'31265'                                                         
         DC    F'21170'                                                         
         DC    F'12610'                                                         
         DC    F'42795'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'31265'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,7,7)      TV WILDSPOT UNIT 7                           
         DC    F'43930'                                                         
         DC    F'31980'                                                         
         DC    F'21875'                                                         
         DC    F'13040'                                                         
         DC    F'43930'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'31980'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,8,8)      TV WILDSPOT UNIT 8                           
         DC    F'45050'                                                         
         DC    F'32680'                                                         
         DC    F'22615'                                                         
         DC    F'13450'                                                         
         DC    F'45050'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'32680'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,9,9)      TV WILDSPOT UNIT 9                           
         DC    F'46145'                                                         
         DC    F'33370'                                                         
         DC    F'23325'                                                         
         DC    F'13870'                                                         
         DC    F'46145'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'33370'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,10,10)    TV WILDSPOT UNIT 10                          
         DC    F'47270'                                                         
         DC    F'34060'                                                         
         DC    F'24050'                                                         
         DC    F'14285'                                                         
         DC    F'47270'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'34060'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,11,11)    TV WILDSPOT UNIT 11                          
         DC    F'48375'                                                         
         DC    F'34730'                                                         
         DC    F'24755'                                                         
         DC    F'14555'                                                         
         DC    F'48375'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'34730'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,12,12)    TV WILDSPOT UNIT 12                          
         DC    F'49480'                                                         
         DC    F'35440'                                                         
         DC    F'25495'                                                         
         DC    F'14890'                                                         
         DC    F'49480'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'35440'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,13,13)    TV WILDSPOT UNIT 13                          
         DC    F'50635'                                                         
         DC    F'36160'                                                         
         DC    F'26275'                                                         
         DC    F'15150'                                                         
         DC    F'50635'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'36160'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,14,14)    TV WILDSPOT UNIT 14                          
         DC    F'52095'                                                         
         DC    F'36800'                                                         
         DC    F'26985'                                                         
         DC    F'15470'                                                         
         DC    F'52095'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'36800'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,15,15)    TV WILDSPOT UNIT 15                          
         DC    F'52825'                                                         
         DC    F'37530'                                                         
         DC    F'27680'                                                         
         DC    F'15780'                                                         
         DC    F'52825'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'37530'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,16,16)    TV WILDSPOT UNIT 16                          
         DC    F'53935'                                                         
         DC    F'38220'                                                         
         DC    F'28440'                                                         
         DC    F'16030'                                                         
         DC    F'53935'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'38220'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,17,17)    TV WILDSPOT UNIT 17                          
         DC    F'55565'                                                         
         DC    F'38905'                                                         
         DC    F'29145'                                                         
         DC    F'16375'                                                         
         DC    F'55565'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'38905'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,18,18)    TV WILDSPOT UNIT 18                          
         DC    F'56645'                                                         
         DC    F'39590'                                                         
         DC    F'29860'                                                         
         DC    F'16660'                                                         
         DC    F'56645'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'39590'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,19,19)    TV WILDSPOT UNIT 19                          
         DC    F'57235'                                                         
         DC    F'40290'                                                         
         DC    F'30605'                                                         
         DC    F'16960'                                                         
         DC    F'57235'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'40290'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,20,20)    TV WILDSPOT UNIT 20                          
         DC    F'58395'                                                         
         DC    F'40985'                                                         
         DC    F'31320'                                                         
         DC    F'17240'                                                         
         DC    F'58395'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'40985'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,21,21)    TV WILDSPOT UNIT 21                          
         DC    F'59515'                                                         
         DC    F'41540'                                                         
         DC    F'32075'                                                         
         DC    F'17560'                                                         
         DC    F'59515'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'41540'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,22,22)    TV WILDSPOT UNIT 22                          
         DC    F'60630'                                                         
         DC    F'42125'                                                         
         DC    F'32820'                                                         
         DC    F'17850'                                                         
         DC    F'60630'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'42125'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,23,23)    TV WILDSPOT UNIT 23                          
         DC    F'61760'                                                         
         DC    F'42700'                                                         
         DC    F'33530'                                                         
         DC    F'18155'                                                         
         DC    F'61760'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'42700'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,24,24)    TV WILDSPOT UNIT 24                          
         DC    F'62850'                                                         
         DC    F'43190'                                                         
         DC    F'34225'                                                         
         DC    F'18495'                                                         
         DC    F'62850'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'43190'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,25,25)    TV WILDSPOT UNIT 25                          
         DC    F'63935'                                                         
         DC    F'43830'                                                         
         DC    F'35020'                                                         
         DC    F'18705'                                                         
         DC    F'63935'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'43830'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,26,26)    TV WILDSPOT UNIT 26                          
         DC    F'65065'                                                         
         DC    F'44440'                                                         
         DC    F'35420'                                                         
         DC    F'19060'                                                         
         DC    F'65065'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'44440'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,27,27)    TV WILDSPOT UNIT 27                          
         DC    F'66210'                                                         
         DC    F'44975'                                                         
         DC    F'35865'                                                         
         DC    F'19275'                                                         
         DC    F'66210'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'44975'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,28,28)    TV WILDSPOT UNIT 28                          
         DC    F'67295'                                                         
         DC    F'45515'                                                         
         DC    F'36300'                                                         
         DC    F'19525'                                                         
         DC    F'67295'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'45515'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,29,29)    TV WILDSPOT UNIT 29                          
         DC    F'68430'                                                         
         DC    F'46125'                                                         
         DC    F'36735'                                                         
         DC    F'19785'                                                         
         DC    F'68430'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'46125'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,30,30)    TV WILDSPOT UNIT 30                          
         DC    F'69555'                                                         
         DC    F'46675'                                                         
         DC    F'37190'                                                         
         DC    F'20060'                                                         
         DC    F'69555'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'46675'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,31,31)    TV WILDSPOT UNIT 31                          
         DC    F'70395'                                                         
         DC    F'47270'                                                         
         DC    F'37600'                                                         
         DC    F'20305'                                                         
         DC    F'70395'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'47270'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,32,32)    TV WILDSPOT UNIT 32                          
         DC    F'71180'                                                         
         DC    F'47825'                                                         
         DC    F'38075'                                                         
         DC    F'20565'                                                         
         DC    F'71180'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'47825'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,33,33)    TV WILDSPOT UNIT 33                          
         DC    F'72060'                                                         
         DC    F'48375'                                                         
         DC    F'38510'                                                         
         DC    F'20810'                                                         
         DC    F'72060'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'48375'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,34,34)    TV WILDSPOT UNIT 34                          
         DC    F'72905'                                                         
         DC    F'48955'                                                         
         DC    F'38940'                                                         
         DC    F'21120'                                                         
         DC    F'72905'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'48955'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,35,35)    TV WILDSPOT UNIT 35                          
         DC    F'73755'                                                         
         DC    F'49485'                                                         
         DC    F'39375'                                                         
         DC    F'21360'                                                         
         DC    F'73755'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'49485'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,36,36)    TV WILDSPOT UNIT 36                          
         DC    F'74600'                                                         
         DC    F'49920'                                                         
         DC    F'39705'                                                         
         DC    F'21605'                                                         
         DC    F'74600'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'49920'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,37,37)    TV WILDSPOT UNIT 37                          
         DC    F'75415'                                                         
         DC    F'50335'                                                         
         DC    F'40010'                                                         
         DC    F'21865'                                                         
         DC    F'75415'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'50335'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,38,38)    TV WILDSPOT UNIT 38                          
         DC    F'76275'                                                         
         DC    F'50810'                                                         
         DC    F'40370'                                                         
         DC    F'22115'                                                         
         DC    F'76275'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'50810'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,39,39)    TV WILDSPOT UNIT 39                          
         DC    F'77095'                                                         
         DC    F'51185'                                                         
         DC    F'40620'                                                         
         DC    F'22360'                                                         
         DC    F'77095'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'51185'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,40,40)    TV WILDSPOT UNIT 40                          
         DC    F'77945'                                                         
         DC    F'51620'                                                         
         DC    F'40970'                                                         
         DC    F'22640'                                                         
         DC    F'77945'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'51620'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,41,41)    TV WILDSPOT UNIT 41                          
         DC    F'78525'                                                         
         DC    F'51890'                                                         
         DC    F'41250'                                                         
         DC    F'22830'                                                         
         DC    F'78525'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'51890'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,42,42)    TV WILDSPOT UNIT 42                          
         DC    F'79075'                                                         
         DC    F'52460'                                                         
         DC    F'41585'                                                         
         DC    F'23070'                                                         
         DC    F'79075'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'52460'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,43,43)    TV WILDSPOT UNIT 43                          
         DC    F'79640'                                                         
         DC    F'52880'                                                         
         DC    F'41875'                                                         
         DC    F'23250'                                                         
         DC    F'79640'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'52880'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,44,44)    TV WILDSPOT UNIT 44                          
         DC    F'80225'                                                         
         DC    F'53310'                                                         
         DC    F'42165'                                                         
         DC    F'23510'                                                         
         DC    F'80225'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'53310'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,45,45)    TV WILDSPOT UNIT 45                          
         DC    F'80765'                                                         
         DC    F'53705'                                                         
         DC    F'42490'                                                         
         DC    F'23690'                                                         
         DC    F'80765'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'53705'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,46,46)    TV WILDSPOT UNIT 46                          
         DC    F'81365'                                                         
         DC    F'54135'                                                         
         DC    F'42820'                                                         
         DC    F'23955'                                                         
         DC    F'81365'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'54135'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,47,47)    TV WILDSPOT UNIT 47                          
         DC    F'81930'                                                         
         DC    F'54565'                                                         
         DC    F'43125'                                                         
         DC    F'24160'                                                         
         DC    F'81930'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'54565'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,48,48)    TV WILDSPOT UNIT 48                          
         DC    F'82500'                                                         
         DC    F'54985'                                                         
         DC    F'43440'                                                         
         DC    F'24370'                                                         
         DC    F'82500'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'54985'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,49,49)    TV WILDSPOT UNIT 49                          
         DC    F'83065'                                                         
         DC    F'55400'                                                         
         DC    F'43760'                                                         
         DC    F'24585'                                                         
         DC    F'83065'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'55400'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,50,50)    TV WILDSPOT UNIT 50                          
         DC    F'83635'                                                         
         DC    F'55815'                                                         
         DC    F'44085'                                                         
         DC    F'24800'                                                         
         DC    F'83635'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'55815'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,51,255)   TV WILDSPOT UNITS 51+                        
         DC    F'110'                                                           
         DC    F'83'                                                            
         DC    F'63'                                                            
         DC    F'32'                                                            
         DC    F'110'                                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'83'                ST, SAME AS SOC                             
                                                                                
                                                                                
                                                                                
         SPACE 1                                                                
         EJECT                                                                  
         DC    AL1(60,40,1,1)      TV WSP&NET SPC/NETWORK UNITS 1               
         DC    F'58335'            PP,CAR,SD                                    
         DC    F'42785'            SOC,GD                                       
         DC    F'27215'            VO,SS                                        
         DC    F'15575'            GS                                           
         DC    F'58335'            SA                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'42785'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,2,2)      TV WSP&NET SPC/NETWORK UNITS 2               
         DC    F'58335'                                                         
         DC    F'42785'                                                         
         DC    F'27215'                                                         
         DC    F'15575'                                                         
         DC    F'58335'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'42785'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,3,3)      TV WSP&NET SPC/NETWORK UNITS 3               
         DC    F'58335'                                                         
         DC    F'42785'                                                         
         DC    F'27215'                                                         
         DC    F'15575'                                                         
         DC    F'58335'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'42785'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,4,4)      TV WSP&NET SPC/NETWORK UNIT 4                
         DC    F'58335'                                                         
         DC    F'42785'                                                         
         DC    F'27215'                                                         
         DC    F'15575'                                                         
         DC    F'58335'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'42785'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,5,5)      TV WSP&NET SPC/NETWORK UNIT 5                
         DC    F'58335'                                                         
         DC    F'42785'                                                         
         DC    F'27215'                                                         
         DC    F'15575'                                                         
         DC    F'58335'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'42785'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,6,6)      TV WSP&NET SPC/NETWORK UNIT 6                
         DC    F'59880'                                                         
         DC    F'43740'                                                         
         DC    F'28200'                                                         
         DC    F'16055'                                                         
         DC    F'59880'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'43740'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,7,7)      TV WSP&NET SPC/NETWORK UNIT 7                
         DC    F'61435'                                                         
         DC    F'44710'                                                         
         DC    F'29200'                                                         
         DC    F'16585'                                                         
         DC    F'61435'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'44710'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,8,8)      TV WSP&NET SPC/NETWORK UNIT 8                
         DC    F'63040'                                                         
         DC    F'45670'                                                         
         DC    F'30155'                                                         
         DC    F'17100'                                                         
         DC    F'63040'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'45670'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,9,9)      TV WSP&NET SPC/NETWORK UNIT 9                
         DC    F'64610'                                                         
         DC    F'46665'                                                         
         DC    F'31220'                                                         
         DC    F'17635'                                                         
         DC    F'64610'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'46665'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,10,10)    TV WSP&NET SPC/NETWORK UNIT 10               
         DC    F'66120'                                                         
         DC    F'47620'                                                         
         DC    F'32155'                                                         
         DC    F'18125'                                                         
         DC    F'66120'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'47620'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,11,11)    TV WSP&NET SPC/NETWORK UNIT 11               
         DC    F'67725'                                                         
         DC    F'48585'                                                         
         DC    F'33150'                                                         
         DC    F'18525'                                                         
         DC    F'67725'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'48585'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,12,12)    TV WSP&NET SPC/NETWORK UNIT 12               
         DC    F'69255'                                                         
         DC    F'49545'                                                         
         DC    F'34140'                                                         
         DC    F'18930'                                                         
         DC    F'69255'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'49545'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,13,13)    TV WSP&NET SPC/NETWORK UNIT 13               
         DC    F'70800'                                                         
         DC    F'50480'                                                         
         DC    F'35125'                                                         
         DC    F'19335'                                                         
         DC    F'70800'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'50480'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,14,14)    TV WSP&NET SPC/NETWORK UNIT 14               
         DC    F'72400'                                                         
         DC    F'51490'                                                         
         DC    F'36160'                                                         
         DC    F'19755'                                                         
         DC    F'72400'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'51490'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,15,15)    TV WSP&NET SPC/NETWORK UNIT 15               
         DC    F'73955'                                                         
         DC    F'52455'                                                         
         DC    F'37115'                                                         
         DC    F'20130'                                                         
         DC    F'73955'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'52455'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,16,16)    TV WSP&NET SPC/NETWORK UNIT 16               
         DC    F'75505'                                                         
         DC    F'53370'                                                         
         DC    F'38075'                                                         
         DC    F'20510'                                                         
         DC    F'75505'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'53370'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,17,17)    TV WSP&NET SPC/NETWORK UNIT 17               
         DC    F'77045'                                                         
         DC    F'54360'                                                         
         DC    F'39050'                                                         
         DC    F'20945'                                                         
         DC    F'77045'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'54360'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,18,18)    TV WSP&NET SPC/NETWORK UNIT 18               
         DC    F'78630'                                                         
         DC    F'55325'                                                         
         DC    F'40010'                                                         
         DC    F'21325'                                                         
         DC    F'78630'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'55325'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,19,19)    TV WSP&NET SPC/NETWORK UNIT 19               
         DC    F'80145'                                                         
         DC    F'56310'                                                         
         DC    F'40980'                                                         
         DC    F'21685'                                                         
         DC    F'80145'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'56310'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,20,20)    TV WSP&NET SPC/NETWORK UNIT 20               
         DC    F'81730'                                                         
         DC    F'57235'                                                         
         DC    F'41975'                                                         
         DC    F'22240'                                                         
         DC    F'81730'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'57235'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,21,21)    TV WSP&NET SPC/NETWORK UNIT 21               
         DC    F'83315'                                                         
         DC    F'58070'                                                         
         DC    F'42870'                                                         
         DC    F'22460'                                                         
         DC    F'83315'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'58070'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,22,22)    TV WSP&NET SPC/NETWORK UNIT 22               
         DC    F'84870'                                                         
         DC    F'58880'                                                         
         DC    F'43840'                                                         
         DC    F'22790'                                                         
         DC    F'84870'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'58880'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,23,23)    TV WSP&NET SPC/NETWORK UNIT 23               
         DC    F'86420'                                                         
         DC    F'59700'                                                         
         DC    F'44835'                                                         
         DC    F'23125'                                                         
         DC    F'86420'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'59700'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,24,24)    TV WSP&NET SPC/NETWORK UNIT 24               
         DC    F'87945'                                                         
         DC    F'60550'                                                         
         DC    F'45775'                                                         
         DC    F'23485'                                                         
         DC    F'87945'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'60550'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,25,25)    TV WSP&NET SPC/NETWORK UNIT 25               
         DC    F'89555'                                                         
         DC    F'61360'                                                         
         DC    F'46735'                                                         
         DC    F'23845'                                                         
         DC    F'89555'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'61360'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,26,26)    TV WSP&NET SPC/NETWORK UNIT 26               
         DC    F'91075'                                                         
         DC    F'62135'                                                         
         DC    F'47335'                                                         
         DC    F'24185'                                                         
         DC    F'91075'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'62135'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,27,27)    TV WSP&NET SPC/NETWORK UNIT 27               
         DC    F'92635'                                                         
         DC    F'62975'                                                         
         DC    F'47965'                                                         
         DC    F'24530'                                                         
         DC    F'92635'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'62975'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,28,28)    TV WSP&NET SPC/NETWORK UNIT 28               
         DC    F'94105'                                                         
         DC    F'63780'                                                         
         DC    F'48550'                                                         
         DC    F'24870'                                                         
         DC    F'94105'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'63780'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,29,29)    TV WSP&NET SPC/NETWORK UNIT 29               
         DC    F'95670'                                                         
         DC    F'64625'                                                         
         DC    F'49145'                                                         
         DC    F'25180'                                                         
         DC    F'95670'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'64625'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,30,30)    TV WSP&NET SPC/NETWORK UNIT 30               
         DC    F'97195'                                                         
         DC    F'65415'                                                         
         DC    F'49755'                                                         
         DC    F'25560'                                                         
         DC    F'97195'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'65415'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,31,31)    TV WSP&NET SPC/NETWORK UNIT 31               
         DC    F'98425'                                                         
         DC    F'66230'                                                         
         DC    F'50330'                                                         
         DC    F'25905'                                                         
         DC    F'98425'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'66230'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,32,32)    TV WSP&NET SPC/NETWORK UNIT 32               
         DC    F'99575'                                                         
         DC    F'66990'                                                         
         DC    F'50890'                                                         
         DC    F'26200'                                                         
         DC    F'99575'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'66990'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,33,33)    TV WSP&NET SPC/NETWORK UNIT 33               
         DC    F'100745'                                                        
         DC    F'67785'                                                         
         DC    F'51520'                                                         
         DC    F'26525'                                                         
         DC    F'100745'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'67785'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,34,34)    TV WSP&NET SPC/NETWORK UNIT 34               
         DC    F'101970'                                                        
         DC    F'68320'                                                         
         DC    F'52090'                                                         
         DC    F'26830'                                                         
         DC    F'101970'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'68320'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,35,35)    TV WSP&NET SPC/NETWORK UNIT 35               
         DC    F'103130'                                                        
         DC    F'69350'                                                         
         DC    F'52725'                                                         
         DC    F'27150'                                                         
         DC    F'103130'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'69350'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,36,36)    TV WSP&NET SPC/NETWORK UNIT 36               
         DC    F'104320'                                                        
         DC    F'69980'                                                         
         DC    F'53120'                                                         
         DC    F'27500'                                                         
         DC    F'104320'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'69980'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,37,37)    TV WSP&NET SPC/NETWORK UNIT 37               
         DC    F'105505'                                                        
         DC    F'70580'                                                         
         DC    F'53515'                                                         
         DC    F'27820'                                                         
         DC    F'105505'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'70580'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,38,38)    TV WSP&NET SPC/NETWORK UNIT 38               
         DC    F'106685'                                                        
         DC    F'71160'                                                         
         DC    F'53895'                                                         
         DC    F'28150'                                                         
         DC    F'106685'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'71160'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,39,39)    TV WSP&NET SPC/NETWORK UNIT 39               
         DC    F'107900'                                                        
         DC    F'71735'                                                         
         DC    F'54290'                                                         
         DC    F'28455'                                                         
         DC    F'107900'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'71735'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,40,40)    TV WSP&NET SPC/NETWORK UNIT 40               
         DC    F'109055'                                                        
         DC    F'72350'                                                         
         DC    F'54700'                                                         
         DC    F'28770'                                                         
         DC    F'109055'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'72350'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,41,41)    TV WSP&NET SPC/NETWORK UNIT 41               
         DC    F'109865'                                                        
         DC    F'72905'                                                         
         DC    F'55085'                                                         
         DC    F'29005'                                                         
         DC    F'109865'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'72905'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,42,42)    TV WSP&NET SPC/NETWORK UNIT 42               
         DC    F'110630'                                                        
         DC    F'73475'                                                         
         DC    F'55485'                                                         
         DC    F'29315'                                                         
         DC    F'110630'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'73475'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,43,43)    TV WSP&NET SPC/NETWORK UNIT 43               
         DC    F'111415'                                                        
         DC    F'74095'                                                         
         DC    F'55850'                                                         
         DC    F'29595'                                                         
         DC    F'111415'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'74095'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,44,44)    TV WSP&NET SPC/NETWORK UNIT 44               
         DC    F'112250'                                                        
         DC    F'74645'                                                         
         DC    F'56255'                                                         
         DC    F'29855'                                                         
         DC    F'112250'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'74645'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,45,45)    TV WSP&NET SPC/NETWORK UNIT 45               
         DC    F'113015'                                                        
         DC    F'75175'                                                         
         DC    F'56675'                                                         
         DC    F'30145'                                                         
         DC    F'113015'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'75175'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,46,46)    TV WSP&NET SPC/NETWORK UNIT 46               
         DC    F'113850'                                                        
         DC    F'75795'                                                         
         DC    F'57015'                                                         
         DC    F'30385'                                                         
         DC    F'113850'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'75795'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,47,47)    TV WSP&NET SPC/NETWORK UNIT 47               
         DC    F'114605'                                                        
         DC    F'76320'                                                         
         DC    F'57415'                                                         
         DC    F'30605'                                                         
         DC    F'114605'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'76320'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,48,48)    TV WSP&NET SPC/NETWORK UNIT 48               
         DC    F'115390'                                                        
         DC    F'76850'                                                         
         DC    F'57775'                                                         
         DC    F'30885'                                                         
         DC    F'115390'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'76850'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,49,49)    TV WSP&NET SPC/NETWORK UNIT 49               
         DC    F'116190'                                                        
         DC    F'77475'                                                         
         DC    F'58150'                                                         
         DC    F'31090'                                                         
         DC    F'116190'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'77475'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,50,50)    TV WSP&NET SPC/NETWORK UNIT 50               
         DC    F'116980'                                                        
         DC    F'78025'                                                         
         DC    F'58555'                                                         
         DC    F'31380'                                                         
         DC    F'116980'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'78025'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,51,255)   TV WSP&NET/NETWORK UNITS 51+                 
         DC    F'152'                                                           
         DC    F'116'                                                           
         DC    F'75'                                                            
         DC    F'42'                                                            
         DC    F'152'                                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'116'              ST, SAME AS SOC                              
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
         DC    F'20900'            PP,CAR,SD                                    
         DC    F'20900'            SOC,GD                                       
         DC    F'20900'            VO,SS                                        
         DC    F'20900'            GS                                           
         DC    F'20900'            SA                                           
         DC    F'20900'            DEM                                          
         DC    F'20900'            E                                            
         DC    F'20900'            GE                                           
         DC    F'20900'            ST                                           
         DC    F'20900'            ALL ELSE-US,SI,SB,PT                         
         SPACE 1                                                                
         DC    AL1(70,44,3,255)    TV PRESENTATION DEMO 3+                      
         DC    F'10500'            PP,CAR,SD                                    
         DC    F'10500'            SOC,GD                                       
         DC    F'10500'            VO,SS                                        
         DC    F'10500'            GS                                           
         DC    F'10500'            SA                                           
         DC    F'10500'            DEM                                          
         DC    F'10500'            E                                            
         DC    F'10500'            GE                                           
         DC    F'10500'            ST                                           
         DC    F'10500'            ALL ELSE-US,SI,SB,PT                         
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
**PAN#1  DC    CL21'061TAGEN68   08/12/03'                                      
         END                                                                    
