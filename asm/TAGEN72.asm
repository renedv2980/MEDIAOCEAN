*          DATA SET TAGEN72    AT LEVEL 065 AS OF 02/12/09                      
*PHASE T70272A,*                                                                
         TITLE 'T70272 - TABLES FOR YEAR 1 CANADIAN CONTRACTS'                  
T70272   CSECT                                                                  
         DC    AL4(USETBLS-T70272)                                              
         DC    AL4(USELUT-T70272)                                               
         DC    AL4(MAJLUT-T70272)                                               
         DC    AL4(AFMCOLS-T70272)                                              
         DC    AL4(RADCOLS-T70272)                                              
         DC    AL4(OFFCOLS-T70272)                                              
         DC    AL4(ONCOLS-T70272)                                               
         DC    AL4(MSWEET-T70272)                                               
         DC    A(0)                                                             
         EJECT                                                                  
*              USE TYPE RATE TABLES                                             
         SPACE 3                                                                
USETBLS  DS    0F                                                               
BSCTTBL  DC    AL1(0,40,0,0)       NATIONAL TV                                  
         DC    F'65550'            PP,SD,CAR                                    
         DC    F'65550'            SOC,GD                                       
         DC    F'47800'            VO,SS                                        
         DC    F'20700'            GS                                           
         DC    F'98350'            SA = PP+50%                                  
         DC    F'65550'            DEM                                          
         DC    F'40100'            E                                            
         DC    F'26800'            GE                                           
         DC    F'65550'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(1,40,0,0)       SHORT LIFE TV 7                              
         DC    F'65550'                                                         
         DC    F'65550'                                                         
         DC    F'47800'                                                         
         DC    F'20700'                                                         
         DC    F'98350'                                                         
         DC    F'65550'                                                         
         DC    F'40100'                                                         
         DC    F'26800'                                                         
         DC    F'65550'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(2,40,0,0)       SHORT LIFE TV 14                             
         DC    F'65550'                                                         
         DC    F'65550'                                                         
         DC    F'47800'                                                         
         DC    F'20700'                                                         
         DC    F'98350'                                                         
         DC    F'65550'                                                         
         DC    F'40100'                                                         
         DC    F'26800'                                                         
         DC    F'65550'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(3,40,0,0)       SHORT LIFE TV 31                             
         DC    F'65550'                                                         
         DC    F'65550'                                                         
         DC    F'47800'                                                         
         DC    F'20700'                                                         
         DC    F'98350'                                                         
         DC    F'65550'                                                         
         DC    F'40100'                                                         
         DC    F'26800'                                                         
         DC    F'65550'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(4,40,0,0)       SHORT LIFE TV 45                             
         DC    F'65550'                                                         
         DC    F'65550'                                                         
         DC    F'47800'                                                         
         DC    F'20700'                                                         
         DC    F'98350'                                                         
         DC    F'65550'                                                         
         DC    F'40100'                                                         
         DC    F'26800'                                                         
         DC    F'65550'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(5,36,0,0)       LOCAL AND REGIONAL TV 1                      
         DC    F'46340'                                                         
         DC    F'44600'                                                         
         DC    F'22910'                                                         
         DC    F'17190'                                                         
         DC    F'69510'                                                         
         DC    F'0'                                                             
         DC    F'26200'                                                         
         DC    F'26200'                                                         
         SPACE 1                                                                
         DC    AL1(6,36,0,0)       LOCAL AND REGIONAL TV 2                      
         DC    F'38370'                                                         
         DC    F'36850'                                                         
         DC    F'13560'                                                         
         DC    F'8140'                                                          
         DC    F'57560'                                                         
         DC    F'0'                                                             
         DC    F'13200'                                                         
         DC    F'13200'                                                         
         SPACE 1                                                                
         DC    AL1(7,36,0,0)       LOCAL AND REGIONAL TV 3                      
         DC    F'33240'                                                         
         DC    F'31640'                                                         
         DC    F'10560'                                                         
         DC    F'6310'                                                          
         DC    F'49860'                                                         
         DC    F'0'                                                             
         DC    F'13200'                                                         
         DC    F'13200'                                                         
         SPACE 1                                                                
         DC    AL1(8,40,0,0)       VIDEO                                        
         DC    F'32000'                                                         
         DC    F'32000'                                                         
         DC    F'23500'                                                         
         DC    F'10500'                                                         
         DC    F'48000'                                                         
         DC    F'32000'                                                         
         DC    F'20000'                                                         
         DC    F'13500'                                                         
         DC    F'32000'            ST, SAME AS SOC                              
         EJECT                                                                  
DORTTBL  DC    AL1(12,40,0,0)      NATIONAL TV                                  
         DC    F'65550'            PP,SD,CAR                                    
         DC    F'65550'            SOC,GD                                       
         DC    F'47800'            VO,SS                                        
         DC    F'20700'            GS                                           
         DC    F'98350'            SA = PP+50%                                  
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'65550'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(13,40,0,0)      SHORT LIFE TV 7                              
         DC    F'65550'                                                         
         DC    F'65550'                                                         
         DC    F'47800'                                                         
         DC    F'20700'                                                         
         DC    F'98350'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'65550'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(14,40,0,0)      SHORT LIFE TV 14                             
         DC    F'65550'                                                         
         DC    F'65550'                                                         
         DC    F'47800'                                                         
         DC    F'20700'                                                         
         DC    F'98350'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'65550'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(15,40,0,0)      SHORT LIFE TV 31                             
         DC    F'65550'                                                         
         DC    F'65550'                                                         
         DC    F'47800'                                                         
         DC    F'20700'                                                         
         DC    F'98350'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'65550'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(16,40,0,0)      SHORT LIFE TV 45                             
         DC    F'65550'                                                         
         DC    F'65550'                                                         
         DC    F'47800'                                                         
         DC    F'20700'                                                         
         DC    F'98350'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'65550'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(17,24,0,0)      LOCAL AND REGIONAL TV 1                      
         DC    F'46340'                                                         
         DC    F'44600'                                                         
         DC    F'22910'                                                         
         DC    F'17190'                                                         
         DC    F'69510'                                                         
         SPACE 1                                                                
         DC    AL1(18,24,0,0)      LOCAL AND REGIONAL TV 2                      
         DC    F'38370'                                                         
         DC    F'36850'                                                         
         DC    F'13560'                                                         
         DC    F'8140'                                                          
         DC    F'57560'                                                         
         SPACE 1                                                                
         DC    AL1(19,24,0,0)      LOCAL AND REGIONAL TV 3                      
         DC    F'33240'                                                         
         DC    F'31640'                                                         
         DC    F'10560'                                                         
         DC    F'6310'                                                          
         DC    F'49860'                                                         
         EJECT                                                                  
BSCRTBL  DC    AL1(24,12,0,0)      NATIONAL RADIO                               
         DC    F'51050'            SV,SS                                        
         DC    F'38325'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(25,12,0,0)      SHORT LIFE RADIO 7                           
         DC    F'26025'                                                         
         DC    F'19550'                                                         
         SPACE 1                                                                
         DC    AL1(26,12,0,0)      SHORT LIFE RADIO 14                          
         DC    F'34425'                                                         
         DC    F'25750'                                                         
         SPACE 1                                                                
         DC    AL1(27,12,0,0)      SHORT LIFE RADIO 31                          
         DC    F'39050'                                                         
         DC    F'29375'                                                         
         SPACE 1                                                                
         DC    AL1(28,12,0,0)      SHORT LIFE RADIO 45                          
         DC    F'44250'                                                         
         DC    F'33225'                                                         
         SPACE 1                                                                
         DC    AL1(29,12,0,0)      LOCAL AND REGIONAL RADIO 1                   
         DC    F'37515'                                                         
         DC    F'28280'                                                         
         SPACE 1                                                                
         DC    AL1(30,12,0,0)      LOCAL AND REGIONAL RADIO 2                   
         DC    F'24375'                                                         
         DC    F'14650'                                                         
         SPACE 1                                                                
         DC    AL1(31,12,0,0)      LOCAL AND REGIONAL RADIO 3                   
         DC    F'21810'                                                         
         DC    F'13085'                                                         
         SPACE 1                                                                
         DC    AL1(33,12,0,0)      AUDIO                                        
         DC    F'25000'                                                         
         DC    F'19000'                                                         
         EJECT                                                                  
WSCTBL   DC    AL1(48,40,1,1)      TV WILDSPOT UNITS 1                          
         DC    F'51085'            PP,CAR,SD                                    
         DC    F'37435'            SOC,GD                                       
         DC    F'25000'            VO,SS                                        
         DC    F'14970'            GS                                           
         DC    F'51085'            SA                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'37435'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(48,40,2,2)      TV WILDSPOT UNITS 2                          
         DC    F'51085'            PP,CAR,SD                                    
         DC    F'37435'            SOC,GD                                       
         DC    F'25000'            VO,SS                                        
         DC    F'14970'            GS                                           
         DC    F'51085'            SA                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'37435'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(48,40,3,3)      TV WILDSPOT UNITS 3                          
         DC    F'51085'            PP,CAR,SD                                    
         DC    F'37435'            SOC,GD                                       
         DC    F'25000'            VO,SS                                        
         DC    F'14970'            GS                                           
         DC    F'51085'            SA                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'37435'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(48,40,4,4)      TV WILDSPOT UNITS 4                          
         DC    F'51085'            PP,CAR,SD                                    
         DC    F'37435'            SOC,GD                                       
         DC    F'25000'            VO,SS                                        
         DC    F'14970'            GS                                           
         DC    F'51085'            SA                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'37435'            ST SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(48,40,5,5)      TV WILDSPOT UNITS 5                          
         DC    F'51085'            PP,CAR,SD                                    
         DC    F'37435'            SOC,GD                                       
         DC    F'25000'            VO,SS                                        
         DC    F'14970'            GS                                           
         DC    F'51085'            SA                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'37435'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,6,6)      TV WILDSPOT UNIT 6                           
         DC    F'52415'                                                         
         DC    F'38295'                                                         
         DC    F'25920'                                                         
         DC    F'15435'                                                         
         DC    F'52415'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'38295'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,7,7)      TV WILDSPOT UNIT 7                           
         DC    F'53800'                                                         
         DC    F'39165'                                                         
         DC    F'26800'                                                         
         DC    F'15970'                                                         
         DC    F'53800'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'39165'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,8,8)      TV WILDSPOT UNIT 8                           
         DC    F'55170'                                                         
         DC    F'40020'                                                         
         DC    F'27695'                                                         
         DC    F'16475'                                                         
         DC    F'55170'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'40020'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,9,9)      TV WILDSPOT UNIT 9                           
         DC    F'56515'                                                         
         DC    F'40865'                                                         
         DC    F'28555'                                                         
         DC    F'16990'                                                         
         DC    F'56515'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'40865'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,10,10)    TV WILDSPOT UNIT 10                          
         DC    F'57890'                                                         
         DC    F'41710'                                                         
         DC    F'29445'                                                         
         DC    F'17485'                                                         
         DC    F'57890'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'41710'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,11,11)    TV WILDSPOT UNIT 11                          
         DC    F'59250'                                                         
         DC    F'42535'                                                         
         DC    F'30315'                                                         
         DC    F'17825'                                                         
         DC    F'59250'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'42535'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,12,12)    TV WILDSPOT UNIT 12                          
         DC    F'60595'                                                         
         DC    F'43405'                                                         
         DC    F'31220'                                                         
         DC    F'18235'                                                         
         DC    F'60595'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'43405'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,13,13)    TV WILDSPOT UNIT 13                          
         DC    F'62005'                                                         
         DC    F'44290'                                                         
         DC    F'32180'                                                         
         DC    F'18560'                                                         
         DC    F'62005'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'44290'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,14,14)    TV WILDSPOT UNIT 14                          
         DC    F'63800'                                                         
         DC    F'45060'                                                         
         DC    F'33045'                                                         
         DC    F'18940'                                                         
         DC    F'63800'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'45060'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,15,15)    TV WILDSPOT UNIT 15                          
         DC    F'64700'                                                         
         DC    F'45955'                                                         
         DC    F'33900'                                                         
         DC    F'19320'                                                         
         DC    F'64700'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'45955'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,16,16)    TV WILDSPOT UNIT 16                          
         DC    F'66055'                                                         
         DC    F'46805'                                                         
         DC    F'34830'                                                         
         DC    F'19625'                                                         
         DC    F'66055'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'46805'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,17,17)    TV WILDSPOT UNIT 17                          
         DC    F'68050'                                                         
         DC    F'47645'                                                         
         DC    F'35695'                                                         
         DC    F'20060'                                                         
         DC    F'68050'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'47645'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,18,18)    TV WILDSPOT UNIT 18                          
         DC    F'69375'                                                         
         DC    F'48490'                                                         
         DC    F'36565'                                                         
         DC    F'20410'                                                         
         DC    F'69375'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'48490'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,19,19)    TV WILDSPOT UNIT 19                          
         DC    F'70090'                                                         
         DC    F'49340'                                                         
         DC    F'37475'                                                         
         DC    F'20765'                                                         
         DC    F'70090'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'49340'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,20,20)    TV WILDSPOT UNIT 20                          
         DC    F'71510'                                                         
         DC    F'50190'                                                         
         DC    F'38355'                                                         
         DC    F'21110'                                                         
         DC    F'71510'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'50190'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,21,21)    TV WILDSPOT UNIT 21                          
         DC    F'72885'                                                         
         DC    F'50865'                                                         
         DC    F'39280'                                                         
         DC    F'21505'                                                         
         DC    F'72885'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'50865'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,22,22)    TV WILDSPOT UNIT 22                          
         DC    F'74245'                                                         
         DC    F'51595'                                                         
         DC    F'40190'                                                         
         DC    F'21860'                                                         
         DC    F'74245'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'51595'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,23,23)    TV WILDSPOT UNIT 23                          
         DC    F'75630'                                                         
         DC    F'52290'                                                         
         DC    F'41065'                                                         
         DC    F'22230'                                                         
         DC    F'75630'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'52290'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,24,24)    TV WILDSPOT UNIT 24                          
         DC    F'76960'                                                         
         DC    F'52895'                                                         
         DC    F'41920'                                                         
         DC    F'22645'                                                         
         DC    F'76960'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'52895'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,25,25)    TV WILDSPOT UNIT 25                          
         DC    F'78305'                                                         
         DC    F'53675'                                                         
         DC    F'42885'                                                         
         DC    F'22905'                                                         
         DC    F'78305'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'53675'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,26,26)    TV WILDSPOT UNIT 26                          
         DC    F'79675'                                                         
         DC    F'54415'                                                         
         DC    F'43385'                                                         
         DC    F'23345'                                                         
         DC    F'79675'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'54415'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,27,27)    TV WILDSPOT UNIT 27                          
         DC    F'81075'                                                         
         DC    F'55090'                                                         
         DC    F'43920'                                                         
         DC    F'23605'                                                         
         DC    F'81075'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'55090'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,28,28)    TV WILDSPOT UNIT 28                          
         DC    F'82410'                                                         
         DC    F'55745'                                                         
         DC    F'44460'                                                         
         DC    F'23915'                                                         
         DC    F'82410'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'55745'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,29,29)    TV WILDSPOT UNIT 29                          
         DC    F'83800'                                                         
         DC    F'56495'                                                         
         DC    F'44990'                                                         
         DC    F'24235'                                                         
         DC    F'83800'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'56495'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,30,30)    TV WILDSPOT UNIT 30                          
         DC    F'85180'                                                         
         DC    F'57160'                                                         
         DC    F'45545'                                                         
         DC    F'24565'                                                         
         DC    F'85180'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'57160'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,31,31)    TV WILDSPOT UNIT 31                          
         DC    F'86210'                                                         
         DC    F'57890'                                                         
         DC    F'46045'                                                         
         DC    F'24865'                                                         
         DC    F'86210'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'57890'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,32,32)    TV WILDSPOT UNIT 32                          
         DC    F'87165'                                                         
         DC    F'58570'                                                         
         DC    F'46620'                                                         
         DC    F'25190'                                                         
         DC    F'87165'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'58570'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,33,33)    TV WILDSPOT UNIT 33                          
         DC    F'88245'                                                         
         DC    F'59250'                                                         
         DC    F'47155'                                                         
         DC    F'25485'                                                         
         DC    F'88245'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'59250'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,34,34)    TV WILDSPOT UNIT 34                          
         DC    F'89285'                                                         
         DC    F'59955'                                                         
         DC    F'47695'                                                         
         DC    F'25860'                                                         
         DC    F'89285'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'59955'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,35,35)    TV WILDSPOT UNIT 35                          
         DC    F'90325'                                                         
         DC    F'60600'                                                         
         DC    F'48225'                                                         
         DC    F'26155'                                                         
         DC    F'90325'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'60600'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,36,36)    TV WILDSPOT UNIT 36                          
         DC    F'91355'                                                         
         DC    F'61135'                                                         
         DC    F'48630'                                                         
         DC    F'26445'                                                         
         DC    F'91355'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'61135'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,37,37)    TV WILDSPOT UNIT 37                          
         DC    F'92360'                                                         
         DC    F'61635'                                                         
         DC    F'48990'                                                         
         DC    F'26780'                                                         
         DC    F'92360'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'61635'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,38,38)    TV WILDSPOT UNIT 38                          
         DC    F'93415'                                                         
         DC    F'62225'                                                         
         DC    F'49440'                                                         
         DC    F'27085'                                                         
         DC    F'93415'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'62225'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,39,39)    TV WILDSPOT UNIT 39                          
         DC    F'94420'                                                         
         DC    F'62685'                                                         
         DC    F'49750'                                                         
         DC    F'27385'                                                         
         DC    F'94420'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'62685'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,40,40)    TV WILDSPOT UNIT 40                          
         DC    F'95465'                                                         
         DC    F'63215'                                                         
         DC    F'50170'                                                         
         DC    F'27730'                                                         
         DC    F'95465'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'63215'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,41,41)    TV WILDSPOT UNIT 41                          
         DC    F'96155'                                                         
         DC    F'63550'                                                         
         DC    F'50515'                                                         
         DC    F'27960'                                                         
         DC    F'96155'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'63550'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,42,42)    TV WILDSPOT UNIT 42                          
         DC    F'96840'                                                         
         DC    F'64245'                                                         
         DC    F'50920'                                                         
         DC    F'28255'                                                         
         DC    F'96840'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'64245'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,43,43)    TV WILDSPOT UNIT 43                          
         DC    F'97530'                                                         
         DC    F'64765'                                                         
         DC    F'51285'                                                         
         DC    F'28480'                                                         
         DC    F'97530'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'64765'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,44,44)    TV WILDSPOT UNIT 44                          
         DC    F'98285'                                                         
         DC    F'65285'                                                         
         DC    F'51635'                                                         
         DC    F'28790'                                                         
         DC    F'98285'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'65285'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,45,45)    TV WILDSPOT UNIT 45                          
         DC    F'98905'                                                         
         DC    F'65770'                                                         
         DC    F'52040'                                                         
         DC    F'29020'                                                         
         DC    F'98905'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'65770'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,46,46)    TV WILDSPOT UNIT 46                          
         DC    F'99640'                                                         
         DC    F'66300'                                                         
         DC    F'52440'                                                         
         DC    F'29345'                                                         
         DC    F'99640'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'66300'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,47,47)    TV WILDSPOT UNIT 47                          
         DC    F'100330'                                                        
         DC    F'66825'                                                         
         DC    F'52815'                                                         
         DC    F'29595'                                                         
         DC    F'100330'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'66825'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,48,48)    TV WILDSPOT UNIT 48                          
         DC    F'101035'                                                        
         DC    F'67330'                                                         
         DC    F'53205'                                                         
         DC    F'29850'                                                         
         DC    F'101035'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'67330'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,49,49)    TV WILDSPOT UNIT 49                          
         DC    F'101720'                                                        
         DC    F'67845'                                                         
         DC    F'53590'                                                         
         DC    F'30105'                                                         
         DC    F'101720'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'67845'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,50,50)    TV WILDSPOT UNIT 50                          
         DC    F'102425'                                                        
         DC    F'68350'                                                         
         DC    F'53990'                                                         
         DC    F'30370'                                                         
         DC    F'102425'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'68350'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(48,40,51,255)   TV WILDSPOT UNITS 51+                        
         DC    F'133'                                                           
         DC    F'103'                                                           
         DC    F'76'                                                            
         DC    F'41'                                                            
         DC    F'133'                                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'103'               ST, SAME AS SOC                             
                                                                                
                                                                                
                                                                                
         SPACE 1                                                                
         EJECT                                                                  
         DC    AL1(60,40,1,1)      TV WSP&NET SPC/NETWORK UNITS 1               
         DC    F'71430'            PP,CAR,SD                                    
         DC    F'52405'            SOC,GD                                       
         DC    F'33330'            VO,SS                                        
         DC    F'19070'            GS                                           
         DC    F'71430'            SA                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'52405'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,2,2)      TV WSP&NET SPC/NETWORK UNITS 2               
         DC    F'71430'                                                         
         DC    F'52405'                                                         
         DC    F'33330'                                                         
         DC    F'19070'                                                         
         DC    F'71430'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'52405'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,3,3)      TV WSP&NET SPC/NETWORK UNITS 3               
         DC    F'71430'                                                         
         DC    F'52405'                                                         
         DC    F'33330'                                                         
         DC    F'19070'                                                         
         DC    F'71430'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'52405'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,4,4)      TV WSP&NET SPC/NETWORK UNIT 4                
         DC    F'71430'                                                         
         DC    F'52405'                                                         
         DC    F'33330'                                                         
         DC    F'19070'                                                         
         DC    F'71430'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'52405'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,5,5)      TV WSP&NET SPC/NETWORK UNIT 5                
         DC    F'71430'                                                         
         DC    F'52405'                                                         
         DC    F'33330'                                                         
         DC    F'19070'                                                         
         DC    F'71430'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'52405'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,6,6)      TV WSP&NET SPC/NETWORK UNIT 6                
         DC    F'73335'                                                         
         DC    F'53560'                                                         
         DC    F'34530'                                                         
         DC    F'19670'                                                         
         DC    F'73335'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'53560'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,7,7)      TV WSP&NET SPC/NETWORK UNIT 7                
         DC    F'75250'                                                         
         DC    F'54745'                                                         
         DC    F'35760'                                                         
         DC    F'20305'                                                         
         DC    F'75250'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'54745'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,8,8)      TV WSP&NET SPC/NETWORK UNIT 8                
         DC    F'77205'                                                         
         DC    F'55940'                                                         
         DC    F'36935'                                                         
         DC    F'20940'                                                         
         DC    F'77205'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'55940'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,9,9)      TV WSP&NET SPC/NETWORK UNIT 9                
         DC    F'79130'                                                         
         DC    F'57150'                                                         
         DC    F'38220'                                                         
         DC    F'21600'                                                         
         DC    F'79130'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'57150'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,10,10)    TV WSP&NET SPC/NETWORK UNIT 10               
         DC    F'80975'                                                         
         DC    F'58310'                                                         
         DC    F'39375'                                                         
         DC    F'22195'                                                         
         DC    F'80975'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'58310'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,11,11)    TV WSP&NET SPC/NETWORK UNIT 11               
         DC    F'82935'                                                         
         DC    F'59495'                                                         
         DC    F'40600'                                                         
         DC    F'22690'                                                         
         DC    F'82935'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'59495'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,12,12)    TV WSP&NET SPC/NETWORK UNIT 12               
         DC    F'84815'                                                         
         DC    F'60675'                                                         
         DC    F'41810'                                                         
         DC    F'23180'                                                         
         DC    F'84815'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'60675'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,13,13)    TV WSP&NET SPC/NETWORK UNIT 13               
         DC    F'86710'                                                         
         DC    F'61825'                                                         
         DC    F'43025'                                                         
         DC    F'23685'                                                         
         DC    F'86710'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'61825'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,14,14)    TV WSP&NET SPC/NETWORK UNIT 14               
         DC    F'88670'                                                         
         DC    F'63060'                                                         
         DC    F'44290'                                                         
         DC    F'24190'                                                         
         DC    F'88670'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'63060'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,15,15)    TV WSP&NET SPC/NETWORK UNIT 15               
         DC    F'90570'                                                         
         DC    F'64240'                                                         
         DC    F'45450'                                                         
         DC    F'24655'                                                         
         DC    F'90570'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'64240'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,16,16)    TV WSP&NET SPC/NETWORK UNIT 16               
         DC    F'92470'                                                         
         DC    F'65360'                                                         
         DC    F'46620'                                                         
         DC    F'25120'                                                         
         DC    F'92470'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'65360'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,17,17)    TV WSP&NET SPC/NETWORK UNIT 17               
         DC    F'94350'                                                         
         DC    F'66570'                                                         
         DC    F'47820'                                                         
         DC    F'25650'                                                         
         DC    F'94350'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'66570'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,18,18)    TV WSP&NET SPC/NETWORK UNIT 18               
         DC    F'96300'                                                         
         DC    F'67760'                                                         
         DC    F'48990'                                                         
         DC    F'26110'                                                         
         DC    F'96300'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'67760'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,19,19)    TV WSP&NET SPC/NETWORK UNIT 19               
         DC    F'98150'                                                         
         DC    F'68955'                                                         
         DC    F'50185'                                                         
         DC    F'26565'                                                         
         DC    F'98150'                                                         
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'68955'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,20,20)    TV WSP&NET SPC/NETWORK UNIT 20               
         DC    F'100085'                                                        
         DC    F'70090'                                                         
         DC    F'51405'                                                         
         DC    F'27240'                                                         
         DC    F'100085'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'70090'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,21,21)    TV WSP&NET SPC/NETWORK UNIT 21               
         DC    F'102030'                                                        
         DC    F'71115'                                                         
         DC    F'52495'                                                         
         DC    F'27505'                                                         
         DC    F'102030'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'71115'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,22,22)    TV WSP&NET SPC/NETWORK UNIT 22               
         DC    F'103930'                                                        
         DC    F'72110'                                                         
         DC    F'53690'                                                         
         DC    F'27920'                                                         
         DC    F'103930'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'72110'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,23,23)    TV WSP&NET SPC/NETWORK UNIT 23               
         DC    F'105840'                                                        
         DC    F'73120'                                                         
         DC    F'54905'                                                         
         DC    F'28315'                                                         
         DC    F'105840'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'73120'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,24,24)    TV WSP&NET SPC/NETWORK UNIT 24               
         DC    F'107705'                                                        
         DC    F'74150'                                                         
         DC    F'56055'                                                         
         DC    F'28760'                                                         
         DC    F'107705'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'74150'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,25,25)    TV WSP&NET SPC/NETWORK UNIT 25               
         DC    F'109670'                                                        
         DC    F'75140'                                                         
         DC    F'57230'                                                         
         DC    F'29190'                                                         
         DC    F'109670'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'75140'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,26,26)    TV WSP&NET SPC/NETWORK UNIT 26               
         DC    F'111535'                                                        
         DC    F'76095'                                                         
         DC    F'57985'                                                         
         DC    F'29620'                                                         
         DC    F'111535'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'76095'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,27,27)    TV WSP&NET SPC/NETWORK UNIT 27               
         DC    F'113445'                                                        
         DC    F'77120'                                                         
         DC    F'58750'                                                         
         DC    F'30045'                                                         
         DC    F'113445'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'77120'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,28,28)    TV WSP&NET SPC/NETWORK UNIT 28               
         DC    F'115245'                                                        
         DC    F'78105'                                                         
         DC    F'59460'                                                         
         DC    F'30455'                                                         
         DC    F'115245'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'78105'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,29,29)    TV WSP&NET SPC/NETWORK UNIT 29               
         DC    F'117165'                                                        
         DC    F'79150'                                                         
         DC    F'60195'                                                         
         DC    F'30835'                                                         
         DC    F'117165'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'79150'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,30,30)    TV WSP&NET SPC/NETWORK UNIT 30               
         DC    F'119030'                                                        
         DC    F'80115'                                                         
         DC    F'60930'                                                         
         DC    F'31300'                                                         
         DC    F'119030'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'80115'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,31,31)    TV WSP&NET SPC/NETWORK UNIT 31               
         DC    F'120540'                                                        
         DC    F'81110'                                                         
         DC    F'61630'                                                         
         DC    F'31730'                                                         
         DC    F'120540'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'81110'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,32,32)    TV WSP&NET SPC/NETWORK UNIT 32               
         DC    F'121940'                                                        
         DC    F'82035'                                                         
         DC    F'62330'                                                         
         DC    F'32090'                                                         
         DC    F'121940'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'82035'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,33,33)    TV WSP&NET SPC/NETWORK UNIT 33               
         DC    F'123370'                                                        
         DC    F'83010'                                                         
         DC    F'63095'                                                         
         DC    F'32480'                                                         
         DC    F'123370'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'83010'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,34,34)    TV WSP&NET SPC/NETWORK UNIT 34               
         DC    F'124880'                                                        
         DC    F'83665'                                                         
         DC    F'63795'                                                         
         DC    F'32855'                                                         
         DC    F'124880'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'83665'           ST, SAME AS SOC                               
         SPACE 1                                                                
         DC    AL1(60,40,35,35)    TV WSP&NET SPC/NETWORK UNIT 35               
         DC    F'126305'                                                        
         DC    F'84925'                                                         
         DC    F'64570'                                                         
         DC    F'33255'                                                         
         DC    F'126305'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'84925'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,36,36)    TV WSP&NET SPC/NETWORK UNIT 36               
         DC    F'127755'                                                        
         DC    F'85700'                                                         
         DC    F'65050'                                                         
         DC    F'33675'                                                         
         DC    F'127755'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'85700'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,37,37)    TV WSP&NET SPC/NETWORK UNIT 37               
         DC    F'129205'                                                        
         DC    F'86435'                                                         
         DC    F'65530'                                                         
         DC    F'34075'                                                         
         DC    F'129205'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'86435'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,38,38)    TV WSP&NET SPC/NETWORK UNIT 38               
         DC    F'130650'                                                        
         DC    F'87140'                                                         
         DC    F'66000'                                                         
         DC    F'34475'                                                         
         DC    F'130650'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'87140'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,39,39)    TV WSP&NET SPC/NETWORK UNIT 39               
         DC    F'132140'                                                        
         DC    F'87855'                                                         
         DC    F'66480'                                                         
         DC    F'34845'                                                         
         DC    F'132140'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'87855'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,40,40)    TV WSP&NET SPC/NETWORK UNIT 40               
         DC    F'133545'                                                        
         DC    F'88600'                                                         
         DC    F'66995'                                                         
         DC    F'35230'                                                         
         DC    F'133545'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'88600'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,41,41)    TV WSP&NET SPC/NETWORK UNIT 41               
         DC    F'134545'                                                        
         DC    F'89285'                                                         
         DC    F'67465'                                                         
         DC    F'35520'                                                         
         DC    F'134545'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'89285'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,42,42)    TV WSP&NET SPC/NETWORK UNIT 42               
         DC    F'135485'                                                        
         DC    F'89980'                                                         
         DC    F'67945'                                                         
         DC    F'35900'                                                         
         DC    F'135485'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'89980'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,43,43)    TV WSP&NET SPC/NETWORK UNIT 43               
         DC    F'136450'                                                        
         DC    F'90735'                                                         
         DC    F'68400'                                                         
         DC    F'36245'                                                         
         DC    F'136450'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'90735'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,44,44)    TV WSP&NET SPC/NETWORK UNIT 44               
         DC    F'137475'                                                        
         DC    F'91420'                                                         
         DC    F'68900'                                                         
         DC    F'36560'                                                         
         DC    F'137475'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'91420'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,45,45)    TV WSP&NET SPC/NETWORK UNIT 45               
         DC    F'138405'                                                        
         DC    F'92065'                                                         
         DC    F'69410'                                                         
         DC    F'36920'                                                         
         DC    F'138405'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'92065'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,46,46)    TV WSP&NET SPC/NETWORK UNIT 46               
         DC    F'139425'                                                        
         DC    F'92815'                                                         
         DC    F'69825'                                                         
         DC    F'37215'                                                         
         DC    F'139425'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'92815'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,47,47)    TV WSP&NET SPC/NETWORK UNIT 47               
         DC    F'140350'                                                        
         DC    F'93460'                                                         
         DC    F'70315'                                                         
         DC    F'37475'                                                         
         DC    F'140350'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'93460'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,48,48)    TV WSP&NET SPC/NETWORK UNIT 48               
         DC    F'141295'                                                        
         DC    F'94120'                                                         
         DC    F'70760'                                                         
         DC    F'37835'                                                         
         DC    F'141295'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'94120'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,49,49)    TV WSP&NET SPC/NETWORK UNIT 49               
         DC    F'142295'                                                        
         DC    F'94880'                                                         
         DC    F'71215'                                                         
         DC    F'38075'                                                         
         DC    F'142295'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'94880'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,50,50)    TV WSP&NET SPC/NETWORK UNIT 50               
         DC    F'143260'                                                        
         DC    F'95550'                                                         
         DC    F'71710'                                                         
         DC    F'38425'                                                         
         DC    F'143260'                                                        
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'95550'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(60,40,51,255)   TV WSP&NET/NETWORK UNITS 51+                 
         DC    F'185'                                                           
         DC    F'142'                                                           
         DC    F'94'                                                            
         DC    F'51'                                                            
         DC    F'185'                                                           
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    F'142'              ST, SAME AS SOC                              
         SPACE 1                                                                
CAUTTBL  DC    AL1(68,44,0,0)      AUDITION INTERVIEW - TV                      
         DC    F'6900'             PP,CAR,SD                                    
         DC    F'6900'             SOC,GD                                       
         DC    F'6900'             VO,SS                                        
         DC    F'6900'             GS                                           
         DC    F'6900'             SA                                           
         DC    F'6900'             DEM                                          
         DC    F'6900'             E                                            
         DC    F'6900'             GE                                           
         DC    F'6900'             ST                                           
         DC    F'6900'             EVERYBODY ELSE-US,SI,SB,PT                   
         SPACE 1                                                                
CAURTBL  DC    AL1(69,12,0,0)      AUDITION INTERVIEW - RADIO                   
         DC    F'6900'             SS,SV                                        
         DC    F'6900'             MV,GS                                        
         EJECT                                                                  
CDMTBL   DC    AL1(70,44,2,2)      TV PRESENTATION DEMO 2                       
         DC    F'25450'            PP,CAR,SD                                    
         DC    F'25450'            SOC,GD                                       
         DC    F'25450'            VO,SS                                        
         DC    F'25450'            GS                                           
         DC    F'25450'            SA                                           
         DC    F'25450'            DEM                                          
         DC    F'25450'            E                                            
         DC    F'25450'            GE                                           
         DC    F'25450'            ST                                           
         DC    F'25450'            ALL ELSE-US,SI,SB,PT                         
         SPACE 1                                                                
         DC    AL1(70,44,3,255)    TV PRESENTATION DEMO 3+                      
         DC    F'12800'            PP,CAR,SD                                    
         DC    F'12800'            SOC,GD                                       
         DC    F'12800'            VO,SS                                        
         DC    F'12800'            GS                                           
         DC    F'12800'            SA                                           
         DC    F'12800'            DEM                                          
         DC    F'12800'            E                                            
         DC    F'12800'            GE                                           
         DC    F'12800'            ST                                           
         DC    F'12800'            ALL ELSE-US,SI,SB,PT                         
         SPACE 1                                                                
BSCHTBL  DC    AL1(80,40,0,0)      NATIONAL TV  - HOURLY RATE                   
         DC    F'8350'             PP,SD,CAR                                    
         DC    F'8350'             SOC,GD                                       
         DC    F'6900'             VO,SS                                        
         DC    F'6900'             GS                                           
         DC    F'12550'            SA = PP+50%                                  
         DC    F'8350'             DEM                                          
         DC    F'5100'             E                                            
         DC    F'3300'             GE                                           
         DC    F'8350'             ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(81,40,0,0)      NATIONAL TV  - OVERTIME HOURLY RATE          
         DC    F'10800'            PP,SD,CAR                                    
         DC    F'10800'            SOC,GD                                       
         DC    F'8700'             VO,SS                                        
         DC    F'8700'             GS                                           
         DC    F'16200'            SA = PP+50%                                  
         DC    F'10800'            DEM                                          
         DC    F'6200'             E                                            
         DC    F'4250'             GE                                           
         DC    F'10800'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(82,40,0,0)      NATIONAL TV  - DBLTIME HOURLY RATE           
         DC    F'12200'            PP,SD,CAR                                    
         DC    F'12200'            SOC,GD                                       
         DC    F'10800'            VO,SS                                        
         DC    F'10800'            GS                                           
         DC    F'18300'            SA = PP+50%                                  
         DC    F'12200'            DEM                                          
         DC    F'7700'             E                                            
         DC    F'5100'             GE                                           
         DC    F'12200'            ST, SAME AS SOC                              
         SPACE 1                                                                
CNMTBL   DC    AL1(100,40,0,0)     NEW MEDIA VIDEO 4 WEEKS                      
         DC    F'11200'            PP,SD,CAR                                    
         DC    F'11200'            SOC,GD                                       
         DC    F'8225'             VO,SS                                        
         DC    F'3675'             GS                                           
         DC    F'16800'            SA = PP+50%                                  
         DC    F'0'                DEM                                          
         DC    F'0'                E                                            
         DC    F'0'                GE                                           
         DC    F'11200'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(101,12,0,0)     NEW MEDIA AUDIO 4 WEEKS                      
         DC    F'8750'             SS,SV                                        
         DC    F'6650'             MV,GS                                        
         SPACE 1                                                                
         DC    AL1(110,40,0,0)     NEW MEDIA VIDEO 8 WEEKS                      
         DC    F'16000'            PP,SD,CAR                                    
         DC    F'16000'            SOC,GD                                       
         DC    F'11750'            VO,SS                                        
         DC    F'5250'             GS                                           
         DC    F'24000'            SA = PP+50%                                  
         DC    F'0'                DEM                                          
         DC    F'0'                E                                            
         DC    F'0'                GE                                           
         DC    F'16000'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(111,12,0,0)     NEW MEDIA AUDIO 8 WEEKS                      
         DC    F'12500'            SS,SV                                        
         DC    F'9500'             MV,GS                                        
         SPACE 1                                                                
         DC    AL1(120,40,0,0)     NEW MEDIA VIDEO 26 WEEKS                     
         DC    F'24000'            PP,SD,CAR                                    
         DC    F'24000'            SOC,GD                                       
         DC    F'17625'            VO,SS                                        
         DC    F'7875'             GS                                           
         DC    F'36000'            SA = PP+50%                                  
         DC    F'0'                DEM                                          
         DC    F'0'                E                                            
         DC    F'0'                GE                                           
         DC    F'24000'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(121,12,0,0)     NEW MEDIA AUDIO 26 WEEKS                     
         DC    F'18750'            SS,SV                                        
         DC    F'14250'            MV,GS                                        
         SPACE 1                                                                
         DC    AL1(130,40,0,0)     NEW MEDIA VIDEO 1 YEAR                       
         DC    F'32000'            PP,SD,CAR                                    
         DC    F'32000'            SOC,GD                                       
         DC    F'23500'            VO,SS                                        
         DC    F'10500'            GS                                           
         DC    F'48000'            SA = PP+50%                                  
         DC    F'0'                DEM                                          
         DC    F'0'                E                                            
         DC    F'0'                GE                                           
         DC    F'32000'            ST, SAME AS SOC                              
         SPACE 1                                                                
         DC    AL1(131,12,0,0)     NEW MEDIA AUDIO 1 YEAR                       
         DC    F'25000'            SS,SV                                        
         DC    F'19000'            MV,GS                                        
         SPACE 1                                                                
CNMATBL  DC    AL1(140,12,0,0)     NEW MEDIA AUDIO 4 WEEKS ADDTL CUTS           
         DC    F'4375'             SS,SV                                        
         DC    F'3325'             MV,GS                                        
         SPACE 1                                                                
         DC    AL1(141,12,0,0)     NEW MEDIA AUDIO 8 WEEKS ADDTL CUTS           
         DC    F'6250'             SS,SV                                        
         DC    F'4750'             MV,GS                                        
         SPACE 1                                                                
         DC    AL1(142,12,0,0)     NEW MEDIA AUDIO 26 WEEKS ADDTL CUTS          
         DC    F'9375'             SS,SV                                        
         DC    F'7125'             MV,GS                                        
         SPACE 1                                                                
         DC    AL1(143,12,0,0)     NEW MEDIA AUDIO 1 YEAR ADDTL CUTS            
         DC    F'12500'            SS,SV                                        
         DC    F'9500'             MV,GS                                        
         SPACE 1                                                                
         DC    AL1(144,12,0,0)     BSC NEW MEDIA AUDIO ADDTL CUTS               
         DC    F'12500'            SS,SV                                        
         DC    F'9500'             MV,GS                                        
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
**PAN#1  DC    CL21'065TAGEN72   02/12/09'                                      
         END                                                                    
