*          DATA SET ACBRA03    AT LEVEL 059 AS OF 09/18/09                      
*PHASE T62403A                                                                  
ACBRA03  TITLE '- Invokable downloads'                                          
         PRINT NOGEN                                                            
                                                                                
ACBRA03  LKSVR TYPE=D,BLOCKS=(B#SAVED,SAVED),REQUEST=*,SYSTEM=ACCSYSQ, +        
               CODE=INIT,IDF=Y                                                  
REQUEST  DS    0X                                                               
                                                                                
INIT     NMOD1 SAVEL,**BO03**                                                   
         LR    R8,RC                                                            
         USING SAVED,R8            R8=A(local saved)                            
         LARL  RC,GLOBALS                                                       
         USING GLOBALS,RC          RC=A(Global literals)                        
         STC   RF,INDEXVAL         Save routine index value                     
         USING LP_D,LOCALLPD                                                    
                                                                                
         LR    R0,R1               Point to caller's LP_D                       
         LHI   R1,L'LOCALLPD                                                    
         LA    RE,LP_D                                                          
         LR    RF,R1                                                            
         MVCL  RE,R0               Copy locally                                 
                                                                                
         L     R9,LP_ABLK1                                                      
         USING WORKD,R9            R9=A(Global w/s)                             
         MVC   SVALP,ALP           Save current ALP value                       
         LA    R0,LP_D                                                          
         ST    R0,ALP              and point to local one                       
                                                                                
         LA    R1,LP_ASVR1         Locate server we are running in              
         LHI   R0,LP_NSVRS                                                      
INIT02   CLC   LP_ASVR+1(3),1(R1)  Match server to server list                  
         JE    INIT04                                                           
         AHI   R1,L'LP_ASVR1                                                    
         JCT   R0,INIT02                                                        
         DC    H'0'                                                             
                                                                                
INIT04   LLC   RE,0(R1)            RE=work index value                          
         ST    RB,LP_ASVR          Point server to me                           
         XC    LP_ASVR1(LP_NSVRS*L'LP_ASVR1),LP_ASVR1                           
         ST    RB,LP_ASVR1         Set A(server 1)                              
         STC   RE,LP_ASVR1         Set work index value in list                 
         ST    R8,LP_ABLK2         Point to my SAVED                            
                                                                                
         LLC   RE,INDEXVAL         Get server index value                       
         SLL   RE,2                *4                                           
         CHI   RE,ROUTABL          Ensure good index value                      
         JL    *+6                                                              
         DC    H'0'                                                             
         LA    RE,ROUTAB(RE)                                                    
         XR    RF,RF                                                            
         ICM   RF,3,0(RE)                                                       
         AR    RF,RB               RF=A(Routine)                                
                                                                                
         XR    R0,R0                                                            
         ICM   R0,3,2(RE)          R0=working storage amount                    
         BZR   RF                                                               
         AHI   R0,7                Round amount to doublewords                  
         SRL   R0,3                                                             
         SLL   R0,3                                                             
         LR    R3,RD               Acquire extra storage from w/s pool          
         AR    R3,R0                                                            
         L     R4,4(RD)                                                         
         ST    R4,4(R3)                                                         
         ST    R3,8(R4)                                                         
         LR    RC,RD                                                            
         LR    RD,R3                                                            
         LR    R4,RC               and clear it                                 
         XR    R2,R2                                                            
         XR    R3,R3                                                            
         MVCL  R4,R2                                                            
         BR    RF                                                               
         DROP  RB                                                               
                                                                                
ROUTAB   DS    0XL4                ** Routine table **                          
         DC    AL2(DLDXXX-ACBRA03,XXXSAVEL)                                     
ROUTABL  EQU   *-ROUTAB                                                         
         EJECT                                                                  
DLDXXX   STM   R2,RB,LP_R2RB                                                    
         LARL  RE,OUTXXX           Point to output map                          
         ST    RE,LP_AOMAP                                                      
         MVC   HELLO,=C'HELLO'                                                  
         GOTOR LP_APUTO,LP_D       Call DDLINK output routine                   
         J     EXITSVR                                                          
                                                                                
OUTXXX   LKOUT H                                                                
                                                                                
XXXREC1  LKOUT R,1                                                              
Array    LKOUT C,1,(A,ARYXXX)                                                   
         LKOUT E                                                                
         LKOUT X                                                                
                                                                                
ARYXXX   LKOUT A,(D,B#SAVED,SAVED),ROWNAME=SAVED                                
         LKOUT C,1,HELLO,CHAR                                                   
         LKOUT E                                                                
         EJECT                                                                  
EXITSVR  MVC   ALP,SVALP           Restore ALP                                  
         J     EXITY                                                            
                                                                                
EXITN    DS    0H                  Set CC not equal                             
EXITL    SR    RE,RE               Set CC low                                   
         J     *+8                                                              
EXITH    LHI   RE,2                Set CC high                                  
         J     EXITCC                                                           
EXITY    LHI   RE,1                Set CC equal                                 
EXITCC   CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
                                                                                
GLOBALS  DS    0D                                                               
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACBRAWRKD                                                      
         PRINT ON                                                               
                                                                                
SAVED    DSECT ,                   ** Saved storage area **                     
                                                                                
SVALP    DS    AL4                 Saved ALP value                              
LOCALLPD DS    XL(LP_LNQ)          Local LP_D                                   
                                                                                
SAVEL    EQU   *-SAVED                                                          
                                                                                
INDEXVAL DS    X                   Routine index value                          
                                                                                
XXXSAVE  DS    0D                  For XXX download                             
HELLO    DS    CL5                                                              
XXXSAVEL EQU   *-XXXSAVE                                                        
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'059ACBRA03   09/18/09'                                      
         END                                                                    
