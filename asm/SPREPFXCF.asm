*          DATA SET SPREPFXCF  AT LEVEL 007 AS OF 03/17/98                      
*PHASE SPFX02C+0                                                                
         TITLE 'SPFX02 - FIX UTILITY - CANADIAN BILLING PROFILES'               
SPFX02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RC,RR=R2                                                
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,CLTFRST                                                     
         BE   FX10                                                              
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
* REQFRST                                                                       
*                                                                               
FX10     DS    0H                                                               
         XC    X,X                                                              
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0B2'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),MED                                                    
         MVC   WORK+7(3),CLT                                                    
         L     RF,ADCLT                                                         
         LA    RF,COFFICE-CLTHDR(RF)                                            
         CLI   0(RF),C' '                                                       
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),0(RF)                                                 
*                                                                               
         GOTO1 GETPROF,DMCB,WORK,X,DATAMGR                                      
         GOTO1 HEXOUT,DMCB,X+11,P+10,1,=C'N'                                    
*                                                                               
         XC    X,X                                                              
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'SB4A'                                                 
         NI    WORK,X'BF'                                                       
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),MED                                                    
         MVC   WORK+7(3),CLT                                                    
         L     RF,ADCLT                                                         
         LA    RF,COFFICE-CLTHDR(RF)                                            
         CLI   0(RF),C' '                                                       
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),0(RF)                                                 
*                                                                               
         GOTO1 GETPROF,DMCB,WORK,X,DATAMGR                                      
         GOTO1 HEXOUT,DMCB,X+2,P+13,2,=C'N'                                     
*                                                                               
         MVC   P+21(1),X+1                                                      
         MVC   P(3),CLT                                                         
         L     RF,ADCLT                                                         
         MVC   P+4(1),COFFICE-CLTHDR(RF)                                        
*                                                                               
         GOTO1 REPORT                                                           
         MVI   MODE,CLTLAST                                                     
         B     EXIT                                                             
*                                                                               
X        DS    XL256                                                            
*                                                                               
         LTORG                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007SPREPFXCF 03/17/98'                                      
         END                                                                    
