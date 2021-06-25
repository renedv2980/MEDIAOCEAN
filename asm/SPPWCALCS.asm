*          DATA SET SPPWCALCS  AT LEVEL 032 AS OF 05/01/02                      
*PHASE T00A79A                                                                  
*-------------------------------------------------------------------*           
* 10MAR98 MHER  CLIENT TAX COMPUTED FROM CLIENT NET DIRECTLY        *           
*               AND UNFORTUNATELY GETPW  MUST BE CHANGED TOO        *           
*-------------------------------------------------------------------*           
         SPACE 2                                                                
*====================================================================*          
* THIS ROUTINE DOES COMPUTATIONS FOR THE WESTERN INTL MEDIA          *          
* PROFIT WITHIN (PW) SUBSYSTEM                                       *          
*                                                                    *          
* PARAMETER LIST - DEFINED IN SPPWBLOCK                              *          
*                                                                    *          
* ===================================================================*          
T00A79   TITLE 'PWCALC  - COMPUTE WI PROFIT WITHIN VALUES'                      
         PRINT NOGEN                                                            
PWCALC   CSECT                                                                  
         NMOD1 WORKX-WORKD,SPPWCALC                                             
         USING WORKD,RC                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING PWBLKD,RA                                                        
*                                                                               
         OC    PWFCTRG,PWFCTRG                                                  
         BNZ   PW10                                                             
         MVC   PWFCTRG,=F'10000'                                                
         MVC   PWFCTRN,=F'8500'                                                 
*                                                                               
* GET NET PERCENT (85% = 8500)                                                  
*                                                                               
PW10     L     R1,PWFCTRN                                                       
         M     R0,=F'10000'                                                     
         D     R0,PWFCTRG                                                       
         ST    R1,NETPCT                                                        
*                                                                               
* CHECK PW PERCENT = 0 (PWPCT = X'00800000' = ONLY 3 BYTES IN EST REC)          
*                                                                               
         CLC   PWPCT,=X'00800000'                                               
         BE    *+14                                                             
         CLC   PWPCT,=X'FF800000'                                               
         BNE   *+10                                                             
         XC    PWPCT,PWPCT                   SET 0 PWPCT                        
*                                                                               
         CLI   PWACT,PWGETBUY                                                   
         BE    PWBUY                                                            
         CLI   PWACT,PWGETGOL                                                   
         BE    PWGOL                                                            
         CLI   PWACT,PWGETPW                                                    
         BE    PWPW                                                             
         MVI   PWERR,PWACTERR                                                   
EXIT     XIT1                                                                   
         EJECT                                                                  
*===================================================================*           
* EFFECTIVE 06MAR98                                                 *           
*                                                                   *           
* CLTTAX= TAXPCT*CLTNET                                             *           
*                                                                   *           
* WIMNET   = (WIMGRSTX-TAX) * NETPCT                                *           
* WIMNETTX = WIMNET + TAX                                           *           
*===================================================================*           
         SPACE 1                                                                
PWBUY    DS    0H                                                               
         OC    PWPCT,PWPCT                                                      
         BNZ   PWBUY0                                                           
         MVC   PWVAL,PWACTBUY      SET CLT$=WIM$                                
         MVC   PWCLTTAX,PWTAX      SET CLTTAX = WIMTAX                          
         B     EXIT                                                             
*                                                                               
PWBUY0   CLC   PWPCT,=X'80000000'  TEST $OVERRIDE (NO PCT)                      
         BNE   PWBUY2                                                           
         XC    PWVAL,PWVAL         CLT$ = 0                                     
         B     EXIT                                                             
*                                                                               
PWBUY2   L     R1,PWACTBUY                                                      
         S     R1,PWTAX                                                         
         M     R0,NETPCT                                                        
         L     RF,=F'10000'                                                     
         BAS   RE,DIV                                                           
         ST    R1,WIMNET                                                        
*                                                                               
         M     R0,=F'10000'        SCALE                                        
         L     RF,NETPCT                                                        
         S     RF,PWPCT            /(NETPCT-PWPCT)                              
         BAS   RE,DIV                                                           
         ST    R1,CLTGRS           SAVE GROSS WITHOUT TAX                       
*                                                                               
         M     R0,NETPCT                                                        
         L     RF,=F'10000'                                                     
         BAS   RE,DIV                                                           
         ST    R1,CLTNET                                                        
* MULTIPLY CLTNET WIMTAX/WIMNET (TAX RATE) TO GET CLTTAX                        
         M     R0,PWTAX                                                         
         L     RF,WIMNET                                                        
         BAS   RE,DIV                                                           
         ST    R1,PWCLTTAX         RETURN CLTTAX                                
         A     R1,CLTGRS                                                        
         ST    R1,CLTGRSTX         AND GROSS + TAX                              
         ST    R1,PWVAL            STORE IT HERE TOO                            
         B     EXIT                                                             
         EJECT                                                                  
*===================================================================*           
* NOTE THAT PWACTGOL IS CLTGRSTX (CLT GROSS INCLUDING TAX)          *           
* GET CLTGRS = (CLTGRSTX/(1+TAXRATE*NETPCT))                        *           
* WIMNET     = CLTGRS*(NETPCT-PWPCT)                                *           
* WIMGRS     = WIMNET/NETPCT                                        *           
*===================================================================*           
         SPACE 1                                                                
PWGOL    L     R1,PWTAXRT          GET TAX RATE                                 
         M     R0,NETPCT           X NETPCT                                     
         L     RF,=F'10000'        SCALE                                        
         BAS   RE,DIV              GIVES NET TAX RATE (5% = 5000)               
*                                                                               
         L     RF,=F'100000'       100 PERCENT TO 3 DEC                         
         AR    RF,R1               GIVES 1+TAXRATE                              
*                                                                               
         L     R1,PWACTGOL         GET CLTGRS INCL TAX                          
         M     R0,=F'100000'       SCALE FOR DIV                                
         BAS   RE,DIV                                                           
         ST    R1,CLTGRS           GIVES CLTGRS                                 
         L     R0,PWACTGOL                                                      
         SR    R0,R1                                                            
         ST    R0,PWCLTTAX         RETURN TAX                                   
*                                                                               
         L     R0,NETPCT                                                        
         S     R0,PWPCT                                                         
         MR    R0,R0                                                            
         L     RF,=F'10000'                                                     
         BAS   RE,DIV                                                           
         ST    R1,WIMNET                                                        
* NOW GROSS UP WIMNET                                                           
         M     R0,=F'10000'                                                     
         L     RF,NETPCT                                                        
         BAS   RE,DIV                                                           
         ST    R1,PWVAL                                                         
*                                                                               
         OC    PWTAXRT,PWTAXRT     TEST ANY TAX                                 
         BZ    EXIT                NO                                           
*                                                                               
         TM    PWFLAG,PWFLAG_NOTAX TEST SUPPRESS TAX                            
         BO    EXIT                                                             
* CALCULATE TAX ON WIMNET                                                       
         L     R1,WIMNET           NET                                          
         M     R0,PWTAXRT          X TAX RATE                                   
         L     RF,=F'100000'                                                    
         BAS   RE,DIV              SCALE                                        
         ST    R1,PWWIMTAX                                                      
         A     R1,PWVAL            AND ADD TO GET WIMGRSTX                      
         ST    R1,PWVAL                                                         
         B     EXIT                                                             
         EJECT                                                                  
*===================================================================*           
*                                                                   *           
* PWPCT = (CLTNET-WIMNET)/CLTGRS ALL AMOUNTS NOT INCLUDING TAX      *           
*                                                                   *           
* TAXRATE IS COMPUTED AS PWTAX/WIMNET                               *           
*===================================================================*           
         SPACE 1                                                                
PWPW     DS    0H                                                               
* GET WIMNET = (WIMGRSTX-TAX)*NETPCT                                            
         ICM   R1,15,PWACTBUY      WIMGRSTX                                     
         BNM   PWPW2                                                            
         SR    R1,R1                                                            
         B     PWPWX                                                            
*                                                                               
PWPW2    S     R1,PWTAX            LESS TAX                                     
         M     R0,NETPCT                                                        
         L     RF,=F'10000'        SCALE                                        
         BAS   RE,DIV                                                           
         ST    R1,WIMNET                                                        
* GET CLTGRS = (CLTGRSTX/(1+TAXRATE*NETPCT))                                    
*            = (CLTGRSTX*WIMNET)/(WIMNET+NETPCT*WIMTAX)                         
         L     R1,PWTAX            WIMTAX                                       
         M     R0,NETPCT           X NETPCT                                     
         L     RF,=F'10000'        SCALE                                        
         BAS   RE,DIV                                                           
         A     R1,WIMNET                                                        
         LR    RF,R1               SAVE DIVISOR                                 
*                                                                               
         L     R1,PWADJBUY         CLTGRSTX                                     
         M     R0,WIMNET           X WIMNET                                     
         BAS   RE,DIV                                                           
         ST    R1,CLTGRS           GIVES CLIENT GROSS                           
*                                                                               
         M     R0,NETPCT                                                        
         L     RF,=F'10000'        SCALE                                        
         BAS   RE,DIV                                                           
         ST    R1,CLTNET                                                        
*                                                                               
         L     R1,CLTNET                                                        
         S     R1,WIMNET                                                        
         M     R0,=F'10000'        SCALE                                        
         L     RF,CLTGRS                                                        
         BAS   RE,DIV                                                           
*                                                                               
PWPWX    ST    R1,PWVAL                                                         
         B     EXIT                                                             
         EJECT                                                                  
DIV      LTR   RF,RF               IF DIVISOR 0                                 
         BZ    NODIV                                                            
*                                                                               
DIV2     SLDA  R0,1                DOUBLE DIVIDEND                              
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         BR    RE                                                               
*                                                                               
NODIV    SR    R1,R1               USER GETS 0 BACK                             
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
NETPCT   DS    F                                                                
PROFIT   DS    F                                                                
CLTNET   DS    F                                                                
CLTNETTX DS    F                                                                
CLTGRS   DS    F                                                                
CLTGRSTX DS    F                                                                
WIMNET   DS    F                                                                
WIMNETTX DS    F                                                                
WIMGRS   DS    F                                                                
WIMGRSTX DS    F                                                                
WORKX    EQU   *                                                                
         SPACE 2                                                                
       ++INCLUDE SPPWBLOCK                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032SPPWCALCS 05/01/02'                                      
         END                                                                    
