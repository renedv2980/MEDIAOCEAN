*          DATA SET SPPWCALCX  AT LEVEL 021 AS OF 05/01/02                      
*PHASE T00A79A                                                                  
*-------------------------------------------------------------------*           
* 06MAR98 MHER  CHANGE TAX CALCULATION TO GROSS UP BY NET-PW PCT    *           
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
         BNE   *+10                                                             
         NC    PWPCT,=X'FF7FFFFF'            TURN OFF BIT                       
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
* EFFECTIVE 06MAR98, THE TAX AMOUNT WILL BE GROSSED UP BY NET-PW PCT*           
* AT THE REQUEST OF WIM.                                            *           
*                                                                   *           
* SUBTRACT TAX BEFORE  NET CALC BECAUSE                             *           
* WIMGRSTX INCLUDES GROSSED UP TAX, NOT TAX ON NET                  *           
*                                                                   *           
* WIMNET   = (WIMGRSTX-TAX) * NETPCT                                *           
* WIMNETTX = WIMNET + TAX                                           *           
* CLTGRSTX = WIMNETTX / (NETPCT-PWPCT)                              *           
*===================================================================*           
         SPACE 1                                                                
PWBUY    DS    0H                                                               
         SR    R1,R1                                                            
         CLC   PWPCT,=X'80000000'  TEST $OVERRIDE (NO PCT)                      
         BNE   PWBUY2                                                           
         ST    R1,PWVAL            CLT$ = WIM$                                  
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
         OC    PWPCT,PWPCT         TEST 0 PW PCT                                
         BNZ   PWBUY4                                                           
         L     R1,WIMNET                                                        
         A     R1,PWTAX                                                         
         ST    R1,WIMNETTX         SET WIMNET + TAX                             
*                                                                               
         L     R1,PWACTBUY                                                      
         S     R1,PWTAX                                                         
         ST    R1,CLTGRS           SET CLTGRS WITHOUT TAX                       
* NEED TO GROSS UP NET TAX                                                      
         L     R1,PWTAX                                                         
         M     R0,=F'10000'                                                     
         L     RF,NETPCT                                                        
         S     RF,PWPCT            /(NETPCT-PWPCT)                              
         BAS   RE,DIV              NOW WE HAVE GROSS TAX                        
         ST    R1,PWCLTTAX         RETURN IT                                    
         A     R1,CLTGRS           ADD GROSS                                    
         ST    R1,CLTGRSTX         AND RETURN GROSS + GROSS TAX                 
         ST    R1,PWVAL            NEED IT HERE TOO                             
         B     EXIT                                                             
*                                                                               
PWBUY4   L     R1,WIMNET                                                        
         A     R1,PWTAX                                                         
         ST    R1,WIMNETTX                                                      
*                                                                               
         M     R0,=F'10000'        SCALE                                        
         L     RF,NETPCT                                                        
         S     RF,PWPCT            /(NETPCT-PWPCT)                              
         BAS   RE,DIV                                                           
         ST    R1,CLTGRSTX         SAVE GROSS WITH TAX                          
         ST    R1,PWVAL                                                         
*                                                                               
PWBUY6   S     R1,CLTGRS                                                        
         ST    R1,PWCLTTAX         RETURN TAX ON GROSS CLT DOLS                 
         B     EXIT                                                             
         EJECT                                                                  
*===================================================================*           
* NOTE THAT PWACTGOL IS CLTGRSTX (CLT GROSS INCLUDING TAX)          *           
* PROFIT   = CLTGRSTX * PWPCT                                       *           
* WIMNETTX = CLTGRSTX * NETPCT - PROFIT                             *           
*                                                                   *           
* RESTATING IT IN MORE SIMPLE TERMS  ---                            *           
* WIMNETTX = CLTGRSTX * (NETPCT-PWPCT)                              *           
* WIMNET   = WIMNETTX / (1 + TAXPCT)                                *           
* WIMGRS   = WIMNET/NETPCT                                          *           
*===================================================================*           
         SPACE 1                                                                
*                                                                               
PWGOL    L     R1,PWACTGOL                                                      
         L     R0,NETPCT                                                        
         S     R0,PWPCT                                                         
         MR    R0,R0                                                            
         L     RF,=F'10000'                                                     
         BAS   RE,DIV                                                           
         ST    R1,WIMNETTX                                                      
*                                                                               
* NOW DIVIDE BY 1+TAXRATE                                                       
*                                                                               
         M     R0,=F'100000'       SCALE FOR DIVISION                           
         L     RF,=F'100000'       START WITH 100.000 (TAX IS 3 DEC)            
         A     RF,PWTAXRT          ADD TAX RATE                                 
         BAS   RE,DIV                                                           
         ST    R1,WIMNET           GIVES WIMNET                                 
*                                                                               
* NOW GROSS UP WIMNET                                                           
*                                                                               
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
* CALCULATE NET TAX                                                             
         L     R1,WIMNETTX         WIMNET+TAX                                   
         S     R1,WIMNET           LESS WIMNET GIVES TAX                        
         A     R1,PWVAL            AND ADD TO GET WIMGRSTX                      
         ST    R1,PWVAL                                                         
         B     EXIT                                                             
         EJECT                                                                  
*===================================================================*           
*                                                                   *           
* PWPCT = NETPCT * (1 - WIMGRSTX/CLTGRSTX)                          *           
* NOTE  -- SINCE WESTERN GROSSES UP TAX FOR THIS CALCULATION        *           
* WE WILL SUBTRACT TAX FROM THE GROSS, NET IT DOWN, ADD THE TAX     *           
* AND GROSS IT BACK UP TO MAKE THE ARITHMETIC WORK                  *           
*                                                                   *           
*===================================================================*           
         SPACE 1                                                                
PWPW     DS    0H                                                               
         L     R1,PWACTBUY         WIMGRSTX                                     
         S     R1,PWTAX            LESS TAX                                     
         M     R0,NETPCT                                                        
         L     RF,=F'10000'        SCALE                                        
         BAS   RE,DIV                                                           
         ST    R1,WIMNET                                                        
*                                                                               
         A     R1,PWTAX            ADD TAX BACK IN                              
         ST    R1,WIMNETTX                                                      
*                                                                               
         M     R0,=F'10000'        SCALE                                        
         L     RF,NETPCT                                                        
         BAS   RE,DIV                                                           
         ST    R1,WIMGRSTX                                                      
* WIMGRSTX/100 SHOULD BE LESS THAN CLTGRSTX                                     
         SR    R0,R0               CLEAR R0                                     
         LA    RF,100                                                           
         BAS   RE,DIV                                                           
         LPR   R1,R1               MAKE RESULT POSITIVE                         
         L     R0,PWADJBUY         = CLTGRSTX                                   
         LPR   R0,R0                                                            
         CR    R1,R0                                                            
         BL    PWPW2                                                            
         L     R1,=F'-10100'       SET HORRENDOUS PW PCT                        
         B     PWPWX                                                            
*                                                                               
PWPW2    L     R1,WIMGRSTX         RESTORE WIMGRSTX                             
         M     R0,=F'10000'        SCALE                                        
         L     RF,PWADJBUY         = CLTGRSTX                                   
         BAS   RE,DIV                                                           
*                                                                               
         L     R0,=F'10000'        (10000 = 100 PCT)                            
         SR    R0,R1                                                            
         LR    R1,R0                                                            
*                                                                               
         L     R0,NETPCT                                                        
         MR    R0,R0                                                            
         L     RF,=F'10000'                                                     
         BAS   RE,DIV                                                           
*                                                                               
PWPWX    ST    R1,PWVAL                                                         
         B     EXIT                                                             
         EJECT                                                                  
DIV      LTR   RF,RF               IF DIVISOR 0                                 
         BNZ   DIV2                                                             
         SR    R1,R1               USER GETS 0 BACK                             
         BR    RE                                                               
*                                                                               
DIV2     SLDA  R0,1                DOUBLE DIVIDEND                              
         DR    R0,RF                                                            
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AH    R1,=H'1'                                                         
         SRA   R1,1                                                             
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
**PAN#1  DC    CL21'021SPPWCALCX 05/01/02'                                      
         END                                                                    
