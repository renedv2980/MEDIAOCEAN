*          DATA SET ACREPSO02  AT LEVEL 008 AS OF 08/17/00                      
*PHASE ACSO02A,*                                                                
*INCLUDE SOFDAT                                                                 
ACSO02   TITLE '- TESTING BED FOR SOFDAT'                                       
ACSO02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACSO**                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA=A(GLOBAL W/S)                             
         L     R9,AMONACC                                                       
         USING ACMD,R9             R9=A(MONACC WORK AREA)                       
         LA    RC,SPACEND                                                       
         USING WORKD,RC            RC=A(LOCAL W/S)                              
                                                                                
RUN      CLI   MODE,RUNFRST                                                     
         BNE   REQ                                                              
         MVI   RCREQREP,C'N'                                                    
         L     RF,ADBXAREA                                                      
         USING BOXD,RF                                                          
         MVI   BOXCOLS+(PLBOXL-PLINE),C'L'                                      
         MVI   BOXCOLS+(PLBOXC1-PLINE),C'C'                                     
         MVI   BOXCOLS+(PLBOXC2-PLINE),C'C'                                     
         MVI   BOXCOLS+(PLBOXC3-PLINE),C'C'                                     
         MVI   BOXCOLS+(PLBOXC4-PLINE),C'C'                                     
         MVI   BOXCOLS+(PLBOXC5-PLINE),C'C'                                     
         MVI   BOXCOLS+(PLBOXC6-PLINE),C'C'                                     
         MVI   BOXCOLS+(PLBOXR-PLINE),C'R'                                      
         MVI   BOXYORN,C'Y'        INITIALISE BOXES                             
         MVI   BOXOFF,C'N'                                                      
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+02,C'T'                                                  
         MVI   BOXROWS+05,C'M'                                                  
         MVI   BOXROWS+99,C'B'                                                  
         B     EXIT                                                             
         DROP  RF                                                               
                                                                                
REQ      CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         MVC   OUTPUT,SPACES                                                    
         LA    R2,SOFBLK                                                        
         USING SOFDATD,R2                                                       
         XC    SOFDATD(SOFDATL),SOFDATD                                         
         LA    R0,QACCOUNT                                                      
         ST    R0,SOFAINP                                                       
         LA    R0,OUTPUT                                                        
         ST    R0,SOFAOUT                                                       
         MVC   SOFACOM,ADCOMFAC                                                 
                                                                                
         MVC   SOFITYPE,QOPT1                                                   
         NI    SOFITYPE,X'0F'                                                   
         CLI   QOPT1,C'A'          SPECIAL FOR INPUT TYPE 10                    
         BNE   *+8                                                              
         MVI   SOFITYPE,SOFITSD8                                                
                                                                                
         MVC   SOFOTYPE,QOPT4                                                   
         NI    SOFOTYPE,X'0F'                                                   
         CLI   QOPT4,C'A'          SEPCIAL FOR OUTPUT TYPE 10                   
         BNE   *+8                                                              
         MVI   SOFOTYPE,SOFOTSD8                                                
                                                                                
         MVC   SOFACFST,QOPT2                                                   
                                                                                
         MVC   SOFSDOW,QOPT3                                                    
         NI    SOFSDOW,X'0F'                                                    
         MVI   SOFIINDS,SOFIIANY+SOFIIOUT                                       
                                                                                
         CLI   QOPT5,C'1'          TEST SINGLE DATE EXPRESSION                  
         BNE   *+8                                                              
         OI    SOFIINDS,SOFIIONE                                                
                                                                                
         CLI   QOPT5,C'E'          TEST SINGLE END DATE EXPRESSION              
         BNE   *+12                                                             
         OI    SOFITYPE,SOFITEND                                                
         OI    SOFIINDS,SOFIIONE                                                
                                                                                
         CLI   QOPT6,C'O'                                                       
         BNE   *+8                                                              
         OI    SOFIINDS,SOFIIF1O+SOFIIF2O                                       
         CLI   QOPT6,C'1'                                                       
         BNE   *+8                                                              
         OI    SOFIINDS,SOFIIF1O                                                
         CLI   QOPT6,C'2'                                                       
         BNE   *+8                                                              
         OI    SOFIINDS,SOFIIF2O                                                
         CLI   QOPT7,C'Y'                                                       
         BNE   *+8                                                              
         OI    SOFIINDS,SOFIISLH                                                
         CLC   QSTART,SPACES                                                    
         BNH   *+10                                                             
         MVC   SOFTODAY,QSTART                                                  
         MVC   SOFCTRY,RCCTRY                                                   
         MVI   SOFSYSN,X'06'       SET ACC SYSTEM                               
         MVC   SOFLANG,RCLANG                                                   
                                                                                
         CLI   QOPT1,C'8'          TEST INPUT FIELD VALIDATION                  
         BE    *+12                                                             
         CLI   QOPT1,C'9'                                                       
         BNE   REQF02                                                           
                                                                                
         LA    R0,L'FLDH+L'FLD     BUILD FIELD HEADER                           
         STC   R0,FLDH                                                          
         MVC   FLD,QACCOUNT                                                     
         LA    R0,FLDH                                                          
         ST    R0,SOFAINP                                                       
                                                                                
REQF02   GOTO1 VSOFDAT,SOFDATD                                                  
                                                                                
         MVC   PLTODAY,SOFTODAY                                                 
         MVC   PLOPTION,QOPT1                                                   
         EDIT  (B2,SOFOVDIP),(6,PLDAYS),0,ZERO=NOBLANK                          
         EDIT  (B2,SOFOVMIP),(6,PLMONTHS),0,ZERO=NOBLANK                        
         EDIT  (B2,SOFOVYIP),(6,PLYEARS),0,ZERO=NOBLANK                         
         SR    RF,RF                                                            
         ICM   RF,3,SOFERROR                                                    
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  PLERROR,DUB                                                      
                                                                                
         CLI   QOPT1,C'8'                                                       
         BE    REQF04                                                           
         CLI   QOPT1,C'9'                                                       
         BE    REQF04                                                           
                                                                                
         MVC   PLIO(6),=C'Input='                                               
         GOTO1 HEXOUT,DMCB,QACCOUNT,PLIO+6,L'QACCOUNT,=C'TOG'                   
         GOTO1 ACREPORT                                                         
         MVC   PLIO(7),=C'Output='                                              
         MVC   PLIO+7(L'OUTPUT),OUTPUT                                          
         B     REQFX                                                            
                                                                                
REQF04   MVC   PLIO(6),=C'Input='                                               
         MVC   PLIO+6(L'FLD),QACCOUNT                                           
         GOTO1 ACREPORT                                                         
         MVC   PLIO(7),=C'Output='                                              
         GOTO1 HEXOUT,DMCB,OUTPUT,PLIO+7,L'OUTPUT,=C'TOG'                       
         OC    SOFERROR,SOFERROR                                                
         BNZ   REQFX                                                            
         TM    SOFOINDS,SOFOINOI                                                
         BNZ   REQFX                                                            
         TM    SOFOINDS,SOFOISFT   TEST SOFT DATE INPUT                         
         BZ    REQF06                                                           
                                                                                
         GOTO1 ACREPORT                                                         
         MVC   SOFITYPE,SOFOTYPE                                                
         MVI   SOFOTYPE,SOFOTPRT   CONVERT TO PRINTABLE                         
         CLI   QOPT1,SOFITYM+X'F0'                                              
         BNE   *+8                                                              
         OI    SOFOTYPE,SOFOTPYM                                                
         MVC   OUTPUT2(L'OUTPUT),OUTPUT                                         
         LA    R0,OUTPUT2                                                       
         ST    R0,SOFAINP                                                       
         LA    R0,FLDH                                                          
         ST    R0,SOFAOUT                                                       
         GOTO1 VSOFDAT,SOFDATD                                                  
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   PLIO(12),=C'Len=xx,Data='                                        
         SR    RF,RF                                                            
         ICM   RF,1,FLDH+5                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  PLIO+4(2),DUB                                                    
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   PLIO+12(0),FLD                                                   
         GOTO1 ACREPORT                                                         
                                                                                
         MVC   SOFITYPE,QOPT1                                                   
         NI    SOFITYPE,X'0F'                                                   
         CLI   QOPT1,C'A'                                                       
         BNE   *+8                                                              
         MVI   SOFITYPE,SOFITSD8                                                
                                                                                
         MVC   SOFOTYPE,QOPT4                                                   
         NI    SOFOTYPE,X'0F'                                                   
         CLI   QOPT4,C'A'                                                       
         BNE   *+8                                                              
         MVI   SOFOTYPE,SOFOTSD8                                                
                                                                                
         OI    SOFIINDS,SOFIIRES   SET TO RESOLVE DATE                          
         LA    R0,FLDH                                                          
         ST    R0,SOFAINP                                                       
         LA    R0,OUTPUT2                                                       
         ST    R0,SOFAOUT                                                       
         GOTO1 VSOFDAT,SOFDATD                                                  
         SR    RF,RF                                                            
         ICM   RF,3,SOFERROR                                                    
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  PLERROR,DUB                                                      
         MVC   PLIO(7),=C'Output='                                              
         MVC   PLIO+7(L'OUTPUT2),OUTPUT2                                        
         GOTO1 ACREPORT                                                         
         MVC   PLIO(12),=C'Len=xx,Data='                                        
         SR    RF,RF                                                            
         ICM   RF,1,FLDH+5                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  PLIO+4(2),DUB                                                    
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   PLIO+12(0),FLD                                                   
         GOTO1 ACREPORT                                                         
         B     REQFX                                                            
                                                                                
REQF06   GOTO1 ACREPORT                                                         
         MVC   PLIO(12),=C'Len=xx,Data='                                        
         SR    RF,RF                                                            
         ICM   RF,1,FLDH+5                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  PLIO+4(2),DUB                                                    
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   PLIO+12(0),FLD                                                   
         GOTO1 ACREPORT                                                         
                                                                                
         MVC   SOFITYPE,SOFOTYPE                                                
         MVI   SOFOTYPE,SOFOTPRT   CONVERT TO PRINTABLE                         
         CLI   QOPT1,SOFITYM+X'F0'                                              
         BNE   *+8                                                              
         OI    SOFOTYPE,SOFOTPYM                                                
         MVC   OUTPUT2(L'OUTPUT),OUTPUT                                         
         LA    R0,OUTPUT2                                                       
         ST    R0,SOFAINP                                                       
         LA    R0,FLDH                                                          
         ST    R0,SOFAOUT                                                       
         GOTO1 VSOFDAT,SOFDATD                                                  
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   PLIO(12),=C'Len=xx,Data='                                        
         SR    RF,RF                                                            
         ICM   RF,1,FLDH+5                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CVD   RF,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  PLIO+4(2),DUB                                                    
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   PLIO+12(0),FLD                                                   
         GOTO1 ACREPORT                                                         
                                                                                
REQFX    GOTO1 ACREPORT                                                         
         GOTO1 ACREPORT                                                         
                                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
         LTORG                                                                  
VSOFDAT  DC    V(SOFDAT)                                                        
         EJECT                                                                  
WORKD    DSECT                     ** LOCAL WORKING STORAGE **                  
SOFBLK   DS    (SOFDATL)X                                                       
FLDH     DS    XL8                                                              
FLD      DS    CL20                                                             
OUTPUT   DS    CL12                OUTPUT DATE EXPRESSION                       
OUTPUT2  DS    CL12                OUTPUT DATE EXPRESSION                       
         EJECT                                                                  
*  DDSOFDATD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSOFDATD                                                      
         PRINT ON                                                               
                                                                                
*  ACREPWORKD                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         ORG   P                                                                
PLINE    DS    0CL(L'P)                                                         
PLBOXL   DS    C                                                                
PLIO     DS    CL32                INPUT/OUTPUT STRING                          
PLBOXC1  DS    C                                                                
PLTODAY  DS    CL(L'SOFTODAY)      TODAY'S DATE                                 
PLBOXC2  DS    C                                                                
PLOPTION DS    CL7                 OPTIONS                                      
PLBOXC3  DS    C                                                                
PLERROR  DS    CL4                 ERROR MESSAGE NUMBER                         
PLBOXC4  DS    C                                                                
PLDAYS   DS    CL6                 #DAYS                                        
PLBOXC5  DS    C                                                                
PLMONTHS DS    CL6                 #MONTHS                                      
PLBOXC6  DS    C                                                                
PLYEARS  DS    CL6                 #YEARS                                       
PLBOXR   DS    C                                                                
         PRINT ON                                                               
                                                                                
*  ACMASTD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
                                                                                
*  ACGENMODES                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
                                                                                
*  DDBIGBOX                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACREPSO02 08/17/00'                                      
         END                                                                    
