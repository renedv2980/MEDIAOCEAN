*          DATA SET SPREPXW02  AT LEVEL 033 AS OF 05/01/02                      
*PHASE SPXW02A                                                                  
         TITLE 'TRANSFER WORKER RECORDS TO SPOT FILE'                           
         PRINT NOGEN                                                            
SPXW02   CSECT                                                                  
         NMOD1 0,SPXW02,RR=R5                                                   
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         ST    R5,RELO                                                          
         SPACE 2                                                                
         CLI   MODE,RUNFRST                                                     
         BNE   M00                                                              
         L     RE,=V(ESTTAB)                                                    
         A     RE,RELO                                                          
         ST    RE,VESTTAB                                                       
         L     RE,=V(CNTTAB)                                                    
         A     RE,RELO                                                          
         ST    RE,VCNTTAB                                                       
         L     RE,=V(MKTTAB)                                                    
         A     RE,RELO                                                          
         ST    RE,VCNTTAB                                                       
         L     RE,=V(ERRDESC)                                                   
         A     RE,RELO                                                          
         ST    RE,VERRDESC                                                      
         B     EXIT                                                             
M00      CLI   MODE,REQFRST                                                     
         BNE   M01                                                              
         MVI   PHASESW,1                                                        
         B     EXIT                                                             
M01      DS    0C                                                               
         CLI   MODE,CLTFRST                                                     
         BNE   EXIT                                                             
         LA    R2,WRKRINDX         *TRANSFER PEPSI RECORDS*                     
         USING UKRECD,R2                                                        
         PACK  DUB,QMKT            BUILD USER INDEX AREA                        
         CVB   RE,DUB                                                           
         STH   RE,SVUSRID                                                       
OPENREC  GOTO1 WORKER,DMCB,=C'INDEX',WRKR4K,WRKRINDX                            
         TM    8(R1),X'80'                                                      
         BO    EXIT                                                             
         CLC   UKUSRID,SVUSRID                                                  
         BNE   OPENREC                                                          
         CLC   UKSYSPRG,=C'SM5'                                                 
         BNE   OPENREC                                                          
GETRECS  GOTO1 WORKER,DMCB,=C'READ',WRKR4K,WRKRINDX,WRKREC                      
         TM    DMCB+8,X'80'        END OF FILE                                  
         BO    END                                                              
         TM    DMCB+8,X'50'        ERROR READING WORKER                         
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R5,WRKREC+7                                                      
         USING PEPREC,R5                                                        
         CLI   PEPKEST,0                                                        
         BE    GETRECS                                                          
         CLI   PEPKDTE,X'FF'       BYPASS TOTAL RECORDS                         
         BE    GETRECS                                                          
         CLI   PHASESW,1                                                        
         BNE   PUTRECS                                                          
         CLC   PEPKAM,SVPKAM       A/M/C OK                                     
         BE    GETREC2              YES - TRY PRODUCT                           
         MVC   SVPKAM,PEPKAM                                                    
         MVC   PEPKAM,BAGYMD                                                    
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),PEPKAM                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BE    GETREC2                                                          
         XC    SVPKAM,SVPKAM                                                    
         MVC   P(36),=C'INVALID CLIENT CODE - RUN TERMINATED'                   
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         SPACE 2                                                                
GETREC2  L     R6,ADCLT                                                         
         USING CLTHDR,R6                                                        
         MVC   PEPKAM,BAGYMD                                                    
         LA    RF,CLIST            SET START OF PRODUCTS                        
         LA    RE,CCLTIFC          END OF PRODUCTS                              
         SR    RE,RF                                                            
         SRL   RE,2                SET FOR BCT                                  
GETREC4  CLI   0(RF),0             PRODUCT NOT IN LIST                          
         BE    GETREC6                                                          
         CLC   0(3,RF),WRKREC+4    IS IT THIS PRODUCT                           
         BE    GETREC6A                                                         
         LA    RF,4(RF)                                                         
         BCT   RE,GETREC4                                                       
         DROP  R6                                                               
GETREC6  XC    WORK,WORK           ERROR - PRODUCT NOT IN LIST                  
         LA    RF,WORK                                                          
         USING COUNTD,RF                                                        
         MVC   CNTPRD,WRKREC+4                                                  
         MVI   CNTFLG,PRDNF                                                     
         BAS   R9,INSERR                                                        
         B     GETRECS                                                          
         DROP  RF                                                               
GETREC6A MVC   PEPKPRD,3(RF)       SET PRODUCT CODE IN KEY                      
         SPACE 2                                                                
* FIND ESTIMATE FOR THIS PRODUCT                                                
         XC    WORK,WORK                                                        
         LA    RF,WORK                                                          
         USING ESTTABD,RF                                                       
         MVC   ETPRD,PEPKPRD                                                    
         MVC   ETEST,PEPKEST                                                    
         L     R9,ETCNTR                                                        
         L    R8,VESTTAB                                                        
         GOTO1 BINSRCH,DMCB,(0,WORK),(R8),(R9),6,2,300                          
         CLI   0(R1),1             FOUND                                        
         BNE   GETREC6B             YES - COUNT RECORDS                         
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),PEPKAM                                                  
         MVC   KEY+4(3),WRKREC+4                                                
         MVC   KEY+7(1),PEPKEST                                                 
         MVC   AREC,ADEST                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     ESTIMATE NOT FOUND                           
         BE    GETRC6A1                                                         
         XC    WORK,WORK                                                        
         LA    RF,WORK                                                          
         USING COUNTD,RF                                                        
         MVC   CNTPRD,WRKREC+4                                                  
         MVC   CNTEST,PEPKEST                                                   
         MVI   CNTFLG,ESTNF                                                     
         BAS   R9,INSERR                                                        
         B     GETRECS                                                          
         DROP  RF                                                               
GETRC6A1 GOTO1 GETEST                                                           
         L     R6,ADEST                                                         
         USING ESTHDR,R6                                                        
         LA    R9,WORK                                                          
         USING ESTTABD,R9                                                       
         MVC   ETPRD,PEPKPRD                                                    
         MVC   ETEST,PEPKEST                                                    
         GOTO1 DATCON,DMCB,(0,ESTART),(2,ETSTART)                               
         GOTO1 DATCON,DMCB,(0,EEND),(2,ETEND)                                   
         L     R9,ETCNTR                                                        
         L     R8,VESTTAB                                                       
         GOTO1 BINSRCH,DMCB,(1,WORK),(R8),(R9),6,2,300                          
         MVC   ETCNTR,8(R1)                                                     
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                ESTIMATE TABLE IS FULL                       
         DROP  R9                                                               
         SPACE 2                                                                
GETREC6B L     RF,0(R1)            CHECK WITHIN ESTIMATE DATES                  
         USING ESTTABD,RF                                                       
         CLC   PEPKDTE,ETSTART     DATE LESS THAN START                         
         BL    GETREC6C             YES - ERROR                                 
         CLC   PEPKDTE,ETEND       DATE GREATER THAN END                        
         BH    GETREC6C                                                         
         B     GETREC8                                                          
         DROP  RF                                                               
GETREC6C XC    WORK,WORK                                                        
         LA    RF,WORK                                                          
         USING COUNTD,RF                                                        
         MVC   CNTPRD,WRKREC+4                                                  
         MVI   CNTFLG,OUTDATE                                                   
         MVC   CNTEST,PEPKEST                                                   
         BAS   R9,INSERR                                                        
         B     GETRECS                                                          
         SPACE 2                                                                
GETREC8  L     R9,MKTCNTR                                                       
         B     GETREC10                                                         
         L     R8,VMKTTAB                                                       
         GOTO1 BINSRCH,DMCB,(X'00',PEPKMKT),(R8),(R9),2,2,500                   
         CLI   0(R1),1             MARKET OK                                    
         BNE   GETREC10             YES - COUNT GOOD RECORD                     
         XC    WORK,WORK            NO - COUNT BAD RECORD                       
         LA    RF,WORK                                                          
         MVC   CNTMKT,PEPKMKT                                                   
         MVC   CNTMPRD,PEPKPRD                                                  
         MVC   CNTEST,PEPKEST                                                   
         MVI   CNTFLG,MKTERR                                                    
         BAS   R9,INSERR                                                        
         B     GETRECS                                                          
         SPACE 2                                                                
GETREC10 LA    RF,WORK             COUNT GOOD RECORDS                           
         XC    WORK,WORK                                                        
         MVC   CNTPRD,WRKREC+4                                                  
         MVC   CNTEST,PEPKEST                                                   
         MVI   CNTFLG,GOODREC                                                   
         BAS   R9,INSERR                                                        
         B     GETRECS                                                          
         DROP  RF                                                               
* PUT RECORDS TO PEPSI-LOCKIN FILE                                              
PUTRECS  LA    R5,WRKREC+7                                                      
         MVC   PEPKAM,BAGYMD       SET AGENCY AND MEDIA                         
         L     R6,ADCLT                                                         
         USING CLTHDR,R6                                                        
         LA    RF,CLIST            SET START OF PRODUCTS                        
         LA    RE,CCLTIFC          END OF PRODUCTS                              
         SR    RE,RF                                                            
         SRL   RE,2                SET FOR BCT                                  
PUTREC2  CLI   0(RF),0             PRODUCT NOT IN LIST                          
         BNE   *+8                                                              
         B     GETRECS             BYPASS RECORD IF INVALID PRODUCT             
*                                                                               
         CLC   0(3,RF),WRKREC+4    IS IT THIS PRODUCT                           
         BE    PUTREC4              YES - PROCEED                               
         LA    RF,4(RF)                                                         
         BCT   RE,PUTREC2                                                       
         DC    H'0'                SEE DC H'0' ABOVE                            
         SPACE 2                                                                
PUTREC4  XC    KEY,KEY                                                          
         MVC   PEPKPRD,3(RF)       SET TO PEPSI NUMERIC CODE                    
         MVC   KEY(13),PEPKEY                                                   
         L     R9,ADD                                                           
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   PUTREC6                                                          
         MVC   AREC,ADBUY          SET BUY RECORD ADDRESS                       
         L     R9,PUT                                                           
         GOTO1 GET                                                              
PUTREC6  LR    RF,R9                                                            
         LA    RE,WRKREC+7                                                      
         ST    RE,AREC                                                          
         CLI   RCWRITE,C'N'                                                     
         BE    GETRECS                                                          
         GOTO1 (RF)                                                             
         B     GETRECS                                                          
         EJECT                                                                  
INSERR   LR    R0,R9               SAVE RETURN ADDRESS                          
         L     R9,CTCNTR           BUILD REPORT TABLE                           
         L     R8,VCNTTAB                                                       
         CLI   ERRORSW,1           IS ERROR ALREADY SET                         
         BE    INSERR2                                                          
*                                                                               
         MVI   DOLSW,0             SCAN RECORD FOR ANY ACTIVITY                 
         LA    R7,PEPELEMS                                                      
         USING PEPDELM,R7                                                       
CHKDEL   CLI   PEPDELM,0           END                                          
         BE    CHKDEL6                                                          
         CLI   PEPDELM,X'10'       LOCKIN ELEMENT                               
         BE    CHKDEL4                                                          
*                                                                               
CHKDEL2  ZIC   RF,1(R7)                                                         
         LTR   RF,RF               TEST FOR A LENGTH                            
         BZ    CHKDEL6                                                          
         AR    R7,RF               BUMP TO NEXT ELEMENT                         
         B     CHKDEL                                                           
*                                                                               
CHKDEL4  OC    PEPDBOOK,PEPDBOOK   DELETED ELEMENT                              
         BZ    *+8                                                              
         MVI   DOLSW,1                                                          
         B     CHKDEL2                                                          
*                                                                               
CHKDEL6  LR    R9,R0               RESTORE RETURN ADDRESS                       
         CLI   DOLSW,0             BYPASS IF NO ACTIVE DOLLARS                  
         BER   R9                                                               
*                                                                               
         L     R9,CTCNTR           RESTORE COUNT                                
         CLI   WORK+4,4            BYPASS ERROR SETTING-GOOD RECORD             
         BE    INSERR2                                                          
         MVI   ERRORSW,1                                                        
INSERR2  GOTO1 BINSRCH,DMCB,(X'01',WORK),(R8),(R9),7,5,300                      
         MVC   CTCNTR,8(R1)                                                     
         OC    0(4,R1),0(R1)                                                    
         BNE   *+6                                                              
         DC    H'0'                REPORT TABLE OVERFLOW                        
         L     RF,0(R1)                                                         
         USING COUNTD,RF                                                        
         SR    RE,RE                                                            
         ICM   RE,3,CNTCNT                                                      
         LA    RE,1(RE)                                                         
         STCM  RE,3,CNTCNT                                                      
         DROP  RF                                                               
         LR    R9,R0                                                            
         BR    R9                                                               
         DROP  R7                                                               
         EJECT                                                                  
END      CLI   PHASESW,1           ERROR                                        
         BNE   END2                                                             
         GOTO1 WORKER,DMCB,=C'CLOSE',WRKR4K,WRKRINDX                            
*        CLI   ERRORSW,1                                                        
*        BE    DORPT                                                            
         MVI   PHASESW,2                                                        
         B     OPENREC                                                          
         SPACE 2                                                                
END2     CLI   RCWRITE,C'Y'                                                     
         BNE   DORPT                                                            
         GOTO1 WORKER,DMCB,=C'DELETE',WRKR4K,WRKRINDX                           
         SPACE 2                                                                
DORPT    L     R8,VCNTTAB                                                       
         USING COUNTD,R8                                                        
         LA    R7,P                                                             
         USING RPTA,R7                                                          
DORPT2   MVC   RPTAPRD,CNTPRD                                                   
         EDIT  CNTEST,(3,RPTAEST)                                               
         EDIT  CNTCNT,(07,RPTARCNT)                                             
         L     RF,VERRDESC                                                      
DORPT4   CLI   0(RF),X'FF'         FIND ERROR MESSAGE                           
         BE    DORPT5                                                           
         CLC   CNTFLG(1),0(RF)                                                  
         BE    DORPT5                                                           
         LA    RF,ERRENT(RF)                                                    
         B     DORPT4                                                           
DORPT5   MVC   RPTAMSG,2(RF)                                                    
         SPACE 2                                                                
DORPT6   CLI   1(RF),1             NORMAL PRODUCT REPORT                        
         BE    DORPT10              YES - PRINT IT                              
         CLI   1(RF),2             MARKET REPORT                                
         BE    *+6                  YES - REFORMAT                              
         DC    H'0'                TABLE ERROR                                  
         L     R6,ADCLT                                                         
         USING CLTHDR,R6                                                        
         LA    RE,CLIST                                                         
         LA    R0,220                                                           
DORPT8   CLC   CNTMPRD,3(RE)                                                    
         BE    DORPT8A                                                          
         LA    RE,4(RE)                                                         
         BCT   R0,DORPT8                                                        
         DC    H'0'                                                             
DORPT8A  MVC   RPTAPRD,0(RE)                                                    
         EDIT  CNTMKT,(4,RPTAMKT)                                               
DORPT10  GOTO1 REPORT                                                           
         LA    R8,LCNTTAB(R8)                                                   
         CLI   0(R8),0                                                          
         BNE   DORPT2                                                           
         B     OPENREC                                                          
*        CLI   ERRORSW,1                                                        
*        BNE   OPENREC                                                          
         CLI   RCWRITE,C'Y'                                                     
         BNE   OPENREC                                                          
         GOTO1 WORKER,DMCB,=C'DELETE',WRKR4K,WRKRINDX                           
         B     OPENREC                                                          
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         LTORG                                                                  
SVPKAM   DS    C                   SAVE ORIGINAL AGENCY MEDIA                   
PHASESW  DC    X'01'                                                            
ERRORSW  DC    X'00'                                                            
DOLSW    DC    X'00'                                                            
RELO     DC    F'0'                                                             
ETCNTR   DC    F'0'                NO OF ENTRIES IN EST TABLE                   
CTCNTR   DC    F'0'                NO OF ENTRIES IN COUNT TABLE                 
MKTCNTR  DC    F'0'                                                             
VESTTAB  DC    F'0'                                                             
VCNTTAB  DC    F'0'                                                             
VMKTTAB  DC    F'0'                                                             
VERRDESC DC    F'0'                                                             
SVUSRID  DS    H                                                                
WRKRINDX DS    CL16                                                             
WRKREC   DS    1000C                                                            
WRKR4K   DS    4096C                                                            
RPTA     DSECT                                                                  
         DS    CL1                                                              
RPTAPRD  DS    CL3                                                              
         DS    CL1                                                              
RPTAEST  DS    CL3                                                              
         DS    CL2                                                              
RPTAMKT  DS    CL4                                                              
         DS    CL2                                                              
RPTARCNT DS    CL7                                                              
         DS    CL2                                                              
RPTAMSG  DS    CL27                                                             
COUNTD   DSECT                                                                  
CNTPRD   DS    CL3                                                              
         ORG   CNTPRD                                                           
CNTMKT   DS    CL2                                                              
CNTMPRD  DS    C                                                                
CNTEST   DS    C                                                                
CNTFLG   DS    C                                                                
CNTCNT   DS    CL2                                                              
LCNTTAB  EQU   7                                                                
         SPACE 2                                                                
ESTTABD  DSECT                                                                  
ETPRD    DS    C                                                                
ETEST    DS    C                                                                
ETSTART  DS    CL2                                                              
ETEND    DS    CL2                                                              
LESTTAB  EQU   6                                                                
         SPACE 2                                                                
PRDNF    EQU   1                                                                
ESTNF    EQU   2                                                                
OUTDATE  EQU   3                                                                
MKTERR   EQU   40                                                               
GOODREC  EQU   4                                                                
ERRDESC  CSECT                                                                  
         DC    AL1(PRDNF),AL1(1)                                                
         DC    CL27'PRODUCT NOT FOUND'                                          
         DC    AL1(ESTNF),AL1(1)                                                
         DC    CL27'ESTIMATE NOT FOUND'                                         
         DC    AL1(OUTDATE),AL1(1)                                              
         DC    CL27'DATA OUTSIDE ESTIMATE DATES'                                
         DC    AL1(MKTERR),AL1(2)                                               
         DC    CL27'MARKET NOT FOUND'                                           
         DC    AL1(GOODREC),AL1(1)                                              
         DC    CL27'RECORDS ADDED OR UPDATED'                                   
         DC    AL1(255),AL1(1)                                                  
         DC    CL27'UNDEFINED ERROR CONDITION'                                  
ERRENT   EQU   29                                                               
ESTTAB   CSECT                                                                  
         DS    1800C                                                            
CNTTAB   CSECT                                                                  
         DS    2100C                                                            
MKTTAB   CSECT                                                                  
         DS    1000C                                                            
       ++INCLUDE SPGENPEPSI                                                     
       ++INCLUDE DMWRKRK                                                        
         PRINT OFF                                                              
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033SPREPXW02 05/01/02'                                      
         END                                                                    
