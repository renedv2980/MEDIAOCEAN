*          DATA SET PPPUB06    AT LEVEL 019 AS OF 05/01/02                      
*PHASE T40606A,+0,NOAUTO                                                        
*                                                                               
***************  CHANGE LOG  ***************                                    
*                                                                               
*  SMYE  01/00     ADD CLIENT-SPECIFIC HANDLING AND CHANGE ALL PRIOR            
*                  USE OF R8 TO MAKE IT AVAILABLE AS A 2ND BASE REG.            
*                    (REGISTER CHANGES COMMENTED AS *NOP*)                      
*                                                                               
*  SMYE  2/96      INCLUDE PUGENOLD (CURRENTLY SAME AS PPGENOLD)                
*                                                                               
*  SMYE  12/07/95  CHANGED VDTCNV TO VDATCON WITH NEW PARAM'S                   
*                                                                               
         TITLE 'T40606  PUBFILE MAINT. NEWSPAPER PREMIUM SCREEN'                
         PRINT NOGEN                                                            
T40606   CSECT                                                                  
         NMOD1 0,T40606                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING T406FFD,RA                                                       
         LA    R8,T40606+4095                                                   
         LA    R8,1(R8)                                                         
         USING T40606+4096,R8      ** NOTE USE OF R8 AS 2ND BASE REG **         
         LA    R9,PUBIO                                                         
         USING PUBREC,R9                                                        
         LA    R4,PUBIO                                                         
         LA    R5,20                                                            
CLEARIO  XC    0(200,R4),0(R4)                                                  
         LA    R4,200(R4)                                                       
         BCT   R5,CLEARIO                                                       
         LA    R3,53                                                            
         LA    R2,PBLPUBH                                                       
         OC    PUBADDR,PUBADDR                                                  
         BZ    ERROR                                                            
         MVC   KEY+27(4),PUBADDR                                                
         BAS   RE,GETPUB                                                        
         MVI   PUBIND,0                                                         
         XC    BDIV,BDIV                                                        
         CLC   BCLT(3),=3X'FF'     "ALL" ENTERED IN SCREEN FIELD ?              
         BNE   *+10                NO                                           
         XC    BCLT,BCLT           YES - SET TO NULLS                           
*NEW CODE 03/10/00                                                              
         CLI   SAVSCRN,X'06'       CHECK SCREEN                                 
         BNE   DATEFX              NOT WHAT WE WANT                             
         CLC   PBLACT(3),=C'ADD'                                                
         BNE   DATEFX              NOT WHAT WE WANT                             
         LA    R2,PREEFFDH                                                      
         CLI   5(R2),0             ANYTHING ENTERED ?                           
         BE    DATEFX              NO                                           
         GOTO1 VDATVAL,DMCB,(0,PREEFFD),WORK                                    
         LA    R3,DATERR                                                        
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         GOTO1 VDATCON,DMCB,(0,WORK),(3,BDATE)    REPLACE BDATE                 
*NEW CODE 03/10/00                                                              
DATEFX   DS    0H                                                               
         LA    R6,PUBREC+33                                                     
         MVI   ELCOD,X'60'                                                      
NEXT60   BAS   RE,NXTEL                                                         
         BL    *-4                                                              
         BE    CKCODE                                                           
         CLI   0(R6),X'61'                                                      
         BE    NEXT60                                                           
         B     CKCOMB                                                           
*                                                                               
CKCODE   EQU   *                                                                
         USING PUBPRMEL,R6                                                      
         CLC   BCODE(3),PUBPCOD                                                 
         BNE   NEXT60                                                           
         CLC   BDATE(3),PUBPSTRT                                                
         BL    NEXT60                                                           
         CLC   BCLT,PUBPCLT        MATCHING CLIENT ?                            
         BNE   NEXT60              NO                                           
*                                                                               
CKCOD60  OI    PUBIND,X'10'        PREMIUM ELEMENT FOUND                        
         MVC   BDIV,PUBPSTRT       SAVE DATE DISPLAYED                          
         B     CKCOMB                                                           
*                                                                               
CKCOMB   LA    R3,COMBERR                                                       
         CLI   BACT,1                                                           
         BNE   PREMSCRN                                                         
         TM    PUBIND,X'10'                                                     
         BNO   PREMSCRN                                                         
         CLC   BDATE(3),PUBPSTRT   PREM. ELE. FOR BDATE CAN'T EXIST             
         BNE   CKCOM60                                                          
         CLC   BCLT,PUBPCLT        MATCHING CLIENT AND DATE ?                   
         BE    ERROR               YES - NO DUPLICATES ALLOWED                  
         B     NEXT60              LOOK FOR MORE EQUAL EFF DATES                
CKCOM60  MVI   PUBIND,0                                                         
         XC    BDIV,BDIV                                                        
         B     PREMSCRN                                                         
         EJECT                                                                  
*                                                                               
PREMSCRN CLI   BYTE2,1             SEC IF ACTION=FORMAT                         
         BE    FORMATP                                                          
         CLI   BACT,2                                                           
         BH    FORMATP                                                          
*                                                                               
*                                                                               
EDIT     DS    0H                                                               
         LA    R7,ELEAREA                                                       
         XC    ELEAREA(250),ELEAREA                                             
         USING PREMEL,R7                                                        
         MVC   PUBPRMEL(2),=X'6028'                                             
         LA    R2,PREEFFDH                                                      
         GOTO1 VDATVAL,DMCB,(0,PREEFFD),WORK                                    
         LA    R3,DATERR                                                        
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
*        GOTO1 VDTCNV,DMCB,(0,WORK),(1,PUBPSTRT)                                
         GOTO1 VDATCON,DMCB,(0,WORK),(3,PUBPSTRT)                               
         MVC   PUBPCOD(3),BCODE                                                 
         LA    R3,ADDERR                                                        
         LA    R4,PREDTS                                                        
         LA    R5,6                                                             
         CLC   0(4,R4),=C'NONE'                                                 
         BE    EDITA                                                            
CKADD    GOTO1 VDATVAL,DMCB,(0,0(R4)),WORK                                      
*        GOTO1 VDTCNV,DMCB,(0,WORK),(1,WORK+10)                                 
         GOTO1 VDATCON,DMCB,(0,WORK),(3,WORK+10)                                
         CLC   PUBPSTRT(3),WORK+10                                              
         BNE   CKAD60                                                           
         CLC   BCLT,PUBPCLT        MATCHING CLIENT AND DATE ?                   
         BE    ERROR               YES                                          
CKAD60   LA    R4,9(R4)                                                         
         BCT   R5,CKADD                                                         
         B     EDITA                                                            
*                                                                               
EDITA    CLC   PREDAYS(3),=C'DEL'                                               
         BE    UPDATE0             SPECIAL DELETE CODE                          
*                                                                               
*                                                                               
         LA    R2,PREDAYSH                                                      
         MVI   PUBPDAYS,0                                                       
         BAS   RE,ANY                                                           
         LA    R3,2        INVALID INPUT                                        
         CLC   =C'ALL',8(R2)                                                    
         BNE   *+12                                                             
         MVI   PUBPDAYS,X'7F'                                                   
         B     EDIT2                                                            
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         LA    R1,PREDAYS                                                       
FINDDAY  LA    R4,DAYTAB                                                        
CKDAY    CLC   0(2,R1),0(R4)                                                    
         BE    MOVEDAY                                                          
         CLC   3(2,R4),=X'0000'      END OF TABLE                               
         BE    ERROR                                                            
         LA    R4,3(R4)                                                         
         B     CKDAY                                                            
MOVEDAY  OC    PUBPDAYS,2(R4)                                                   
         BCTR  R5,0                                                             
         BCTR  R5,0                                                             
         BCTR  R5,0                                                             
         LTR   R5,R5                                                            
         BNP   EDIT2                                                            
         CLI   2(R1),C','                                                       
         BNE   ERROR                                                            
         LA    R1,3(R1)                                                         
         B     FINDDAY                                                          
*                M O   T U   W E   T H   F R   S A   S U                        
DAYTAB   DC    X'D4D640E3E420E6C510E3C808C6D904E2C102E2E4010000'                
         DS    0H                                                               
*                                                                               
EDIT2    LA    R2,PREMINSH                                                      
         MVC   PUBPMINS(4),=PL4'0'                                              
         CLI   5(R2),0                                                          
         BE    EDIT3                                                            
         MVC   PUBPMINS(4),=C'PAGE'                                             
         CLC   PREMINS(4),=C'PAGE'                                              
         BE    EDIT3                                                            
         TM    4(R2),X'08'         CHK FOR NUMERICS                             
         BNZ   EDIT2B                                                           
         LA    R3,3                NON NUMERIC INPUT                            
         B     ERROR                                                            
EDIT2B   BAS   RE,PACK                                                          
         MVC   PUBPMINS(4),DUB+4                                                
*                                                                               
EDIT3    LA    R2,PREMINCH                                                      
         MVC   PUBPMINC(5),=PL5'0'                                              
         CLI   5(R2),0                                                          
         BE    EDIT4                                                            
         ZIC   R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,PREMINC,(R5)                                       
         CLI   DMCB,X'FF'                                                       
         BNE   *+12                                                             
         LA    R3,2                INVALID INPUT                                
         B     ERROR                                                            
*                                                                               
         L     R0,DMCB+4                                                        
         CVD   R0,DUB                                                           
         ZAP   PUBPMINC,DUB                                                     
*                                                                               
EDIT4    LA    R2,PRETOFCH                                                      
         MVI   PUBPTYPC,0                                                       
         BAS   RE,ANY                                                           
         CLI   PRETOFC,C'1'                                                     
         BNE   EDIT6A                                                           
         OI    PUBPTYPC,X'80'                                                   
         B     EDIT7                                                            
EDIT6A   CLI   PRETOFC,C'2'                                                     
         BNE   EDIT6B                                                           
         OI    PUBPTYPC,X'40'                                                   
         B     EDIT7                                                            
EDIT6B   CLI   PRETOFC,C'3'                                                     
         BNE   EDIT6C                                                           
         OI    PUBPTYPC,X'20'                                                   
         B     EDIT7                                                            
EDIT6C   CLI   PRETOFC,C'4'                                                     
         BNE   EDIT6D                                                           
         OI    PUBPTYPC,X'10'                                                   
         B     EDIT7                                                            
EDIT6D   CLI   PRETOFC,C'5'                                                     
         BNE   ERROR                                                            
         OI    PUBPTYPC,X'08'                                                   
*                                                                               
EDIT7    LA    R2,PRECLMOH                                                      
         MVC   PUBPCLMO(2),=PL2'0'                                              
         CLI   5(R2),0                                                          
         BE    EDIT8                                                            
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(0,PRECLMO),(R5)                                   
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     R5,DMCB+4                                                        
         CVD   R5,DUB                                                           
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'0'     ZERO REMAINDER                                
         BNE   ERROR                                                            
         MVC   PUBPCLMO(2),DUB+4                                                
*                                                                               
EDIT8    LA    R2,PRECLDAH                                                      
         MVC   PUBPCLDA(2),=PL2'0'                                              
         CLI   5(R2),0                                                          
         BE    EDITL                                                            
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(0,PRECLDA),(R5)                                   
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     R5,DMCB+4                                                        
         CVD   R5,DUB                                                           
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'0'     MUST GET ZERO REMAINDER                       
         BNE   ERROR                                                            
         CP    DUB+4(2),=P'31'                                                  
         BH    ERROR                                                            
         CP    PUBPCLMO(2),=P'0'    MO AND DA CAN'T BOTH BE MINUS               
         BNL   EDIT8X                                                           
         CP    DUB+4(2),=P'0'                                                   
         BL    ERROR                                                            
EDIT8X   MVC   PUBPCLDA(2),DUB+4                                                
         B     EDITL                                                            
         EJECT                                                                  
*NOP*EDITL    LA    R8,ELEAREA+40                                               
*NOP*         USING TBLEL,R8                                                    
EDITL    LA    R3,ELEAREA+40                                                    
         USING TBLEL,R3                                                         
         MVI   MXSW,0                                                           
         LA    R4,6                                                             
         LA    R5,PRECH1H                                                       
EDITLA   CLI   5(R5),0                                                          
         BNE   EDITLB                                                           
         LR    R2,R5                                                            
         BAS   RE,BUMPFLD                                                       
         CLI   5(R5),0                                                          
         BE    NEXTF                                                            
         LA    R3,1                                                             
         B     ERROR                                                            
*                                                                               
NEXTF    BAS   RE,BUMPFLD                                                       
         BNE   UPDATE                                                           
         B     EDITLA                                                           
*                                                                               
EDITLB   LA    R2,0(R5)                                                         
         ST    R3,FULL             SAVE ELEAREA ADDRESS                         
         LA    R3,2        INVALID FIELD                                        
*                                                                               
         TM    PUBPTYPC,B'11000000'    CHECK IF 1=FLAT,2=BW+FLAT                
         BZ    EDITLBA                                                          
         SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(2,8(R2)),(R0)                                     
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     R0,DMCB+4                                                        
         CVD   R0,DUB                                                           
         CP    DUB,=P'9999999'                                                  
         BH    ERROR                                                            
         CP    DUB,=P'0'                                                        
         BNH   ERROR                                                            
         MP    DUB,=P'1000'                                                     
         B     EDITLBB                                                          
*                                                                               
EDITLBA  DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(5,8(R2)),(R0)                                     
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     R0,DMCB+4                                                        
         CVD   R0,DUB                                                           
         CP    DUB,=P'999999999'                                                
         BH    ERROR                                                            
         CP    DUB,=P'0'                                                        
         BNH   ERROR                                                            
EDITLBB  DS    0H                                                               
         L     R3,FULL             RESTORE ELEAREA ADDRESS                      
*        MVC   PUBPTBCH(5),DUB+3                                                
         MVC   PUBPTBCH(6),DUB+2                                                
         BAS   RE,BUMPFLD                                                       
         LR    R2,R5                                                            
         LA    R3,1      MISSING FIELD                                          
         CLI   5(R2),0                                                          
         BE    ERROR                                                            
         L     R3,FULL             RESTORE ELEAREA ADDRESS                      
         CLC   8(4,R2),=C'SPOT'                                                 
         BNE   EDITM0                                                           
         ZAP   PUBPTBLN,=P'0'                                                   
         B     ELOK                                                             
EDITM0   EQU   *                                                                
         CLI   8(R2),C'U'                                                       
         BNE   EDITM1                                                           
         LA    R3,DUPLEV                                                        
         CLI   MXSW,1                                                           
         BE    ERROR                                                            
         L     R3,FULL             RESTORE ELEAREA ADDRESS                      
         MVC   PUBPTBLN(5),=P'999999999'                                        
         MVI   MXSW,1                                                           
         B     ELOK                                                             
EDITM1   LA    R3,2           INVALID FIELD                                     
         SR    R0,R0                                                            
         IC    R0,5(R2)                                                         
         GOTO1 VCASHVAL,DMCB,(2,8(R2)),(R0)                                     
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     R0,DMCB+4                                                        
         AH    R0,=H'50'                                                        
         CVD   R0,DUB                                                           
         DP    DUB,=P'100'                                                      
         CP    DUB(6),=P'0'                                                     
         BNH   ERROR                                                            
         CP    DUB(6),=P'999999999'                                             
         BH    ERROR                                                            
         L     R3,FULL             RESTORE ELEAREA ADDRESS                      
         MVC   PUBPTBLN(5),DUB+1                                                
         CP    PUBPTBLN(5),=P'999999999'                                        
         BE    CKMAX                                                            
*                                                                               
ELOK     MVC   PUBPTBEL(2),=X'610E'                                             
*NOP*    LA    R8,14(R8)                                                        
         LA    R3,14(R3)                                                        
         B     NEXTF                                                            
*                                                                               
CKMAX    CLI   MXSW,1                                                           
         BE    ERROR                                                            
         MVI   MXSW,1                                                           
         B     ELOK                                                             
*                                                                               
*NOP*    DROP  R8                                                               
         DROP  R3                                                               
         EJECT                                                                  
UPDATE   LA    R6,ELEAREA+40                                                    
         LA    R2,PRECH1H                                                       
         LA    R3,NOCHGS                                                        
         CLI   0(R6),X'61'                                                      
         BNE   ERROR                                                            
         LA    R3,NOMAX                                                         
         CLI   MXSW,1                                                           
         BC    0,ERROR             NOP TEST FOR UNLIM                           
         TM    PUBIND,X'10'                                                     
         BNO   UPDATE2             NO RATES DISPLAYED - ADD NEW ONES            
UPDATE0  LA    R6,PUBREC+33                                                     
         MVI   ELCOD,X'60'                                                      
UPDATE1  BAS   RE,NXTEL                                                         
         BL    *-4                                                              
         BE    CKCDDAT                                                          
         CLI   0(R6),X'61'                                                      
         BE    UPDATE1                                                          
         B     UPDATE2                                                          
*                                                                               
CKCDDAT  CLC   2(3,R6),PUBPCOD                                                  
         BL    UPDATE1                                                          
         BH    UPDATE2                                                          
CKDAT    CLC   5(3,R6),BDIV                                                     
         BH    CKCDDAT1                                                         
         BNE   UPDATE2                                                          
         CLC   27(3,R6),BCLT       SAME CLIENT CODE ?                           
         BE    DELETE60                                                         
         B     UPDATE1                                                          
*                                                                               
CKCDDAT1 SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'61'                                                      
         BE    CKCDDAT1                                                         
         CLI   0(R6),X'60'                                                      
         BL    UPDATE1                                                          
         BH    UPDATE2                                                          
         CLC   2(3,R6),PUBPCOD                                                  
         BNE   UPDATE2                                                          
         B     CKDAT                                                            
*                                                                               
DELETE60 GOTO1 VRECUP,DMCB,(1,PUBREC),0(R6)                                     
         CLI   0(R6),X'61'                                                      
         BNE   CLEARREC                                                         
         B     DELETE60                                                         
*                                                                               
CLEARREC SR    R5,R5                                                            
         IC    R5,PUBREC+25                                                     
         SLL   R5,8                                                             
         IC    R5,PUBREC+26                                                     
         SR    RE,RE                                                            
         LA    RE,PUBREC                                                        
         AR    RE,R5                                                            
         SR    RF,RF                                                            
         LA    RF,PUBREC+1999                                                   
         LA    RF,2000(RF)                                                      
         SR    RF,RE                                                            
         XCEF                                                                   
         B     UPDATE2                                                          
*                                                                               
UPDATE2  CLC   PREDAYS(3),=C'DEL'     SPECIAL DELETE CODE                       
         BNE   UPDATE2A                                                         
         NI    PBLMEDH+4,X'DF'     UNVALIDATE MEDIA                             
         B     WRITEIT                                                          
*                                                                               
UPDATE2A DS    0H                                                               
         LA    R6,PUBREC+33                                                     
         MVI   ELCOD,X'60'                                                      
UPDATE2C BAS   RE,NXTEL                                                         
         BL    *-4                                                              
         BE    UPDATE2D                                                         
         CLI   0(R6),X'61'                                                      
         BE    UPDATE2C                                                         
         B     UPDATE2G                                                         
*                                                                               
UPDATE2D CLC   2(3,R6),PUBPCOD                                                  
         BL    UPDATE2C                                                         
         BH    UPDATE2G                                                         
         CLC   5(3,R6),PUBPSTRT                                                 
         BH    UPDATE2C                                                         
         BNE   UPDATE2G                                                         
         LA    R3,ADDERR                                                        
         LA    R2,PREEFFDH                                                      
         CLC   27(3,R6),BCLT       SAME CLIENT CODE ?                           
         BE    ERROR               YES                                          
         B     UPDATE2C                                                         
*NOP*    B     ERROR                                                            
*                                                                               
UPDATE2G DS    0H                                                               
         CLI   BCLT,C' '           CLIENT- SPECIFIC ?                           
         BNH   *+10                NO                                           
         MVC   PUBPCLT,BCLT        ADD CLIENT-SPECIFIC FIELD                    
         GOTO1 VRECUP,DMCB,(1,PUBREC),0(R7),0(R6)                               
         LA    R7,40(R7)                                                        
         LA    R6,40(R6)                                                        
         CLI   0(R7),0                                                          
         BE    WRITEIT                                                          
         LA    R5,0(R6)                                                         
*           SAVE ADDRESS OF FIRST 61 ADDED                                      
UPDATE3  GOTO1 VRECUP,DMCB,(1,PUBREC),0(R7),0(R6)                               
         LA    R7,14(R7)                                                        
         CLI   0(R7),0                                                          
         BE    WRITEIT                                                          
         CP    2(5,R7),2(5,R6)                                                  
         BL    UPDATE4                                                          
         BE    ERRORL                                                           
         LA    R6,14(R6)                                                        
         B     UPDATE5                                                          
*                                                                               
UPDATE4  LA    R6,0(R5)            RESET R6 TO FIRST 61 ADDED                   
UPDATE4A CP    2(5,R7),2(5,R6)                                                  
         BE    ERRORL                                                           
         BL    UPDATE3                                                          
         LA    R6,14(R6)                                                        
UPDATE5  CLI   0(R6),X'61'                                                      
         BE    UPDATE4A                                                         
         B     UPDATE3                                                          
*                                                                               
ERRORL   LA    R2,PRECH1H                                                       
         LA    R3,DUPLEV                                                        
         B     ERROR                                                            
         DROP  R7                                                               
         EJECT                                                                  
WRITEIT  MVC   KEY+27(4),PUBADDR                                                
         BAS   RE,PUTPUB                                                        
         B     DONE                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
FORMATP  DS    0H                                                               
         CLI   SAVSCRN,X'06'                                                    
         BNE   FMT2                                                             
         CLI   BACT,1                                                           
         BNE   FMT5                                                             
         CLI   PREEFFDH+5,0        CHK FOR INPUT                                
         BE    FMT5                NO                                           
*NOP*    BAS   R8,PUTDATES                                                      
         BAS   RE,PUTDATES                                                      
         MVI   BYTE2,0             TO GENERATE TURNAROUND                       
         B     EDIT                                                             
*                                                                               
FMT2     LA    R4,PBLLAST                                                       
         GOTO1 VCALLOV,WORK,(R4),X'D90406F6'                                    
         CLI   4(R1),X'FF'                                                      
         BE    VIRGERR                                                          
         MVI   SAVSCRN,X'06'                                                    
FMT5     CLI   BACT,1                                                           
         BNE   PUTFLDS                                                          
*NOP*    BAS   R8,PUTDATES                                                      
         BAS   RE,PUTDATES                                                      
         B     CKSRDS                                                           
*                                                                               
PUTFLDS  LA    R2,PBLSCRH                                                       
         LA    R3,NOPREM                                                        
         TM    PUBIND,X'10'                                                     
         BNZ   *+16                                                             
*NOP*    BAS   R8,PUTDATES                                                      
         BAS   RE,PUTDATES                                                      
         NI    PBLMEDH+4,X'DF'       UNVALIDATE MED SO STND REC                 
*                                   WON'T BE CHANGED                            
         B     ERROR                                                            
*        GOTO1 VDTCNV,DMCB,(1,PUBPSTRT),(3,PREEFFD)                             
         GOTO1 VDATCON,DMCB,(3,PUBPSTRT),(5,PREEFFD)                            
         FOUT  PREEFFDH                                                         
         XC    PREDAYS,PREDAYS                                                  
         CLI   PUBPDAYS,X'7F'                                                   
         BNE   *+14                                                             
         MVC   PREDAYS(3),=C'ALL'                                               
         B     PUTFLDS1                                                         
         LA    R1,PREDAYS                                                       
         LA    R4,PDAYTAB                                                       
CKBITS   MVC   WORK(1),PUBPDAYS                                                 
         NC    PUBPDAYS(1),0(R4)                                                
         CLC   PUBPDAYS,WORK                                                    
         BE    NEXTDAY                                                          
         MVC   0(2,R1),1(R4)                                                    
         OC    PUBPDAYS,PUBPDAYS                                                
         BZ    PUTFLDS1                                                         
         MVI   2(R1),C','                                                       
         LA    R1,3(R1)                                                         
*                                                                               
NEXTDAY  CLC   3(2,R4),=X'0000'                                                 
         BE    PUTFLDS1                                                         
         LA    R4,3(R4)                                                         
         OC    PUBPDAYS,PUBPDAYS                                                
         BZ    PUTFLDS1                                                         
         B     CKBITS                                                           
*                  M O   T U   W E   T H   F R   S A   S U                      
PDAYTAB  DC    X'BFD4D6DFE3E4EFE6C5F7E3C8FBC6D9FDE2C1FEE2E40000'                
PUTFLDS1 FOUT  PREDAYSH                                                         
*                                                                               
         CLC   PUBPMINS(4),=C'PAGE'                                             
         BNE   EDTMINS                                                          
         MVC   PREMINS(7),=C'PAGE   '                                           
         B     EDTMINC                                                          
EDTMINS  MVC   PACKED4,PUBPMINS                                                 
         EDIT  PACKED4,(7,PREMINS),ALIGN=LEFT                                   
EDTMINC  FOUT  PREMINSH                                                         
         MVC   PACKED5,PUBPMINC                                                 
         EDIT  PACKED5,(9,PREMINC),2,ALIGN=LEFT                                 
         FOUT PREMINCH                                                          
         MVI   PRETOFC,C'1'                                                     
         TM    PUBPTYPC,X'80'                                                   
         BO    PUTTYP                                                           
         MVI   PRETOFC,C'2'                                                     
         TM    PUBPTYPC,X'40'                                                   
         BO    PUTTYP                                                           
         MVI   PRETOFC,C'3'                                                     
         TM    PUBPTYPC,X'20'                                                   
         BO    PUTTYP                                                           
         MVI   PRETOFC,C'4'                                                     
         TM    PUBPTYPC,X'10'                                                   
         BO    PUTTYP                                                           
         MVI   PRETOFC,C'5'                                                     
         TM    PUBPTYPC,X'08'                                                   
         BO    PUTTYP                                                           
         XC    PRETOFC,PRETOFC                                                  
PUTTYP   MVC   MYBYTE,PUBPTYPC                                                  
         FOUT  PRETOFCH                                                         
         MVC   PACKED2(2),PUBPCLMO                                              
         EDIT  PACKED2,(3,PRECLMO),FLOAT=-,ALIGN=LEFT                           
         FOUT  PRECLMOH                                                         
         MVC   PACKED2(2),PUBPCLDA                                              
         EDIT  PACKED2,(3,PRECLDA),FLOAT=-,ALIGN=LEFT                           
         FOUT  PRECLDAH                                                         
*                                                                               
*    CLEAR DISCOUNT LEVEL FIELDS (PRECH1 TO 6 AND PREMX1 TO 6)                  
         LA    R5,PRECH1H                                                       
         LA    R4,6                                                             
PUTCLR   DS    0H                                                               
         XC    8(10,R5),8(R5)      CLEAR PRECH                                  
         FOUT  (R5)                                                             
         BAS   RE,BUMPFLD                                                       
         XC    8(9,R5),8(R5)       CLEAR PREMX                                  
         FOUT  (R5)                                                             
         BAS   RE,BUMPFLD                                                       
         BCT   R4,PUTCLR                                                        
*                                                                               
*    SEE IF ANY PREMIUM TABLE ELEMENTS EXIST, IF NOT SEND BLANK                 
*              DISCOUNT LEVELS           R6 POINTS TO PUBPRMEL                  
         LA    R5,PRECH1H                                                       
NEXTLVL  SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'61'                                                      
         BNE   PUTLAST                                                          
         USING PUBPTBEL,R6                                                      
*        MVC   PACKED5,PUBPTBCH                                                 
         MVC   PACKED6,PUBPTBCH                                                 
         TM    MYBYTE,B'11000000'   CHECK IF 1=FLAT,2=BW+FLAT                   
         BZ    PUTSIZ0                                                          
         XC    DUB,DUB                                                          
         MVC   DUB+2(6),PACKED6                                                 
         DP    DUB,=P'1000'                                                     
         EDIT  (P5,DUB),(10,8(R5)),2,ALIGN=LEFT                                 
         B     PUTSIZE                                                          
*                                                                               
PUTSIZ0  DS    0H                                                               
         EDIT  PACKED6,(10,8(R5)),5,ALIGN=LEFT,DROP=3                           
*                                                                               
PUTSIZE  DS    0H                                                               
         FOUT  (R5)                                                             
         BAS   RE,BUMPFLD                                                       
         MVC   PACKED5,PUBPTBLN                                                 
         CLC   PACKED5(5),=PL5'0'                                               
         BNE   PUTSIZE2                                                         
         MVC   08(9,R5),=C'SPOT     '                                           
         B     FOUTIT                                                           
PUTSIZE2 EQU   *                                                                
         CLC   PACKED5(5),=PL5'999999999'                                       
         BNE   EDITSIZE                                                         
         MVC   08(9,R5),=C'UNLIMITED'                                           
         B     FOUTIT                                                           
EDITSIZE EDIT  PACKED5,(9,08(R5)),COMMAS=YES,ALIGN=LEFT                         
FOUTIT   FOUT  (R5)                                                             
         BAS   RE,BUMPFLD                                                       
         BE    NEXTLVL                                                          
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'61'                                                      
         BE    DEAD                                                             
PUTLAST  DS    0H                                                               
*NOP*    BAS   R8,PUTDATES                                                      
         BAS   RE,PUTDATES                                                      
         CLI   BACT,3                   SRDS                                    
         BNE   CKSRDS                                                           
         B     PROTECT                                                          
DEAD     DC    H'0'                TOO MANY PREMIUM CHARGE ELEMENTS             
*                                                                               
*                                                                               
PROTECT  OI    PREEFFDH+1,X'20'                                                 
         OI    PREDAYSH+1,X'20'                                                 
         OI    PREMINSH+1,X'20'                                                 
         OI    PREMINCH+1,X'20'                                                 
         OI    PRETOFCH+1,X'20'                                                 
         OI    PRECLMOH+1,X'20'                                                 
         OI    PRECLDAH+1,X'20'                                                 
         LA    R5,PRECH1H                                                       
PROT1    OI    1(R5),X'20'                                                      
         BAS   RE,BUMPFLD                                                       
         BE    PROT1                                                            
         MVI   SAVSCRN,0                                                        
         CLI   BACT,2                                                           
         BH    DONE                                                             
         BE    PROT2                                                            
         LA    R2,PREEFFDH                                                      
         B     EXIT                                                             
*                                                                               
PROT2    LA    R2,PREEFFDH                                                      
         B     EXIT                                                             
*                                                                               
CKSRDS   CLC   AGYALPHA(2),PUBKAGY                                              
         BNE   PROTECT                                                          
         CLI   BACT,2                                                           
         BE    PROT2                                                            
         LA    R2,PREEFFDH                                                      
         B     EXIT                                                             
*                                                                               
*                                                                               
*    ROUTINES                                                                   
*                                                                               
NXTEL    EQU   *           BUMP TO NEXT ELEMENT AND TEST CODE                   
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    *+12                                                             
         CLC   0(1,R6),ELCOD                                                    
         BR    RE                                                               
         LTR   R6,R6                                                            
         BR    RE                                                               
*                                                                               
PUTDATES DS    0H                                                               
         ST    RE,FULL             STORE RETURN ADDRESS                         
         XC    PREDTS,PREDTS                                                    
         LA    R6,PUBREC+33                                                     
         USING PUBPRMEL,R6                                                      
         MVI   ELCOD,X'60'                                                      
         LA    R4,PREDTSH+8                                                     
         LA    R5,6                                                             
DATES1   BAS   RE,NXTEL                                                         
         BL    *-4                                                              
         BE    DATES2                                                           
         CLI   0(R6),X'61'                                                      
         BE    DATES1                                                           
         B     DATES3                                                           
DATES2   EQU   *                                                                
         CLC   PUBPCOD(3),BCODE                                                 
         BNE   DATES1                                                           
         CLC   PUBPCLT(3),BCLT                                                  
         BNE   DATES1                                                           
*        GOTO1 VDTCNV,DMCB,(1,PUBPSTRT),(3,0(R4))                               
         GOTO1 VDATCON,DMCB,(3,PUBPSTRT),(5,0(R4))                              
         CLI   BACT,1                                                           
         BE    DATES2C                                                          
         CLC   PREEFFD(8),0(R4)                                                 
         BNE   *+14                                                             
         XC    0(8,R4),0(R4)                                                    
         B     DATES1                                                           
*                                                                               
DATES2C  MVI   8(R4),C','                                                       
         LA    R4,9(R4)                                                         
         BCT   R5,DATES1                                                        
*                                                                               
DATES3   FOUT  PREDTSH                                                          
         L     RE,FULL             RESTORE RETURN ADDRESS                       
         CLI   PREDTS,0                                                         
         BNE   *+12                                                             
         MVC   PREDTS(4),=C'NONE'                                               
*NOP*    BR    R8                                                               
         BR    RE                                                               
         BCTR  R4,R0                                                            
         MVI   0(R4),0                                                          
*NOP*    BR    R8                                                               
         BR    RE                                                               
*                                                                               
         SPACE 2                                                                
*                                  FIND NEXT UNPROTECTED FIELD                  
BUMPFLD  DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,0(R5)                                                         
         AR    R5,R0                                                            
         CLI   0(R5),10                                                         
         BLR   RE                  EOS                                          
         TM    1(R5),X'20'                                                      
         BNZ   BUMPFLD                                                          
         BR    RE                  CC = 0                                       
         EJECT                                                                  
DONE     MVI   BYTE3,1                                                          
         B     EXXMOD                                                           
*                                                                               
SPACES   DC    40C' '                                                           
PUBIND   DS    CL1                                                              
ACTSW    DS    CL1                                                              
MXSW     DS    CL1                                                              
MYBYTE   DS    X                                                                
PACKED4  DS    PL4                                                              
PACKED5  DS    PL5                                                              
PACKED6  DS    PL6                                                              
PACKED2  DS    PL2                                                              
ELCOD    DS    CL1                                                              
NOPREM   EQU   145                                                              
*                                                                               
VIRGERR  DC    H'0'                                                             
COMBERR  EQU   112                                                              
DATERR   EQU   20                                                               
DUPLEV   EQU   170                                                              
ADDERR   EQU   173                                                              
NOCHGS   EQU   174                                                              
NOMAX    EQU   175                                                              
         EJECT                                                                  
CLEARWRK LTR   R5,R5                                                            
         BCR   8,RE                                                             
         CH    R5,=H'250'                                                       
         BNH   CLEAREST                                                         
         XC    0(250,R4),0(R4)                                                  
         LA    R4,250(R4)                                                       
         SH    R5,=H'250'                                                       
         B     CLEARWRK                                                         
*                                                                               
CLEAREST BCTR  R5,R0                                                            
         EX    R5,VARCLEAR                                                      
         BR    RE                                                               
*                                                                               
VARCLEAR XC    0(0,R4),0(R4)                                                    
*                                                                               
ANY      CLI   5(R2),0                                                          
         BNE   ANY2                                                             
         LA    R3,1                                                             
         B     ERROR                                                            
*                                                                               
ANY2     TM    4(R2),X'10'                                                      
         BCR   8,RE                                                             
         LA    R3,3                                                             
         B     ERROR                                                            
*                                                                               
PACK     SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BCR   8,RE                                                             
         TM    4(R2),X'08'                                                      
         BCR   8,RE                                                             
         BCTR  R1,R0                                                            
         EX    R1,VARPACK                                                       
         CVB   R0,DUB                                                           
         BR    RE                                                               
*                                                                               
VARPACK  PACK  DUB,8(0,R2)                                                      
*                                                                               
*              COMMUNICATIONS WITH DATA MANAGER (PUBDIR)                        
*                                                                               
READPUB  MVC   COMMAND,=C'DMREAD'                                               
         MVC   KEYSAVE,KEY                                                      
         B     PUBDIRY                                                          
*                                                                               
PUBDIRY  NTR                                                                    
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBDIR',             X        
               KEY,KEY,(TERMNAL,0)                                              
         B     DMCHECK                                                          
         EJECT                                                                  
GETPUB   MVC   COMMAND,=C'GETREC'                                               
         B     PUBFILE                                                          
*                                                                               
PUTPUB   MVC   COMMAND,=C'PUTREC'                                               
         B     PUBFILE                                                          
*                                                                               
PUBFILE  NTR                                                                    
         LA    R2,KEY+27                                                        
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'PUBFILE',            X        
               (R2),PUBIO,(TERMNAL,DMWORK)                                      
         B     DMCHECK                                                          
         EJECT                                                                  
*              DATA MANAGER ERRORS AND EXIT                                     
*                                                                               
DMCHECK  MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BNZ   DMERRS                                                           
         XIT                                                                    
*                                                                               
DMERRS   L     RD,4(RD)                                                         
         LM    RE,RC,12(RD)                                                     
         SR    R3,R3                                                            
         B     ERROR                                                            
         EJECT                                                                  
*              EXITS FROM PROGRAM                                               
*                                                                               
ERROR    L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(4,DMCB)                            
*                                                                               
EXIT     OI    6(R2),OI1C       INSERT CURSOR                                   
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
EXXMOD   XMOD1 1                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
ELEAREA  DS    500C                                                             
*                                                                               
         EJECT                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE PUGENOLD                                                       
PUBIO    DS    4000C                                                            
         ORG   PUBIO                                                            
       ++INCLUDE PUBREC                                                         
         EJECT                                                                  
*                                                                               
PREMEL   DSECT                                                                  
       ++INCLUDE PUBPRMEL                                                       
         EJECT                                                                  
*                                                                               
TBLEL    DSECT                                                                  
       ++INCLUDE PUBPTBEL                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE FLDIND                                                         
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPPUBFFD                                                       
         ORG   PBLLAST                                                          
       ++INCLUDE PPPUBF6D                                                       
         ORG   T406FFD                                                          
         DS    CL16                                                             
BMED     DS    CL1                                                              
BACT     DS    CL1                                                              
BSCR     DS    CL1                                                              
OLNUM    DS    CL1                                                              
PUBADDR  DS    F                                                                
LTLADDR  DS    F                                                                
BPUB     DS    CL6                                                              
BCLT     DS    CL3                                                              
BDIV     DS    CL3                                                              
BDATE    DS    CL3                                                              
APROF    DS    CL1                                                              
SAVSCRN  DS    CL1                                                              
APROF13  DS    CL1                                                              
BCODE    DS    CL3                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019PPPUB06   05/01/02'                                      
         END                                                                    
