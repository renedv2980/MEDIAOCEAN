*          DATA SET RECBT53T   AT LEVEL 179 AS OF 05/01/02                      
*PHASE T80253A +0                                                               
         TITLE 'T80253 - HISTORY DISPLAY'                                       
*                                                                               
*******************************************************************             
*                                                                 *             
*        RECNT53 (T80253) --- HISTORY DISPLAY                     *             
*                                                                 *             
* --------------------------------------------------------------- *             
* REFER TO RECNTHIST FOR PAST HISTORY                             *             
*                                                                 *             
*                                                                 *             
* 20DEC95 SKU REWORD HIST/INFO SCREEN                             *             
*                                                                 *             
* 12DEC95 SKU 2K CONTRACT SUPPORT                                 *             
*                                                                 *             
* 05FEB96 RHV ADD DISPLAYING OF CFX FEATURE IN INFO               *             
*                                                                 *             
* 28FEB96 WSB TELL IN INFO WHETHER BOP ADDED IF STEREO IS ACTIVE  *             
*                                                                 *             
* 26MAR96 BU  DISPLAY CONVERTED INFORMATION FOR PETRY             *             
*                                                                 *             
* 27MAR96 SKU SUPPORT EXPANDED X'21' MG SEND ELEMENT              *             
*                                                                 *             
* 26JUL96 SKU SUPPORT TRANSITION TO THMS EC TIME STAMPING         *             
*                                                                 *             
* 13FEB97 RHV DISPLAY BATCH CONF FLAG                             *             
*                                                                 *             
* 24FEB97 RHV DISPLAY MOD HISTORY                                 *             
*                                                                 *             
* 15APR97 RHV ACTIVITY DATE IN MOVE HIST DISPLAY                  *             
*                                                                 *             
* 07MAY97 RHV R16 CLOSEOUT HISTORY IN INFO DISPLAY               *              
*                                                                 *             
* 22MAY97 SKU TKO DARE HISTORY                                    *             
*                                                                 *             
* 13JUN97 SKU DARE REMOVE FLAG                                    *             
*                                                                 *             
* 26AUG97 SKU DARE AGENCY CHANGES MANUAL FLAG                     *             
*                                                                 *             
* 23OCT97 BU  HIST/$$$ FOR ALTERNATE CALENDAR X'53'/X'54' ELTS    *             
*                                                                 *             
* 14JAN98 BU  ADDITIONAL BUYNUM OPTS:  $$=FOR  $$$=ALT            *             
*                                                                 *             
* 11FEB98 JRD SHOW WHICH ALT CALENDAR USED ON HIST SCREEN         *             
*             USE R7 AS 3RD BASE REGISTER                         *             
*                                                                 *             
* 13JUL98 AST SHOWS CONTRACTS THAT HAVE BEEN EXCLUDED FROM RER    *             
*                                                                 *             
* 01FEB99 RHV 'FIRST SENT' DISPLAY                                *             
*                                                                 *             
* 05MAR99 SKU DISPLAY IF DARE IN HIST-ONLY SCREEN                 *             
*                                                                 *             
*******************************************************************             
* IF YOU ARE GOING TO ADD ANYMORE MESSAGES TO THE BOTTOM OF THE                 
* SCREEN MAKE SURE YOU CHECK FOR END OF SCREEN (HISLAST)!!!                     
* YOU MAY NEED TO MOVE SOME MESSAGES TO THE RIGHT                               
*******************************************************************             
*                                                                               
T80253   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80253,R9,R7                                                   
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
         LR    R8,RA                                                            
         AH    R8,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R8                                                       
*                                                                               
* CLEAR DISPLAY FIELDS                                                          
         LA    R1,HISLIN1H                                                      
         LA    R0,13                                                            
         SR    RE,RE                                                            
HIST5    OI    6(R1),X'80'                                                      
         XC    8(79,R1),8(R1)                                                   
         IC    RE,0(R1)                                                         
         AR    R1,RE                                                            
         BCT   R0,HIST5                                                         
*                                                                               
         CLI   CONBNUMH+5,0        NO INPUT?                                    
         BNE   HIST20                                                           
*                                                                               
HIST10   DS    0H                                                               
         BAS   RE,DISDTES          DISPLAY HISTORY DATES                        
         B     EXXMOD                                                           
*                                                                               
HIST20   DS    0H                                                               
         CLC   =C'CON',CONBNUM     conversion info display                      
         BE    HIST55              YES                                          
         CLC   =C'INV',CONBNUM     INVOICE# DATE DISPLAY                        
         BE    HIST50              YES                                          
         CLC   CONBNUM(4),=C'DARE' DARE DATE DISPLAY                            
         BE    HIST45              YES                                          
         CLC   CONBNUM(3),=C'$$$'  ALTERNATE CALENDAR DOLLARS                   
         BE    HIST60              YES                                          
         CLC   CONBNUM(3),=C'ALT'  ALTERNATE CALENDAR DOLLARS                   
         BE    HIST60              YES                                          
         CLC   CONBNUM(2),=C'$$'   FORECAST DOLLARS                             
         BE    HIST35              YES                                          
         CLC   CONBNUM(3),=C'FOR'  FORECAST DOLLARS                             
         BE    HIST35              YES                                          
         CLC   CONBNUM(3),=C'RER'  RER?                                         
         BE    HIST70              YES                                          
         CLC   CONBNUM(3),=C'SON'  SONNET?                                      
         BE    HIST75              YES                                          
         CLC   =C'$NEXT$$',CONBNUM ALTERNATE CALENDAR DOLLARS NEXT              
         BE    HIST60A             YES                                          
         CLC   =C'$NEXT$',CONBNUM  FORECAST DOLLARS NEXT                        
         BE    HIST35A             YES                                          
         CLC   =C'$NEXT',CONBNUM   ESTIMATE/INVOICE DOLLARS NEXT                
         BE    HIST30              YES                                          
         CLI   CONBNUM,C'$'        ESTIMATE/INVOICE DOLLARS                     
         BE    HIST30              YES                                          
         CLC   =C'INFO',CONBNUM                                                 
         BE    HIST40                                                           
         LA    R3,INVINP                                                        
         LA    R2,CONBNUMH                                                      
         B     ERROR                                                            
*                                                                               
HIST30   DS    0H                                                               
         BAS   RE,DISDOLS                                                       
         B     EXXMOD                                                           
*                                                                               
HIST35   DS    0H                                                               
         MVC   TWAHIST(3),CONBNUM                                               
HIST35A  DS    0H                                                               
         BAS   RE,DISFOR$$                                                      
         B     EXXMOD                                                           
*                                                                               
HIST40   DS    0H                                                               
         BAS   RE,DISDTES          DISPLAY HISTORY DATES                        
         BAS   RE,DISINFO          DISPLAY ADDITIONAL INFO                      
         B     EXXMOD                                                           
*                                                                               
HIST45   DS    0H                                                               
         BAS   RE,DISDARE          DISPLAY HISTORY DATES FOR DARE               
         B     EXXMOD                                                           
                                                                                
HIST50   DS    0H                                                               
         LA    R3,INVINP                                                        
         LA    R2,CONBNUMH                                                      
         TM    TWATIME,X'10'       REP PROFILE 8 SET?                           
         BZ    ERROR                                                            
         BAS   RE,DISINV#          DISPLAY INVOICE#                             
         B     EXXMOD                                                           
*                                                                               
HIST55   DS    0H                                                               
         BAS   RE,DISCONV          DISPLAY convert info                         
         B     EXXMOD                                                           
                                                                                
HIST60   DS    0H                                                               
         MVC   TWAHIST(3),CONBNUM                                               
HIST60A  DS    0H                                                               
         GOTO1 =A(DISALT$),DMCB,(RC),RR=YES                                     
*                                  DISPLAY ALTERNATE CALENDAR $$                
         B     EXXMOD                                                           
*                                                                               
HIST70   DS    0H                                                               
         BAS   RE,DISRER           DISPLAY RER INFO                             
         B     EXXMOD                                                           
*                                                                               
HIST75   DS    0H                                                               
         BAS   RE,DISSON           DISPLAY SONNET INFO                          
         B     EXXMOD                                                           
*                                                                               
         EJECT                                                                  
*                                                                               
* DISPLAY HISTORICAL DATES                                                      
DISDTES  NTR1                                                                   
         LA    R2,HISLIN1H                                                      
         MVC   9(L'CONCRDT,R2),CONCRDT                                          
         GOTO1 DATCON,DMCB,(3,RCONHDRD),(5,9+L'CONCRDT(R2))                     
         LA    R3,9+L'CONCRDT+10(R2)                                            
*                                                                               
* CHECK FOR ALTERNATE BUCKETS                                                   
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A4'                                                     
         BAS   RE,GETEL                                                         
         BNE   DD02                                                             
         USING RCONAXEL,R6                                                      
         TM    RCONAXFL,X'80'                                                   
         BZ    *+10                                                             
         MVC   0(L'REPALTC,R3),REPALTC                                          
         TM    RCONAXFL,X'40'                                                   
         BZ    *+10                                                             
         MVC   0(L'STAALTC,R3),STAALTC                                          
         DROP  R6                                                               
DD02     DS    0H                                                               
*                                                                               
* GET X'20' SEND ELEM FOR SEND DATES                                            
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   DD10                                                             
         USING RCONSEND,R6                                                      
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         MVC   9(L'CONLSBR,R2),CONLSBR                                          
         GOTO1 DATCON,DMCB,(2,RCONSRDT),(5,9+L'CONLSBR(R2))                     
         OC    RCONSRTI,RCONSRTI                                                
         BZ    DD05                                                             
         MVC   19+L'CONLSBR(2,R2),RCONSRTI                                      
         MVI   21+L'CONLSBR(R2),C':'                                            
         MVC   22+L'CONLSBR(2,R2),RCONSRTI+2                                    
DD05     DS    0H                                                               
         TM    RCONMODR+1,X'40'    FOR GRAPHNET CONTRACTS, SKIP                 
         BNZ   DD10                LAST SENT BY STATION DATE/TIME               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         MVC   9(L'CONLSBS,R2),CONLSBS                                          
                                                                                
         TM    RCONSENF,X'01'      SHOW IF TAKEOVER CONTRACT 'DONE'             
         BZ    DD06                                                             
         MVC   9(L'CONTKOV,R2),CONTKOV                                          
         B     DD08                                                             
*                                                                               
* FOR GRAPHNET, SKIP LAST SENT BY STATION                                       
*                                                                               
DD06     DS    0H                                                               
*        TM    RCONMODR+1,X'40'    FOR GRAPHNET CONTRACTS, SHOW LAST            
*        BZ    DD08                CONF DATE/TIME INSTEAD OF SENT               
*        MVC   9(L'CONLCON,R2),CONLCON                                          
*        XC    9(L'CONLSBR,R2),9(R2)                                            
*        B     DD10                                                             
                                                                                
DD08     DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,RCONSSDT),(5,9+L'CONLSBS(R2))                     
         OC    RCONSSTI,RCONSSTI                                                
         BZ    DD10                                                             
         MVC   19+L'CONLSBS(2,R2),RCONSSTI                                      
         MVI   21+L'CONLSBS(R2),C':'                                            
         MVC   22+L'CONLSBS(2,R2),RCONSSTI+2                                    
         DROP  R6                                                               
DD10     DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   DD20                                                             
         USING RCONXEL,R6                                                       
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         TM    RCONCONF,X'40'      CONFIRMED NOW                                
         BZ    *+14                                                             
         MVC   9(L'CONCONF,R2),CONCONF                                          
         B     DD20                                                             
         TM    RCONCONF,X'20'      CONFIRMED PREVIOUSLY                         
         BZ    DD15                                                             
         MVC   9(L'CONPCNF,R2),CONPCNF                                          
         LA    R3,9+L'CONPCNF(R2)                                               
         EDIT  RCONMOD,(3,(R3)),ALIGN=LEFT,ZERO=NOBLANK                         
         B     DD20                                                             
DD15     DS    0H                                                               
         TM    RCONCONF,X'80'      NOT CONFIRMED                                
         BZ    DD20                                                             
         MVC   9(L'CONNCNF,R2),CONNCNF                                          
*                                                                               
DD20     DS    0H                  DISPLAY MOD HISTORY HERE                     
         TM    RCONCONF,X'40'+X'20' CONF NOW OR PREVIOUSLY?                     
         BZ    DD25                NO                                           
         DROP  R6                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'22'                                                     
         BAS   RE,GETEL                                                         
         BNE   DD25                                                             
         ZIC   R1,0(R2)            BUMP TO NEXT DISPLAY LINE                    
         AR    R2,R1                                                            
         LA    R3,9(R2)                                                         
         MVC   0(2,R3),=C'CF'                                                   
         LA    R3,2(R3)                                                         
         LA    R4,3                                                             
         LA    R6,22(R6)           FIRST SET OF MOD HISTORY                     
DD22     DS    0H                                                               
         OC    0(10,R6),0(R6)      ANY MOD HIST?                                
         BZ    DD24                                                             
         MVI   1(R3),C'm'                                                       
         LA    R3,2(R3)                                                         
         EDIT  (1,0(R6)),(3,0(R3)),ALIGN=LEFT,ZERO=NOBLANK                      
         AR    R3,R0                                                            
         MVC   0(2,R3),=C'/v'                                                   
         LA    R3,2(R3)                                                         
         EDIT  (1,1(R6)),(3,0(R3)),ALIGN=LEFT,ZERO=NOBLANK                      
         AR    R3,R0                                                            
         GOTO1 DATCON,DMCB,(2,2(R6)),(11,1(R3))                                 
         LA    R3,10(R3)                                                        
         MVC   0(2,R3),4(R6)                                                    
         MVI   2(R3),C':'                                                       
         MVC   3(2,R3),6(R6)                                                    
         LA    R3,5(R3)                                                         
DD24     DS    0H                                                               
         SH    R6,=H'10'                                                        
         BCT   R4,DD22                                                          
*                                                                               
DD25     DS    0H                                                               
         ZIC   R1,0(R2)            BUMP TO NEXT DISPLAY LINE                    
         AR    R2,R1                                                            
         GOTO1 DISVDATE,DMCB,(R2)  DISPLAY LAST 3 VERSION DATES                 
                                                                                
         LA    R6,RCONREC          DOES THIS CONTRACT HAVE ANY MAKEGOOD         
         MVI   ELCODE,X'21'        OFFER WIP OR SENT?                           
         BAS   RE,GETEL                                                         
         BNE   DD28                                                             
         USING RCONMGEL,R6                                                      
         GOTO1 DISMGWIP,DMCB,(R2)  DISPLAY MAKEGOOD OFFER WIP                   
         B     DD30                                                             
         DROP  R6                                                               
*                                                                               
DD28     DS    0H                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         MVC   9(L'NOMGO,R2),NOMGO                                              
*                                                                               
DD30     DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'15'                                                     
         BAS   RE,GETEL                                                         
         BNE   DD40                                                             
         USING RCONECEL,R6                                                      
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         MVC   9(L'DTLSTEC,R2),DTLSTEC                                          
         GOTO1 DATCON,DMCB,(2,RCONECDT),(5,9+L'DTLSTEC(R2))                     
         MVC   45(L'TIMELBL,R2),TIMELBL                                         
*                                                                               
* PRIOR TO 7/27/96, EC TIME STAMPING WAS STORED EITHER AS PWOS OR HEX.          
* A NEW ROUTINE USING THMS WILL STORE THE TIME STAMP AS PWOS ALWAYS             
* WITH AUTOMATIC DDS TIME ADJUSTED                                              
*                                                                               
         CLC   RCONECDT,=X'C0FA'   SKIP THE FOLLOWING CHECK IF LATER            
         BH    DD35                THAN 7/27/96                                 
         CLI   RCONECTM,X'19'                                                   
         BH    DD33                                                             
         CLI   RCONECTM,X'0F'                                                   
         BH    DD35                                                             
         CLI   RCONECTM,X'0A'                                                   
         BL    DD35                                                             
*                                                                               
DD33     EQU   *                                                                
         EDIT  (1,RCONECTM),(2,HALF),ZERO=NOBLANK                               
         MVC   WORK(2),HALF                                                     
         B     DD39                                                             
*                                                                               
DD35     EQU   *                   DISPLAY TIME HH:MM                           
         GOTO1 HEXOUT,DMCB,RCONECTM,WORK,1,=C'TOG'                              
*                                                                               
DD39     EQU   *                                                                
         MVI   WORK+2,C':'         INSERT SEPARATOR                             
         GOTO1 HEXOUT,DMCB,RCONECTM+1,WORK+3,1,=C'TOG'                          
*                                                                               
         MVC   45+L'TIMELBL(5,R2),WORK                                          
*                                  INSERT TIME INTO SCREEN                      
         OC    RCONECDS,RCONECDS   ANY 'DATE LAST STORED'?                      
         BZ    DD40                NO  - NO 'DATE XFERRED' EITHER               
         ZIC   R1,0(R2)            YES - BUMP TO NEXT LINE                      
         AR    R2,R1                                                            
         MVC   9(L'DTLSTST,R2),DTLSTST                                          
         GOTO1 DATCON,DMCB,(2,RCONECDS),(5,9+L'DTLSTST(R2))                     
         MVC   45(L'TIMELBL,R2),TIMELBL                                         
         MVC   45+L'TIMELBL+1(4,R2),RCONECTS                                    
         MVC   45+L'TIMELBL(2,R2),45+L'TIMELBL+1(R2)                            
         MVI   45+L'TIMELBL+2(R2),C':'                                          
         OC    RCONECDX,RCONECDX   ANY 'DATE LAST XFERRED'?                     
         BZ    DD40                NO  -                                        
         ZIC   R1,0(R2)            YES - BUMP TO NEXT LINE                      
         AR    R2,R1                                                            
         MVC   9(L'DTLSTDX,R2),DTLSTDX                                          
         GOTO1 DATCON,DMCB,(2,RCONECDX),(5,9+L'DTLSTDX(R2))                     
         MVC   45(L'TIMELBL,R2),TIMELBL                                         
         MVC   45+L'TIMELBL+1(4,R2),RCONECTX                                    
         MVC   45+L'TIMELBL(2,R2),45+L'TIMELBL+1(R2)                            
         MVI   45+L'TIMELBL+2(R2),C':'                                          
         DROP  R6                                                               
*                                                                               
DD40     DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'16'        JDS DATE & TIME ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   DD50                                                             
         USING RCONE2EL,R6                                                      
*                                                                               
         OC    RCONE2DS,RCONE2DS   ANY 'DATE LAST STORED'?                      
         BZ    DD50                NO, NO 'DATE SENT' EITHER                    
         ZIC   R1,0(R2)            YES, GO TO NEXT LINE                         
         AR    R2,R1                                                            
         MVC   9(L'DTLSSTJD,R2),DTLSSTJD                                        
         GOTO1 DATCON,DMCB,(2,RCONE2DS),(5,9+L'DTLSSTJD(R2))                    
         MVC   45(L'TIMELBL,R2),TIMELBL                                         
         MVC   45+L'TIMELBL(2,R2),RCONE2TS                                      
         MVI   45+L'TIMELBL+2(R2),C':'                                          
         MVC   45+L'TIMELBL+3(2,R2),RCONE2TS+2                                  
         MVI   45+L'TIMELBL+5(R2),C':'                                          
         MVC   45+L'TIMELBL+6(2,R2),RCONE2TS+4                                  
*                                                                               
         OC    RCONE2DX,RCONE2DX   ANY 'DATE LAST SENT'?                        
         BZ    DD50                NO                                           
         ZIC   R1,0(R2)            YES, BUMP TO NEXT LINE                       
         AR    R2,R1                                                            
         MVC   9(L'DTLSSEJD,R2),DTLSSEJD                                        
         GOTO1 DATCON,DMCB,(2,RCONE2DX),(5,9+L'DTLSSEJD(R2))                    
         MVC   45(L'TIMELBL,R2),TIMELBL                                         
         MVC   45+L'TIMELBL(2,R2),RCONE2TX                                      
         MVI   45+L'TIMELBL+2(R2),C':'                                          
         MVC   45+L'TIMELBL+3(2,R2),RCONE2TX+2                                  
         MVI   45+L'TIMELBL+5(R2),C':'                                          
         MVC   45+L'TIMELBL+6(2,R2),RCONE2TX+4                                  
         DROP  R6                                                               
*                                                                               
DD50     DS    0H                                                               
         ZIC   R1,0(R2)            YES, BUMP TO NEXT LINE                       
         AR    R2,R1                                                            
         LA    R1,HISLAST                                                       
         CR    R2,R1                                                            
         BNL   DDX                                                              
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'        DARE ORDER??                                 
         BAS   RE,GETEL                                                         
         BNE   DDX                                                              
         USING RCONDREL,R6                                                      
         TM    RCONDRFG,X'80'                                                   
         BZ    DDX                                                              
         MVC   9(L'LINKWITH,R2),LINKWITH                                        
         GOTO1 HEXOUT,DMCB,RCONDRLK,32(R2),4,=C'TOG'                            
         DROP  R6                                                               
*                                                                               
DDX      DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* DISPLAY LAST 3 REP AND STA VERSION DATES                                      
**********************************************************************          
DISVDATE NTR1                                                                   
         L     R2,0(R1)                                                         
                                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISVD05                                                          
                                                                                
         USING RCONSEND,R6                                                      
         CLI   RCONSNLN,RCONSN2Q   STATION MULTI-SEND HAS NEWER DSECT           
         BNL   DISVD100              FOR VERSION DATES                          
         CLI   RCONSNLN,RCONSNLQ   ONLY REVISED ELEMENT HAS VERSION             
         BL    DISVD05               DATES                                      
         OC    RCONSRD1(15),RCONSRD1                                            
         BNZ   DISVD10                                                          
                                                                                
DISVD05  MVC   9(37,R2),=C'No previous version date(s) available'               
         B     DISVXIT                                                          
                                                                                
DISVD10  DS    0H                                                               
         ZIC   R4,RCONSRV          GET CURRENT REP VERSION NUMBER               
         LA    R5,RCONSRD3                                                      
         LA    R1,3                                                             
         MVC   9(4,R2),=C'Rep:'                                                 
         OC    RCONSRD1(6),RCONSRD1 SKIP IF NO VERSION DATES                    
         BZ    DISVD40                                                          
                                                                                
DISVD20  DS    0H                                                               
         ST    R1,FULL                                                          
         OC    0(2,R5),0(R5)                                                    
         BZ    *+8                                                              
         S     R4,=F'2'            FIND EARLIEST VERSION DATE SAVED             
         S     R5,=F'2'                                                         
         L     R1,FULL                                                          
         BCT   R1,DISVD20                                                       
                                                                                
         LA    R4,2(R4)                                                         
         LA    R5,RCONSRD1                                                      
                                                                                
         LA    R3,14(R2)                                                        
         LA    R1,3                                                             
                                                                                
DISVD30  DS    0H                                                               
         ST    R1,FULL                                                          
         MVI   0(R3),C'v'                                                       
         EDIT  (R4),(3,1(R3)),ALIGN=LEFT,ZERO=NOBLANK                           
         AR    R3,R0                                                            
         LA    R4,2(R4)            REP VERSION INCREMENT BY 2'S                 
         GOTO1 DATCON,DMCB,(2,(R5)),(5,2(R3))                                   
         LA    R3,12(R3)                                                        
         LA    R5,2(R5)                                                         
         OC    0(2,R5),0(R5)                                                    
         BZ    *+12                                                             
         L     R1,FULL                                                          
         BCT   R1,DISVD30                                                       
*                                                                               
DISVD40  DS    0H                                                               
         ZIC   R1,0(R2)            BUMP TO DISPLAY STA VERSION DATES            
         AR    R2,R1                                                            
         LA    R5,RCONSSD1                                                      
         LA    R1,3                                                             
         MVC   9(4,R2),=C'Sta:'                                                 
         LA    R3,14(R2)                                                        
         OC    RCONSSD1(9),RCONSSD1                                             
         BZ    DISVXIT                                                          
                                                                                
DISVD50  DS    0H                                                               
         ST    R1,FULL                                                          
         MVI   0(R3),C'v'                                                       
         EDIT  (1,2(R5)),(3,1(R3)),ALIGN=LEFT,ZERO=NOBLANK                      
         AR    R3,R0                                                            
         GOTO1 DATCON,DMCB,(2,(R5)),(5,2(R3))                                   
         LA    R3,12(R3)                                                        
         LA    R5,3(R5)                                                         
         OC    0(3,R5),0(R5)                                                    
         BZ    *+12                                                             
         L     R1,FULL                                                          
         BCT   R1,DISVD50                                                       
         B     DISVXIT                                                          
         EJECT                                                                  
*                                                                               
* FOLLOWING DISPLAY VERSION DATE/# FOR NEWER X'20' DSECT THAT WAS               
* RELEASED WITH STATION MULTI-SEND                                              
*                                                                               
DISVD100 DS    0H                                                               
         OC    RCONSRD1(18),RCONSRD1                                            
         BNZ   DISVD110                                                         
         MVC   9(37,R2),=C'No previous version date(s) available'               
         B     DISVXIT                                                          
*                                                                               
* DISPLAY REP VERSION DATES                                                     
*                                                                               
DISVD110 DS    0H                                                               
         LA    R5,RCONSRD1                                                      
         LA    R4,RCONSRV1                                                      
         MVC   9(4,R2),=C'Rep:'                                                 
         B     DISVD140                                                         
*                                                                               
* DISPLAY STA VERSION DATES                                                     
*                                                                               
DISVD120 DS    0H                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
                                                                                
         LA    R5,RCONS2D1                                                      
         LA    R4,RCONS2V1                                                      
         MVC   9(4,R2),=C'Sta:'                                                 
                                                                                
DISVD140 DS    0H                                                               
         OC    0(9,R5),0(R5)                                                    
         BZ    DISVXIT                                                          
         LA    R1,3                                                             
         LA    R3,14(R2)                                                        
                                                                                
DISVD150 DS    0H                                                               
         ST    R1,FULL                                                          
         MVI   0(R3),C'v'                                                       
         EDIT  (1,0(R4)),(3,1(R3)),ALIGN=LEFT,ZERO=NOBLANK                      
         AR    R3,R0                                                            
         GOTO1 DATCON,DMCB,(2,(R5)),(5,2(R3))                                   
         LA    R3,12(R3)                                                        
         LA    R5,2(R5)                                                         
         LA    R4,1(R4)                                                         
         OC    0(2,R5),0(R5)                                                    
         BZ    *+12                                                             
         L     R1,FULL                                                          
         BCT   R1,DISVD150                                                      
*                                                                               
         CLI   RCONSNLN,RCONSN3Q   HAVE FIRST SENT INFO?                        
         BL    DISVXIT                                                          
*                                                                               
         CLI   9(R2),C'R'          REP OR STATION LINE                          
         BNE   DISVD160                                                         
         OC    RCONSRVF(3),RCONSRVF                                             
         BZ    DISVXIT                                                          
         MVC   0(13,R3),=C'First Sent: v'                                       
         EDIT  RCONSRVF,(3,13(R3)),ALIGN=LEFT,ZERO=NOBLANK                      
         AR    R3,R0                                                            
         GOTO1 DATCON,DMCB,(2,RCONSRDF),(5,14(R3))                              
         B     DISVXIT                                                          
*                                                                               
DISVD160 DS    0H                                                               
         OC    RCONSSVF(3),RCONSSVF                                             
         BZ    DISVXIT                                                          
         MVC   0(13,R3),=C'First Sent: v'                                       
         EDIT  RCONSSVF,(3,13(R3)),ALIGN=LEFT,ZERO=NOBLANK                      
         AR    R3,R0                                                            
         GOTO1 DATCON,DMCB,(2,RCONSSDF),(5,14(R3))                              
                                                                                
DISVXIT  DS    0H                                                               
         CLI   9(R2),C'R'          LOOP AGAIN TO DO STA IF JUST                 
         BE    DISVD120            DISPLAYED REP VERSION DATA                   
         XIT1  REGS=(R2)                                                        
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
*                                                                               
**********************************************************************          
DISRER   NTR1                                                                   
         LA    R2,HISLIN1H                                                      
*                                                                               
*&&DO                                                                           
         LA    R6,RCONREC          DOES THIS K HAVE EXT. DESCR. ELEM?           
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   DRX                 NO, EXIT                                     
         USING RCONXEL,R6                                                       
         MVC   BYTE2,RCONSTAT                                                   
         DROP  R6                                                               
*&&                                                                             
*                                                                               
         LA    R6,RCONREC          DOES THIS K HAVE RER ELEM?                   
         MVI   ELCODE,X'2F'                                                     
         BAS   RE,GETEL                                                         
         BNE   DRX                 NO, EXIT                                     
         USING RCONRER,R6                                                       
         TM    RCONFRER,X'01'          CONTRACT EXCLUDED?                       
         BZ    DR20                NO, PRINT UNEXCLUDED INFO                    
*                                                                               
* PRINT RER EXCLUDED INFO                                                       
         OC    RCONURER,RCONURER    UNEXCLUDE DATE?                             
         BZ    DR15                NO, DON'T PRINT                              
         MVC   9(14,R2),=C'RER Unexcluded'                                      
         ZIC   R1,0(R2)            NEXT FIELD                                   
         AR    R2,R1                                                            
         GOTO1 DATCON,DMCB,(2,RCONURER),(10,9(R2))                              
         ZIC   R1,0(R2)            NEXT FIELD                                   
         AR    R2,R1                                                            
*                                                                               
DR15     MVC   9(12,R2),=C'RER Excluded'                                        
         ZIC   R1,0(R2)            NEXT FIELD                                   
         AR    R2,R1                                                            
         GOTO1 DATCON,DMCB,(2,RCONXRER),(10,9(R2))                              
         B     DRX                                                              
                                                                                
DR20     DS    0H                                                               
* PRINT RER UNEXCLUDED INFO                                                     
         MVC   9(12,R2),=C'RER Excluded'                                        
         ZIC   R1,0(R2)            NEXT FIELD                                   
         AR    R2,R1                                                            
         GOTO1 DATCON,DMCB,(2,RCONXRER),(10,9(R2))                              
         ZIC   R1,0(R2)            NEXT FIELD                                   
         AR    R2,R1                                                            
         MVC   9(14,R2),=C'RER Unexcluded'                                      
         ZIC   R1,0(R2)            NEXT FIELD                                   
         AR    R2,R1                                                            
         GOTO1 DATCON,DMCB,(2,RCONURER),(10,9(R2))                              
DRX      DS    0H                                                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* DISPLAY SONNET HISTORY                                                        
**********************************************************************          
DISSON   NTR1                                                                   
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A3'        GET SONNET ELEM                              
         BAS   RE,GETEL                                                         
         BNE   DISSONX                                                          
*                                                                               
         USING RCONSON,R6                                                       
         ZIC   R1,RCONSONL                                                      
         CHI   R1,RCONSOVQ         LENGTH = OVERHEAD?                           
         BE    DISSONX             YES - NEW CONTRACT, EXIT NOW                 
*                                                                               
*----------------*                                                              
* SENT DATE/TIME *                                                              
*----------------*                                                              
         LA    R2,HISLIN1H                                                      
         MVC   8(14,R2),=C'Sent From Rep:'                                      
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   DSON030                                                          
         XC    WORK(20),WORK                                                    
         USING RCONSEND,R6                                                      
         TM    RCONSENF,X'80'      LAST SENT BY REP?                            
         BNO   DSON010                                                          
*                                                                               
         MVC   WORK(2),RCONSRDT                  SAVE REP SENT DATE             
         GOTO1 HEXIN,DMCB,RCONSRTI,WORK+2,6                                     
         DROP  R6                                                               
*                                                                               
DSON010  LA    R6,RCONREC          MAKEGOOD OFFER ?                             
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BE    DSON012             NO                                           
*                                                                               
         OC    WORK(5),WORK                                                     
         BZ    DSON030                                                          
         B     DSON020                                                          
*                                                                               
         USING RCONMGEL,R6                                                      
DSON012  TM    RCONMGFG,X'40'      YES - LAST SENT BY REP?                      
         BZ    DSON020                                                          
*                                                                               
         MVC   WORK+10(2),RCONMGDT     ,,DO OLD ELEMENT DATA                    
         MVC   WORK+12(3),RCONMGTM                                              
*                                                                               
         CLI   RCONMGLN,RCONMGLQ       ,,UNLESS WE HAVE A NEW ELEMENT           
         BE    DSON014                 NO                                       
*                                                                               
         MVC   WORK+10(2),RCONRMDT     YES,NEW ELEM                             
         MVC   WORK+12(3),RCONRMTM                                              
*                                                                               
DSON014  CLC   WORK(2),WORK+10     COMPARE ORDER DATE TO MKGD SENT DATE         
         BH    DSON020                                                          
         BL    DSON018                                                          
*                                                                               
* IF SAME DATE/ COMPARE TIME SENT                                               
*                                                                               
         CLC   WORK+2(3),WORK+12   COMPARE SENT TIMES                           
         BH    DSON020                                                          
*                                                                               
DSON018  MVC   WORK(2),WORK+10     MKGD ELEM GETS PREFERENCE                    
         MVC   WORK+2(3),WORK+12                                                
         DROP  R6                                                               
*                                                                               
DSON020  DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,WORK),(7,15+8(R2))          DATE                  
         GOTO1 HEXOUT,DMCB,WORK+2,WORK+10,3,0                                   
         MVC   7+15+8(2,R2),WORK+10                                             
         MVI   9+15+8(R2),C':'                                                  
         MVC   10+15+8(2,R2),WORK+12                                            
*                                                                               
DSON030  DS    0H                                                               
         ZIC   RE,0(R2)            NEXT FIELD                                   
         AR    R2,RE                                                            
*                                                                               
*--------------*                                                                
* SENT HISTORY *                                                                
*--------------*                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A3'        GET SONNET ELEM                              
         BAS   RE,GETEL                                                         
         BNE   DISSONX                                                          
*                                                                               
         USING RCONSON,R6                                                       
         ZIC   R5,RCONSONL                                                      
         CHI   R5,RCONSOVQ         LENGTH = OVERHEAD?                           
         BE    DISSONX             YES - NEW CONTRACT, EXIT NOW                 
*                                                                               
         AHI   R5,-RCONSOVQ        SUBTRACT OVERHEAD TO GET LENGTH OF           
         SR    R4,R4                 MINI ELEMENT LIST                          
         D     R4,=A(L'RCONSONM)   DIV BY LENGTH OF MINI ELEM                   
*                                                                               
         MVC   8(08,R2),=C'History:'                                            
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
*                                                                               
         LA    R3,RCONSONM         POINT TO MINI ELEMENT LIST                   
         CHI   R5,6                ONLY SHOW SIX                                
         BNH   DSON032                                                          
*                                                                               
         AHI   R5,-6               SKIP THE EXTRA ENTRIES                       
         MHI   R5,L'RCONSONM                                                    
         LA    R3,R5(R3)                                                        
         LA    R5,6                                                             
*                                                                               
DSON032  OC    0(3,R3),0(R3)       ANY MEMBER BOXID ?                           
         BZ    DSON040             NO                                           
*                                                                               
         MVC   16(3,R2),0(R3)                                                   
*                                                                               
         OC    3(2,R3),3(R3)       DATE?                                        
         BNZ   DSON034                                                          
*                                                                               
**       MVC   26(13,R2),=C'Current Boxid'                                      
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         B     DSON040                      AND THIS IS LAST ID                 
*                                                                               
DSON034  DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,3(R3)),(7,20(R2))                                 
         CLI   5(R3),C'Y'          APPROVED?                                    
         BNE   *+10                                                             
         MVC   26(8,R2),=C'APPROVED'                                            
*                                                                               
         LA    R3,L'RCONSONM(R3)   BUMP TO NEXT MEMBER                          
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         BCT   R5,DSON032                                                       
         DC    H'0'                SHOULD NEVER GET HERE                        
*                                                                               
DSON040  DS    0H                                                               
*                                                                               
*--------------*                                                                
* LAST COMMENT *                                                                
*--------------*                                                                
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
*                                                                               
         MVC   8(13,R2),=C'Last Comment:'                                       
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A5'        GET SONNET COMMENTS                          
         USING RCONSCEL,R6                                                      
         BAS   RE,GETEL                                                         
         BNE   DSON050                                                          
*                                                                               
DSON042  DS    0H                                                               
         ZIC   RE,1(R6)            ELEMENT LENGTH                               
         AHI   RE,-(RCONSCOV+1)    SUBTRACT OVERHEAD  +1 FOR EX                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   21(0,R2),RCONSCCM   COPY COMMENT                                 
*                                                                               
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    DSON042                                                          
         DROP  R6                                                               
*                                                                               
DSON050  DS    0H                                                               
*                                                                               
DISSONX  DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* DISPLAY MAKEGOOD OFFER STATUS                                                 
**********************************************************************          
DISMGWIP NTR1                                                                   
         L     R2,0(R1)                                                         
                                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISMGX                                                           
                                                                                
         USING RCONMGEL,R6                                                      
*                                                                               
         TM    RCONMGFG,X'80'      WIP OR SENT?                                 
         BZ    DISMG10                                                          
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         MVC   9(L'MGOWIP,R2),MGOWIP                                            
*                                                                               
DISMG10  DS    0H                  SENT BY REP OR STA?                          
         CLI   RCONMGLN,RCONXMGQ                                                
         BE    DISMG50                                                          
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         MVC   9(L'MGOSENT,R2),MGOSENT                                          
         TM    RCONMGFG,X'40'                                                   
         BNZ   DISMG20                                                          
         MVC   32(3,R2),=C'sta'                                                 
*                                                                               
DISMG20  DS    0H                                                               
         OC    RCONMGDT,RCONMGDT                                                
         BZ    DISMGX                                                           
         GOTO1 DATCON,DMCB,(2,RCONMGDT),(5,10+L'MGOSENT(R2))                    
         GOTO1 HEXOUT,DMCB,RCONMGTM,20+L'MGOSENT(R2),1,=C'TOG'                  
         MVI   22+L'MGOSENT(R2),C':'                                            
         GOTO1 HEXOUT,DMCB,RCONMGTM+1,23+L'MGOSENT(R2),1,=C'TOG'                
         B     DISMGX                                                           
*                                                                               
* NEW DISPLAY FOR EXPANDED X'21' ELEMENT                                        
*                                                                               
DISMG50  DS    0H                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         MVC   9(L'MGOSENT,R2),MGOSENT                                          
         OC    RCONRMDT,RCONRMDT                                                
         BZ    DISMG60                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(2,RCONRMDT),(5,10+L'MGOSENT(R2))                    
         GOTO1 HEXOUT,DMCB,RCONRMTM,20+L'MGOSENT(R2),1,=C'TOG'                  
         MVI   22+L'MGOSENT(R2),C':'                                            
         GOTO1 HEXOUT,DMCB,RCONRMTM+1,23+L'MGOSENT(R2),1,=C'TOG'                
*                                                                               
DISMG60  DS    0H                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         MVC   9(L'MGOSENT,R2),MGOSENT                                          
         MVC   3+L'MGOSENT(3,R2),=C'sta'                                        
         OC    RCONMGDT,RCONMGDT                                                
         BZ    DISMGX                                                           
         GOTO1 DATCON,DMCB,(2,RCONMGDT),(5,10+L'MGOSENT(R2))                    
         GOTO1 HEXOUT,DMCB,RCONMGTM,20+L'MGOSENT(R2),1,=C'TOG'                  
         MVI   22+L'MGOSENT(R2),C':'                                            
         GOTO1 HEXOUT,DMCB,RCONMGTM+1,23+L'MGOSENT(R2),1,=C'TOG'                
*                                                                               
DISMGX   DS    0H                                                               
         XIT1  REGS=(R2)                                                        
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* DISPLAY DARE DATES                                                            
**********************************************************************          
DISDARE  NTR1                                                                   
         LA    R2,HISLIN1H                                                      
*                                                                               
* GET X'1D' DARE ELEM FOR DARE DATES                                            
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'                                                     
         BAS   RE,GETEL                                                         
         BNE   DDAR0080                                                         
         USING RCONDREL,R6                                                      
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         TM    RCONDRFG,X'80'      LINKED WITH AGENCY ORDER?                    
         BNO   DDAR0010            NO                                           
         MVC   9(L'LINKWITH,R2),LINKWITH                                        
         PRINT GEN                                                              
         GOTO1 HEXOUT,DMCB,RCONDRLK,32(R2),4,=C'TOG'                            
         PRINT NOGEN                                                            
         B     DDAR0020                                                         
DDAR0010 EQU   *                                                                
         TM    RCONDRFG,X'01'      TKO DARE??                                   
         BZ    DDAR0015                                                         
         MVC   9(L'TKODWITH,R2),TKODWITH                                        
         GOTO1 HEXOUT,DMCB,RCONDRLK,44(R2),4,=C'TOG'                            
         B     DDAR0020                                                         
*                                                                               
DDAR0015 EQU   *                                                                
         MVC   9(L'NOTLINKD,R2),NOTLINKD                                        
DDAR0020 EQU   *                                                                
         ZIC   R1,0(R2)            BUMP TO NEXT LINE                            
         AR    R2,R1                                                            
         CLC   RCONDRDA(4),RCONDRDR                                             
*                                  APPROVED MORE RECENTLY?                      
         BH    DDAR0030            NO  - REJECTED                               
         OC    RCONDRDA(4),RCONDRDA                                             
*                                  ANY VALUE HERE?                              
         BZ    DDAR0030            NO  - CHECK REJECTED                         
         MVC   9(L'DRLSTAP,R2),DRLSTAP                                          
         GOTO1 DATCON,DMCB,(2,RCONDRDA),(11,9+L'DRLSTAP(R2))                    
         MVC   50+L'TIMELBL(5,R2),TIMELBL                                       
         LA    R3,55+L'TIMELBL+2(R2)                                            
         GOTO1 HEXOUT,DMCB,RCONDRTA,(R3),2,=C'TOG'                              
         ZIC   R1,0(R2)            BUMP TO NEXT LINE                            
         AR    R2,R1                                                            
         MVC   9(L'CONLSBS,R2),CONLSBS                                          
         GOTO1 DATCON,DMCB,(2,RCONSSDT),(5,9+L'CONLSBS(R2))                     
DDAR0030 EQU   *                                                                
         OC    RCONDRDR(4),RCONDRDR                                             
*                                  ANY VALUE HERE?                              
         BZ    DDAR0040            NO  - CHECK APPROVED                         
         MVC   9(L'DRLSTRJ,R2),DRLSTRJ                                          
         GOTO1 DATCON,DMCB,(2,RCONDRDR),(11,9+L'DRLSTRJ(R2))                    
         MVC   50+L'TIMELBL(5,R2),TIMELBL                                       
         LA    R3,55+L'TIMELBL+2(R2)                                            
         GOTO1 HEXOUT,DMCB,RCONDRTR,(R3),2,=C'TOG'                              
         ZIC   R1,0(R2)            BUMP TO NEXT LINE                            
         AR    R2,R1                                                            
DDAR0040 EQU   *                                                                
         CLC   RCONDRDA(4),RCONDRDR                                             
*                                  APPROVED MORE RECENTLY?                      
         BNH   DDAR0050            YES - ALREADY DISPLAYED                      
         OC    RCONDRDA(4),RCONDRDA                                             
*                                  ANY VALUE HERE?                              
         BZ    DDAR0050            NO  -                                        
         MVC   9(L'DRLSTAP,R2),DRLSTAP                                          
         GOTO1 DATCON,DMCB,(2,RCONDRDA),(11,9+L'DRLSTAP(R2))                    
         MVC   50+L'TIMELBL(5,R2),TIMELBL                                       
         LA    R3,55+L'TIMELBL+2(R2)                                            
         GOTO1 HEXOUT,DMCB,RCONDRTA,(R3),2,=C'TOG'                              
*                                                                               
DDAR0050 EQU   *                   CHECK FOR DELIVERY NOTIFICATION              
         OC    RCONDRDD(4),RCONDRDD                                             
*                                  ANY VALUE HERE?                              
         BZ    DDAR0090            NO  -                                        
         ZIC   R1,0(R2)            BUMP TO NEXT LINE                            
         AR    R2,R1                                                            
         MVC   9(L'DRDELDT,R2),DRDELDT                                          
         GOTO1 DATCON,DMCB,(2,RCONDRDD),(11,9+L'DRDELDT(R2))                    
         MVC   50+L'TIMELBL(5,R2),TIMELBL                                       
         LA    R3,55+L'TIMELBL+2(R2)                                            
         GOTO1 HEXOUT,DMCB,RCONDRTD,(R3),2,=C'TOG'                              
         B     DDAR0090                                                         
*                                                                               
DDAR0080 DS    0H                                                               
         MVC   9(L'NOTDARE,R2),NOTDARE                                          
*                                                                               
DDAR0090 DS    0H                                                               
         TM    RCONDRFG,X'04'      EDI CONVERTED?                               
         BZ    DDAR0095                                                         
         ZIC   R1,0(R2)            BUMP TO NEXT LINE                            
         AR    R2,R1                                                            
         MVC   9(L'EDICONV,R2),EDICONV                                          
*                                                                               
DDAR0095 DS    0H                                                               
         CLI   RCONDRLN,RCONDL2Q                                                
         BL    DDARX                                                            
         TM    RCONDRF2,X'08'      REMOVED??                                    
         BZ    DDAR0100                                                         
         ZIC   R1,0(R2)            BUMP TO NEXT LINE                            
         AR    R2,R1                                                            
         MVC   9(L'DAREREM,R2),DAREREM                                          
         B     DDARX                                                            
*                                                                               
DDAR0100 DS    0H                                                               
         TM    RCONDRF2,X'04'      MANUAL CHANGES??                             
         BZ    DDARX                                                            
         ZIC   R1,0(R2)            BUMP TO NEXT LINE                            
         AR    R2,R1                                                            
         MVC   9(L'DAREMAN,R2),DAREMAN                                          
*                                                                               
DDARX    DS    0H                                                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* DISPLAY CONVERSION INFORMATION                                                
**********************************************************************          
DISCONV  NTR1                                                                   
         LA    R2,HISLIN1H                                                      
         TM    RCONMODR+1,X'10'    CONVERTED ORDER?                             
         BO    DCON0020            YES                                          
         MVC   9(22,R2),=C'NOT CONVERTED CONTRACT'                              
         B     DCON0200            EXIT                                         
DCON0020 EQU   *                                                                
*                                                                               
* GET X'76' CONVERSION CATEGORY ELEMENT                                         
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'76'                                                     
         BAS   RE,GETEL                                                         
         BNE   DCON0040                                                         
         MVC   9(14,R2),=C'CATEGORY CODE:'                                      
         MVC   24(4,R2),2(R6)                                                   
         ZIC   RF,0(R2)            BUMP TO NEXT LINE                            
         AR    R2,RF                                                            
DCON0040 EQU   *                                                                
*                                                                               
* GET X'77' CONVERSION CONTROL ELEMENT                                          
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'77'                                                     
         BAS   RE,GETEL                                                         
         BNE   DCON0200                                                         
         MVC   9(08,R2),=C'CONTROL:'                                            
         MVI   18(R2),C'('                                                      
         MVI   22(R2),C')'                                                      
         GOTO1 HEXOUT,DMCB,2(R6),19(R2),1,=C'TOG'                               
         LA    R3,25(R2)           SET A(FIRST INDICATOR)                       
         TM    2(R6),X'80'         CANCELLED?                                   
         BNO   DCON0045            no                                           
         MVC   0(09,R3),=C'CANCELLED'                                           
         LA    R3,12(R3)           BUMP TO NEXT SLOT                            
DCON0045 EQU   *                                                                
         TM    2(R6),X'40'         CANCELLED?                                   
         BNO   DCON0050            no                                           
         MVC   0(09,R3),=C'PENDING  '                                           
         LA    R3,12(R3)           BUMP TO NEXT SLOT                            
DCON0050 EQU   *                                                                
         TM    2(R6),X'20'         UNCONFIRMED?                                 
         BNO   DCON0055            no                                           
         MVC   0(11,R3),=C'UNCONFIRMED'                                         
         LA    R3,13(R3)           BUMP TO NEXT SLOT                            
DCON0055 EQU   *                                                                
         ZIC   RF,0(R2)            BUMP TO NEXT LINE                            
         AR    R2,RF                                                            
         MVC   09(04,R2),=C'MOD:'                                               
         MVC   WORK+20(1),3(R6)                                                 
         EDIT  (1,WORK+20),(2,19(R2)),FILL=0                                    
         ZIC   RF,0(R2)            BUMP TO NEXT LINE                            
         AR    R2,RF                                                            
*                                                                               
         MVC   9(08,R2),=C'CURRENT:'                                            
         LA    R3,19(R2)           SET A(FIRST SLOT ON LINE)                    
         GOTO1 FLAGDISP,DMCB,4(R6) DISPLAY FLAGS                                
         L     R2,SAVENXR2         RESET LINE IN CASE BUMPED                    
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               BUMP TO NEXT LINE                            
         CLI   5(R6),0             ANY PRIOR VALUES?                            
         BZ    DCON0080            NO                                           
         MVC   9(08,R2),=C'PRIOR  :'                                            
         LA    R3,19(R2)           SET A(FIRST SLOT ON LINE)                    
         GOTO1 FLAGDISP,DMCB,5(R6) DISPLAY FLAGS                                
         L     R2,SAVENXR2         RESET LINE IN CASE BUMPED                    
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               BUMP TO NEXT LINE                            
DCON0080 EQU   *                                                                
         CLI   6(R6),0             ANY LAST CONFIRM VALUES?                     
         BZ    DCON0200            NO                                           
         MVC   9(08,R2),=C'LAST CF:'                                            
         LA    R3,19(R2)           SET A(FIRST SLOT ON LINE)                    
         GOTO1 FLAGDISP,DMCB,6(R6) DISPLAY FLAGS                                
         L     R2,SAVENXR2         RESET LINE IN CASE BUMPED                    
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               BUMP TO NEXT LINE                            
DCON0200 DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*   FLAGDISP:  INTERROGATE BITS, DISPLAY PERTINENT INFO                         
*        R3 CONTAINS LINE ADDRESS                                               
*        0(R1) CONTAINS BYTE WITH FLAG                                          
*                                                                               
FLAGCTR  DS    XL1                                                              
SAVENXR2 DS    F                                                                
*                                                                               
FLAGDISP NTR1                                                                   
         XC    FLAGCTR,FLAGCTR     CLEAR COUNTER                                
         ST    R2,SAVENXR2         SAVE A(NEXT R2)                              
         L     R6,0(R1)            SET A(FLAG BEING DISPLAYED)                  
         TM    0(R6),X'80'         UNAPPROVED CHGS?                             
         BNO   FLDP0020            NO                                           
         MVC   0(10,R3),=C'UNAPP CHGS'                                          
         LA    R3,15(R3)           BUMP DOWN LINE                               
         ZIC   RF,FLAGCTR                                                       
         LA    RF,1(RF)                                                         
         STC   RF,FLAGCTR                                                       
FLDP0020 EQU   *                                                                
         TM    0(R6),X'40'         REP TR'D?                                    
         BNO   FLDP0040            NO                                           
         MVC   0(10,R3),=C'REP TR-D  '                                          
         LA    R3,15(R3)           BUMP DOWN LINE                               
         ZIC   RF,FLAGCTR                                                       
         LA    RF,1(RF)                                                         
         STC   RF,FLAGCTR                                                       
FLDP0040 EQU   *                                                                
         TM    0(R6),X'20'         REP TR'D W/REPS?                             
         BNO   FLDP0060            NO                                           
         MVC   0(14,R3),=C'REP TR-D W/RPT'                                      
         LA    R3,18(R3)           BUMP DOWN LINE                               
         ZIC   RF,FLAGCTR                                                       
         LA    RF,1(RF)                                                         
         STC   RF,FLAGCTR                                                       
FLDP0060 EQU   *                                                                
         CLI   FLAGCTR,3           THREE DISPLAYS?                              
         BL    FLDP0070            YES                                          
         MVI   FLAGCTR,0           CLEAR COUNTER                                
         ZIC   RF,0(R2)            BUMP TO NEXT LINE                            
         AR    R2,RF                                                            
         ST    R2,SAVENXR2         SAVE A(NEXT LINE)                            
         LA    R3,19(R2)           RESET A(NEXT ENTRY)                          
FLDP0070 EQU   *                                                                
         TM    0(R6),X'10'         REP TR'D W/REPS?                             
         BNO   FLDP0080            NO                                           
         MVC   0(10,R3),=C'STA TR-D  '                                          
         LA    R3,15(R3)           BUMP DOWN LINE                               
         ZIC   RF,FLAGCTR                                                       
         LA    RF,1(RF)                                                         
         STC   RF,FLAGCTR                                                       
FLDP0080 EQU   *                                                                
         CLI   FLAGCTR,3           THREE DISPLAYS?                              
         BL    FLDP0090            YES                                          
         MVI   FLAGCTR,0           CLEAR COUNTER                                
         ZIC   RF,0(R2)            BUMP TO NEXT LINE                            
         AR    R2,RF                                                            
         ST    R2,SAVENXR2         SAVE A(NEXT LINE)                            
         LA    R3,19(R2)           RESET A(NEXT ENTRY)                          
FLDP0090 EQU   *                                                                
         TM    0(R6),X'08'         STA TR'D W/REPS?                             
         BNO   FLDP0100            NO                                           
         MVC   0(14,R3),=C'STA TR-D W/RPT'                                      
         LA    R3,20(R3)           BUMP DOWN LINE                               
FLDP0100 EQU   *                                                                
         CLI   FLAGCTR,3           THREE DISPLAYS?                              
         BL    FLDP0110            YES                                          
         MVI   FLAGCTR,0           CLEAR COUNTER                                
         ZIC   RF,0(R2)            BUMP TO NEXT LINE                            
         AR    R2,RF                                                            
         ST    R2,SAVENXR2         SAVE A(NEXT LINE)                            
         LA    R3,19(R2)           RESET A(NEXT ENTRY)                          
FLDP0110 EQU   *                                                                
         TM    0(R6),X'04'         HDLN MOD?                                    
         BNO   FLDP0120            NO                                           
         MVC   0(10,R3),=C'HDLN MOD  '                                          
         LA    R3,15(R3)           BUMP DOWN LINE                               
FLDP0120 EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* DISPLAY BUCKETS (ACTUAL AND INVOICED)                                         
* R3 = ACTUAL DOLLARS        R4 = INVOICED DOLLARS                              
DISDOLS  NTR1                                                                   
         LA    R3,RCONELEM                                                      
         SR    R0,R0                                                            
* GET FIRST X'03' ELEM IN R3                                                    
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0             NO 03 OR 04 ELEMS                            
         BE    XIT                                                              
         CLI   0(R3),X'03'                                                      
         BL    *-18                                                             
* GET FIRST X'04' ELEM IN R4                                                    
         LR    R4,R3                                                            
         CLI   0(R4),X'04'                                                      
         BNL   DDL10                                                            
*                                                                               
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),X'04'                                                      
         BNL   DDL10                                                            
         CLI   0(R4),0                                                          
         BNE   *-18                                                             
*                                                                               
* DISPLAY BUCKETS                                                               
DDL10    DS    0H                                                               
         LA    R2,HISLIN1H                                                      
         LA    R5,13                                                            
         CLC   CONBNUM(5),=C'$NEXT'                                             
         BNE   DDL20                                                            
         SR    R0,R0                                                            
*                                                                               
* FIND NEXT BUCS TO BE DISPLAYED                                                
         CLC   TWAACTBC,2(R3)                                                   
         BE    *+18                                                             
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BNE   *-20                                                             
*                                                                               
         CLC   TWAINVBC,2(R4)                                                   
         BE    *+18                                                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BNE   *-20                                                             
*                                                                               
DDL20    CLI   0(R3),X'03'         MAKE SURE STILL 03 ELEM                      
         BNE   DDL30                                                            
         GOTO1 DATCON,DMCB,(3,2(R3)),(6,9(R2))                                  
         GOTO1 DATCON,DMCB,(2,4(R3)),(5,16(R2))                                 
         EDIT  (4,6(R3)),(12,25(R2)),2,COMMAS=YES,FLOAT=$,MINUS=YES             
*                                                                               
DDL30    CLI   0(R4),X'04'         MAKE SURE STILL 04 ELEM                      
         BNE   DDL40                                                            
         GOTO1 DATCON,DMCB,(3,2(R4)),(6,49(R2))                                 
         GOTO1 DATCON,DMCB,(2,4(R4)),(5,56(R2))                                 
         EDIT  (4,6(R4)),(12,65(R2)),2,COMMAS=YES,FLOAT=$,MINUS=YES             
*                                                                               
DDL40    DS    0H                                                               
         ZIC   R0,1(R3)            NEXT 03                                      
         AR    R3,R0                                                            
         IC    R0,1(R4)            NEXT 04                                      
         AR    R4,R0                                                            
         IC    R0,0(R2)            NEXT SCREEN LINE                             
         AR    R2,R0                                                            
         BCT   R5,DDL20                                                         
*                                                                               
         XC    TWAACTBC,TWAACTBC                                                
         XC    TWAINVBC,TWAINVBC                                                
         MVC   CONBNUM(7),=C'$      '                                           
         OI    CONBNUMH+6,X'80'    XMIT FIELD                                   
         CLI   0(R3),3                                                          
         BNE   *+16                                                             
         MVC   TWAACTBC,2(R3)                                                   
         MVC   CONBNUM(7),=C'$NEXT  '                                           
         CLI   0(R4),4                                                          
         BNE   *+16                                                             
         MVC   TWAINVBC,2(R4)                                                   
         MVC   CONBNUM(7),=C'$NEXT  '                                           
*                                                                               
         OI    CONBNUMH+1,X'01'    FLD MODIFIED                                 
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* DISPLAY BUCKETS (FORECAST)                                                    
* R3 = FORECAST DOLLARS                                                         
DISFOR$$ NTR1                                                                   
         LA    R3,RCONELEM                                                      
         SR    R0,R0                                                            
* GET FIRST X'23' ELEM IN R3                                                    
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0             NO 23                                        
         BE    XIT                                                              
         CLI   0(R3),X'23'                                                      
         BL    *-18                                                             
* GET FIRST X'04' ELEM IN R4                                                    
         LR    R4,R3                                                            
         CLI   0(R4),X'04'                                                      
         BNL   DDF10                                                            
*                                                                               
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),X'04'                                                      
         BNL   DDF10                                                            
         CLI   0(R4),0                                                          
         BNE   *-18                                                             
*                                                                               
* DISPLAY BUCKETS                                                               
DDF10    DS    0H                                                               
         LA    R2,HISLIN1H                                                      
         LA    R5,13                                                            
         CLC   CONBNUM(6),=C'$NEXT$'                                            
         BNE   DDF20                                                            
         SR    R0,R0                                                            
*                                                                               
* FIND NEXT BUCS TO BE DISPLAYED                                                
         CLC   TWAACTBC,2(R3)                                                   
         BE    *+18                                                             
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BNE   *-20                                                             
*                                                                               
         CLC   TWAINVBC,2(R4)                                                   
         BE    *+18                                                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BNE   *-20                                                             
*                                                                               
DDF20    CLI   0(R3),X'23'         MAKE SURE STILL 23 ELEM                      
         BNE   DDF30                                                            
         GOTO1 DATCON,DMCB,(3,2(R3)),(6,9(R2))                                  
         GOTO1 DATCON,DMCB,(2,4(R3)),(5,16(R2))                                 
         EDIT  (4,6(R3)),(12,25(R2)),2,COMMAS=YES,FLOAT=$,MINUS=YES             
*                                                                               
DDF30    CLI   0(R4),X'04'         MAKE SURE STILL 04 ELEM                      
*****>   BNE   DDF40                                                            
         B     DDF40               NO DISPLAY OF X'04' ELEMENT                  
*                                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(3,2(R4)),(6,49(R2))                                 
         GOTO1 DATCON,DMCB,(2,4(R4)),(5,56(R2))                                 
         EDIT  (4,6(R4)),(12,65(R2)),2,COMMAS=YES,FLOAT=$,MINUS=YES             
*                                                                               
DDF40    DS    0H                                                               
         ZIC   R0,1(R3)            NEXT 23                                      
         AR    R3,R0                                                            
         IC    R0,1(R4)            NEXT 04                                      
         AR    R4,R0                                                            
         IC    R0,0(R2)            NEXT SCREEN LINE                             
         AR    R2,R0                                                            
         BCT   R5,DDF20                                                         
*                                                                               
         XC    TWAACTBC,TWAACTBC                                                
         XC    TWAINVBC,TWAINVBC                                                
****>>   MVC   CONBNUM(7),=C'$$     '                                           
         MVC   CONBNUM(7),MYSPACES                                              
         MVC   CONBNUM(3),TWAHIST  RELOAD ORIGINAL VALUE                        
         OI    CONBNUMH+6,X'80'    XMIT FIELD                                   
         CLI   0(R3),3                                                          
         BNE   *+16                                                             
         MVC   TWAACTBC,2(R3)                                                   
         MVC   CONBNUM(7),=C'$NEXT$ '                                           
         CLI   0(R4),4                                                          
         BNE   *+16                                                             
         MVC   TWAINVBC,2(R4)                                                   
         MVC   CONBNUM(7),=C'$NEXT$ '                                           
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* MORE CONTRACT INFORMATION                                                     
**********************************************************************          
DISINFO  NTR1                                                                   
         LA    R2,HISLIN1H                                                      
         LA    R3,4                                                             
*                                                                               
DISF010  ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R3,DISF010                                                       
*                                                                               
* CLEAR DISPLAY FIELDS                                                          
         LR    R1,R2                                                            
         LA    R0,9                                                             
         SR    RE,RE                                                            
DISF015  XC    8(79,R1),8(R1)                                                   
         IC    RE,0(R1)                                                         
         AR    R1,RE                                                            
         BCT   R0,DISF015                                                       
*                                                                               
*        MVC   9(L'CONMRES,R2),CONMRES  LAST MOD REASON                         
         LA    R3,9(R2)                                                         
*                                                                               
         TM    RCONMODR,X'80'      K HEADLINE CHANGE                            
         BZ    DISF020                                                          
         MVC   0(L'CONMRES1,R3),CONMRES1                                        
         MVI   L'CONMRES1(R3),C','                                              
         LA    R3,2+L'CONMRES1(R3)                                              
*                                                                               
DISF020  DS    0H                                                               
         TM    RCONMODR,X'40'      BUY CHANGE                                   
         BZ    DISF030                                                          
         MVI   BYTE,L'CONMRES2+1                                                
         BAS   RE,LINEWRAP                                                      
         MVC   0(L'CONMRES2,R3),CONMRES2                                        
         MVI   L'CONMRES2(R3),C','                                              
         LA    R3,2+L'CONMRES2(R3)                                              
*                                                                               
DISF030  DS    0H                                                               
         TM    RCONMODR,X'10'      NOT PENDING                                  
         BZ    DISF032                                                          
         MVI   BYTE,L'CONMRES7+1                                                
         BAS   RE,LINEWRAP                                                      
         MVC   0(L'CONMRES7,R3),CONMRES7                                        
         MVI   L'CONMRES7(R3),C','                                              
         LA    R3,2+L'CONMRES7(R3)                                              
*                                                                               
DISF032  DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISF033                                                          
         USING RCONXEL,R6                                                       
         TM    RCONSTAT,X'01'      K CONFIRMED BY BATCH CONFIRM                 
         BZ    DISF033                                                          
         DROP  R6                                                               
         MVI   BYTE,L'CONMRESD+1                                                
         BAS   RE,LINEWRAP                                                      
         MVC   0(L'CONMRESD,R3),CONMRESD                                        
         MVI   L'CONMRESD(R3),C','                                              
         LA    R3,2+L'CONMRESD(R3)                                              
*                                                                               
DISF033  DS    0H                  K CLOSEOUT BY RE16 REPORT                    
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'2B'        CLOSEOUT HIST ELEM                           
         BAS   RE,GETEL                                                         
         BNE   DISF035                                                          
         MVI   BYTE,L'CONMRESE+9                                                
         BAS   RE,LINEWRAP                                                      
         USING RCON16EL,R6                                                      
         MVC   0(L'CONMRESE,R3),CONMRESE                                        
         LA    R3,1+L'CONMRESE(R3)                                              
         GOTO1 DATCON,DMCB,(2,RCON16DT),(11,0(R3))                              
         MVI   8(R3),C','                                                       
         LA    R3,10(R3)                                                        
         DROP  R6                                                               
*                                                                               
DISF035  DS    0H                                                               
         TM    RCONMODR,X'04'      K ADDED BY BUYCOPY                           
         BZ    DISF038                                                          
         MVI   BYTE,L'CONMRES9+1                                                
         BAS   RE,LINEWRAP                                                      
         MVC   0(L'CONMRES9,R3),CONMRES9                                        
         MVI   L'CONMRES9(R3),C','                                              
         LA    R3,2+L'CONMRES9(R3)                                              
*                                                                               
DISF038  DS    0H                                                               
*&&DO                                                                           
         TM    RCONMODR,X'20'      RESET/BUYLINE 1 ADDED                        
         BZ    DISF040                                                          
         MVI   BYTE,L'CONMRES3+1                                                
         BAS   RE,LINEWRAP                                                      
         MVC   0(L'CONMRES3,R3),CONMRES3                                        
         MVI   L'CONMRES3(R3),C','                                              
         LA    R3,2+L'CONMRES3(R3)                                              
*&&                                                                             
*                                                                               
DISF040  DS    0H                                                               
         TM    RCONMODR+1,X'80'    ACE CONTRACT                                 
         BZ    DISF050                                                          
         MVI   BYTE,L'CONMRES4+1                                                
         BAS   RE,LINEWRAP                                                      
         MVC   0(L'CONMRES4,R3),CONMRES4                                        
         MVI   L'CONMRES4(R3),C','                                              
         LA    R3,2+L'CONMRES4(R3)                                              
*                                                                               
DISF050  DS    0H                                                               
         TM    RCONMODR+1,X'40'    GRAPHNET CONTRACT                            
         BZ    DISF060                                                          
         MVI   BYTE,L'CONMRES5+1                                                
         BAS   RE,LINEWRAP                                                      
         MVC   0(L'CONMRES5,R3),CONMRES5                                        
         MVI   L'CONMRES5(R3),C','                                              
         LA    R3,2+L'CONMRES5(R3)                                              
*                                                                               
DISF060  DS    0H                                                               
         TM    RCONMODR+1,X'20'    MON DATA ADDED                               
         BZ    DISF065                                                          
         MVI   BYTE,L'CONMRES6+1                                                
         BAS   RE,LINEWRAP                                                      
         MVC   0(L'CONMRES6,R3),CONMRES6                                        
         MVI   L'CONMRES6(R3),C','                                              
         LA    R3,2+L'CONMRES6(R3)                                              
*                                                                               
DISF065  DS    0H                                                               
         TM    RCONMODR+1,X'10'    KATZ CONVERTED CONTRACT                      
         BZ    DISF067                                                          
         MVI   BYTE,L'CONMRES8+1                                                
         BAS   RE,LINEWRAP                                                      
         MVC   0(L'CONMRES8,R3),CONMRES8                                        
         MVI   L'CONMRES8(R3),C','                                              
         LA    R3,2+L'CONMRES8(R3)                                              
*                                                                               
DISF067  DS    0H                                                               
         TM    TWASTREO,X'80'      IS STEREO ON?                                
         BNO   DISF070             NO                                           
         LA    R6,RCONREC          YES                                          
         MVI   ELCODE,X'10'        CHECK FOR BOP ELEMENT                        
         BAS   RE,GETEL            IS IT HERE?                                  
         BNE   DISF070             NO                                           
         MVI   BYTE,L'CONMRESB+1                                                
         BAS   RE,LINEWRAP                                                      
         MVC   0(L'CONMRESB,R3),CONMRESB   YES, TELL STEREO BOP ENTERED         
         MVI   L'CONMRESB(R3),C','                                              
         LA    R3,2+L'CONMRESB(R3)                                              
*                                                                               
DISF070  DS    0H                                                               
         CLI   TWAACCS,C'$'        STATION?                                     
         BE    DISF071                                                          
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISF071                                                          
         USING RCONXEL,R6                                                       
         CLI   RCONCFX#,0                                                       
         BE    DISF071                                                          
         MVI   BYTE,L'CONMRESA+4                                                
         BAS   RE,LINEWRAP                                                      
         MVC   0(L'CONMRESA,R3),CONMRESA                                        
         LA    R3,L'CONMRESA(R3)                                                
         CLI   RCONCFX#,X'FF'                                                   
         BNE   DISF070A                                                         
         MVC   0(2,R3),=C'0,'                                                   
         LA    R3,3(R3)                                                         
         B     DISF071                                                          
DISF070A EDIT  RCONCFX#,(3,0(R3)),ALIGN=LEFT                                    
         AR    R3,R0                                                            
         MVI   0(R3),C','                                                       
         LA    R3,2(R3)                                                         
         DROP  R6                                                               
*                                                                               
DISF071  DS    0H                       DISPLAY COVERSHEET STATUS               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'A6'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISF072                                                          
         MVI   BYTE,L'CONMRESF+1                                                
         BAS   RE,LINEWRAP                                                      
         MVC   0(L'CONMRESF,R3),CONMRESF                                        
         MVI   L'CONMRESF(R3),C','                                              
         LA    R3,2+L'CONMRESF(R3)                                              
*                                                                               
DISF072  DS    0H                       DISPLAY MOVE HISTORY                    
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'2A'                                                     
         BAS   RE,GETEL                 DO WE HAVE A MOVE HISTORY ELEM?         
         BNE   DISF074                  NO - SKIP MOVE HIST DISPLAY             
         MVI   BYTE,L'CONMRESC+24                                               
         BAS   RE,LINEWRAP                                                      
         MVC   0(L'CONMRESC,R3),CONMRESC                                        
         LA    R3,L'CONMRESC(R3)                                                
         USING RCONMMEL,R6                                                      
         MVC   0(2,R3),RCONMMOR         ORIGINAL REP CODE                       
         MVI   2(R3),C' '                                                       
         LA    R3,3(R3)                                                         
         GOTO1 HEXOUT,DMCB,RCONMMOC,(R3),4                                      
         LA    R3,8(R3)                                                         
         CLC   =X'FFFF',RCONMMDT   MANUAL TKO ENTRY?                            
         BE    DISF073             YES - DON'T SHOW DATE                        
         MVC   0(4,R3),=C' on '                                                 
         LA    R3,4(R3)                                                         
         GOTO1 DATCON,DMCB,(2,RCONMMDT),(11,0(R3))                              
         LA    R3,8(R3)                                                         
         DROP  R6                                                               
DISF073  EQU   *                                                                
         MVC   0(2,R3),=C', '                                                   
         LA    R3,2(R3)                                                         
DISF074  DS    0H                                                               
         BCTR  R3,0                                                             
         BCTR  R3,0                                                             
         MVI   0(R3),C' '          ERASE LAST COMMA                             
*                                                                               
* LAST MODIFICATION DATE                                                        
*                                                                               
         OC    RCONMODD,RCONMODD                                                
         BZ    DISF075                                                          
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         MVC   9(L'CONMDATE,R2),CONMDATE                                        
         GOTO1 DATCON,DMCB,(3,RCONMODD),(5,9+L'CONMDATE(R2))                    
*                                                                               
* BUYLINE 1 ADDED DATE                                                          
*                                                                               
DISF075  DS    0H                                                               
         CLC   RCONCREA,RCONHDRD                                                
         BNE   DISF080             CHECK IF BUYLINE ADDED                       
         TM    RCONMODR,X'10'                                                   
         BZ    DISF090                                                          
*                                                                               
DISF080  DS    0H                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         MVC   9(L'CONBUY1D,R2),CONBUY1D                                        
         GOTO1 DATCON,DMCB,(3,RCONCREA),(5,9+L'CONBUY1D(R2))                    
*                                                                               
* DISPLAY CONTRACT LENGTH                                                       
*                                                                               
DISF090  DS    0H                                                               
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         MVC   9(L'CONKLEN,R2),CONKLEN                                          
         LA    R3,9+L'CONKLEN(R2)                                               
         EDIT  RCONLEN,(4,0(R3))                                                
         MVC   5(5,R3),=C'bytes'                                                
*                                                                               
         B     XIT                                                              
***********************************************************************         
* CHECKS ROOM LEFT ON DISPLAY LINE, BUMPTS TO NEXT LINE IF NECESSARY            
*                                                                               
* BYTE: AMOUNT OF ROOM TO CHECK FOR            ** INPUT **                      
* R2: DISPLAY LINE SCREEN HEADER                                                
* R3: POINTER TO POSITION ON DISPLAY LINE                                       
*                                              ** OUTPUT **                     
* R2: WILL POINT TO NEXT LINE HEADER IF WRAP WAS NECESSARY                      
* R3: WILL POINT TO FIRST POSITION ON NEXT LINE IF WRAP WAS NECESSARY           
*                                                                               
* BY THE WAY, THIS ROUTINE CLOBBERS R0,R1                                       
***********************************************************************         
LINEWRAP DS    0H                                                               
         ZIC   R0,BYTE             NECESSARY LEN                                
         AR    R0,R3               CURRENT LINE POS                             
*                                                                               
         LA    R1,8(R2)            START OF CURRENT LINE                        
         LA    R1,L'HISLIN1(R1)    1 BYTE PAST EOL                              
*                                                                               
         CR    R0,R1               NECESSARY POSITION VS. EOL                   
         BL    LWX                 IT FITS                                      
         ZIC   R1,0(R2)            POINT R2 TO NEXT LINE                        
         AR    R2,R1                                                            
         LA    R3,9(R2)            POINT R3 TO FIRST POSITION+1                 
LWX      BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY INVOICE # FOR CONTRACT IF ANY                                         
***********************************************************************         
DISINV#  NTR1                                                                   
         LA    R2,HISLIN1H                                                      
         SR    R4,R4                                                            
         SR    R5,R5                                                            
                                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'04'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+6                                                              
         LR    R4,R6                                                            
                                                                                
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'18'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         LA    R5,RCONDVIL-RCONDVEL(R6)                                         
                                                                                
DISINV10 DS    0H                                                               
         LTR   R4,R4               CHECK IF BOTH INVOICE$ AND # FOUND           
         BNZ   DISINV20            EXIT WITH MESSAGE IF NONE OF THE             
         LTR   R5,R5               ABOVE                                        
         BNZ   DISINV20                                                         
         LA    RF,HISLIN1H                                                      
         CR    R2,RF                                                            
         BH    DISINVX                                                          
         MVC   9(L'NOINVOIZ,R2),NOINVOIZ                                        
         B     DISINVX                                                          
                                                                                
DISINV20 DS    0H                                                               
         LTR   R5,R5                                                            
         BZ    DISINV30                                                         
         LTR   R4,R4                                                            
         BZ    DISINV70                                                         
         CLC   2(2,R4),1(R5)                                                    
         BH    DISINV80                                                         
                                                                                
DISINV30 DS    0H                  DISPLAY YEAR/MONTH FROM INVOICE$             
         GOTO1 DATCON,DMCB,(3,2(R4)),(6,9(R2))                                  
         SR    R3,R3                                                            
                                                                                
DISINV40 DS    0H                  AGGREGATE ALL INVOICE $ FOR THIS             
         L     RF,6(R4)            MONTH/YEAR                                   
         AR    R3,RF                                                            
         ZIC   RF,1(R4)                                                         
         AR    RF,R4                                                            
         CLI   0(RF),X'04'                                                      
         BNE   DISINV50                                                         
         CLC   2(2,RF),2(R4)                                                    
         BNE   DISINV50                                                         
         LR    R4,RF                                                            
         B     DISINV40                                                         
                                                                                
DISINV50 DS    0H                  DISPLAY INVOICE$                             
         EDIT  (R3),(12,18(R2)),2,COMMAS=YES,FLOAT=$,MINUS=YES                  
                                                                                
         LTR   R5,R5               ANY INVOICE# FOR THIS YEAR/MONTH?            
         BZ    DISINV60                                                         
         CLC   2(2,R4),1(R5)                                                    
         BE    DISINV80                                                         
                                                                                
DISINV60 DS    0H                                                               
         ZIC   RF,1(R4)            NO, BUMP TO NEXT ELEMENT                     
         AR    R4,RF                                                            
         CLI   0(R4),X'04'                                                      
         BE    *+6                                                              
         SR    R4,R4                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     DISINV10                                                         
                                                                                
DISINV70 DS    0H                  DISPLAY YEAR/MONTH FROM INV# ELEMENT         
         GOTO1 DATCON,DMCB,(3,1(R5)),(6,9(R2))                                  
                                                                                
DISINV80 DS    0H                  DISPLAY INVOICE#                             
         MVI   34(R2),C'#'                                                      
         ZIC   RF,0(R5)                                                         
         SH    RF,=H'4'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   35(0,R2),3(R5)                                                   
                                                                                
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
                                                                                
         ZIC   RF,1(R4)                                                         
         AR    R4,RF                                                            
         CLI   0(R4),X'04'                                                      
         BE    *+6                                                              
         SR    R4,R4                                                            
                                                                                
         ZIC   RF,0(R5)                                                         
         AR    R5,RF                                                            
         ZIC   RF,1(R6)                                                         
         AR    RF,R6                                                            
         CR    R5,RF                                                            
         BL    DISINV10                                                         
         SR    R5,R5                                                            
         B     DISINV10                                                         
                                                                                
DISINVX  DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
* MESSAGE CONSTANTS                                                             
CONCRDT  DC    C'Contract header created on    '                                
CONLSBR  DC    C'Contract last sent by rep on  '                                
CONLSBS  DC    C'Contract last sent by sta on  '                                
CONLCON  DC    C'Contract last cnfd by sta on  '                                
CONTKOV  DC    C'Takeover contract *done* on   '                                
CONCONF  DC    C'Contract is confirmed'                                         
CONNCNF  DC    C'Contract has not been confirmed'                               
CONPCNF  DC    C'Contract was previously confirmed at Mod Num '                 
CONMRES  DC    C'Attributes: '                                                  
CONMRES1 DC    C'Header changed'                                                
CONMRES2 DC    C'Buy changed'                                                   
CONMRES3 DC    C'Reset/Buy 1 added'                                             
CONMRES4 DC    C'ACE'                                                           
CONMRES5 DC    C'EasyLink'                                                      
CONMRES6 DC    C'MON/PACE data added'                                           
CONMRES7 DC    C'Buy added'                                                     
CONMRES8 DC    C'Converted'                                                     
CONMRES9 DC    C'Buycopy'                                                       
CONMRESA DC    C'CFX at Mod Num '                                               
CONMRESB DC    C'BOP'                                                           
CONMRESC DC    C'Moved from '                                                   
CONMRESD DC    C'Batch Confirm'                                                 
CONMRESE DC    C'R16 Closeout on'                                               
CONMRESF DC    C'Coversheet Attached'                                           
CONMDATE DC    C'Last mod date was '                                            
CONBUY1D DC    C'Buyline 1 added on '                                           
CONKLEN  DC    C'Contract size is '                                             
DTLSTEC  DC    C'Last ECd     :  Date: '                                        
DTLSTST  DC    C'Last Stored  :  Date: '                                        
DTLSTDX  DC    C'Last xferred :  Date: '                                        
DTLSSTJD DC    C'Last Stored  :  Date: '                                        
DTLSSEJD DC    C'Last Sent    :  Date: '                                        
DRDELDT  DC    C'Delivery Notification :  Date: '                               
DRLSTAP  DC    C'Last Approval         :  Date: '                               
DRLSTRJ  DC    C'Last Rejection        :  Date: '                               
TIMELBL  DC    C'Time: '                                                        
LINKWITH DC    C'Linked With DARE Order  '                                      
TKODWITH DC    C'Takeover Linked With Agency Order  '                           
NOTLINKD DC    C'Not Linked with Any Agency Order'                              
NOTDARE  DC    C'Not A DARE Order'                                              
EDICONV  DC    C'EDI Converted'                                                 
DAREREM  DC    C'DARE Order Removed'                                            
NOINVOIZ DC    C'No Invoice $ and Invoice # Found'                              
NOMGO    DC    C'No Makegood Offer Attached'                                    
MGOWIP   DC    C'Makegood Offer Work-In-Progress'                               
MGOSENT  DC    C'Makegood Offer Sent by Rep on'                                 
DAREMAN  DC    C'User Initiated Manual Changes'                                 
REPALTC  DC    C'Uses rep level alt. calendars'                                 
STAALTC  DC    C'Uses station level alt. calendars'                             
NORER    DC    C'Contract excluded from RER Tapes'                              
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
XIT      XIT1                                                                   
       ++INCLUDE RECNTWR2K                                                      
         EJECT                                                                  
         ORG   CONLAST                                                          
       ++INCLUDE RECNTECD                                                       
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE FAUTL                                                          
         EJECT                                                                  
*                                                                               
* DISPLAY BUCKETS (ALTERNATE CALENDAR)                                          
* R3 = FORECAST DOLLARS                                                         
DISALT$  CSECT                                                                  
         NMOD1 0,*DALT*                                                         
         L     RC,0(R1)            RESET A(WORKSPACE)                           
****>>>>                                                                        
*                                                                               
* DISPLAY BUCKETS (ACTUAL AND INVOICED)                                         
* R3 = ACTUAL DOLLARS        R4 = INVOICED DOLLARS                              
         LA    R3,RCONELEM                                                      
         SR    R0,R0                                                            
* GET FIRST X'53' ELEM IN R3                                                    
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0             NO 53 OR 54 ELEMS                            
         BE    XIT                                                              
         CLI   0(R3),X'53'                                                      
         BL    *-18                                                             
* GET FIRST X'54' ELEM IN R4                                                    
         LR    R4,R3                                                            
         CLI   0(R4),X'54'                                                      
         BNL   DALT0020                                                         
*                                                                               
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),X'54'                                                      
         BNL   DALT0020                                                         
         CLI   0(R4),0                                                          
         BNE   *-18                                                             
*                                                                               
* DISPLAY BUCKETS                                                               
DALT0020 DS    0H                                                               
         LA    R2,HISLIN1H                                                      
         LA    R5,13                                                            
         CLC   CONBNUM(7),=C'$NEXT$$'                                           
         BNE   DALT0040                                                         
         SR    R0,R0                                                            
*                                                                               
* FIND NEXT BUCS TO BE DISPLAYED                                                
         CLC   TWAACTBC,2(R3)                                                   
         BE    *+18                                                             
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BNE   *-20                                                             
*                                                                               
         CLC   TWAINVBC,2(R4)                                                   
         BE    *+18                                                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BNE   *-20                                                             
*                                                                               
DALT0040 CLI   0(R3),X'53'         MAKE SURE STILL 53 ELEM                      
         BNE   DALT0060                                                         
         GOTO1 DATCON,DMCB,(3,2(R3)),(6,9(R2))                                  
         GOTO1 DATCON,DMCB,(2,4(R3)),(5,16(R2))                                 
         EDIT  (4,6(R3)),(12,25(R2)),2,COMMAS=YES,FLOAT=$,MINUS=YES             
*                                                                               
DALT0060 CLI   0(R4),X'54'         MAKE SURE STILL 54 ELEM                      
         BNE   DALT0080                                                         
         GOTO1 DATCON,DMCB,(3,2(R4)),(6,49(R2))                                 
         GOTO1 DATCON,DMCB,(2,4(R4)),(5,56(R2))                                 
         EDIT  (4,6(R4)),(12,65(R2)),2,COMMAS=YES,FLOAT=$,MINUS=YES             
*                                                                               
DALT0080 DS    0H                                                               
         ZIC   R0,1(R3)            NEXT 53                                      
         AR    R3,R0                                                            
         IC    R0,1(R4)            NEXT 54                                      
         AR    R4,R0                                                            
         IC    R0,0(R2)            NEXT SCREEN LINE                             
         AR    R2,R0                                                            
         BCT   R5,DALT0040                                                      
*                                                                               
         XC    TWAACTBC,TWAACTBC                                                
         XC    TWAINVBC,TWAINVBC                                                
***>>>   MVC   CONBNUM(7),=C'ALT  '                                             
         MVC   CONBNUM(7),MYSPACES                                              
         MVC   CONBNUM(3),TWAHIST  RELOAD ORIGINAL VALUE                        
         OI    CONBNUMH+6,X'80'    XMIT FIELD                                   
         CLI   0(R3),X'53'                                                      
         BNE   *+16                                                             
         MVC   TWAACTBC,2(R3)                                                   
         MVC   CONBNUM(7),=C'$NEXT$$'                                           
         CLI   0(R4),X'54'                                                      
         BNE   *+16                                                             
         MVC   TWAINVBC,2(R4)                                                   
         MVC   CONBNUM(7),=C'$NEXT$$'                                           
*                                                                               
         OI    CONBNUMH+1,X'01'    FLD MODIFIED                                 
         XIT1                                                                   
****>>>>                                                                        
         EJECT                                                                  
         LTORG                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'179RECBT53T  05/01/02'                                      
         END                                                                    
