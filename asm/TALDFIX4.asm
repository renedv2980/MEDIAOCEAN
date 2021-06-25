*          DATA SET TALDFIX4   AT LEVEL 085 AS OF 05/01/02                      
*PHASE TALDFI4A TALDFIX4                                                        
*INCLUDE SCANNER                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE DATCON                                                                 
         TITLE 'TALDFIX4 - TALENT LOAD/DUMP FILE FIX MODULE'                    
*                                                                               
*                                                                               
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)      PASS FIRST BYTE X'00'= INITIALISE                           
*                                   X'01'= RECORD IN CORE                       
*                                   X'FF'= END OF FILE                          
*                   RETURN VALUE    X'00'= KEEP RECORD                          
*                                   X'FF'= PURGE RECORD                         
*                                   X'FF'/C'EOJ'=PURGE & CAUSE EOJ              
* P2=A(TAPEOUT)     PASS FIRST BYTE X'80'= TAPE INPUT                           
*                                   X'40'= TAPE OUTPUT                          
*                                   X'20'= RECORD IS I/S FILE RECORD            
* P3=A(PARAM CARD)  PASS FIRST BYTE C'Y' = YOU ASKED ME TO RETURN               
*                   RETURN          C'R' = RETURN BACK TO EXTERNAL              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
         SPACE 2                                                                
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDEXT                                              
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         SPACE 2                                                                
         CLI   PLIST+8,C'Y'        RETURN CALL AS REQUESTED LAST TIME           
         BE    DMXRET                                                           
         CLI   PLIST,X'00'         FIRST CALL TO INITILISE                      
         BE    DMXINIT                                                          
         CLI   PLIST,X'01'         NORMAL CALL TO PROCESS RECORD                
         BE    DMXREC                                                           
         CLI   PLIST,X'FF'         LAST CALL ON EOF                             
         BE    DMXEOF                                                           
         B     DMXIT                                                            
         SPACE 1                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
         SPACE 1                                                                
DMXKERET L     R1,APARM            KEEP RECORD AND RETURN TO ME                 
         MVI   0(R1),0                                                          
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
         SPACE 1                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),0                                                          
         B     DMXIT                                                            
         SPACE 1                                                                
DMXPGRET L     R1,APARM            PURGE RECORD AND RETURN TO ME                
         MVI   0(R1),X'FF'                                                      
         MVI   8(R1),C'R'                                                       
         B     DMXIT                                                            
         SPACE 1                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 1                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
* INITIALISE LOGIC - FIRST CALL HERE - NO RETURN ALLOWED                        
*                                                                               
DMXINIT  L     R2,APARAMC          POSSIBLE PARAM CARD                          
         LTR   R2,R2                                                            
         BZ    DMXIT                                                            
         CLI   0(R2),X'41'                                                      
         BL    DMXIT                                                            
         MVC   CARD,SPACES                                                      
         MVC   CARD(75),0(R2)                                                   
         GOTO1 =V(SCANNER),DMCB,(C'C',CARD),(10,BLOCK)                          
         ZIC   R4,DMCB+4                                                        
         LTR   R4,R4                                                            
         BE    BADPARAM                                                         
         LA    R2,BLOCK                                                         
         SPACE 1                                                                
INIT2    DS    0H                                                               
         SPACE 1                                                                
INITNXT  LA    R2,32(R2)                                                        
         BCT   R4,INIT2                                                         
         B     DMXIT                                                            
         SPACE 1                                                                
BADPARAM MVC   P(30),=CL30'**BAD PARAMETER CARD**'                              
         GOTO1 VPRINTER                                                         
         MVC   P(80),CARD                                                       
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
* PROCESS RECORD LOGIC - RECORD IN AREC                                         
*                                                                               
DMXREC   DS    0H                                                               
*****************************************                                       
*        L     R6,AREC                                                          
*        CLI   0(R6),TLINCDQ       IF INVOICE RECORD                            
*        BNE   DMXKEEP                                                          
*        LR    R4,R6                                                            
*        MVI   ELCODE,TAPDELQ                                                   
*        BAS   RE,GETEL                                                         
*        BNE   DMXKEEP                                                          
*        USING TAPDD,R4                                                         
*        TM    TAPDINV+5,X'0F'                                                  
*        BZ    DMXKEEP                                                          
*        CLI   TAPDINV+5,3                                                      
*        BE    DMXKEEP                                                          
*        AP    RECCOUNT,=P'1'      ADD TO RECCOUNT                              
*        B     DMXKEEP                                                          
*****************************************                                       
         L     R6,AREC                                                          
         CLI   0(R6),TLCACDQ       IF CAST RECORD                               
         BNE   DMXKEEP                                                          
         LR    R4,R6                                                            
         USING TLRCD,R4                                                         
         LA    R4,TLRCELEM         PT TO FIRST ELEMENT                          
*                                                                               
DMXREC1  MVC   ELCD,0(R4)          SAVE THIS ELEMENT CODE                       
         ZIC   R1,1(R4)                                                         
         AR    R4,R1               PT TO NEXT ELEMENT                           
         CLI   0(R4),0                                                          
         BE    DMXREC3                                                          
         CLC   ELCD,0(R4)          IF PREV EL CODE NOT GREATER                  
         BNH   DMXREC1             KEEP LOOKING                                 
         B     DMXREC50            ELSE GO FIX RECORD                           
*                                                                               
DMXREC3  XC    CURSTART,CURSTART   CLEAR TACR DATES                             
         XC    CUREND,CUREND                                                    
         LR    R4,R6                                                            
         MVI   ELCODE,TACRELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DMXRECX                                                          
         USING TACRD,R4                                                         
DMXREC5  MVC   CURSTART(6),TACRSTRT SAVE CURRENT TACR DATES                     
         BAS   RE,NEXTEL                                                        
         BNE   DMXRECX                                                          
         CLC   CURSTART,TACRSTRT   IF START DATE IS LOWER                       
         BL    DMXREC5             GO GET NEXT EL                               
         BH    DMXREC50            IF IT'S HIGHER, GO PROCESS RECORD            
         CLC   CUREND,TACREND      IF SAME, CHECK END DATE                      
         BH    DMXREC50            IF HIGHER, GO PROCESS RECORD                 
         B     DMXREC5                                                          
*                                                                               
         USING TLRCD,R4                                                         
DMXREC50 LR    R4,R6                                                            
         LA    R4,TLRCELEM         R4=A(ELEMENT ON RECORD)                      
         LA    R2,SAVELS           R2=A(ENTRY IN EL TABLE)                      
*                                                                               
DMXREC55 ZIC   R1,1(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R4)       SAVE ELEMENT IN TABLE                        
         MVI   0(R4),X'FF'         MARK IT DELETED ON RECORD                    
         LA    R4,1(R1,R4)         PT TO NEXT ELEMENT IN RECORD                 
         LA    R2,1(R1,R2)                            IN TABLE                  
         MVI   0(R2),0             MARK END OF TABLE                            
         CLI   0(R4),0             KEEP LOOPING TILL END OF RECORD              
         BNE   DMXREC55                                                         
*                                                                               
         MVI   ELCODE,X'FF'        DELETE ELEMENTS                              
         BAS   RE,DELEL                                                         
*                                                                               
         LA    R2,SAVELS                                                        
DMXREC70 CLI   0(R2),0             IF NOT END OF TABLE                          
         BE    DMXREC90                                                         
         ZIC   R1,1(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT(0),0(R2)    MOVE TO ELEMENT                              
         BAS   RE,ADDEL            ADD THEM BACK TO RECORD                      
         LA    R2,1(R1,R2)         BUMP TO NEXT ENTRY IN TABLE                  
         B     DMXREC70                                                         
*                                                                               
DMXREC90 AP    RECCOUNT,=P'1'      ADD TO RECCOUNT                              
         GOTO1 TRACE,DMCB,=C'CAST REC' TRACE OUT RECORD                         
DMXRECX  B     DMXKEEP                                                          
*                                                                               
***********************************************************                     
         CLI   0(R6),TLNXCDQ       IF HOLD RECORD                               
         BNE   DMXREC2                                                          
*                                                                               
         USING TLNXD,R6                                                         
         MVC   OLDKEY,TLNXKEY      SAVE OLD KEY                                 
         LA    R2,OLDKEY                                                        
         USING OLDKEYD,R2                                                       
         XC    TLNXKEY,TLNXKEY     SET UP NEW KEY                               
         MVC   TLNXCD,OLDCD        OLD RECORD EQUATE                            
         MVC   TLNXAGY,OLDAGY      OLD AGENCY                                   
         MVC   TLNXNID,OLDNID      OLD NETWORK ID                               
         MVC   TLNXNCLI,OLDNCLI    OLD CLIENT                                   
         MVC   TLNXNPRD,OLDNPRD    OLD PRODUCT                                  
         MVI   TLNXMED,C'T'        NEW HARD-CODED MEDIA TO TELEVISION           
         MVC   TLNXUSE,=C'CLA'     NEW HARD-CODED USE TO CLA                    
         MVC   TLNXDATE,OLDDATE    OLD DATE ADDED                               
         BAS   RE,GETUID           GET USER ID NUM FROM OLD CHARACTER           
         MVC   TLNXUID,WORK        NEW USER ID NUMBER                           
         MVC   TLNXCCDE,OLDCCDE    OLD CHANGE CODE                              
         MVC   TLNXSEQ,OLDSEQ      OLD SEQUENCE NUMBER                          
         BAS   RE,FIXTANX          FIX TRANSFER DETAILS ELEMENT                 
         AP    NXCOUNT,=P'1'       ADD TO HOLD COUNT                            
         GOTO1 TRACE,DMCB,=C'HOLD REC' TRACE OUT RECORD                         
         B     DMXKEEP                                                          
*                                                                               
DMXREC2  CLI   0(R6),TLDVCDQ       IF ADVICE RECORD                             
         BNE   DMXREC4                                                          
         BAS   RE,FIXTANX          FIX TRANSFER DETAILS ELEMENT                 
         BNE   DMXKEEP                                                          
         AP    DVCOUNT,=P'1'       ADD TO ADVICE COUNT                          
         GOTO1 TRACE,DMCB,=C'ADVICE REC' TRACE OUT RECORD                       
         B     DMXKEEP                                                          
*                                                                               
DMXREC4  CLI   0(R6),TLAKCDQ       IF ALIAS RECORD                              
         BNE   DMXKEEP                                                          
         USING TLAKD,R6                                                         
         MVI   TLAKMED,C'T'        NEW HARD-CODED MEDIA AS TELEVISION           
         AP    AKCOUNT,=P'1'       ADD TO ALIAS COUNT                           
         GOTO1 TRACE,DMCB,=C'ALIAS REC' TRACE OUT RECORD                        
         B     DMXKEEP                                                          
         EJECT                                                                  
*              ROUTINE TO LOOK UP USER ID NUMBER BASED ON OLD 6                 
*              CHARACTER CODE                                                   
         SPACE 1                                                                
GETUID   NTR1                                                                   
         LA    R1,UIDTAB                                                        
GETUID5  CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   OLDUID,2(R1)                                                     
         BE    *+12                                                             
         LA    R1,L'UIDTAB(R1)                                                  
         B     GETUID5                                                          
*                                                                               
         MVC   WORK(2),0(R1)       RETURN NUMBER                                
         B     DMXIT                                                            
         SPACE 2                                                                
*              ROUTINE TO FIX TANXUID FROM 6 TO 8 (INCREASING ELEMENT           
*              SIZE BY TWO)                                                     
         SPACE 1                                                                
FIXTANX  NTR1                                                                   
         XR    R0,R0               CHANGED FLAG                                 
         LR    R4,R6                                                            
         MVI   ELCODE,TANXELQ      FIX TRANSFER DETAILS ELEMENT                 
         BAS   RE,GETEL                                                         
         B     *+8                                                              
FIXTANX5 BAS   RE,NEXTEL                                                        
         BNE   FIXTANXX                                                         
*                                                                               
         LA    R0,1                                                             
         XC    OLDEL,OLDEL                                                      
         ZIC   R1,1(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OLDEL(0),0(R4)                                                   
         MVI   0(R4),X'FF'         MARK OLD ELEMENT DELETED                     
         MVI   ELCODE,X'FF'                                                     
         BAS   RE,DELEL            AND DELETE IT                                
         MVI   ELCODE,TANXELQ      RESET ELEMENT CODE                           
*                                                                               
         XC    ELEMENT,ELEMENT     SET UP NEW ELEMENT                           
         LA    R2,ELEMENT                                                       
         USING TANXD,R2                                                         
         LA    RE,OLDEL                                                         
         USING OLDELD,RE                                                        
         MVC   TANXEL(OLDENCID-OLDELCD),OLDELCD                                 
         OC    TANXUID,SPACES      PAD ENLARGED FIELD WITH SPACES               
         MVC   TANXNCID(OLDEX-OLDENCID),OLDENCID                                
         MVI   TANXLEN,TANXLNQ     SET NEW LENGTH                               
         BAS   RE,ADDEL            AND ADD UPDATED ELEMENT BACK                 
         B     FIXTANX5                                                         
*                                                                               
FIXTANXX CH    R0,=H'1'                                                         
         BE    YES                                                              
         B     NO                                                               
         DROP  RE,R2                                                            
         EJECT                                                                  
*        TRACE ROUTINE                                                          
*                                                                               
TRACE    NTR1                                                                   
         L     R4,AREC                                                          
         LH    R3,DATADISP         PRINT OUT KEY                                
         ZIC   R0,0(R1)            R0=L'LITERAL                                 
         L     R2,0(R1)            R2=A(LITERAL)                                
****     CLI   TACRELS,0                                                        
****     BE    TR2                                                              
****     CP    RECCOUNT,=P'100'                                                 
****     BH    TR100                                                            
****     B     TR10                                                             
*                                                                               
*TR2     CLI   BADREC,C'Y'                                                      
*        BNE   TR100                                                            
*        CP    FFCOUNT,=P'10'                                                   
*        BH    TR100                                                            
*        B     TR10                                                             
*                                                                               
TR10     GOTO1 =V(PRNTBL),DMCB,((R0),(R2)),(R4),C'DUMP',(R3),=X'01C4'           
*                                                                               
TR15     L     R4,AREC             A(RECORD)                                    
         AH    R4,DATADISP         + DISPLACEMENT TO FIRST ELEMENT              
         MVI   ELCODE,0                                                         
         BAS   RE,FIRSTEL                                                       
         BNE   TR100                                                            
*                                                                               
TR20     ZIC   R3,1(R4)            PRINT ELEMENT                                
         GOTO1 =V(PRNTBL),DMCB,0,(R4),C'DUMP',(R3),=X'01C4'                     
         BAS   RE,NEXTEL           REPEAT UNTIL NO MORE ELEMENTS                
         BE    TR20                                                             
*                                                                               
TR100    DS    0H                                                               
         B     DMXIT                                                            
         SPACE 3                                                                
RECCOUNT DC    PL6'0'                                                           
NXCOUNT  DC    PL6'0'                                                           
DVCOUNT  DC    PL6'0'                                                           
AKCOUNT  DC    PL6'0'                                                           
FFCOUNT  DC    PL6'0'                                                           
         EJECT                                                                  
* REQUESTED RETURN - DATA IN AREC AS LEFT PREVIOUSLY                            
*                                                                               
DMXRET   DS    0H                                                               
         L     R3,AREC             POINT TO LAST RECORD                         
         B     DMXPURGE                                                         
         EJECT                                                                  
* END-OF-FILE LOGIC - LAST CALL HERE - RETURN ALLOWED                           
*                                                                               
DMXEOF   DS    0H                                                               
         MVC   P+1(21),=CL21'NUMBER OF HNW INV = '                              
         EDIT  RECCOUNT,(10,P+24),ALIGN=LEFT,COMMAS=YES,ZERO=NOBLANK            
         GOTO1 VPRINTER                                                         
***      MVC   P+1(21),=CL21'CAST RECS ORDERED = '                              
*        EDIT  RECCOUNT,(10,P+24),ALIGN=LEFT,COMMAS=YES,ZERO=NOBLANK            
*        GOTO1 VPRINTER                                                         
*        MVC   P+1(21),=CL21'CAST RECS W/FF ONLY='                              
*        EDIT  FFCOUNT,(10,P+24),ALIGN=LEFT,COMMAS=YES,ZERO=NOBLANK             
***      GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         SPACE 3                                                                
DELEL    NTR1                                                                   
         GOTO1 =V(HELLO),DMCB,(C'D',=C'TALFIL'),(ELCODE,AREC),0                 
         B     DMXIT                                                            
         SPACE 3                                                                
ADDEL    NTR1                                                                   
         GOTO1 =V(HELLO),DMCB,(C'P',=C'TALFIL'),(0,AREC),ELEMENT                
         B     DMXIT                                                            
         SPACE 3                                                                
         GETEL (R4),DATADISP,ELCODE                                             
         EJECT                                                                  
DATADISP DC    H'40'                                                            
         SPACE 2                                                                
UIDTAB   DS    0CL8                                                             
         DC    H'0017',C'SJR   '                                                
         DC    H'1521',C'YNRT  '                                                
         DC    H'0615',C'ADNY  '                                                
         DC    H'3173',C'BDPE  '                                                
         DC    H'0034',C'BDNY  '                                                
         DC    H'1487',C'BDNYM '                                                
         DC    H'2996',C'BJNYT '                                                
         DC    H'4144',C'BSBZNY'                                                
         DC    H'1354',C'DNCH  '                                                
         DC    H'1287',C'DNNYT '                                                
         DC    H'2654',C'DFLANS'                                                
         DC    H'0216',C'DFLA  '                                                
         DC    H'1192',C'DFNYT '                                                
         DC    H'1287',C'DNNYT '                                                
         DC    H'4165',C'DFZNY '                                                
         DC    H'4387',C'DNGB  '                                                
         DC    H'2467',C'DCDE  '                                                
         DC    H'1504',C'HRCH  '                                                
         DC    H'3065',C'HRSFT '                                                
         DC    H'2940',C'JWNYBT'                                                
         DC    H'0199',C'JWSF  '                                                
         DC    H'0022',C'JWNY  '                                                
         DC    H'1369',C'OMCH  '                                                
         DC    H'0198',C'JWDE  '                                                
         DC    H'0194',C'JWCH  '                                                
         DC    H'1053',C'NCNY  '                                                
         DC    H'1376',C'OMNYT '                                                
         DC    H'1339',C'OMNY  '                                                
         DC    H'2927',C'SPNJ  '                                                
         DC    H'3404',C'WILAT '                                                
         DC    X'FF'                                                            
         LTORG                                                                  
         SPACE 3                                                                
*TASYSCATS                                                                      
         PRINT OFF                                                              
       ++INCLUDE TASYSCATS                                                      
         PRINT ON                                                               
         EJECT                                                                  
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
WORK     DS    CL32                                                             
*                                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
         SPACE 1                                                                
CARD     DS    CL80                                                             
BLOCK    DS    320C                                                             
ELCODE   DS    XL1                                                              
ELEMENT  DS    CL256                                                            
SVDATE   DS    XL3                                                              
OLDKEY   DS    CL(L'TLNXKEY)                                                    
OLDEL    DS    CL(L'ELEMENT)                                                    
SAVELS   DS    CL2000                                                           
BADREC   DS    CL1                                                              
CURSTART DS    XL3                                                              
ELCD     DS    XL1                                                              
CUREND   DS    XL3                                                              
WORKX    EQU   *                                                                
         SPACE 2                                                                
OLDKEYD  DSECT                                                                  
OLDCD    DS    XL1                                                              
OLDAGY   DS    CL6                                                              
OLDNID   DS    CL8                                                              
OLDNCLI  DS    CL3                                                              
OLDNPRD  DS    CL3                                                              
OLDDATE  DS    XL3                                                              
OLDUID   DS    CL6                                                              
OLDCCDE  DS    XL1                                                              
OLDSEQ   DS    XL1                                                              
         SPACE 2                                                                
OLDELD   DSECT                                                                  
OLDELCD  DS    XL1                                                              
OLDELEN  DS    XL1                                                              
OLDEAGY  DS    CL6                                                              
OLDEUID  DS    CL6                                                              
OLDENCID DS    CL8                                                              
OLDESEC  DS    XL1                                                              
OLDEMDTE DS    XL3                                                              
OLDEUDTE DS    XL3                                                              
OLDEADTE DS    XL3                                                              
OLDECOM  DS    CL4                                                              
OLDETYPE DS    CL1                                                              
OLDECCDE DS    XL1                                                              
OLDESTAT DS    XL1                                                              
         DS    CL3                                                              
OLDELNQ  EQU   *-OLDELD                                                         
OLDEX    EQU   *                                                                
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         SPACE 2                                                                
         PRINT OFF                                                              
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSCATSD                                                     
       ++INCLUDE TASYSEQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'085TALDFIX4  05/01/02'                                      
         END                                                                    
