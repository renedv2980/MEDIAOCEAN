*          DATA SET RECNT62    AT LEVEL 001 AS OF 03/29/10                      
*PHASE T80262A,*                                                                
*INCLUDE REGENDHT                                                               
         TITLE 'T80262 - DARE CONFIRMATION'                                     
***********************************************************************         
*                                                                     *         
*  RECNT62  (T80262 )  --  DARE CONFIRMATION                          *         
*                                                                     *         
*---------------------------------------------------------------------*         
*                                                                     *         
*  UPDATE HISTORY:                                                    *         
*                                                                     *         
* 09JUN94 (SKU) --- ORIGINATION DATE                                  *         
*                                                                     *         
* 20OCT94 (SKU) --- PASS INFORMATION CHUNK TO EDICT FOR REPORTING     *         
*                                                                     *         
* 28DEC94 (SKU) --- ADD MULTI-DARE AGENCY ASSIGNMENTS SUPPORT         *         
*                                                                     *         
* 20JAN95 (SKU) --- EXPAND TO TV FOR TV STATIONS                      *         
*                                                                     *         
* 22MAR95 (SKU) --- REWRITE X'41' HEADER REC AS X'51' IN ANTICIPATION *         
*                   FOR FUTURE MAKEGOODS                              *         
*                                                                     *         
* 27MAR95 (SKU) --- INSTEAD OF REWRITING X'41', ADD X'51' RECS        *         
*                                                                     *         
* 09OCT95 (SKU) --- 2K CONTRACT SUPPORT                               *         
*                                                                     *         
* 07FEB96 (SKU) --- BYPASS SENDING CONFIRMATION TO PQ FOR KATZ/EDI    *         
*                                                                     *         
* 20FEB96 (SKU) --- SPECIAL AGENCY EQUIVALENCY CHECK FOR EDI ED2-DE   *         
*                                                                     *         
* 25JUL96 (SKU) --- CHANGE TO USE THMS TIME MACRO                     *         
*                                                                     *         
* 08OCT96 (SKU) --- LOW POWER STATION                                 *         
*                                                                     *         
* 12MAR97 (SKU) --- SUPPORT VARIOUS/BRAND DARE ORDERS                 *         
*                                                                     *         
* 23MAY97 (SKU) --- CONFIRM WITH COMMENTS                             *         
*                                                                     *         
* 23JUL97 (SKU) --- 4K CONTRACT SUPPORT                               *         
*                                                                     *         
* 01OCT97 (SKU) --- ADD SPOT COUNT ELEMENT                            *         
*                                                                     *         
* 11MAR97 (SKU) --- DON'T SEND BUYS THAT WERE ZERO OUT: RBUYRTS=X'20' *         
*                   VIA CONTRACT PROFILE 36                           *         
*                                                                     *         
* 19JAN99 (RHV) --- WRITE ALL 41 RECS AS 51 RECS                      *         
*                                                                     *         
* 13SEP99 (SKU) --- DO NOT SAVE 51 RECORD FOR CONFIRMED EDI ORDERS    *         
*                                                                     *         
* 03FEB00 (SKU) --- SPOT ZEROED OUT BIT CHANGED FROM X'20' TO X'01'   *         
*                                                                     *         
* 03MAY00 (SKU) --- VARIOUS/BRAND BUG FIX                             *         
*                                                                     *         
* 18JAN01 (HWO) --- IGNORE AGY OFFICE WHEN PRNT'N DARE CONFIRMATION   *         
*                                                                     *         
* 01FEB01 (SKU) --- FIX BUG OF NOT SAVING DARE RECORDS                *         
*                                                                     *         
* 04MAY01 (SKU) --- MAKE SURE CORRECT DARE ORDER IS RETRIEVED         *         
*                   BUG FIX IN CLEANUP ROUTINE                        *         
*                                                                     *         
* 24AUG01 (SKU) --- CLEANUP X'0B01' UPON CONFIRMATION                 *         
*                                                                     *         
* 17OCT01 (SKU) --- ADD DARE AUDIT TRAIL                              *         
*                                                                     *         
* 12DEC01 (SKU) --- PARTIAL CONFIRM BUG FIX                           *         
*                                                                     *         
* 12SEP02 (SKU) --- DARE TRAILER RECORD COUNT BUG FIX                 *         
*                                                                     *         
* 14NOV02 (HQ ) --- SKIP CONVERTING 41 TO 51 FOR PENDING CONFIRM      *         
*                                                                     *         
* 28APR04 (HQ ) --- STOP ALL ACTIONS ON NON-UPDATEABLE SYSTEM         *         
*                                                                     *         
* 24MAY04 (HQ ) --- SUPPRESS CONFIRM DARE TRANSACTION FOR XML ORDER   *         
*                                                                     *         
* 20AUG04 (HQ ) --- MERGE TV AND RADIO CODE                           *         
*                                                                     *         
* 23NOV04 (HQ ) --- FIX AM STATION CONFIRM POINTPERSON BUG            *         
*                                                                     *         
* 24FEB05 (SKU) --- REMOVE UID SUPPORT                                *         
*                                                                     *         
* 06FEB09 (SKU) --- ESPERANTO SUPPORT                                 *         
*                                                                     *         
* 29MAR10 (SKU) --- FIX DATCON CALL TO DISPLAY PRINTABLE CHARACTER    *         
***********************************************************************         
T80262   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYWORKX-MYWORKD,T80262,RR=R3                                     
         LR    R9,RC                                                            
         USING MYWORKD,R9          LOCAL WORK AREA                              
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,VTWA                                                          
         USING TWAD,RA                                                          
         L     R8,ASPULAR                                                       
         USING SPOOLD,R8                                                        
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
         ST    R8,ASPOOLD                                                       
         ST    RB,SAVERB                                                        
         ST    RD,SAVERD                                                        
         LR    R7,RA                                                            
         AH    R7,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R7                                                       
         ST    R3,RELO                                                          
*                                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL - ALLOW ACTION                  
         BE    MAIN100                                                          
         GOTO1 (RFCHKSYS,VREPFACS),DMCB,ACOMFACS                                
         BE    MAIN100                                                          
         LA    R3,991                                                           
         OI    CONCACTH+6,X'40'    FORCE CURSOR TO ACTION                       
         B     ERROR                                                            
*                                                                               
MAIN100  DS    0H                                                               
         BAS   RE,INITIAL                                                       
         BAS   RE,CONFIRM          PRINT CONFIRMATION TO EDICT/PQ               
         GOTO1 =A(UPDTCON),RR=RELO ADD SPOT COUNT INFO TO CON                   
*                                  RESET REVISION NUMBER                        
         GOTO1 =A(CLEANUP),RR=RELO DELETE DARE AGENCY ORDER RECORDS             
                                                                                
         GOTO1 =A(DOAUDIT),RR=RELO ADD AUDIT TRAIL FOR CONFIRMATION             
                                                                                
         B     BEXIT                                                            
         EJECT                                                                  
***********************************************************************         
*              INITIALIZATION                                                   
***********************************************************************         
INITIAL  NTR1                                                                   
         L     R1,AFACILS                                                       
         LM    R2,R4,8(R1)                                                      
         ST    R3,ATIA                                                          
         MVC   SCANNER(16),24(R4)                                               
         LA    R2,BOOKVAL          FIND ADDRESSES OF CORE RESIDENT              
         SR    R3,R3               MODULES WITH PHONY CALLOV READS.             
         LA    R4,17                                                            
         SPACE 2                                                                
INIT2    DS    0H                                                               
         CH    R3,=H'9'                                                         
         BE    INIT2A                                                           
         CH    R3,=H'10'                                                        
         BE    INIT2A                                                           
         CH    R3,=H'11'                                                        
         BE    INIT2A                                                           
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A00'                                           
         STC   R3,DMCB+7                                                        
         GOTO1 CALLOV,DMCB                                                      
         MVC   0(4,R2),DMCB                                                     
         LA    R2,4(R2)                                                         
INIT2A   LA    R3,1(R3)                                                         
         BCT   R4,INIT2                                                         
         SPACE 1                                                                
         MVI   DMCB+7,X'E0'        USE DEMOCON INSTEAD OF DEMCON                
         GOTO1 CALLOV,DMCB                                                      
         MVC   DEMCON,DMCB                                                      
         SPACE 1                                                                
         LA    R2,VCOMMON          SET UP COMMON ENTRIES                        
         SR    R3,R3                                                            
         LA    R4,AANY                                                          
         LA    R5,VCOUNT                                                        
         SPACE 2                                                                
INIT4    ST    R2,0(R4)            A(COMMON)                                    
         STC   R3,0(R4)            ROUTINE NUMBER                               
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,INIT4                                                         
         SPACE 1                                                                
         ST    R2,PQOPEN                                                        
         STC   R3,PQOPEN                                                        
         SPACE 2                                                                
         MVC   FILENAME,SPACES                                                  
         MVI   DMSOURCE,C'A'                                                    
         GOTO1 DATCON,DMCB,(5,0),(10,RCDATE)                                    
         SPACE 2                                                                
         MVC   SPOOLDM,DATAMGR                                                  
         MVC   SPOOLBUF,ATIA                                                    
         XC    VPRINT,VPRINT                                                    
         MVC   RCDATCON,DATCON                                                  
         MVC   RCCOMFAC,ACOMFACS                                                
         MVI   PAVMEDIA,C'T'                                                    
         MVI   ION,1                                                            
         MVC   AIO,AIO4                                                         
         MVI   DMOUTBTS,X'7D'                                                   
         MVI   DMFILE,C'R'                                                      
         MVI   FLAGS,0                                                          
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* PRINT DARE ORDER CONFIRMATION WHILE IGNORING AGENCY OFFICE                    
**********************************************************************          
CONFIRM  NTR1                                                                   
         XC    IOAREA(32),IOAREA                                                
         MVI   RAGK2TYP,RAGK2TYQ                                                
         MVC   RAGK2AGY,RCONKAGY                                                
         MVC   RAGK2AOF,RCONKAOF                                                
         MVC   RAGK2REP,RCONKREP                                                
         MVC   KEY,IOAREA                                                       
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RAGY2KEY),KEYSAVE                                          
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE!                               
         GOTO1 VGETREC,DMCB,IOAREA                                              
                                                                                
         LA    R5,KEY                                                           
         USING RDARKEY,R5                                                       
         XC    KEY,KEY                                                          
         MVI   RDARKTYP,X'41'                                                   
         MVC   RDARKREP,REPALPHA                                                
         MVC   RDARKSTA(5),RCONKSTA                                             
         CLI   RDARKSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   RDARKSTA+4,C'T'     MUST SPECIFY IF TV                           
*CHANGE* OC    RDARKSTA,MYSPACES                                                
         OC    RDARKSTA,=CL6' '                                                 
         OC    RAGY2DAR,RAGY2DAR   NULL EQUIVALENCY CODE?                       
         BZ    CONF14                                                           
         MVC   RDARKAGY,RAGY2DAR   EQUIVALENCY CODE                             
                                                                                
         LA    R6,RCONREC                                                       
         USING RCONDREL,R6                                                      
         MVI   ELCODE,X'1D'        GET DARE ELEMENT                             
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,RAGY2DAR         THERE ARE MAX 4 AGENCY ASSIGNMENT            
         LA    R3,4                COMBINATIONS WE NEED TO CHECK                
         B     CONF10                                                           
*                                                                               
** PREP KEY FOR SKIP READING: SKIP TO NEXT AGENCY OFFICE IF AGENCY              
** OFFICE DIDN'T CHANGE                                                         
*                                                                               
PRVKEY   USING RDARKEY,KEYSAVE                                                  
CONF08   CLC   RDARKAOF,PRVKEY.RDARKAOF  SAME AGENCY OFFICE?                    
         DROP  PRVKEY                                                           
         BNE   CONF09                                                           
         XR    R0,R0                                                            
         ICM   R0,3,RDARKAOF                                                    
         AHI   R0,1                   INCREMENT AGENCY OFFICE FIELD             
         STCM  R0,3,RDARKAOF                                                    
CONF09   XC    RDARKORD(7),RDARKORD  CLEAR FELDS AFTER AGENCY OFFICE            
*                                                                               
CONF10   DS    0H                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(RDARKAOF-RDARKEY),KEYSAVE   SAME KEY?                        
         BNE   CONF11                                                           
         XC    RDARKORD(7),RDARKORD  CLEAR FIELDS AFTER AGENCY OFFICE           
         MVC   RDARKORD,RCONDRLK     MOVE IN ORDER # FOR RDHI                   
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(RDARKAOF-RDARKEY),KEYSAVE   SAME KEY?                        
         BNE   CONF11                                                           
         CLC   RDARKORD,RCONDRLK     SAME ORDER NUMBER?                         
         BNE   CONF08                NO -- SKIP READ                            
         CLI   RDARKRT,X'10'         AGENCY HEADER?                             
         BE    CONF20                YES -- DARE RECORD BUILT...                
         B     CONF14                                                           
         DROP  R6                                                               
*                                                                               
CONF11   CLI   RAGY2FXL,RAGY2FLQ   ONLY NEWER AGENCY RECORDS                    
         BNL   *+6                 HAS MULTI-DARE AGENCY ASSIGNMENTS            
         DC    H'0'                                                             
         MVC   KEY,KEYSAVE                                                      
*                                                                               
CONF12   LA    R4,5(R4)                                                         
         OC    0(3,R4),0(R4)         NULL EQUIVALENCY CODE?                     
         BZ    CONF14                YES                                        
         CLC   RDARKAGY,0(R4)        SAME EQUIVALENCY CODE?                     
         BNE   *+12                                                             
         BCT   R3,CONF12             CHECK NEXT EQUIVALENCY CODE                
         B     CONF14                                                           
*                                                                               
         MVC   RDARKAGY,0(R4)      EQUIVALENCY CODE                             
         XC    RDARKAOF(9),RDARKAOF  CLEAR FIELDS AFTER AGENCY CODE             
         BCT   R3,CONF10                                                        
*                                  SPECIAL FOR SELTEL                           
CONF14   CLC   =C'SZ',REPALPHA                                                  
         BNE   CONF15                                                           
         CLC   =C'1342  ',RAGK2AGY AND AGENCY 1342 ONLY                         
         BNE   CONF15                                                           
         MVC   RDARKAGY(5),=C'ED2DE'                                            
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BE    CONF20                                                           
         DROP  R5                                                               
*                                                                               
* INSTEAD OF ABEND, JUST EXIT PROGRAM. THE CONTRACT MIGHT HAVE BEEN             
* IN DARE BUT NOT THIS MODIFICATION VERSION                                     
*                                                                               
CONF15   DS    0H                                                               
         CLI   RCONKSTA+4,C'F'     DO NOT ABEND FOR RADIO                       
         BE    CONF18                                                           
         CLI   RCONKSTA+4,C'A'                                                  
         BE    CONF18                                                           
*                                                                               
         DC    H'0'                ABEND FOR TV                                 
*                                                                               
CONF18   DS    0H                                                               
         MVI   SPMODE,X'FF'                                                     
         GOTO1 SPOOL,PARAS,(R8)    CLOSE THE PQ                                 
         XMOD1 2                   EXIT THIS OVERLAY                            
*                                                                               
CONF20   DS    0H                                                               
         MVC   TWASVDKY,KEY        SAVE OFF FOR WHEN WE DELETE                  
         LA    R6,RCONREC                                                       
         USING RCONDREL,R6                                                      
         MVI   ELCODE,X'1D'        GET DARE ELEMENT                             
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    RCONDRFG,X'04'+X'02' KATZ/EDI DARE ORDER? OR ONE SHOT ?          
         BZ    CONF25                                                           
         OI    FLAGS,X'80'         FLAG ORDER AS EDI                            
         B     CONFX               YES, SKIP SENDING CONFIRMATION TO PQ         
         DROP  R6                                                               
*                                                                               
CONF25   DS    0H                                                               
         LA    R6,RCONREC                                                       
         USING RCONSEND,R6                                                      
         MVI   ELCODE,X'20'        GET SEND ELEMENT                             
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   VERSION,RCONSRV                                                  
         DROP  R6                                                               
*                                                                               
CONF28   DS    0H                                                               
         L     R6,AIO4                                                          
         GOTO1 VGETREC,DMCB,(R6)   USE IO4                                      
         USING RDARREC,R6                                                       
*                                                                               
*        MVI   ELCODE,X'0F'        XML                                          
*        BRAS  RE,GETEL                                                         
*        BNE   CONF29                                                           
*        TM    RDARFLG2-RDARFLEM(R6),X'80'                                      
*        BO    CONFX               XML - SKIP ORDCFM DARE TRANSACTION           
CONF29   DS    0H                                                               
         L     R6,AIO4                                                          
*                                                                               
         CLC   RDARREP#,RCONKCON   RETRIEVED DARE RECORD MUST BE LINKED         
         BNE   CONF11              TO CORRECT CONTRACT NUMBER                   
*                                                                               
         MVC   REVNUM,RDARRNUM     SAVE OFF REVISION NUMBER                     
*                                                                               
         GOTO1 PQOPEN                                                           
         EJECT                                                                  
* PRINT EDICT HEADER CARD                                                       
         MVC   P+4(5),=C'*HDR*'                                                 
         MVC   P+9(14),=C'EDICT=*DDSDARR'                                       
         MVI   P+34,C'W'           WIDE REPORT - 132 CHARS                      
         BAS   RE,PRINT                                                         
                                                                                
* PRINT A ++DDS CARD                                                            
         LA    R5,P                                                             
         USING EDICTD,R5                                                        
         MVC   EDIDDSID,=C'++DDS'                                               
         MVI   EDISYST,C'D'                                                     
         MVC   EDIPROG,=C'CNF'     TYPE=CONFIRM                                 
         MVC   EDIIDEN,=C'TRN'                                                  
                                                                                
* INFORMATIONAL CHUNK FOR EDICT REPORTING                                       
                                                                                
         MVC   EDIRDRRP,RDARKREP   REP CODE                                     
         MVC   EDIRDRAG,RDARKAGY   AGENCY CODE                                  
         MVC   EDIRDRST,RDARKSTA   STATION CODE                                 
                                                                                
CONF30   DS    0H                                                               
         MVC   EDIRDRMD,RDARMEDI   MEDIA CODE                                   
                                                                                
         EDIT  RDAREST#,(3,EDIRDRES),ALIGN=LEFT                                 
                                                                                
* AGENCY ORDER #                                                                
         ZAP   WORK+17(5),=P'0'                                                 
         MVO   WORK+17(5),RDARKORD                                              
         EDIT  (P5,WORK+17),(8,EDIRDRAN),ALIGN=LEFT                             
                                                                                
* CONTRACT #                                                                    
         OC    RDARREP#,RDARREP#                                                
         BZ    CONF40                                                           
         ZAP   WORK+17(5),=P'0'                                                 
         MVO   WORK+17(5),RDARREP#                                              
         EDIT  (P5,WORK+17),(8,EDIRDRCN),ALIGN=LEFT                             
                                                                                
         MVC   EDIRDRSP,RCONSAL    SALESMAN CODE                                
                                                                                
CONF40   DS    0H                                                               
         MVC   EDIRDRBY,RDARBUYC   BUYER CODE                                   
         DROP  R6                                                               
                                                                                
         MVI   ELCODE,X'02'        DESCRIPTIVE ELEMENT #2                       
         BRAS  RE,GETEL                                                         
         BNE   CONF50                                                           
         USING RDARCLEM,R6                                                      
         MVC   EDIRDRCL,RDARCLI    CLIENT CODE                                  
         MVC   EDIRDRP1,RDARPRD1   PRODUCT CODE 1                               
         MVC   EDIRDRP2,RDARPRD2   PRODUCT CODE 2                               
         DROP  R5,R6                                                            
                                                                                
CONF50   DS    0H                                                               
         BAS   RE,PRINT                                                         
         EJECT                                                                  
* ORDER CONFIRMATION LINE                                                       
         L     R6,AIO4                                                          
         USING RDARREC,R6                                                       
                                                                                
         GOTO1 HEXOUT,DMCB,RDARKORD,TWAGORDN,4,=C'TOG'                          
                                                                                
         LA    R3,2                COUNT OUTPUT LINES START AT 2 FOR            
*                                    HEADER + TRAILER LINES                     
                                                                                
         LA    R5,P                                                             
         USING RORDCFMD,R5                                                      
         MVC   ROCFTID,=C'ORDCFM'                                               
         MVC   ROCFORDR,TWAGORDN   AGENCY ORDER NUMBER                          
         MVC   ROCFFRID,RDARRCVR   ID OF SENDER                                 
         MVC   ROCFTOID,RDARSNDR   ID OF RECEIVER                               
         GOTO1 DATCON,DMCB,(5,WORK),(X'20',ROCFDATE)                            
*                                                                               
         ZAP   WORK(4),=P'0'                                                    
         THMS  DDSTIME=YES                                                      
         ST    R0,WORK             ACTUAL TIME ADJUSTMENT                       
         ST    R1,DUB              DDS TIME                                     
         AP    WORK(4),DUB(4)                                                   
*                                                                               
         CP    WORK(4),=P'240000'  PAST MIDNIGHT?                               
         BL    CONF60                                                           
         SP    WORK(4),=P'240000'  YES, BUMP TO NEXT DAY AND ADJUST             
         GOTO1 ADDAY,DMCB,ROCFDATE,(X'20',ROCFDATE),F'1'                        
*                                                                               
CONF60   DS    0H                                                               
         ICM   R1,15,WORK                                                       
         SRL   R1,12               SHIFT OFF SECONDS AND SIGN                   
         STH   R1,HALF                                                          
         GOTO1 HEXOUT,DMCB,HALF,ROCFTIME,2,0                                    
*                                                                               
         MVC   ROCFQSTA,RDARKSTA   STATION                                      
         CLI   ROCFQSTA+4,C'L'     IS IT LOW POWER?                             
         BE    CONF70              YES - LEAVE AS IS                            
         MVI   ROCFQSTA+5,C'V'     INSERT LAST CHAR OF MEDIA                    
         CLI   ROCFQSTA+4,C'T'     IS IT A TV STATION                           
         BE    CONF70              YES - LEAVE AS IS                            
         MVI   ROCFQSTA+5,C'M'     NO INSERT RADIO MEDIA                        
*                                                                               
* CONTRACT NUMBER                                                               
*                                                                               
CONF70   DS    0H                                                               
         GOTO1 HEXOUT,DMCB,RDARREP#,ROCFRPCN,4,=C'TOG'                          
*                                                                               
* RETURN TO SENDER INFO                                                         
         MVC   ROCFRTRN,RDARRTS                                                 
         DROP  R5                                                               
                                                                                
         BAS   RE,PRINT                                                         
*                                                                               
         CLI   RCONKSTA+4,C'A'                                                  
         BE    CONF72                                                           
         CLI   RCONKSTA+4,C'F'                                                  
         BNE   CONF75              TV - SKIP                                    
*                                                                               
CONF72   GOTO1 =A(ORDSAL),RR=RELO                                               
*                                                                               
* CONFIRM WITH COMMENTS, IF ANY                                                 
CONF75   DS    0H                                                               
         LA    R6,IOAREA                                                        
         USING RCFCREC,R6                                                       
         XC    IOAREA(32),IOAREA                                                
         MVI   RCFCKTYP,RCFCKTYQ                                                
         MVC   RCFCKREP,REPALPHA                                                
         MVC   RCFCKCON,RCONKCON                                                
         MVC   KEY,IOAREA                                                       
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RCFCKEY),KEYSAVE                                           
         BNE   CONF120                                                          
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,IOAREA                                              
*                                                                               
         LA    R5,P                                                             
         USING RORDCOMD,R5                                                      
         MVC   ROCMTID,=C'ORDCOM'                                               
         MVC   ROCMORDR,TWAGORDN                                                
         MVC   ROCMBLIN,=C'0000'   GLOBAL COMMENT                               
         TM    RCFCIFLG,X'80'                                                   
         BZ    CONF80                                                           
         MVC   ROCMTEXT(L'MGOYMSG),MGOYMSG                                      
         B     CONF90                                                           
         DROP  R6                                                               
*                                                                               
CONF80   DS    0H                                                               
         MVC   ROCMTEXT(L'MGONMSG),MGONMSG                                      
*                                                                               
CONF90   DS    0H                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BNE   CONF95                                                           
         USING RCFCIEL,R6                                                       
         CLC   RCFCIVER,RCFCIDVR   SKIP IF WE'VE SENT THIS ALREADY              
         BNH   CONF120                                                          
         MVC   RCFCIDVR,VERSION    SET VERSION NUMBER SO WE DON'T               
*                                  SEND THIS AGAIN ON NEXT CONFIRM              
CONF95   DS    0H                                                               
         LA    R6,IOAREA                                                        
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BE    CONF100                                                          
         BAS   RE,PRINT                                                         
         LA    R3,1(R3)            BUMP COUNTER                                 
         B     CONF120                                                          
*                                                                               
CONF100  DS    0H                                                               
         MVI   ROCMCONT,C'*'                                                    
         BAS   RE,PRINT                                                         
         LA    R3,1(R3)            BUMP COUNTER                                 
*                                                                               
         MVC   ROCMTID,=C'ORDCOM'                                               
         MVC   ROCMORDR,TWAGORDN                                                
         MVC   ROCMBLIN,=C'0000'   GLOBAL COMMENT                               
         USING RCFCTEL,R6                                                       
         CLI   RCFCTLEN,3          MIN LENGTH                                   
         BL    CONF110                                                          
         ZIC   R1,RCFCTLEN                                                      
         CLI   RCFCTLEN,73                                                      
         BL    *+8                                                              
         LA    R1,72               MAX LENGTH                                   
*                                                                               
         SH    R1,=H'3'            OVERHEAD + 1                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ROCMTEXT(0),RCFCTEXT                                             
*                                                                               
CONF110  DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         BE    CONF100                                                          
         BAS   RE,PRINT                                                         
         LA    R3,1(R3)            BUMP COUNTER                                 
*                                  UPDATE CFC REC WITH NEW VERSION NUM          
         GOTO1 VPUTREC,DMCB,IOAREA                                              
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* ORDER CONFIRMATION LINE NUMBER EQUIVALENTS LINE                               
*                                                                               
CONF120  DS    0H                                                               
         GOTO1 =A(BUILDBUY),RR=RELO BUILD BUY ARRAY IN IO4                      
         L     R6,AIO4                                                          
                                                                                
         OC    0(2,R6),0(R6)       CHECK IF NO BUYS                             
         BZ    CONF160                                                          
                                                                                
         LA    R5,P                                                             
         USING RORDLIND,R5                                                      
         XC    P,P                 CLEAR PRINT LINE                             
                                                                                
CONF130  DS    0H                                                               
         MVC   ROLNTID,=C'ORDLIN'                                               
         MVC   ROLNORDR,TWAGORDN                                                
                                                                                
* CONSTRUCT A LINE OF AGENCY BUY AND REP BUY LINE NUMBERS                       
         EDIT  (1,0(R6)),(4,ROLNBLIN),FILL=0        AGENCY                      
         LA    R2,ROLNRLIN                                                      
                                                                                
CONF140  DS    0H                  REP BUY LINE NUMBERS                         
         EDIT  (1,1(R6)),(4,0(R2)),FILL=0                                       
         CLC   0(1,R6),2(R6)                                                    
         BNE   CONF150                                                          
         LA    R2,L'ROLNBLIN(R2)                                                
         LA    RF,ROLNRLIN+L'ROLNRLIN                                           
         CR    R2,RF                                                            
         BNL   CONF150                                                          
         LA    R6,2(R6)                                                         
         B     CONF140                                                          
                                                                                
CONF150  DS    0H                                                               
         LA    R3,1(R3)            ONE MORE FOR COUNTER                         
         OC    2(2,R6),2(R6)       CHECK IF NO MORE                             
         BZ    CONF160                                                          
         MVI   ROLNCONT,C'*'       ELSE SIGNFY MORE TO COME                     
         BAS   RE,PRINT                                                         
         LA    R6,2(R6)                                                         
         B     CONF130                                                          
                                                                                
CONF160  DS    0H                                                               
         BAS   RE,PRINT                                                         
         DROP  R5                                                               
         EJECT                                                                  
* ORDER TRAILER LINE                                                            
CONF170  DS    0H                                                               
         LA    R5,P                                                             
         USING RORDTLRD,R5                                                      
         MVC   ROTRTID,=C'ORDTLR'                                               
         MVC   ROTRORDR,TWAGORDN   AGENCY ORDER NUMBER                          
         EDIT  (R3),(6,ROTRRCCT),FILL=0                                         
         BAS   RE,PRINT                                                         
         DROP  R5                                                               
                                                                                
CONFCLOS DS    0H                                                               
         MVI   SPMODE,X'FF'                                                     
         GOTO1 SPOOL,PARAS,(R8)    CLOSE THE PQ                                 
                                                                                
CONFX    DS    0H                                                               
         B     XIT                                                              
*********************************************************************           
* PRINT A LINE                                                                  
*********************************************************************           
PRINT    NTR1                                                                   
         GOTO1 SPOOL,PARAS,(R8)                                                 
         MVI   LINE,2              FORCE EVERYTHING TO PAGE ONE                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES ENTERABLE FROM BASE OR OVERLAY                          
         SPACE 3                                                                
VCOMMON  NTR1  BASE=SAVERB                                                      
         SRL   RF,24                                                            
         B     VBRANCH(RF)                                                      
         SPACE 2                                                                
VBRANCH  B     BANY                                                             
         B     BHIGH                                                            
         B     BSEQ                                                             
         B     BREAD                                                            
         B     BGETREC                                                          
         SPACE 1                                                                
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         SPACE 1                                                                
         B     VPQOPEN                                                          
         SPACE 2                                                                
BANY     B     XIT                 SPARE ROUTINE                                
         SPACE 2                                                                
BANY2    MVC   WORK,SPACES                                                      
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   WORK(0),8(R2)                                                    
         EJECT                                                                  
BHIGH    MVC   COMMAND,=C'DMRDHI'                                               
         MVI   DMCOM,C'H'                                                       
         MVC   KEYSAVE,KEY                                                      
         B     VDIR                                                             
         SPACE 2                                                                
BSEQ     MVC   COMMAND,=C'DMRSEQ'                                               
         MVI   DMCOM,C'S'                                                       
         B     VDIR                                                             
         SPACE 2                                                                
BREAD    MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         MVI   DMCOM,C'R'                                                       
         SPACE 2                                                                
VDIR     MVC   FILENAME(6),=C'DEMFIL'                                           
         CLI   DMFILE,C'M'                                                      
         BNE   *+10                                                             
         MVC   FILENAME(6),=C'DEMDIR'                                           
         CLI   KEY,C'A'                                                         
         BE    *+10                                                             
         MVC   FILENAME(3),=C'PAV'                                              
         CLI   DMFILE,C'R'                                                      
         BNE   VALL                                                             
         MVC   FILENAME(6),=C'REPDIR'                                           
         CLI   KEY,X'12'           USE PARENT REP ON INVENTORY                  
         BNE   VALL                                                             
         MVC   KEY+10(2),INTREPNO                                               
         MVC   KEYSAVE+10(2),INTREPNO                                           
         SPACE 2                                                                
VALL     ZIC   R3,ION              1,2,3                                        
         BCTR  R3,0                0,1,2                                        
         SLL   R3,2                0,4,8                                        
         L     R3,AIO(R3)                                                       
         ICM   RF,15,=AL4(CONLENQ)                                              
         XCEF  0(R3),(RF)                                                       
         CLI   DMFILE,C'D'         FOR DEMFIL                                   
         BNE   *+16                                                             
         MVC   0(20,R3),KEY        PUT WHOLE KEY INTO I/O                       
         MVC   KEY(4),DAMAJOR      AND D/A OF MAJOR INTO KEY                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),                         X        
               FILENAME,KEY,(R3),(TERMNAL,0)                                    
         MVC   KEY,0(R3)                                                        
         CLI   DMFILE,C'M'                                                      
         BNE   *+10                                                             
         MVC   DAMAJOR,KEY+19                                                   
         CLI   DMCOM,C'R'                                                       
         BNE   DMCHECK                                                          
         CLC   KEY(18),KEYSAVE                                                  
         BE    *+8                                                              
         OI    DMCB+8,X'10'                                                     
         CLI   DMFILE,C'M'                                                      
         BE    DMCHECK                                                          
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+8                                                              
         OI    DMCB+8,X'10'                                                     
         CLI   DMFILE,C'R'                                                      
         BNE   DMCHECK                                                          
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+8                                                              
         OI    DMCB+8,X'10'                                                     
         B     DMCHECK                                                          
         SPACE 2                                                                
BGETREC  ZIC   R3,ION                                                           
         BCTR  R3,0                                                             
         SLL   R3,2                                                             
         L     R3,AIO(R3)                                                       
         GOTO1 DATAMGR,DMCB,(DMINBTS,=C'GETREC'),                      X        
               =C'REPFILE',KEY+28,(R3),(TERMNAL,DMWORK)                         
         SPACE 2                                                                
DMCHECK  MVC   DUB(1),DMCB+8                                                    
         NC    DUB(1),DMOUTBTS                                                  
         BZ    XIT                                                              
BEXIT    L     RD,SAVERD                                                        
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* OPEN THE PRINT QUEUE                                                          
**********************************************************************          
VPQOPEN  XC    SPOOLKEY,SPOOLKEY                                                
         OI    SPOOLIND,SPUINIT ALLOW USER VALUES-CLASS,LPP,COPIES              
         LA    R3,SPOOLKEY                                                      
         USING PQPLD,R3                                                         
         MVC   PLSUBID,=C'DCF'                                                  
         MVC   PLDESC(2),=C'DC'                                                 
         MVC   PLDESC+3(8),CONCNUM                                              
         MVI   PLCLASS,C'G'                                                     
         DROP  R3                                                               
                                                                                
VPQ20    DS    0H                                                               
         LA    RE,TWASPKEY                                                      
         ST    RE,SPOOLQLK         SET EXTENDED KEY ADDRESS                     
         USING PQPLD,RE                                                         
         XC    0(133,RE),0(RE)                                                  
         MVI   QLEXTRA,X'FF'                                                    
         MVC   QLRETNL,=H'6'                                                    
         MVC   QLRETND,=H'2'                                                    
         MVC   QLRETNL,=H'36'      ORDER WORKSHEETS                             
         MVI   QLSYS,C'R'          FORM/COPIES COLUMN                           
         MVC   QLPRG,=C'CO'                                                     
         DROP  RE                                                               
         GOTO1 SPOOL,PARAS,(R8)                                                 
         MVC   SPOOLRPN,SPOOLKEY+19                                             
         B     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RECFCMSG                                                       
       ++INCLUDE RECNTWR2K                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
       ++INCLUDE SPDARDARED                                                     
         EJECT                                                                  
       ++INCLUDE REGENDAR                                                       
EDICTD   DSECT                                                                  
       ++INCLUDE EDIDDSHD                                                       
         EJECT                                                                  
       ++INCLUDE EDILINKD                                                       
         EJECT                                                                  
         PRINT ON                                                               
*                                                                               
* LOCAL VARIABLES                                                               
*                                                                               
MYWORKD  DSECT                                                                  
RELO     DS    A                                                                
FLAGS    DS    X                   X'80' = ORDER IS EDI                         
REVNUM   DS    X                   REVISION NUMBER                              
VERSION  DS    X                   CURRENT CONTRACT VERSION                     
MYWRK    DS    CL27                                                             
HDRDA    DS    XL4                                                              
PASAREA  DS    XL1600                                                           
*                                  X'40' = CONFIRMED VARIOUS ORDER              
MYWORKX  EQU   *                                                                
         EJECT                                                                  
CFCD     DSECT                                                                  
       ++INCLUDE REGENCFC                                                       
         PRINT ON                                                               
T80262   CSECT                                                                  
         DROP  RB                                                               
*********************************************************************           
* READ CONTRACT BUY RECORDS AND BUILD BUY ARRAY                                 
* 255 X 2 BYTES                                                                 
* BYTE 1 = AGENCY BUY LINE NUMBER                                               
* BYTE 2 = CONTRACT BUY LINE NUMBER                                             
*********************************************************************           
BUILDBUY NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO4                                                          
         ICM   RF,15,=AL4(CONLENQ)                                              
         XCEF  0(R6),(RF)          CLEAR SORT AREA                              
         SR    R5,R5               SORT RECORD COUNTER                          
                                                                                
         XC    RBUYKEY,RBUYKEY                                                  
         MVI   RBUYKTYP,X'0B'                                                   
         MVC   RBUYKREP,REPALPHA                                                
         MVC   RBUYKCON,TWACNUM                                                 
         MVC   KEY,RBUYKEY                                                      
                                                                                
         GOTO1 VHIGH                                                            
         CLC   KEY(22),KEYSAVE                                                  
         BNE   BUILDX                                                           
                                                                                
BUILD10  DS    0H                                                               
         GOTO1 VGETREC,DMCB,RBUYREC                                             
         OC    RBUYTSPT,RBUYTSPT   BUY HAS ZERO SPOTS                           
         BNZ   BUILD20                                                          
         TM    RBUYRTS,X'01'+X'20' BUY WAS ZEROED OUT, SKIP                     
         BNZ   BUILDSEQ                                                         
                                                                                
BUILD20  DS    0H                                                               
         OC    RBUYAGBL,RBUYAGBL                                                
         BZ    BUILDSEQ                                                         
         MVC   0(1,R6),RBUYAGBL    AGENCY ORDER BUY LINE                        
         MVC   1(1,R6),RBUYKLIN    REP BUY LINE                                 
         LA    R6,2(R6)                                                         
         LA    R5,1(R5)            # OF RECORDS                                 
                                                                                
BUILDSEQ DS    0H                                                               
         GOTO1 VSEQ                                                             
         CLC   KEY(22),KEYSAVE                                                  
         BE    BUILD10                                                          
                                                                                
         L     R6,AIO4                                                          
         GOTO1 XSORT,DMCB,(R6),(R5),2,1,0                                       
                                                                                
BUILDX   DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* RESET DARE REVISION NUMBER IN X'1D'                                           
* ADD X'2D' SPOT COUNT ELEMENT                                                  
***********************************************************************         
UPDTCON  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'1D'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RCONDREL,R6                                                      
         MVI   RCONDRRV,0          RESET REVISION NUMBER                        
         DROP  R6                                                               
*                                                                               
UPC10    DS    0H                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'2D'        ADD ONLY ONE SPOT COUNT ELEMENT              
         BRAS  RE,GETEL            FOR THE FIRST CONFIRMATION                   
         BE    UPCX                                                             
*                                                                               
         XC    KEY,KEY                                                          
KEYD     USING RDARKEY,KEY                                                      
         MVC   KEY(RDARKRT-RDARKEY),TWASVDKY                                    
         MVI   KEYD.RDARKRT,X'50'  GET TRAILER RECORD                           
         DROP  KEYD                                                             
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   UPCX                                                             
*                                                                               
         L     R6,AIO4                                                          
         USING RDARREC,R6                                                       
         GOTO1 VGETREC,DMCB,(R6)   USE IO4                                      
*                                                                               
         XC    TWAELEM,TWAELEM                                                  
WKD      USING RCON2DEL,TWAELEM                                                 
*                                                                               
         MVI   WKD.RCON2DCD,X'2D'                                               
         MVI   WKD.RCON2DLN,RCON2DLQ                                            
         GOTO1 DATCON,DMCB,(5,0),(2,WKD.RCON2DDT)                               
         MVC   WKD.RCON2DTS,RDARTSPT                                            
         MVC   WKD.RCON2DTD,RDARTDOL                                            
         DROP  WKD,R6                                                           
*                                                                               
         GOTO1 VADDELEM,DMCB,RCONREC,TWAELEM                                    
*                                                                               
UPCX     DS    0H                                                               
         MVI   UPDATE,C'Y'                                                      
         MVC   KEY+28,TWAKADDR                                                  
         GOTO1 VGETREC,DMCB,AIO4                                                
         GOTO1 VPUTREC,DMCB,RCONREC                                             
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DELETE CORRESPONDING DARE AGENCY RECORDS (HEADER/BUY/TRAILER)                 
*                                                                               
* READ X'41' HEADER REC AND ADD BACK AS X'51' TO PRESERVE ROUTING               
* INFO, ETC. FOR FUTURE MAKEGOOD OFFERS                                         
***********************************************************************         
CLEANUP  NTR1  BASE=*,LABEL=*                                                   
         TM    TWADARE,X'20'        PENDING ?                                   
         BO    CLEANUPX             YES, DO NOT CONVERT 41 TO 51                
*                                                                               
         XCEFL PASAREA,1600                                                     
         XC    HDRDA,HDRDA                                                      
*                                                                               
         L     R6,AIO4                                                          
         USING RDARREC,R6                                                       
*                                                                               
         MVC   KEY(L'TWASVDKY),TWASVDKY  DELETE ALL OLD 51 RECORDS              
         MVI   KEY,X'51'                                                        
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
*                                                                               
CLEAN02  DS    0H                                                               
         CLC   KEY(RDARKRT-RDARKEY),KEYSAVE                                     
         BNE   CLEAN03             DONE                                         
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,(R6)                                                
*                                                                               
         CLI   KEY+RDARKRT-RDARKEY,X'10'                                        
         BNE   CLEAN02A                                                         
         BAS   RE,DELPAS           DELETE PASSIVE PTS                           
*                                                                               
CLEAN02A DS    0H                                                               
         OI    RDARCNTL,X'80'                                                   
         GOTO1 VPUTREC,DMCB,(R6)                                                
         OI    KEY+27,X'80'                                                     
         GOTO1 VWRITE                                                           
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VSEQ                                                             
         B     CLEAN02                                                          
*                                                                               
CLEAN03  DS    0H                                                               
         MVC   KEY(L'TWASVDKY),TWASVDKY                                         
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* ADD HEADER RECORD BACK AS X'51'                                               
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,(R6)   USE IO4                                      
*                                                                               
         TM    RDARMISC,X'10'      IF VARIOUS, MARK CONFIRMED/BRAND             
         BZ    CLEAN05             SO USER WILL EXPECT BRAND ORDERS             
         OI    RDARMISC,X'04'                                                   
         OI    FLAGS,X'40'                                                      
         GOTO1 VPUTREC,DMCB,(R6)   USE IO4                                      
         B     CLEAN30                                                          
*                                                                               
* IF BRAND, NEED TO GET VARIOUS ORDER AND FLAG THIS BRAND CONFIRMED             
*                                                                               
CLEAN05  DS    0H                                                               
         TM    RDARMISC,X'08'                                                   
         BZ    CLEAN30                                                          
         MVI   ELCODE,X'0B'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING RDARBREM,R6                                                      
         MVC   KEY(RDARKORD-RDARKEY),TWASVDKY                                   
         MVC   KEY+20(4),RDARBRVN                                               
         MVI   KEY+24,X'35'                                                     
         GOTO1 VHIGH                                                            
         DROP  R6                                                               
*                                                                               
         USING RDARREC,R6                                                       
CLEAN10  DS    0H                                                               
         CLC   KEY(RDARKSEQ-RDARKEY),KEYSAVE                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,(R6)   USE IO4                                      
         CLC   RDARPDON,TWASVDKY+20                                             
         BE    CLEAN20                                                          
         GOTO1 VSEQ                                                             
         B     CLEAN10                                                          
*                                                                               
CLEAN20  DS    0H                                                               
         OI    RDARPDFG,X'80'      BRAND ORDER IS CONFIRMED                     
         GOTO1 VPUTREC,DMCB,(R6)                                                
         DROP  R6                                                               
*                                                                               
* CHECK ALL VARIOUS BRAND RECORDS '35' AND SEE IF ALL BRANDS ARE INDEED         
* CONFIRMED. IF SO, FLAG THE VARIOUS RECORD. THIS WILL ALLOW THE                
* BRAND CONTRACTS TO BE ABLE TO CREATE MAKEGOOD OFFERS                          
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(RDARKRT-RDARKEY),0(R6)                                       
         MVI   KEY+24,X'35'                                                     
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(RDARKSEQ-RDARKEY),KEYSAVE                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CLEAN25  DS    0H                                                               
         USING RDARREC,R6                                                       
         MVI   UPDATE,C'N'                                                      
         GOTO1 VGETREC,DMCB,(R6)   USE IO4                                      
         TM    RDARPDFG,X'80'      IS THIS BRAND CONFIRMED?                     
         BZ    CLEAN28                                                          
         GOTO1 VSEQ                                                             
         CLC   KEY(RDARKSEQ-RDARKEY),KEYSAVE                                    
         BE    CLEAN25                                                          
         DROP  R6                                                               
*                                                                               
CLEAN27  DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(RDARKRT-RDARKEY),0(R6)                                       
         MVI   KEY+24,X'10'                                                     
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(RDARKSEQ-RDARKEY),KEYSAVE                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING RDARREC,R6                                                       
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,(R6)   USE IO4                                      
         OI    RDARMISC,X'02'      MARK ALL BRANDS CONFIRMED                    
         GOTO1 VPUTREC,DMCB,(R6)                                                
         DROP  R6                                                               
*                                                                               
CLEAN28  DS    0H                                                               
         MVC   KEY(L'TWASVDKY),TWASVDKY                                         
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CLEAN30  DS    0H                                                               
         L     R6,AIO4                                                          
         USING RDARREC,R6                                                       
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,(R6)   USE IO4                                      
*                                                                               
*        TM    RDARMISC,X'04'      DON'T CLEANUP FOR CONFIRMED VARIOUS          
         TM    FLAGS,X'40'         DON'T CLEANUP FOR CONFIRMED VARIOUS          
         BO    CLEAN35                                                          
*                                                                               
         CLI   KEY+RDARKRT-RDARKEY,X'10'                                        
         BNE   CLEAN30A                                                         
*        MVC   HDRDA,KEY+28                                                     
*                                                                               
*   PULL OLD KEYS PRE-P/P-S/P CHANGE:                                           
*        PASAREA: KEY BUILD AREA                                                
*        AIO4:  CURRENT LOCATION OF AGENCY ORDER RECORD                         
*        AIO2:  IO AREA                                                         
*                                                                               
         GOTO1 (RFGENDTR,VREPFACS),DMCB,(X'41',ACOMFACS),PASAREA,AIO4, X        
               AIO2                                                             
                                                                                
*                                                                               
CLEAN30A DS    0H                                                               
         OI    RDARCNTL,X'80'      DELETE 41 RECORD                             
         GOTO1 VPUTREC,DMCB,(R6)                                                
         MVI   KEY+27,X'80'                                                     
         GOTO1 VWRITE              DELETE 41 KEY                                
*                                                                               
CLEAN35  DS    0H                                                               
         BAS   RE,CHKDAREC                                                      
*        TM    RDARDELS,X'80'+X'40' HARD OR SOFT DELETE?                        
         BNZ   CLEAN60             YES - DON'T WRITE 51 REC                     
*                                                                               
         TM    FLAGS,X'80'         EDI ORDER?                                   
         BNZ   CLEAN60             YES - DON'T WRITE 51 REC                     
*                                                                               
         MVI   RDARKTYP,X'51'      REWRITE HEADER AS X'51'                      
         NI    RDARCNTL,X'FF'-X'80'                                             
*                                                                               
         CLI   RDARKRT,X'10'       HEADER RECORD?                               
         BNE   CLEAN45                                                          
         MVI   ELCODE,X'0F'                                                     
         BRAS  RE,GETEL                                                         
         BNE   *+8                                                              
         USING RDARFLEM,R6                                                      
         NI    RDARFLG1,X'FF'-X'02' DO GENERATE PASSIVES                        
         DROP  R6                                                               
         L     R6,AIO4             RESET                                        
         USING RDARREC,R6                                                       
*                                                                               
*   PULL NEW KEYS PRE-P/P-S/P CHANGE:                                           
*        PASAREA:   KEY BUILD AREA                                              
*        AIO4:  CURRENT LOCATION OF AGENCY ORDER RECORD                         
*        AIO2:  IO AREA                                                         
*                                                                               
         LA    R4,PASAREA          SET A(KEY BUILD AREA)                        
         A     R4,=F'800'          ADD 800 TO ADDRESS                           
         GOTO1 (RFGENDTR,VREPFACS),DMCB,(X'41',ACOMFACS),(R4),AIO4,    X        
               AIO2                                                             
*                                                                               
         TM    RDARMISC,X'10'      IF VARIOUS, MARK CONFIRMED/BRAND             
         BZ    CLEAN40             SO USER WILL EXPECT BRAND ORDERS             
         OI    RDARMISC,X'04'                                                   
         DROP  R6                                                               
*                                                                               
* ADD ACTIVITY ELEMENT                                                          
*                                                                               
CLEAN40  DS    0H                                                               
         XC    TWAELEM,TWAELEM                                                  
         LA    R5,TWAELEM                                                       
         USING RDARCFEM,R5                                                      
         MVI   RDARCFCD,X'F1'                                                   
         MVI   RDARCFLN,RDARCFLQ                                                
*                                                                               
* GET TODAY'S DATE AND TIME                                                     
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,RDARCFDT)                                   
         TIME  DEC                 R0 HAS HH:MM:SS:TT                           
         STCM  R0,4,RDARCFTM+1     SAVE MM                                      
         SRL   R0,24                                                            
         LR    R4,R0                                                            
         LA    R4,DDSTMADJ(R4)     ADD HH FOR DDS CLOCK                         
         EDIT  (R4),(2,WORK+17),FILL=0,ZERO=NOBLANK                             
         GOTO1 HEXIN,DMCB,WORK+17,RDARCFTM,2,0                                  
         DROP  R5                                                               
*                                                                               
         GOTO1 VADDELEM,DMCB,(R6),TWAELEM                                       
*                                                                               
* ADDITIONAL CHECK FOR KATZ/EDI ORDERS FOR DUPLICATE X'51' RECORDS ON           
* FILE                                                                          
*                                                                               
CLEAN45  DS    0H                                                               
         L     R6,AIO4                                                          
         USING RDARREC,R6                                                       
         MVC   KEY(L'TWASVDKY),RDARKEY                                          
         MVI   UPDATE,C'Y'                                                      
         OI    DMINBTS,X'08'       READ DELETES                                 
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RDARKEY),KEYSAVE  '51' REC AREADY ON FILE?                 
         BE    CLEAN50                 YES - OVERWRITE IT                       
         GOTO1 VADDREC,DMCB,(R6),,KEY  NO - ADD IT                              
         MVC   HDRDA,KEY           SAVE D/A TO UPDATE PASSIVE PTS               
         B     CLEAN60                                                          
         DROP  R6                                                               
*                                                                               
CLEAN50  DS    0H                  RESTORE ACTIVE KEY IF NEEDED                 
         TM    KEY+27,X'80'        KEY DELETED?                                 
         BZ    CLEAN55                                                          
         NI    KEY+27,X'FF'-X'80'                                               
         GOTO1 VWRITE                                                           
*                                                                               
CLEAN55  DS    0H                  REWRITE OLD 51 RECORD                        
         MVC   HDRDA,KEY+28                                                     
         MVI   UPDATE,C'Y'                                                      
         OI    DMINBTS,X'08'       READ DELETES                                 
         GOTO1 VGETREC,DMCB,AIO3   USE IO3                                      
         GOTO1 VMOVEREC,DMCB,(R6),AIO3                                          
         GOTO1 VPUTREC,DMCB,AIO3                                                
*                                                                               
* REREAD HEADER KEY AND MARK DELETED                                            
*                                                                               
CLEAN60  DS    0H                                                               
*                                                                               
*   PROCESS OLD VS NEW PASSIVE POINTERS                                         
*                                                                               
         L     R6,AIO4                                                          
         USING RDARREC,R6                                                       
         CLI   RDARKRT,X'10'       HEADER RECORD?                               
         BNE   CLEAN60A                                                         
         DROP  R6                                                               
*                                                                               
         LA    R4,PASAREA          A(PASSIVE BUILD AREA)                        
         LA    R4,800(R4)          R4->NEW PASSIVE POINTERS                     
         OC    HDRDA,HDRDA                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RFGENDTR,VREPFACS),DMCB,(X'02',ACOMFACS),PASAREA,(R4), X        
               HDRDA                                                            
* ADDITIONAL ERROR CHECKING                                                     
         MVC   KEY,0(R4)                                                        
         GOTO1 VHIGH                                                            
         CLC   KEYSAVE(27),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VGETREC,DMCB,AIO3                                                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*                                                                               
CLEAN60A DS    0H                                                               
         L     R6,AIO4                                                          
         USING RDARREC,R6                                                       
*                                                                               
         MVC   KEY(L'TWASVDKY),RDARKEY                                          
         MVI   KEY,X'41'                                                        
         OI    DMINBTS,X'08'                                                    
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RDARKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VSEQ                                                             
                                                                                
         CLC   KEY(RDARKRT-RDARKEY),KEYSAVE  SAME ORDER?                        
         BE    CLEAN30                       YES - PROCESS RECORD               
*                                                                               
* REMOVE X'0B01' SHADOW AGENCY BUY RECORDS, IF ANY                              
*                                                                               
         LA    R6,KEY                                                           
         USING RBUYREC,R6                                                       
         XC    KEY,KEY                                                          
         MVC   RBUYKTYP(2),=X'0B01'                                             
         MVC   RBUYKREP,REPALPHA                                                
         MVC   RBUYKCON,TWACNUM                                                 
         DROP  R6                                                               
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
*                                                                               
CLEAN80  DS    0H                                                               
         CLC   KEY(RBUYKPLN-RBUYKEY),KEYSAVE                                    
         BNE   CLEANUPX            DONE                                         
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,AIO4                                                
*                                                                               
         L     R6,AIO4                                                          
         USING RBUYREC,R6                                                       
         OI    RBUYCNTL,X'80'                                                   
         GOTO1 VPUTREC,DMCB,AIO4                                                
         DROP  R6                                                               
*                                                                               
         OI    KEY+27,X'80'                                                     
         GOTO1 VWRITE                                                           
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VSEQ                                                             
         B     CLEAN80                                                          
*                                                                               
CLEANUPX DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* CHKDAREC:  FOR DIFFERENT TYPES OF DARE RECORD, CHECK IF DELETE                
*  BIT HAS BEEN TURNED ON. IF IT IS, SKIP REWRITING IT BACK AS X'51'            
*                                                                               
CHKDAREC NTR1                                                                   
*                                                                               
         L     R6,AIO4                                                          
         USING RDARREC,R6          SET OUTPUT RECORD AREA                       
         CLI   RDARKRT,X'10'       AGENCY ORDER HEADER?                         
         BNE   CDAR0040            NO                                           
*                                                                               
         LA    R3,RDARELEM         YES -                                        
         USING RDARELEM,R3                                                      
         TM    RDARDELS,X'80'+X'40'                                             
         B     CDARXIT                                                          
         DROP  R3                                                               
*                                                                               
CDAR0040 EQU   *                                                                
         CLI   RDARKRT,X'40'       BUY?                                         
         BNE   CDAR0080            NO                                           
         CLI   RDARKSRT,X'00'      BUY HEADER?                                  
         BNE   CDAR0120            NO  - ORB/COMMT/DETAIL                       
*                                                                               
         LA    R3,RDARELEM         YES -                                        
         USING RDARBYEL,R3                                                      
         TM    RDARBYDL,X'80'+X'40'                                             
         B     CDARXIT                                                          
         DROP  R3                                                               
*                                                                               
CDAR0080 EQU   *                                                                
         BH    CDARNO              SKIP OVER RECORD TYPES                       
*                                     50 (TRAILER) + 60 (EQUIVS)                
*                                                                               
*   REMAINING RECORD TYPES ARE 15, 20, 30, 40/10, 40/20, 40/30, AND             
*        SOFT/HARD DELETE BYTE IS IN SAME PLACE IN ALL ELEMENTS.                
*        STANDARD COMMENT ELEMENT FORMAT USED, ARBITRARILY.                     
*                                                                               
CDAR0120 EQU   *                                                                
         LA    R3,RDARELEM                                                      
         USING RDARELE2,R3                                                      
         TM    RDARELDL,X'80'+X'40'                                             
         BNZ   CDARYES                                                          
         DROP  R3                                                               
*                                                                               
CDARNO   SR    RC,RC                                                            
CDARYES  LTR   RC,RC                                                            
CDARXIT  XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* DELPAS: DELETE PASSIVE KEYS                                                   
*  AIO4->DARE RECORD                                                            
*                                                                               
DELPAS   NTR1                                                                   
*   PULL OLD KEYS PRE-P/P-S/P CHANGE:                                           
*        AIO2: KEY BUILD AREA                                                   
*        AIO4: CURRENT LOCATION OF AGENCY ORDER RECORD                          
*        AIO3: IO AREA                                                          
*                                                                               
         GOTO1 (RFGENDTR,VREPFACS),DMCB,(X'81',ACOMFACS),PASAREA,AIO4, X        
               AIO3                                                             
*                                                                               
         LA    R4,PASAREA          A(PASSIVE BUILD AREA)                        
         LA    R4,800(R4)          R4->ALL NULL                                 
         LHI   RF,800              THIS WILL DELETE ALL THE OLD PTS             
         XCEF  (R4)                                                             
         GOTO1 (RFGENDTR,VREPFACS),DMCB,(X'02',ACOMFACS),PASAREA,(R4), X        
               HDRDA                                                            
*                                                                               
DELPASX  XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ADD DARE CONFIRMATION AUDIT TRAIL TO X'51' RECORD                             
***********************************************************************         
DOAUDIT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY                                                          
KEYD     USING RDARKEY,KEY                                                      
         MVC   KEY(RDARKRT-RDARKEY),TWASVDKY                                    
         MVI   KEY,X'51'                                                        
         MVI   KEYD.RDARKRT,X'70'  GET TRAILER RECORD                           
         DROP  KEYD                                                             
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DOAUDX                                                           
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,AIO4                                                
*                                                                               
         MVC   WORK(4),HELLO       RECORD DARE HISTORY                          
         MVC   WORK+4(4),DATCON                                                 
         XC    DMCB+4(4),DMCB+4                                                 
         MVI   DMCB+4,X'FF'        VALID ACTION                                 
         MVI   DMCB+5,DHCONFIQ     ACTION CONFIRM                               
         MVC   DMCB+6(1),REVNUM    REVISION NUMBER                              
         GOTO1 =V(REGENDHT),DMCB,AIO4,,WORK,RR=RELO                             
*                                                                               
         GOTO1 VPUTREC,DMCB,AIO4                                                
*                                                                               
DOAUDX   XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SEND ORDSAL MESSAGE FOR RADIO EDI ORDERS                                      
***********************************************************************         
ORDSAL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    P,P                 CLEAR PRINT LINE                             
         LA    R2,P                                                             
         USING RORDSALD,R2                                                      
         MVC   ROSPTID,=C'ORDSAL'                                               
         MVC   ROSPORDR,TWAGORDN   AGENCY ORDER NUMBER                          
*                                                                               
         L     R6,AIO4             CHECK IF WE NEED TO SEND                     
         MVI   ELCODE,X'0A'        SALESPERSON/POINTPERSON                      
         BRAS  RE,GETEL                                                         
         BNE   ORDSAL10                                                         
         USING RDARPPEL,R6                                                      
         CLC   RDARPPSP,SPACES                                                  
         BE    ORDSAL10                                                         
*                                                                               
         GOTO1 =A(GETSALNM),RR=RELO                                             
         MVC   ROSPSALP,RDARPPSP                                                
         MVC   ROSPSALN,WORK                                                    
         DROP  R6                                                               
*                                                                               
ORDSAL10 DS    0H                                                               
         L     R6,AIO4                                                          
         USING RDARREC,R6                                                       
         CLI   RDARBSTS,C'A'       OPENED?                                      
         BE    ORDSAL20                                                         
         MVI   ROSPOKCF,C'Y'       CONFIRMING W/O PREVIOUS OPEN                 
         B     ORDSAL30                                                         
*                                                                               
ORDSAL20 DS    0H                                                               
         OC    ROSPSALP,ROSPSALP                                                
         BZ    ORDSALX             NO SALESPERON NOR FLAG TO SEND               
*                                  JUST SKIP MESSAGE                            
ORDSAL30 DS    0H                                                               
         GOTO1 SPOOL,PARAS,(R8)                                                 
         MVI   LINE,2              FORCE EVERYTHING TO PAGE ONE                 
         AHI   R3,1                BUMP COUNTER                                 
*                                                                               
ORDSALX  DS    0H                                                               
         XIT1  REGS=(R3)           RETURN RECORD COUNTER                        
         DROP  R2,R6                                                            
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*   GETSALNM :  RETRIEVE SALESPERSON NAME FOR UNLINKED REJECTIONS               
*               ON EXIT, WORK HAS EXPANDED NAME                                 
*                                                                               
         DS    0F                                                               
GETSALNM NTR1  BASE=*,WORK=(R4,500),LABEL=*                                     
*                                                                               
         L     R6,AIO4             CHECK IF WE NEED TO SEND                     
         MVI   ELCODE,X'0A'        SALESPERSON/POINTPERSON                      
         BRAS  RE,GETEL            FOR BOTH OPEN/REJECT                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RDARPPEL,R6                                                      
*                                                                               
         XC    KEY,KEY             GET SALESPERSON NAME                         
         XC    WORK,WORK                                                        
         LA    R3,KEY                                                           
         USING RSALREC,R3                                                       
         MVI   RSALKTYP,X'06'                                                   
         MVC   RSALKREP,REPALPHA                                                
         MVC   RSALKSAL,RDARPPSP                                                
*                                                                               
         L     R6,AIO4             CHECK IF UNWIRED                             
         MVI   ELCODE,X'0F'        YES, NEED TO GET POINTPERSON                 
         BRAS  RE,GETEL            FOR BOTH OPEN/REJECT                         
         BNE   GSAL10                                                           
         USING RDARFLEM,R6                                                      
         TM    RDARFLG1,X'01'      UNWIRED?                                     
         BZ    GSAL10                                                           
         DROP  R6                                                               
*                                                                               
         MVI   RSALKTYP,X'31'                                                   
         DROP  R3                                                               
*                                                                               
GSAL10   DS    0H                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(27),KEYSAVE                                                  
         BE    GSAL20                                                           
*        DC    H'0'                DUMP IN BACK GROUND INSTEAD                  
         LA    R3,980                                                           
         GOTO1 GETTXT,DMCB,(R3),0,(C'E',0),0,0,0                                
         DC    H'0',C'$ABEND'      ABEND                                        
*                                                                               
GSAL20   DS    0H                                                               
         GOTO1 VGETREC,DMCB,(R4)                                                
         USING RSALREC,R4                                                       
         MVC   WORK(L'RSALNAME),RSALNAME                                        
         DROP  R4                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001RECNT62   03/29/10'                                      
         END                                                                    
