*          DATA SET RECNT33    AT LEVEL 020 AS OF 05/07/98                      
*          DATA SET RECNT33    AT LEVEL 164 AS OF 10/03/96                      
*PHASE T80233A                                                                  
         TITLE 'T80233 - CONTRACT MOVE MODULE - REPPAK'                         
*INCLUDE HEXOUT                                                                 
*                                                                               
***********************************************************************         
*                                                                     *         
*        RECNT33 (T80233) --- CONTRACT MOVE                           *         
*                                                                     *         
* ------------------------------------------------------------------- *         
*                                                                     *         
* 13JAN98 RHV  MOVED ACTION                                           *         
* 20MAY97 RHV  MOVE CFC COMMENT RECS WITH CONTRACT                    *         
* 03OCT96 SKU  SUPPORT LOW POWER STATION                              *         
* 16AUG96 RHV  IT'S A BOY!!!                                          *         
*                                                                     *         
***********************************************************************         
*                                                                               
T80233   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 (MYWORKX-MYWORKD),T80233,CLEAR=YES                               
         LR    R9,RC                                                            
         USING MYWORKD,R9                                                       
         L     RC,0(R1)            WORK                                         
         USING GENOLD,RC                                                        
         USING TWAD,RA                                                          
         LR    R8,RA                                                            
         AH    R8,=Y(TWAWORKQ)                                                  
         USING TWAWORK,R8                                                       
**********************************************************************          
* MAIN PROGRAM HERE **************************************************          
**********************************************************************          
*                                                                               
         BAS   RE,VALDEST          VALIDATE DESTINATION REP                     
*                                                                               
         BAS   RE,WRITEK           WRITE DUPLICATE OF K W/NEW REP               
*                                                                               
         BAS   RE,WRITEB           WRITE DUPLICATE OF BUYS                      
*                                                                               
         BAS   RE,WRITEM           WRITE DUPLICATE OF MAKEGOODS                 
*                                                                               
         BAS   RE,WRITECFC         WRITE DUPLICATE OF CFC RECORD                
*                                                                               
         MVC   WORK(4),DATAMGR     SETUP BLOCK FOR REGENDEL                     
         MVC   WORK+4(4),AIO3                                                   
         LA    RF,DMWORK                                                        
         STCM  RF,15,WORK+8                                                     
         MVC   WORK+12(4),DATCON                                                
         GOTOX (RFGENDEL,VREPFACS),DMCB,RCONKEY,WORK                            
*                                                                               
         BAS   RE,RESULT           DISPLAY RESULTS                              
*                                                                               
         B     EXXMOD                                                           
*                                                                               
**********************************************************************          
* DISPLAY RESULTS ON SCREEN                                                     
**********************************************************************          
RESULT   NTR1                                                                   
         OI    MVKDREPH+1,X'20'                                                 
         OI    MVKDREPH+6,X'80'                                                 
*                                                                               
         NI    MVKTXT3H+1,X'FF'-X'0C'                                           
         OI    MVKTXT3H+6,X'80'                                                 
*                                                                               
         GOTO1 =V(HEXOUT),DMCB,NEWCON,MVKDCON,L'NEWCON,RR=Y                     
         NI    MVKDCONH+1,X'FF'-X'0C'                                           
         OI    MVKDCONH+6,X'80'                                                 
*                                                                               
         XC    CONBACT,CONBACT                                                  
         OI    CONBACTH+6,X'80'                                                 
*                                                                               
         NI    CONCNUMH+4,X'FF'-X'20'                                           
*                                                                               
         GOTO1 VDISMSG,DMCB,160,0,0,0                                           
*                                                                               
RESX     B     XIT                                                              
*                                                                               
**********************************************************************          
* VALIDATE DESTINATIONá                                              *          
**********************************************************************          
VALDEST  DS    0H                                                               
         NTR1                                                                   
*                                                                               
         LA    R2,MVKDREPH              DESTINATION FLD                         
*                                                                               
         LA    R3,1                                                             
         CLI   5(R2),0                  ZERO LEN INPUT?                         
         BE    ERROR                    YES - ERROR, REQ FIELD                  
         MVC   NEWREP,8(R2)                                                     
*                                                                               
         LA    R3,608                                                           
         CLC   REPALPHA,8(R2)           SOURCE SAME AS DESTINATION?             
         BE    ERROR                    YES - INVALID DESTINATION               
*                                                                               
****>    CLC   TWARMAST,8(R2)           IS TARGET SOURCE MASTER?                
****>    BE    VALDX                    YES - VALID                             
* MASTER CAN'T BE TARGET                                                        
*                                                                               
         LA    R3,609                                                           
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RREPREC,R6                                                       
         MVI   RREPKTYP,X'01'                                                   
         MVC   RREPKREP,8(R2)                                                   
         GOTO1 VHIGH                    LOOKUP REPREC FOR DESTINATION           
         CLC   KEY(L'RREPKEY),KEYSAVE   EXISTS?                                 
         BNE   ERROR                    NO - INVALID DEST                       
         GOTO1 VGETREC,DMCB,IOAREA                                              
         LA    R6,IOAREA                                                        
*                                                                               
         CLC   REPALPHA,=C'B3'          SOME SPECIAL CODE TO LET ROB &          
         BE    *+14                     EJOR COPY TO EACH OTHER                 
         CLC   REPALPHA,=C'B4'                                                  
         BNE   VALD05                                                           
         CLC   NEWREP,=C'B3'                                                    
         BE    VALD10                                                           
         CLC   NEWREP,=C'B4'                                                    
         BNE   ERROR                                                            
*                                                                               
VALD05   CLC   RREPMAST,REPALPHA        IS SOURCE MASTER FOR TARGET?            
         BE    VALD10                                                           
         CLC   RREPMAST,TWARMAST        SOURCE & TARGET MASTERS MATCH?          
         BNE   ERROR                    NO - INVALID MOVE                       
         DROP  R6                                                               
*                                                                               
VALD10   DS    0H                       LOOKUP STATION ON DEST REP              
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'                                                        
         MVC   KEY+20(2),NEWREP         DESTINATION REP CODE                    
         MVC   KEY+22(5),RCONKSTA       SOURCE STATION CALL LETTERS             
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RSTAKEY),KEYSAVE   EXISTS?                                 
         BE    *+12                     YES                                     
         OI    VALFLAGS,X'80'           NO - SET FLAG                           
         B     VALD25                                                           
         GOTO1 VGETREC,DMCB,IOAREA      SAVE STATION RECORD FOR LATER           
VALD25   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'04'                                                        
         MVC   KEY+23(2),NEWREP         DESTINATION REP CODE                    
         MVC   KEY+25(2),RCONKOFF       SOURCE OFFICE                           
         GOTO1 VHIGH                                                            
         CLC   KEY(L'ROFFKEY),KEYSAVE   EXISTS?                                 
         BE    *+8                      YES                                     
         OI    VALFLAGS,X'40'           NO - SET FLAG                           
*                                                                               
         CLI   RCONTYPE,0               IS THERE A CONTYPE                      
         BE    VALD26                   NO - DON'T BOTHER CHECKING DEST         
         XC    KEY,KEY                                                          
         MVI   KEY,X'32'                                                        
         MVC   KEY+24(2),NEWREP         DESTINATION REP CODE                    
         MVC   KEY+26(2),RCONTYPE       SOURCE CONTYPE                          
         GOTO1 VHIGH                                                            
         CLC   KEY(L'ROFFKEY),KEYSAVE   EXISTS?                                 
         BE    *+8                      YES                                     
         OI    VALFLAGS,X'20'           NO - SET FLAG                           
VALD26   DS    0H                                                               
*                                                                               
* CHECK IF COMPETITIVE STATIONS EXIST IN STA REC OF TARGET REP                  
*                                                                               
         CLI   RCONKSTA+4,C'A'        SKIP COMPETITIVE CHECK FOR RADIO          
         BE    VALD29                                                           
         CLI   RCONKSTA+4,C'F'                                                  
         BE    VALD29                                                           
*                                                                               
         LA    R6,RCONREC                                                       
         MVI   ELCODE,X'06'             GET SPL/EPL ELEM                        
         BAS   RE,GETEL                 DNE - SKIP THIS                         
         BNE   VALD29                                                           
         USING RCONSPEL,R6                                                      
         ZIC   R2,RCONSPNU              NUMBER OF MINI ELEMS                    
         LTR   R2,R2                    NONE?                                   
         BZ    VALD29                   YES SKIP THIS                           
         LA    R3,RCONSPST              1ST STATION                             
         DROP  R6                                                               
         MVI   ELCODE,X'02'                                                     
VALD27   DS    0H                                                               
         CLC   RCONKSTA,0(R3)           STATION OF RECORD?                      
         BE    VALD28B                                                          
         LA    R6,IOAREA                RSTAREC                                 
         BAS   RE,GETEL                 FIRST COMPETITIVE ELEM                  
         B     *+8                                                              
VALD28   DS    0H                                                               
         BAS   RE,NEXTEL                HAVE WE FOUND A MATCH?                  
         BNE   VALD28A                  NO - FLAG IT                            
         USING RSTAMKEL,R6                                                      
         CLC   RSTAMKST,0(R3)           STATION IN STA REC LIST                 
         DROP  R6                                                               
         BNE   VALD28                   NO - TRY NEXT ELEM                      
VALD28B  LA    R3,9(R3)                 ELSE - NEXT MINI ELEM                   
         BCT   R2,VALD27                LOOP                                    
         B     VALD29                                                           
*                                                                               
VALD28A  OI    VALFLAGS,X'10'                                                   
*                                                                               
VALD29   DS    0H                                                               
         CLI   VALFLAGS,0               ANY MISSING RECORDS?                    
         BE    VALD90                   NO - SKIP ERROR MSG                     
         LA    R1,WORK2                 MSG BUFFER                              
         LA    RF,WORK2                                                         
*                                                                               
         TM    VALFLAGS,X'80'                                                   
         BZ    VALD30                                                           
         MVC   0(7,R1),=C'Station'                                              
         LA    R1,7(R1)                                                         
*                                                                               
VALD30   DS    0H                                                               
         TM    VALFLAGS,X'40'                                                   
         BZ    VALD35                                                           
         CR    R1,RF                                                            
         BE    *+14                                                             
         MVC   0(2,R1),=C', '                                                   
         LA    R1,2(R1)                                                         
         MVC   0(6,R1),=C'Office'                                               
         LA    R1,6(R1)                                                         
*                                                                               
VALD35   DS    0H                                                               
         TM    VALFLAGS,X'20'                                                   
         BZ    VALD40                                                           
         CR    R1,RF                                                            
         BE    *+14                                                             
         MVC   0(2,R1),=C', '                                                   
         LA    R1,2(R1)                                                         
         MVC   0(7,R1),=C'Contype'                                              
         LA    R1,7(R1)                                                         
*                                                                               
VALD40   DS    0H                                                               
         TM    VALFLAGS,X'10'                                                   
         BZ    VALD45                                                           
         CR    R1,RF                                                            
         BE    *+14                                                             
         MVC   0(2,R1),=C', '                                                   
         LA    R1,2(R1)                                                         
         MVC   0(15,R1),=C'Competitive Sta'                                     
         LA    R1,15(R1)                                                        
*                                                                               
VALD45   DS    0H                                                               
         LA    R3,610                                                           
         ST    RF,DMCB+12               ERR MSG                                 
         SR    R1,RF                                                            
         STC   R1,DMCB+12               ERR MSG LENGTH                          
         GOTO1 GETTXT,DMCB,(R3),0,(C'E',DMCB),,0,0                              
         OI    MVKDREPH+6,X'40'                                                 
         L     RD,BASERD                                                        
         B     EXXMOD                                                           
*                                                                               
VALD90   DS    0H                                                               
*                                                                               
VALDX    B     XIT                                                              
*                                                                               
**********************************************************************          
* WRITE COPY OF CONTRACT WITH NEW REP CODE                           *          
**********************************************************************          
WRITEK   DS    0H                                                               
         NTR1                                                                   
*                                                                               
STA      USING RSTAREC,IOAREA           STAION REC SHOULD BE HERE               
         CLI   STA.RSTAKTYP,X'02'       BUT LETS BE SURE                        
         BE    *+6                                                              
         DC    H'0'                     CLEAR!                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'8C'           PASSIVE POINTER KEY TYPE                     
         MVC   KEY+21(2),NEWREP    NEW REP CODE                                 
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(23),KEYSAVE     SAME REP?                                    
         BNE   *+10                                                             
*                                                                               
*              GET NEXT CONTRACT NUMBER                                         
         ZAP   WORK(5),=P'99999999'                                             
         ZAP   WORK+5(5),=P'99999999'                                           
         MVO   WORK+5(5),KEY+23(4) K NUMBER                                     
         SP    WORK(5),WORK+5(5)   GET POSITIVE                                 
         AP    WORK(5),=P'1'       NEXT K NUMBER                                
         MVO   WORK+10(5),WORK(5)                                               
         MVC   NEWCON,WORK+10                                                   
*                                                                               
         LA    R4,RCONREC               SETUP MVCL                              
         ZICM  R5,RCONLEN,2                                                     
         L     R2,AIO2                                                          
         LR    R3,R5                                                            
*                                                                               
         MVCL  R2,R4                    COPY SOURCE K TO IO2                    
         L     R2,AIO2                  RESET POINTER                           
*                                                                               
D        USING RCONREC,R2                                                       
*                                                                               
         MVC   D.RCONKREP,NEWREP        ADJUST KEY                              
         MVC   D.RCONKGRP,STA.RSTAGRUP                                          
         MVC   D.RCONKCON,NEWCON                                                
*                                                                               
         DROP  STA                                                              
*                                                                               
         LA    R6,D.RCONREC                                                     
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   WK010                                                            
         USING RCONSEND,R6                                                      
         OC    RCONSSID,RCONSSID                                                
         BZ    WK010                                                            
         BAS   RE,GETREPID                                                      
         MVC   RCONSSID,WORK                                                    
         DROP  R6                                                               
*                                                                               
WK010    DS    0H                     DELETE DARE ELEM IF PRESENT               
         GOTO1 VDELELEM,DMCB,(X'1D',D.RCONREC)                                  
*                                     COVERSHEET ELEM IF PRESENT                
         GOTO1 VDELELEM,DMCB,(X'A6',D.RCONREC)                                  
*                                                                               
* ANY FURTHER CHANGES TO THE NEW CONTRACT SHOULD BE MADE HERE.                  
*                                                                               
*                                                                               
         LA    R6,D.RCONREC  * IF THERE IS A COMBO ELEM THEN SOMETHING          
         MVI   ELCODE,X'17'    HAS GONE TERRIBLY WRONG HERE                     
         BAS   RE,GETEL                                                         
         BNE   *+6                                                              
         DC    H'0'            CLEAR!                                           
*                                                                               
         LA    R6,D.RCONREC                                                     
         MVI   ELCODE,X'2A'                                                     
         BAS   RE,GETEL        IS THERE A PREVIOUS MOVE HISTORY ELEM?           
         BNE   WK025           NO - GO MAKE ONE                                 
         USING RCONMMEL,R6     YES - UPDATE THE EXISTING ONE                    
         GOTO1 DATCON,DMCB,(5,0),(2,RCONMMDT) TODAY'S DATE (COMPRESSED)         
         MVC   RCONMMOC,RCONKCON              OLD K NUM                         
         MVC   RCONMMOR,REPALPHA              OLD REPCODE                       
         DROP  R6                                                               
         B     WK030                                                            
*                                                                               
WK025    DS    0H                                                               
         XC    WORK,WORK                CREATE THE MOVE HISTORY ELEM            
         LA    R6,WORK                                                          
         USING RCONMMEL,R6                                                      
         MVI   RCONMMCD,X'2A'                                                   
         MVC   RCONMMLN,=AL1(RCONMMLQ)                                          
         GOTO1 DATCON,DMCB,(5,0),(2,RCONMMDT) TODAY'S DATE (COMPRESSED)         
         MVC   RCONMMOC,RCONKCON              OLD K NUM                         
         MVC   RCONMMOR,REPALPHA              OLD REPCODE                       
         DROP  R6                                                               
         GOTO1 VADDELEM,DMCB,D.RCONREC,WORK                                     
*                                                                               
WK030    DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'ADDREC',=C'REPFILE',KEY,D.RCONREC,DMWORK         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DSKADDR,KEY              SAVE DISK ADDRESS                       
*                                                                               
*                                       BUILD PASSIVE PTRS IN IO3               
         MVC   WORK(4),DATCON                                                   
         GOTOX (RFGENPTR,VREPFACS),DMCB,D.RCONREC,AIO3,WORK                     
         L     R6,AIO3                                                          
         ZIC   R7,2(R6)                                                         
         LTR   R7,R7                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R6,3(R6)                                                         
WK050    DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'RCONKEY),0(R6)                                             
         MVC   KEY+27(1),RCONCNTL                                               
         MVC   KEY+28(4),DSKADDR                                                
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REPDIR',KEY,KEY                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R6,L'RCONKEY(R6)                                                 
         BCT   R7,WK050                                                         
*                                                                               
WKX      B     XIT                                                              
         DROP  D                                                                
**********************************************************************          
* WRITE COPIES OF BUYS WITH NEW REP CODE & K NUMBER                  *          
**********************************************************************          
WRITEB   DS    0H                                                               
         NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
DB       USING RBUYREC,R4                                                       
         MVI   DB.RBUYKTYP,X'0B'        BUILD KEY FOR SOURCE BUY                
         MVC   DB.RBUYKREP,RCONKREP                                             
         ICM   R0,15,RCONKCON                                                   
         ICM   R1,15,=X'99999999'                                               
         SR    R1,R0                    GET 9'S COMPLIMENT                      
         STCM  R1,15,FULL                                                       
         PACK  DB.RBUYKCON+0(1),FULL+3(1)   REVERSE IT                          
         PACK  DB.RBUYKCON+1(1),FULL+2(1)                                       
         PACK  DB.RBUYKCON+2(1),FULL+1(1)                                       
         PACK  DB.RBUYKCON+3(1),FULL+0(1)                                       
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 VHIGH                                                            
         B     WB020                                                            
WB010    DS    0H                                                               
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 VSEQ                                                             
WB020    CLC   KEY(RBUYKPLN-RBUYKEY),KEYSAVE    SAME K?                         
         BNE   WB200                    NO - DONE WITH BUYS                     
         MVC   OLDBYCON,DB.RBUYKCON     SAVE SOURCE K NUM 9'S & REVERS          
         MVC   SAVEKEY,KEY              SO WE CAN RESTORE SEQ LOOP              
         MVI   UPDATE,C'Y'                                                      
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 VGETREC,DMCB,AIO2        SOURCE BUY IN IO2                       
         L     R4,AIO2                  SETUP FOR MVCL                          
         ZICM  R5,DB.RBUYLEN,2                                                  
         L     R2,AIO3                  KEEP COPY OF BUY IN IO3                 
         LR    R3,R5                                                            
         MVCL  R2,R4                                                            
         L     R4,AIO2                  POINT TO OLD BUYREC                     
         OI    DB.RBUYCNTL,X'80'        DELETE SOURCE RECORD                    
         GOTO1 VPUTREC,DMCB,AIO2        PUT DELETED RECORD                      
         OI    KEY+27,X'80'             DELETE SOURCE KEY                       
         GOTO1 VWRITE                   PUT DELETED KEY                         
         L     R4,AIO3                  POINT TO NEW BUYREC                     
         MVC   DB.RBUYKREP,NEWREP       NEW REP CODE INTO BUY                   
         ICM   R0,15,NEWCON             REVERSE & 9'S NEW K NUM                 
         ICM   R1,15,=X'99999999'                                               
         SR    R1,R0                                                            
         STCM  R1,15,FULL                                                       
         PACK  DB.RBUYKCON+0(1),FULL+3(1)                                       
         PACK  DB.RBUYKCON+1(1),FULL+2(1)                                       
         PACK  DB.RBUYKCON+2(1),FULL+1(1)                                       
         PACK  DB.RBUYKCON+3(1),FULL+0(1)                                       
         GOTO1 DATAMGR,DMCB,=C'ADDREC',=C'REPFIL',KEY,AIO3,DMWORK               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   NEWBYCON,DB.RBUYKCON         SAVE BUYREC K NUMBER                
         MVC   DSKADDR,KEY              SAVE DISK ADDRESS                       
         LA    R4,KEY                   POINT TO KEY                            
         MVI   DB.RBY2KTYP,X'9B'        LOOKUP PASSIVE BUYKEY                   
         MVC   DB.RBY2KREP,RCONKREP                                             
         MVC   DB.RBY2KADV,RCONKADV                                             
         MVC   DB.RBY2KPRD,RCONPRD                                              
         MVC   DB.RBY2KCON,OLDBYCON                                             
         L     R2,AIO3                                                          
         MVC   DB.RBY2KPLN,22(R2)       PLAN CODE FROM NEW BUYREC               
         MVC   DB.RBY2KMLN,25(R2)       MASTER LINE NUMBER                      
         MVC   DB.RBY2KLIN,26(R2)       LINE NUMBER                             
         OI    DMINBTS,X'08'       PASS BACK DELETED RECORDS                    
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RBUYKEY),KEYSAVE   PASSIVE KEY EXIST?                      
         BNE   WB100                    NO - SKIP CREATING NEW ONE              
         MVC   WORK(L'RBUYKEY),KEY      YES - MAKE COPY OF IT                   
         OI    KEY+27,X'80'             DELETE ORIGINAL KEY                     
         GOTO1 VWRITE                                                           
         MVC   KEY(L'RBUYKEY),WORK      WORK ON COPY OF ORIGINAL                
         MVC   DB.RBY2KREP,NEWREP       NEW REP CODE                            
         MVC   DB.RBY2KCON,NEWBYCON     NEW BUY K NUM                           
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REPDIR',KEY,KEY                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
WB100    DS    0H                       RESTORE SEQ LOOP                        
         MVC   KEY(L'RBUYKEY),SAVEKEY   ORIGINAL BUYREC                         
         OI    DMINBTS,X'08'            WHICH IS NOW DELETED                    
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RBUYKEY),KEYSAVE   FOUND ORIGINAL BUY?                     
         BE    *+6                                                              
         DC    H'0'                     OH SHIT                                 
         B     WB010                                                            
WB200    DS    0H                                                               
*                                                                               
WBX      B     XIT                                                              
         DROP  DB                                                               
*                                                                               
**********************************************************************          
* WRITE COPIES OF MAKEGOODS WITH NEW REP CODE & K NUMBER                        
**********************************************************************          
WRITEM   DS    0H                                                               
         NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
DM       USING RMKGREC,R4                                                       
         MVI   DM.RMKGKTYP,X'11'        BUILD KEY FOR SOURCE MAKEGOOD           
         MVC   DM.RMKGKREP,RCONKREP                                             
         MVC   DM.RMKGKOFF,RCONKOFF                                             
         MVC   DM.RMKGKSTA,RCONKSTA                                             
         MVC   DM.RMKGKCON,OLDBYCON                                             
         GOTO1 VHIGH                                                            
         B     WM020                                                            
WM010    GOTO1 VSEQ                                                             
WM020    CLC   KEY(RMKGKGRP-RMKGKEY),KEYSAVE    SAME K?                         
         BNE   WM200                    NO - DONE WITH MAKEGOODS                
         MVC   SAVEKEY,KEY              SO WE CAN RESTORE SEQ LOOP              
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,AIO2        SOURCE MKG IN IO2                       
         L     R4,AIO2                  SETUP FOR MVCL                          
         ZICM  R5,DM.RMKGLEN,2                                                  
         L     R2,AIO3                  KEEP COPY OF MKG IN IO3                 
         LR    R3,R5                                                            
         MVCL  R2,R4                                                            
         L     R4,AIO2                  POINT TO OLD MKGREC                     
         OI    DM.RMKGCNTL,X'80'        DELETE SOURCE RECORD                    
         GOTO1 VPUTREC,DMCB,AIO2        PUT DELETED RECORD                      
         OI    KEY+27,X'80'             DELETE SOURCE KEY                       
         GOTO1 VWRITE                   PUT DELETED KEY                         
         L     R4,AIO3                  POINT TO NEW MKGREC                     
         MVC   DM.RMKGKREP,NEWREP       NEW REP CODE INTO MKG                   
         MVC   DM.RMKGKCON,NEWBYCON     NEW K NUM INTO MKG                      
         GOTO1 DATAMGR,DMCB,=C'ADDREC',=C'REPFIL',KEY,AIO3,DMWORK               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DSKADDR,KEY              SAVE DISK ADDRESS                       
         MVC   KEY(L'RMKGKEY),SAVEKEY   ORIGINAL MKGREC                         
         OI    DMINBTS,X'08'            WHICH IS NOW DELETED                    
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RMKGKEY),KEYSAVE   FOUND ORIGINAL MKG?                     
         BE    *+6                                                              
         DC    H'0'                     OH SHIT                                 
         B     WM010                                                            
WM200    DS    0H                                                               
*                                                                               
WMX      B     XIT                                                              
         DROP  DM                                                               
*                                                                               
*********************************************************************           
* WRITECFC - COPY OVER ANY CFC COMMENT RECORD                                   
*********************************************************************           
WRITECFC NTR1                                                                   
         XC    KEY,KEY             READ FOR CFC REC ON OLD K                    
         LA    R6,KEY                                                           
         USING RCFCREC,R6                                                       
         MVI   RCFCKTYP,RCFCKTYQ                                                
         MVC   RCFCKREP,RCONKREP                                                
         MVC   RCFCKCON,RCONKCON                                                
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RCFCKEY),KEYSAVE  HAVE A CFC REC?                          
         BNE   XIT                     NO - GET OUT                             
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,DMCB,IOAREA                                              
         LA    R6,IOAREA                                                        
         OI    RCFCCNTL,X'80'          DELETE CFC RECORD                        
         GOTO1 VPUTREC,DMCB,RCFCREC                                             
         LA    R6,KEY                                                           
         OI    RCFCKEY+27,X'80'        DELETE CFC KEY                           
         GOTO1 VWRITE,DMCB,RCFCKEY                                              
*                                                                               
         LA    R6,IOAREA               USE OLD CFC REC TO MAKE NEW ONE          
         NI    RCFCCNTL,X'FF'-X'80'    TURN OFF DELETE                          
         MVC   RCFCKREP,NEWREP         NEW REP                                  
         MVC   RCFCKCON,NEWCON         NEW K NUM                                
         GOTO1 VADDREC,DMCB,RCFCREC    ADD NEW CFC REC                          
         B     XIT                                                              
         DROP  R6                                                               
*********************************************************************           
* READ THE CONTROL FILE FOR THE REP SENDING ID                                  
* RETURNS REP ID NUMBER IN WORK                                                 
*********************************************************************           
GETREPID NTR1                                                                   
         XC    WORK,WORK                                                        
         LA    R3,573                                                           
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+15(3),=C'KAM'                                                
         CLC   =C'AM',NEWREP                                                    
         BE    GETRID10                                                         
         MVC   KEY+15(3),=C'KNA'                                                
         CLC   =C'NK',NEWREP                                                    
         BE    GETRID10                                                         
         MVC   KEY+15(3),=C'KCO'                                                
         CLC   =C'CQ',NEWREP                                                    
         BE    GETRID10                                                         
         MVC   KEY+15(3),=C'ROB'                                                
         CLC   =C'B4',NEWREP                                                    
         BE    GETR100                                                          
         MVC   KEY+15(3),=C'ABC'                                                
         MVC   KEY+18(2),RCONKOFF  INSERT OFFICE INTO AMCAST                    
         CLC   =C'RS',NEWREP                                                    
         BE    GETR100                                                          
         XC    KEY+15(5),KEY+15                                                 
         MVC   KEY+15(2),=C'KR'    KATZ RADIO                                   
         MVC   KEY+17(2),RCONKOFF  INSERT OFFICE INTO KATZ RADIO                
         CLC   =C'KU',NEWREP                                                    
         BE    GETR100                                                          
         XC    KEY+15(5),KEY+15                                                 
         MVC   KEY+15(2),=C'KH'    KATZ HISPANIC                                
         MVC   KEY+17(2),RCONKOFF  INSERT OFFICE INTO KATZ HISPANIC             
         CLC   =C'KF',NEWREP                                                    
         BE    GETR100                                                          
         XC    KEY+15(5),KEY+15                                                 
         MVC   KEY+15(4),=C'EAST'  EASTMAN                                      
         MVC   KEY+19(2),RCONKOFF  INSERT OFFICE INTO EASTMAN                   
         CLC   =C'EA',NEWREP                                                    
         BE    GETR100                                                          
         XC    KEY+15(6),KEY+15                                                 
         MVC   KEY+15(3),=C'CHR'   CHRISTAL                                     
         MVC   KEY+18(2),RCONKOFF  INSERT OFFICE INTO CHRISTAL                  
         CLC   =C'CR',NEWREP                                                    
         BE    GETR100                                                          
         XC    KEY+15(5),KEY+15                                                 
         MVC   KEY+15(3),=C'SYN'   SYNDICATION                                  
         MVC   KEY+18(2),RCONKOFF  INSERT OFFICE INTO SYNDICATION               
         CLC   =C'K4',NEWREP                                                    
         BE    GETR100                                                          
         XC    KEY+15(5),KEY+15                                                 
         MVC   KEY+15(3),=C'BAN'   BANNER                                       
         MVC   KEY+18(2),RCONKOFF  INSERT OFFICE INTO BANNER                    
         CLC   =C'BF',NEWREP                                                    
         BE    GETR100                                                          
         XC    KEY+15(5),KEY+15                                                 
         MVC   KEY+15(3),=C'SEN'   SENTRY                                       
         MVC   KEY+18(2),RCONKOFF  INSERT OFFICE INTO SENTRY                    
         CLC   =C'S3',NEWREP                                                    
         BE    GETR100                                                          
         XC    KEY+15(5),KEY+15    CLEAR IT OUT AGAIN                           
         MVC   KEY+15(4),=C'EJOR'                                               
         CLC   =C'B3',NEWREP                                                    
         BE    GETR100                                                          
         B     ERROR                                                            
*                                                                               
GETRID10 DS    0H                                                               
         MVC   KEY+18(2),=C'CR'    KATZ USES DIFFERENT OFFICE CODES             
         CLC   =C'CA',RCONKOFF     FOR CR, CL, AND DN                           
         BE    GETRID25                                                         
         MVC   KEY+18(2),=C'CL'                                                 
         CLC   =C'CV',RCONKOFF                                                  
         BE    GETRID25                                                         
         MVC   KEY+18(2),=C'DN'                                                 
         CLC   =C'DV',RCONKOFF                                                  
         BE    GETRID25                                                         
*                                                                               
         MVC   KEY+18(2),RCONKOFF                                               
*                                                                               
* SPECIAL DESTINATION ROUTING FOR KATZ TV REPS                                  
*                                                                               
GETRID25 DS    0H                                                               
         CLI   RCONTYPE,C'N'       FOR CONTINENTAL AND TYPE N ONLY              
         BNE   GETRID30                                                         
         CLC   =C'NY',RCONKOFF                                                  
         BNE   GETR100                                                          
         MVI   KEY+20,C'U'                                                      
         B     GETR100                                                          
*                                                                               
GETRID30 DS    0H                  FOR ALL THREE KATZ REPS: AM, CQ, NK          
         CLI   RCONTYPE,C'P'       FOR TYPE P                                   
         BNE   GETRID40                                                         
         CLC   =C'NY',RCONKOFF                                                  
         BE    GETR50                                                           
         CLC   =C'PH',RCONKOFF                                                  
         BE    GETR50                                                           
         CLC   =C'LA',RCONKOFF     FOR LA, ID ENDS WITH P INSTEAD               
         BNE   GETR100                                                          
         MVI   KEY+20,C'P'                                                      
         B     GETR100                                                          
*                                                                               
GETRID40 DS    0H                                                               
         CLI   RCONTYPE,C'Z'       FOR TYPE Z                                   
         BNE   GETR60                                                           
         CLC   =C'NY',RCONKOFF                                                  
         BE    GETR50                                                           
         CLC   =C'PH',RCONKOFF                                                  
         BNE   GETR100                                                          
*                                                                               
GETR50   DS    0H                                                               
         MVI   KEY+20,C'D'                                                      
         B     GETR100                                                          
*                                                                               
GETR60   DS    0H                  FOR KATZ NY OFFICES                          
         CLC   =C'NY',RCONKOFF                                                  
         BNE   GETR100                                                          
*                                                                               
         CLI   RCONKSTA+4,C' '     TV ONLY, REDUNDANT BUT...                    
         BE    GETR63                                                           
         CLI   RCONKSTA+4,C'L'     TV ONLY, REDUNDANT BUT...                    
         BNE   GETR100                                                          
*                                                                               
GETR63   DS    0H                                                               
         LA    R4,KATZLIST         CHECK FOR REPS WITH 'S' ENDING               
*                                                                               
GETR65   CLC   0(4,R4),RCONKSTA                                                 
         BE    GETR70                                                           
         LA    R4,4(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BE    GETR80                                                           
         B     GETR65                                                           
*                                                                               
GETR70   DS    0H                                                               
         MVI   KEY+20,C'S'                                                      
         B     GETR100                                                          
*                                                                               
GETR80   DS    0H                  CHECK FOR REPS WITH 'B' ENDING               
         LA    R4,KATZLST2                                                      
*                                                                               
GETR85   CLC   0(4,R4),RCONKSTA                                                 
         BE    GETR90                                                           
         LA    R4,4(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BE    GETR100                                                          
         B     GETR85                                                           
*                                                                               
GETR90   DS    0H                                                               
         MVI   KEY+20,C'B'                                                      
*                                                                               
GETR100  DS    0H                                                               
         OC    KEY+15(10),MYSPACES SPACE PAD                                    
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',KEY,AIO3                      
*                                                                               
         CLI   8(R1),0                                                          
         BNE   ERROR                                                            
*                                                                               
         L     R6,AIO3                                                          
         CLC   KEY(25),0(R6)                                                    
         BNE   ERROR                                                            
*                                                                               
         LA    R6,28(R6)                                                        
GETR110  CLI   0(R6),X'02'       TEST DESC ELEMENT                              
         BE    GETR120                                                          
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   GETR110                                                          
         B     ERROR                                                            
*                                                                               
GETR120  MVC   WORK(2),2(R6)      SIGN-ON                                       
         B     XIT                                                              
*                                                                               
XIT      XIT1                                                                   
*                                                                               
KATZLIST DS    0CL4                SIGN-ON ENDS WITH A 'S'                      
* NATIONAL                         IE: KNANYS                                   
         DC    C'WMAR'                                                          
         DC    C'WTTO'                                                          
         DC    C'WABM'                                                          
         DC    C'WCGV'                                                          
         DC    C'WVTV'                                                          
         DC    C'WFTC'                                                          
         DC    C'KNXV'                                                          
         DC    C'KUSI'                                                          
         DC    C'WFTS'                                                          
* CONTINENTAL                                                                   
         DC    C'WVLA'                                                          
         DC    C'KBMT'                                                          
         DC    C'KIII'                                                          
         DC    C'KFSM'                                                          
         DC    C'KGBT'                                                          
         DC    C'KTBS'                                                          
         DC    C'KTUL'                                                          
         DC    C'WALB'                                                          
         DC    C'WCTV'                                                          
         DC    C'WRCB'                                                          
         DC    C'WRBL'                                                          
         DC    C'WCTI'                                                          
         DC    C'WHNT'                                                          
         DC    C'WMAZ'                                                          
         DC    C'WOI '                                                          
         DC    C'WPTA'                                                          
         DC    C'WSIL'                                                          
         DC    C'KSNF'                                                          
         DC    C'WXOW'                                                          
         DC    C'WLFI'                                                          
         DC    C'WQAD'                                                          
         DC    C'KCAU'                                                          
         DC    C'WSBT'                                                          
         DC    C'WTHI'                                                          
* AMERICA                                                                       
         DC    C'WZZM'                                                          
         DC    C'WBIR'                                                          
         DC    C'KFOR'                                                          
         DC    C'WCPX'                                                          
         DC    C'KOMO'                                                          
         DC    C'KSDK'                                                          
         DC    C'WJLA'                                                          
         DC    C'WPEC'                                                          
         DC    X'FF'                                                            
*                                                                               
* AMERICA                                                                       
*                                                                               
KATZLST2 DS    0CL4                SIGN-ON ENDS WITH A 'B'                      
         DC    C'WGRZ'             IE: KAMNYB                                   
         DC    C'WYFF'                                                          
         DC    C'WGAL'                                                          
         DC    C'WREG'                                                          
         DC    C'WTVF'                                                          
         DC    C'WTKR'                                                          
         DC    C'WNCN'                                                          
         DC    C'KENS'                                                          
         DC    C'WNEP'                                                          
         DC    C'WXII'                                                          
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE RECNTWR2K                                                      
         ORG   CONLAST                                                          
       ++INCLUDE RECNTD6D                                                       
         EJECT                                                                  
*                                                                               
MYWORKD  DSECT                                                                  
NEWCON   DS    CL4                                                              
NEWREP   DS    CL2                                                              
DSKADDR  DS    XL4                                                              
OLDBYCON DS    CL4                                                              
NEWBYCON DS    CL4                                                              
SAVEKEY  DS    CL27                                                             
VALFLAGS DS    CL1    X'80'  STATION MISSING                                    
*                     X'40'  OFFICE MISSING                                     
*                     X'20'  CONTYPE MISSING                                    
MYWORKX  EQU   *                                                                
*                                                                               
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE REGENCFC                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020RECNT33   05/07/98'                                      
         END                                                                    
