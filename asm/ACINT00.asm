*          DATA SET ACINT00    AT LEVEL 065 AS OF 05/09/08                      
*PHASE T61900A,*                                                                
*INCLUDE TWABLD                                                                 
*INCLUDE BMONVAL                                                                
*INCLUDE KHDUMMY                                                                
         TITLE 'T61900 - INTERAGENCY RECEIVABLE CONTROLLER'                     
T61900   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LENWORK,T61900,R7,RR=R2,CLEAR=YES                                
         ST    R2,RELO                                                          
         L     RA,4(R1)            RA=A(TWA)                                    
         USING T619FFD,RA                                                       
         SPACE 1                                                                
         LR    R8,RC               R8=A(SPOOL WORK AREAS)                       
         USING SPOOLD,R8                                                        
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES INIT SPACES                          
         LA    RC,SPOOLEND                                                      
         SPACE 1                                                                
         USING GEND,RC                                                          
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RD,SYSRD            SET RD FOR GENCON RETURN                     
         LR    R9,R1                                                            
         ST    R9,SYSPARMS         FIGURE START OF PROGRAM WORK AREAS           
         LA    R9,IO                                                            
         AH    R9,=Y(LENIOAS)      GRABBING 3 2000 BYTE I/O AREAS               
         ST    R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          R9=A(PROGRAM WORK AREAS)                     
         ST    R7,BASER7           SAVE SECOND BASE REGISTER                    
         SPACE 1                                                                
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
         SPACE 1                                                                
         NI    CONKEYH+1,X'FF'-X'20' UNPROTECT ACTION FOR GENCON                
         OI    CONSERVH+6,X'01'    SERVICE FIELD IS ALWAYS MODIFIED             
         OI    CONSERVH+6,X'80'                                                 
         XC    CONHEAD,CONHEAD     CLEAR MESSAGE AREA                           
         SPACE 1                                                                
         MVI   RACHANGE,C'N'                                                    
         TM    CONRECH+4,X'20'     TEST IF RECORD CHANGED                       
         BZ    *+12                YES                                          
         TM    CONACTH+4,X'20'     TEST IF ACTION CHANGED                       
         BO    *+8                 NO                                           
         MVI   RACHANGE,C'Y'       NOTE CHANGE IN EITHER                        
         SPACE 1                                                                
INT01    BAS   RE,CALLGENC         OFF TO GENCON                                
         CLI   GOAGAIN,C'Y'        TEST FOR FLAG TO CALL GENCON AGAIN           
         BNE   INT02               NO                                           
         NI    CONRECH+6,X'BF'     RESET INSERT CURSOR BIT                      
         B     INT01                                                            
         SPACE 1                                                                
INT02    OI    CONRECH+4,X'20'     SET PREV VALIDATED BITS                      
         OI    CONACTH+4,X'20'                                                  
         MVC   LASTOV,OVERLAY      SAVE THIS TIME OVERLAY                       
         MVC   LASTCOMP,COMPANY    SAVE THIS TIME COMPANY                       
*                                                                               
         CLI   OFFLINE,C'Y'        ARE WE OFFLINE?                              
         BE    XIT                 YES - CAN'T CHECK TWA                        
         LA    R2,CONACTH          NO, DO SOME SECURITY CHECKING                
         MVI   ERROR,SECLOCK                                                    
         CLC   CONREC(3),=C'PROFILE'                                            
         BNE   XIT                 IS RECORD PROFILE?                           
         CLC   CONACT(3),=C'LIST'  YES, IS IT A LIST?                           
         BE    XIT                 YES, ALL DONE                                
         CLC   CONACT(3),=C'PEEL'  NO, IS A PEEL?                               
         BE    INT03               YES, CHECK SECURITY                          
         OI    PROPROH+1,X'20'     NO, PROTECT SPECIAL FIELD                    
         OI    PROPROH+6,X'80'     AND SET TO TRANSMIT                          
         CLC   CONACT(3),=C'DEL'   IS IT DELETE?                                
         BNE   XIT                 NO                                           
         TM    TWAAUTH,X'10'       MUST BE ON TO DELETE                         
         BZ    ERREND                                                           
         CLI   DDS,C'Y'            IF A DDS TERMINAL, UNPROTECT IT              
         BNE   XIT                                                              
         NI    PROPROH+1,X'FF'-X'20'                                            
         B     XIT                                                              
*                                                                               
INT03    TM    TWAAUTH,X'10'       MUST BE ON TO DELETE                         
         BZ    ERREND                                                           
         B     XIT                                                              
         EJECT                                                                  
*****************************************************************               
*              INITIALIZE SYSTEM ADDRESSES                      *               
*****************************************************************               
         SPACE 1                                                                
SYSINIT  NTR1                                                                   
*                                  GET TERMINAL VALUES                          
         MVI   DDS,C'N'                                                         
         CLI   TWAOFFC,C'*'        TEST FOR DDS TERMINAL                        
         BNE   *+8                                                              
         MVI   DDS,C'Y'                                                         
         MVC   TERM,TWATRM                                                      
         MVC   USERID,TWAORIG                                                   
         SPACE 1                                                                
*                                  SET UP CERTAIN ADDRESSES                     
         L     R1,SYSPARMS                                                      
         MVC   COMPANY,0(R1)       GET COMPANY CODE                             
         L     R2,0(R1)            GET A(TIOB)                                  
         LA    R2,0(R2)            CLEAR HIGH ORDER BYTE                        
         USING TIOBD,R2            TRANSLATOR I/O BLOCK                         
         ST    R2,ATIOB            SAVE A(TIOB)                                 
*                                  EXTRACT TIOB VALUES                          
         MVC   MODFRST,TIOBFRST    TWA INDEX 1ST FLD INPUT                      
         MVC   MODLAST,TIOBLAST    TWA INDEX LAST FLD INPUT                     
         MVC   CURDISP,TIOBCURD    CURSOR DISPLACEMENT                          
         SPACE 1                                                                
         ZIC   RE,TIOBAID          GET PF KEY INPUT THIS TIME                   
         LA    RF,12                                                            
         CR    RE,RF                                                            
         BNH   *+6                 ADJUST FOR PF13-PF24                         
         SR    RE,RF                                                            
         STC   RE,PFKEY                                                         
         DROP  R2                                                               
         SPACE 1                                                                
         L     RF,20(R1)                                                        
         ST    RF,AXTRAI           SAVE A(EXTRA INFO BLOCK)                     
         MVC   AGYOPTS,0(R1)                                                    
         MVC   AGYCTRY,1(RF)                                                    
         MVC   AGYLANG,3(RF)                                                    
         MVC   AGYCURR,4(RF)                                                    
         CLI   AGYLANG,0                                                        
         BNE   *+8                                                              
*&&US*&& MVI   AGYLANG,LANGEUS                                                  
*&&UK*&& MVI   AGYLANG,LANGEUK                                                  
         CLI   AGYCTRY,0                                                        
         BNE   *+8                                                              
*&&US*&& MVI   AGYCTRY,CTRYUSA                                                  
*&&UK*&& MVI   AGYCTRY,CTRYGBR                                                  
         SPACE 1                                                                
         L     RF,8(R1)                                                         
         USING ACCFACSD,RF                                                      
         MVC   ACCEMU,AACCEMU                                                   
         DROP  RF                                                               
         SPACE 1                                                                
         LM    R3,R4,12(R1)        GET SOME ADDRS FOR WORK DONE                 
         ST    R3,ATIA             BEFORE GOING TO GENCON                       
         ST    R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
         MVC   CALLOV,CCALLOV                                                   
         MVC   DATAMGR,CDATAMGR                                                 
         MVC   ADDAY,CADDAY                                                     
         MVC   GETMSG,CGETMSG                                                   
         MVC   GETPROF,CGETPROF                                                 
         MVC   HELLO,CHELLO                                                     
         MVC   SCANNER,CSCANNER                                                 
         MVC   DATCON,CDATCON                                                   
         MVC   CUREDIT,CCUREDIT                                                 
         MVC   PARSNIP,CPARSNIP                                                 
         SPACE 1                                                                
         XC    DMCB,DMCB           OBTAIN CORE-RESIDENT ADDRESSES               
         LA    R2,CORETAB                                                       
         LA    R0,CORES            COUNTER                                      
         LA    R3,COREFACS         POINT TO ADDRESS AREA                        
         L     RF,CALLOV                                                        
         LA    R1,DMCB                                                          
         MVC   4(3,R1),=X'D9000A'  T000AXX WHERE XX IS PHASE NUMBER             
         SPACE 1                                                                
SYS1     MVC   7(1,R1),0(R2)                                                    
         GOTO1 (RF),(R1),0                                                      
         MVC   0(4,R3),0(R1)       SAVE MODULE ADDRESS                          
         LA    R2,1(R2)            NEXT MODULE NUMBER                           
         LA    R3,4(R3)            NEXT ADDRESS                                 
         BCT   R0,SYS1                                                          
         SPACE 1                                                                
         SR    R3,R3               SET UP COMMON ENTRIES FOR PROGRAM            
         LA    R2,VCOMMON                                                       
         LA    R4,INTCOMM          START ADDRESSES                              
         LA    R5,NINTCOMM         NUMBER OF ADDRESSES                          
         SPACE 1                                                                
SYS6     ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,SYS6                                                          
         SPACE 1                                                                
SYS8     CLI   TWAMODE,1           TEST IF OFFLINE                              
         BNE   SYS10               NO                                           
         SPACE 1                                                                
         L     RE,TWADCONS                                                      
         USING TWADCOND,RE                                                      
         MVC   BINSRCH,TBINSRCH    YES-GET EXTRA ADDRESSES                      
*        MVC   COVAIL,TCOVAIL                                                   
         DROP  RE                                                               
         EJECT                                                                  
*****************************************************************               
*              OTHER INITIALIZATION                             *               
*****************************************************************               
         SPACE 1                                                                
SYS10    MVC   DUMPSYSD,=C'**SYSD**' SEED SYSD WITH DUMP COMMENTS               
         MVC   DUMPASRT,=C'*ASSORT*'                                            
         MVC   DUMPATRN,=C'*ADTRND*'                                            
         MVC   DUMPINT,=C'*INTGEN*'                                             
         MVC   DUMPUDAT,=C'*USEFUL*'                                            
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(1,TODAYP)                                     
         GOTO1 (RF),(R1),(1,TODAYP),(0,WORK)                                    
*                                                                               
         GOTO1 ADDAY,DMCB,(C'Y',WORK),WORK+6,1                                  
         GOTO1 (RF),(R1),,WORK+12,-1                                            
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,NEXTP)                                 
         GOTO1 (RF),(R1),(0,WORK+12),(1,LASTP)                                  
*                                                                               
         LA    R1,BUFF             MAKE BUFFER AREAS ADDRESSABLE                
         MVC   0(8,R1),=C'**AIO4**'                                             
         LA    R1,8(R1)                                                         
         ST    R1,AIO4                                                          
         LA    R1,2000(R1)                                                      
         MVC   0(8,R1),=C'**AIO5**'                                             
         LA    R1,8(R1)                                                         
         ST    R1,AIO5                                                          
         LA    R1,2000(R1)                                                      
         MVC   0(8,R1),=C'**AIO6**'                                             
         ST    R1,AIO6                                                          
         LH    R1,=Y(OFFBLK-SUBSYSD)                                            
         LA    R1,SUBSYSD(R1)                                                   
         ST    R1,AOFFBLK                                                       
         LH    R1,=Y(TSARBLK-SUBSYSD)                                           
         LA    R1,SUBSYSD(R1)                                                   
         ST    R1,ATSARBLK                                                      
         SPACE 1                                                                
*                                  SET SYSTEM DEPENDENT VALUES                  
         L     R1,RELO                                                          
         L     RF,=V(TWABLD)                                                    
         AR    RF,R1                                                            
         ST    RF,VTWABLD                                                       
         L     RF,=V(BMONVAL)                                                   
         AR    RF,R1                                                            
         ST    RF,VBMONVAL                                                      
         L     RF,=V(DUMMY)        END OF SYSTEM BASE                           
         AR    RF,R1                                                            
         ST    RF,SYSDUMMY                                                      
         L     RF,=A(RECACTS)      A(RECORD/ACTION TABLE)                       
         AR    RF,R1                                                            
         ST    RF,ARECACT                                                       
         SPACE 1                                                                
         MVI   SYSTEM,C'A'         ACCOUNT                                      
         MVI   MAXIOS,NIOS         SET NUMBER OF I/O AREAS                      
         MVC   SIZEIO,=AL4(LIOS)   LENGTH OF THE I/O AREAS                      
         MVC   GETUSER,USER        1ST ROUTINE CALLED BY GENCON                 
         SPACE 1                                                                
         MVC   LKEY,=H'42'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=Y(L'ACSTATUS)                                           
         MVC   DATADISP,=Y(ACRECORD-ACKEYD)                                     
         MVC   SYSDIR,=C'ACCOUNT '                                              
         MVC   SYSFIL,=C'ACCOUNT '                                              
         MVC   REQFILE,=C'ACCREQ ' REQTWA REQUEST FILE NAME                     
         OI    GENSTAT1,OKADDEL    OK FOR GENCON TO ADD OVER DEL RECS           
         MVI   USEIO,C'Y'          ONLY ACCPAK DOES THIS                        
         MVI   ACTELOPT,C'N'       NO ACTIVITY ELEMENTS                         
         MVI   GETMSYS,X'06'       USES GETMSG FOR SYSTEM X'06'(ACC)            
         SPACE 1                                                                
         MVC   LWORK,=AL4(LENWORK)    WE TOOK XXXXX BYTES IN NMOD               
         MVC   RCPROG(2),=C'AC'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D9061900'  PRESET FOR SYSTEM CALLOVS T619XX          
         LA    R1,BUFF             SET ADDRESS OF SAVED STORAGE                 
         ST    R1,ASTARTSV                                                      
         MVI   NTWA,X'81'          TAKE 1 LARGE 6K TWA                          
         MVI   LRECACT,L'RECACTS   SET LENGTH OF RECACT TABLE ENTRY             
*                                                                               
*YS15    GOTO1 DATAMGR,DMCB,=C'DTFADD ',=C'ACCOUNT '                            
*        L     RE,12(R1)                                                        
*        TM    ISFTYPE-ISDTF(RE),ISFTEMU                                        
*        BZ    *+8                                                              
         MVI   EMULATE,C'Y'        SET FLAG FOR EMULATED ACCOUNT FILE           
         B     XIT                                                              
         EJECT                                                                  
********************************************************************            
* THIS ROUTINE CALLS GENCON BUT DOES NOT RETAIN CONTROL WHEN GENCON*            
* EXITS.  INSTEAD, THE ROUTINE THAT CALLS THIS WILL GET CONTROL.   *            
* THIS ALLOWS THE CONTROLLER TO CALL GENCON AGAIN WITH A NEW RECORD*            
* AND ACTION IF, FOR INSTANCE, AN OVERLAY HAD A LIST ACTION AND A  *            
* SELECTION WAS MADE.                                              *            
********************************************************************            
         SPACE 1                                                                
CALLGENC NTR1                                                                   
         ST    RD,SYSRD            GENCON USES THIS TO EXIT ITSELF              
*                                  WE GET CONTROL BACK                          
         MVI   GOAGAIN,C'N'        DON'T GO AGAIN UNLESS OVERLAY WANTS          
         GOTO1 GENCON,DMCB,(R8)    TO CALL ANOTHER OVERLAY                      
         B     XIT                                                              
         EJECT                                                                  
******************************************************************              
*              SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY    *              
******************************************************************              
         SPACE 3                                                                
         DS    0H                                                               
VCOMMON  NTR1  BASE=SYSRB,LABEL=NO                                              
         L     RA,ATWA                                                          
         L     R9,ASYSD                                                         
         L     R7,BASER7                                                        
         L     R8,ASPOOLD                                                       
         ST    RD,COMMRD                                                        
         SRL   RF,24                                                            
         B     VBRANCH(RF)                                                      
         SPACE 1                                                                
VBRANCH  B     VUSER                                                            
         B     VSETCOMP                                                         
         B     VSETHEIR                                                         
         B     GETLED                                                           
         B     VVALOFF                                                          
         B     VVALACCT                                                         
         B     VVALCLI                                                          
         B     VVALPROD                                                         
         B     VVINCM                                                           
         B     VVALCOS                                                          
         B     VVALMED                                                          
         B     VVALEST                                                          
         B     VVALPERI                                                         
         B     VCANWEDL                                                         
         B     VANYNAME                                                         
         B     VACCELS                                                          
         B     VGETLOW                                                          
         B     VGETDUE                                                          
         B     VNAMEIN                                                          
         B     VNAMEOUT                                                         
         B     VNAMEIT                                                          
         B     CALL0                                                            
         B     CALL                                                             
         B     RETURN                                                           
         B     CLEARF                                                           
         B     ERRCUR                                                           
         B     CHKACC                                                           
         B     VVALPERE                                                         
         DC    24X'00'                                                          
         SPACE 1                                                                
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         EJECT                                                                  
********************************************************************            
* HOOK ROUTINE CALLED BY GENCON AFTER STORAGE HAS BEEN INITIALIZED *            
********************************************************************            
         SPACE 2                                                                
VUSER    DS    0H                                                               
         SPACE 1                                                                
         OC    USERNAME,USERNAME   TEST IF I HAVE BEEN HERE BEFORE              
         BNZ   VUSER10             YES                                          
*                                                                               
         BAS   RE,CHECK            ACTION CONTROL FOR CHECK SYSTEM              
*                                                                               
         MVC   FILENAME,=CL8'CTFILE'                                            
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,TWAORIG     READ ID RECORD                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         GOTO1 HELLO,DMCB,(C'G',FILENAME),('CTORGELQ',AIO),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   VUSER1              NO                                           
         L     R4,12(R1)                                                        
         USING CTORGD,R4                                                        
         MVC   USERNAME,CTORGNAM                                                
         MVC   USERADDR,CTORGADD                                                
         DROP  R4                                                               
*                                                                               
VUSER1   XC    FILENAME,FILENAME                                                
         GOTO1 SETCOMP                                                          
*                                                                               
         MVC   UNIT(2),PRODLEDG    GET PRODUCTION LEDGER                        
         GOTO1 GETLDG                                                           
         BE    *+6                                                              
         DC    H'0'                BIG TROUBLE                                  
         MVC   APROLEDG,THISLEDG   SAVE POINTER TO LEDGER DATA                  
*                                                                               
         MVC   UNIT(2),RECVLEDG    AND RECEIVABLES                              
         GOTO1 GETLDG                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ARCVLEDG,THISLEDG                                                
*                                                                               
         MVC   UNIT(2),BANKLEDG                                                 
         GOTO1 GETLDG                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ABNKLEDG,THISLEDG                                                
*                                                                               
         MVC   UNIT(2),=C'SI'                                                   
         GOTO1 GETLDG                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AINCLEDG,THISLEDG                                                
*                                                                               
VUSER2   L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         MVC   OFFACOMF,ACOMFACS   INITIALIZE OFFAL BLOCK                       
         MVC   OFFAALPH,TWAAGY                                                  
         MVC   OFFACPY,COMPANY                                                  
         MVC   OFFACST1(OFFAOPOS-OFFACST1),COMPSTA1                             
         MVC   OFFALIMA,TWAACCS                                                 
         MVC   OFFAAUTH,TWAAUTH                                                 
         CLI   TWAMODE,1           TEST OFFLINE                                 
         BE    VUSER4              YES                                          
*                                                                               
         MVI   OFFAINDS,OFFAIOFF                                                
         MVI   OFFAACT,OFFAINI     SET TO INITIALIZE                            
         CLC   COMPANY,LASTCOMP    TEST FOR CHANGE IN COMPANIES                 
         BNE   *+14                YES                                          
         MVI   OFFAACT,OFFARES                                                  
         MVC   OFFASAV(OFFASAVL),SAVEOFFA                                       
         GOTO1 OFFAL                                                            
         BE    VUSER10                                                          
         DC    H'0'                                                             
*                                                                               
VUSER4   OI    OFFAINDS,OFFAIOFF+OFFAIOFS NOTE OFFLINE UNDER SPOOF              
         MVI   OFFAACT,OFFAINI                                                  
         GOTO1 OFFAL                                                            
         BE    VUSER10                                                          
         DC    H'0'                                                             
*                                  AGENCY NAME & ADDRESS FROM ID REC.           
*        SPACE 1                                                                
*USER6   GOTO1 GETFACT,DMCB,0                                                   
*        L     R4,DMCB                                                          
*        MVC   FACTWRK(FACTSLEN),0(R4)  SAVE SYSTEM INFO BLOCK                  
         SPACE 1                                                                
VUSER10  CLI   RACHANGE,C'Y'       TEST IF RECORD/ACTION CHANGED                
         BNE   VUSER12                                                          
         MVI   CALLSP,0            YES-CLEAR STACK POINTER                      
         SPACE 1                                                                
         CLI   CONWHENH+5,0        TEST ANY INPUT IN WHEN                       
         BE    VUSER12             NO                                           
*                                                                               
         ZIC   R1,CONACTH+5        GET ACTION FIELD LENGTH                      
         ZIC   RE,CONRECH+5        GET RECORD FIELD LENGTH                      
         LA    R0,3                                                             
         CR    R1,R0                                                            
         BL    *+6                                                              
         LR    R1,R0               MAXIMUM COMPARE DONE BY GENCON IS 3          
         CR    RE,R0                                                            
         BL    *+6                                                              
         LR    RE,R0                                                            
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,REPCOMP          TEST FOR REPORT ACTION                       
         BE    VUSER12             YES-LEAVE WHEN FIELD ALONE                   
*                                                                               
         BCTR  RE,0                                                             
*        EX    R1,LISCOMP          TEST FOR EST/LIST                            
*        BE    VUSER12             YES-LEAVE WHEN ALONE                         
         EX    R1,SELCOMP          TEST FOR SELECT                              
         BE    VUSER12             YES                                          
         EX    R1,PEECOMP          TEST FOR PEEL                                
         BE    VUSER12             YES                                          
*                                                                               
VUSER11  XC    CONWHEN,CONWHEN     CLEAR UNNECESSARY WHEN FIE                   
         MVI   CONWHENH+5,0                                                     
         OI    CONWHENH+6,X'80'    XMIT BACK                                    
*                                                                               
VUSER12  CLI   PFKEY,PF12          TEST FOR PF12 (RETURN)                       
         BNE   VUSERX              NO                                           
         CLI   CALLSP,0            TEST ANY THING ON STACK                      
         BE    VUSERX              NO                                           
         SPACE 1                                                                
         LA    R0,RETURNS          R0=COUNTER                                   
         LA    RE,RETTAB                                                        
         CLC   TWASCR,0(RE)        TEST SCREEN AGAINST RETURN LIST              
         BE    VUSER15             RETURN IS SUPPORTED                          
         LA    RE,L'RETTAB(RE)                                                  
         BCT   R0,*-14                                                          
         B     VUSERX              CANNOT RETURN FROM THIS SCREEN               
         SPACE 1                                                                
VUSER15  MVI   PFKEY,0             CLEAR OUT PF12 SETTING                       
         GOTO1 VRETURN                                                          
         SPACE 1                                                                
VUSERX   B     XIT                                                              
*                                                                               
REPCOMP  CLC   CONACT(0),=CL8'REPORT'                                           
*ISCOMP  CLC   CONACT(0),=CL8'LIST'                                             
SELCOMP  CLC   CONACT(0),=CL8'SELECT'                                           
PEECOMP  CLC   CONACT(0),=CL8'PEEL'                                             
         EJECT                                                                  
*****************************************************************               
* SUB-ROUTINE TO PREVENT MANUAL INPUT OF CHECK/LIST OR          *               
* ESTIMATE--CALLED BY VUSER                                     *               
*****************************************************************               
         SPACE 1                                                                
CHECK    NTR1  ,                                                                
         LA    R2,CONRECH                                                       
         CLI   5(R2),0             TEST NO INPUT                                
         BE    CHECKX                                                           
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   CONREC(0),=CL8'CHECK'                                            
         BNE   CHECKX                                                           
*                                                                               
         LA    R2,CONACTH                                                       
         CLI   5(R2),0                                                          
         BE    CHECKX                                                           
         CLI   RACHANGE,C'Y'                                                    
         BE    CHECK10             IF ANYTHING CHANGED - REVALIDATE             
         TM    TWASTAT1,VALKEYOK   IF VALKEY WAS DONE - DONT VALIDATE           
         BO    CHECKX                                                           
         CLI   TWASCR,X'F3'        IF WE ARE CURRENTLY PAST LOGIN               
         BNL   CHECKX                SCREEN DONT VALIDATE                       
*        CLI   TWASCR,X'FF'                                                     
*        BE    CHECKX                                                           
*                                                                               
CHECK10  ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   CONACT(0),=CL8'LIST'                                             
         BE    CHECKR                                                           
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   CONACT(0),=CL8'ESTIMATE'                                         
         BE    CHECKR                                                           
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   CONACT(0),=CL8'REVIEW'                                           
         BE    CHECKR                                                           
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   CONACT(0),=CL8'DRAFT'                                            
         BE    CHECKR                                                           
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   CONACT(0),=CL8'FILTER'                                           
         BE    CHECKR                                                           
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   CONACT(0),=CL8'UPDATE'                                           
         BE    CHECKR                                                           
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   CONACT(0),=CL8'QUIT'                                             
         BNE   CHECKX                                                           
*                                                                               
CHECKR   MVI   ERROR,INVACT                                                     
         B     ERREND                                                           
*                                                                               
CHECKX   B     XIT                                                              
         EJECT                                                                  
*****************************************************************               
*              COMPANY RECORD AND VALUES                        *               
*****************************************************************               
         SPACE 3                                                                
VSETCOMP DS    0H                                                               
         PUSH  USING                                                            
         LA    R4,KEY                                                           
         USING ACKEYD,R4                                                        
         MVC   KEY,SPACES                                                       
         MVC   ACKEYACC(1),COMPANY                                              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
*                                                                               
         MVI   ELCODE,CPYELQ       X'10' COMPANY ELEMENT                        
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CPYELD,R6                                                        
         XC    CKMOSL,CKMOSL                                                    
         XC    CMOSLOCK,CMOSLOCK                                                
         XC    COMPGMOA,COMPGMOA                                                
         MVC   COMPSTA1,CPYSTAT1    COMPANY STATUS BYTE 1                       
         MVC   COMPSTA2,CPYSTAT2                                                
         MVC   COMPSTA3,CPYSTAT3                                                
         MVC   COMPSTA4,CPYSTAT4                                                
         MVC   COMPSTA5,CPYSTAT5                                                
         MVC   COMPSTA6,CPYSTAT6                                                
         MVC   COMPSTA7,CPYSTAT7                                                
         MVC   COMPSTA8,CPYSTAT8                                                
         CLI   CPYLN,CPYLN3Q       LONG ENOUGH FOR THESE FIELDS?                
         BL    SETCOMP2            NO                                           
         MVC   COMPSTA9,CPYSTAT9                                                
         MVC   COMPSTAA,CPYSTATA                                                
         XC    COMPGMOA,CPYGLMOA                                                
*                                                                               
SETCOMP2 MVC   PRODLEDG,CPYPROD     PRODUCTION U/L                              
         MVC   RECVLEDG,CPYRECV     RECEIVABLES U/L                             
         MVC   BANKLEDG,CPYBANK     BANK U/L                                    
*                                                                               
         LA    R3,COMPNAME                                                      
         MVC   0(L'COMPNAME,R3),SPACES                                          
         GOTO1 NAMEIT                                                           
*                                                                               
SETCOMPX B     XIT                                                              
         SPACE 2                                                                
         POP   USING                                                            
         EJECT                                                                  
***************************************************************                 
*              LEDGER HEIRARCHY                               *                 
***************************************************************                 
         SPACE 3                                                                
VSETHEIR DS    0H                                                               
         LA    R4,KEY                                                           
         USING ACKEYD,R4                                                        
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'CUL),CUL                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         BAS   RE,SECCHECK         (SECURITY CHECK)                             
         SPACE 1                                                                
         MVI   ELCODE,ACLTELQ      LEDGER ELEMENT                               
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACLEDGD,R6                                                       
         MVC   LEDGTOFF,ACLTOFF    OFFICE POSITION ON THIS LEDGER               
         SPACE 1                                                                
         MVI   ELCODE,ACHRELQ      GET HEIRARCHY ELEMENT                        
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACHEIRD,R6                                                       
         MVC   LLEVA,ACHRLEVA       AND SET LENGTHS                             
         MVC   LLEVAB,ACHRLEVB                                                  
         MVC   LLEVABC,ACHRLEVC                                                 
         MVC   LLEVABCD,ACHRLEVD                                                
         CLC   PRODLEDG,UNIT       TEST IF PRODUCTION                           
         BNE   XIT                 NO-ALL DONE                                  
*                                                                               
         ZIC   R1,LCLIPRO                                                       
         ZIC   R0,LCLI                                                          
         SR    R1,R0                                                            
         STC   R1,LPRO                                                          
         IC    R1,LCLIJOB                                                       
         IC    R0,LCLIPRO                                                       
         SR    R1,R0                                                            
         STC   R1,LJOB                                                          
         IC    R1,LCLIJOB                                                       
         IC    R0,LCLI                                                          
         SR    R1,R0                                                            
         STC   R1,LPROJOB                                                       
         B     XIT                                                              
         EJECT                                                                  
******************************************************************              
* GETLDG - SUB-ROUTINE TO VALIDATE A LEDGER AND GET ITS LEDGER   *              
*          TABLE ENTRY                                           *              
*                                                                *              
*          ROUTINE MAINTAINS LEDGTAB IN SAVAREA TO OPTIMIZE      *              
*          READING LEDGER RECORDS.                               *              
*          ON ENTRY, CUL=COMPANY/UNIT/LEDGER                     *              
*          ON EXIT, CC=EQ IF VALID LEDGER AND THISLEDG=A(LEDGER  *              
*                   TABLE ENTRY)                                 *              
*                   CC=NEQ IF INVALID LEDGER                     *              
******************************************************************              
         SPACE 2                                                                
GETLED   XC    THISLEDG,THISLEDG   CLEAR TABLE ENTRY POINTER                    
         LA    R0,LDGTBMAX         R0=LOOP COUNTER                              
         LA    R2,LEDGTAB          R2=A(LEDGER TABLE)                           
         USING LDGTABD,R2                                                       
*                                                                               
GETLED1  OC    LDGTUL,LDGTUL       TEST FOR EOT                                 
         BZ    GETLED2             YES                                          
         CLC   LDGTUL,UNIT         MATCH ON UNIT LEDGER                         
         BE    GETLEDY                                                          
         LA    R2,LDGTABL(R2)                                                   
         BCT   R0,GETLED1                                                       
         SH    R2,=Y(LDGTABL)      BACK UP TO LAST ENTRY                        
*                                                                               
GETLED2  MVC   SAVEKEY1,KEY        SAVE PRESENT KEY                             
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'CUL),CUL                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVC   KEY,SAVEKEY1        RESTORE CALLER'S KEY                         
         BNE   GETLEDN             CAN'T FIND LEDGER RECORD                     
*                                                                               
GETLED3  L     R4,AIO                                                           
         USING ACKEYD,R4                                                        
         MVC   LDGTUL,UNIT                                                      
         LA    R6,ACRECORD                                                      
         SR    R0,R0                                                            
*                                                                               
GETLED4  CLI   0(R6),0             TEST FOR EOR                                 
         BE    GETLEDY             YES                                          
         CLI   0(R6),ACLTELQ       TEST FOR LEDGER ELEMENT                      
         BNE   GETLED5                                                          
         USING ACLEDGD,R6                                                       
         MVC   LDGTTYPE,ACLTTYPE                                                
         MVC   LDGTLIKE,ACLTLIKE                                                
         MVC   LDGTOFFP,ACLTOFF    OFFICE POSITION                              
         B     GETLED8                                                          
*                                                                               
GETLED5  CLI   0(R6),ACHRELQ       TEST FOR HEIRARCHY ELEMENT                   
         BNE   GETLED6                                                          
         USING ACHEIRD,R6                                                       
         MVC   LDGTLVA,ACHRLEVA                                                 
         MVC   LDGTLVB,ACHRLEVB                                                 
         MVC   LDGTLVC,ACHRLEVC                                                 
         MVC   LDGTLVD,ACHRLEVD                                                 
         B     GETLED8                                                          
*                                                                               
GETLED6  CLI   0(R6),ACSTELQ                                                    
         BNE   GETLED8                                                          
         USING ACSTATD,R6                                                       
         MVC   LDGTSEC,ACSTSECY+1 EXTRACT SECURITY NUMBER                       
*                                                                               
GETLED8  IC    R0,1(R6)            NEXT ELEMENT                                 
         AR    R6,R0                                                            
         B     GETLED4                                                          
*                                                                               
GETLEDY  ST    R2,THISLEDG                                                      
         CR    RB,RB               SET CC=EQ                                    
         B     XIT                                                              
*                                                                               
GETLEDN  LTR   RB,RB               SET CC=NEQ                                   
         B     XIT                                                              
         DROP  R2,R6                                                            
         EJECT                                                                  
*******************************************************************             
* ACCELS - EXTRACT ACCOUNT RECORD ADCONS AND VALUES               *             
*          AT ENTRY, AIO=A(ACCOUNT RECORD)                        *             
*******************************************************************             
         SPACE 1                                                                
VACCELS  XC    RECADS,RECADS       CLEAR RECORD ADCONS                          
         XC    RECVALS2,RECVALS2   AND VALUES                                   
         L     R4,AIO                                                           
         USING ACKEYD,R4                                                        
         LA    R6,ACRECORD         R6=A(FIRST ELEMENT)                          
         SR    R0,R0                                                            
*                                                                               
VACCEL2  CLI   0(R6),0             TEST FOR EOR                                 
         BE    VACCELX             YES                                          
         CLI   0(R6),ACNMELQ       TEST FOR NAME ELEMENT                        
         BNE   VACCEL3                                                          
         USING ACNAMED,R6                                                       
         MVC   RECNAME,SPACES                                                   
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=Y(ACNMNAME-ACNAMED+1)                                        
         EX    R1,*+8                                                           
         B     VACCEL8                                                          
         MVC   RECNAME(0),ACNMNAME                                              
*                                                                               
VACCEL3  CLI   0(R6),ACSTELQ                                                    
         BNE   VACCEL4                                                          
         USING ACSTATD,R6                                                       
         ST    R6,ASTATEL                                                       
         MVC   RECOFFC,SPACES                                                   
         MVC   RECSTAT,ACSTSTAT                                                 
         MVC   RECSTATX,ACSTSTX                                                 
         MVC   RECCOST,ACSTCOST                                                 
         MVC   RECSECY,ACSTSECY                                                 
         MVC   RECFILT1(2),ACSTFILT                                             
         MVC   RECFILT3,ACSTANAL                                                
         MVC   RECFILT4,ACSTSUB                                                 
         CLI   ACSTLEN,ACSTLNQ1    TEST FOR SHORT ELEMENT                       
         BE    VACCEL8             YES-ALL DONE                                 
         MVC   RECSTAT2,ACSTSTA2                                                
         MVC   RECFILT5,ACSTFLT5                                                
         B     VACCEL8                                                          
*                                                                               
VACCEL4  CLI   0(R6),ACBLELQ                                                    
         BNE   *+8                                                              
         ST    R6,ABALEL                                                        
         CLI   0(R6),ACPRELQ       TEST FOR PROFILE ELEM                        
         BNE   *+8                                                              
         ST    R6,APROFEL                                                       
*                                                                               
VACCEL5  CLI   0(R6),ACSPELQ       TEST FOR SPECIAL ACCT POINTER                
         BNE   VACCEL8             NO                                           
         USING ACSPECD,R6                                                       
         CLI   ACSPTYP,ACSPOAN     TEST FOR ANALYSIS POINTER                    
         BNE   *+10                NO                                           
         MVC   RECANAL,ACSPACCT                                                 
         B     VACCEL8                                                          
*                                                                               
VACCEL8  IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     VACCEL2                                                          
*                                                                               
VACCELX  B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
**************************************************************                  
* GETLOW - VALIDATE LOWEST LEVEL ACCOUNT                     *                  
*          AT ENTRY, KEY CONTAINS ACCOUNT RECORD KEY         *                  
*          ON EXIT, CC=EQ IF KEY IS VALID                    *                  
*                   CC=NEQ IF KEY IS INVALID/ERROR IS SET    *                  
**************************************************************                  
         SPACE 1                                                                
VGETLOW  GOTO1 GETLDG              ESTABLISH THE LEDGER                         
         BNE   VGETLOL                                                          
         ICM   R2,15,THISLEDG                                                   
         USING LDGTABD,R2          R2=A(LEDGER TABLE ENTRY)                     
         CLI   OFFLINE,C'Y'        TEST IF OFFLINE                              
         BE    VGETLO2             YES - CAN'T CHECK TWA                        
         CLC   TWAAUTH+1(1),LDGTSEC                                             
         BL    VGETLOS             SECURITY VIOLATION                           
*                                                                               
VGETLO2  MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                READ THE ACCOUNT RECORD                      
         CLC   KEY(42),KEYSAVE                                                  
         BNE   VGETLOA                                                          
         GOTO1 ACCELS                                                           
         TM    RECSTAT,X'60'       TEST LOCKED/CLOSED                           
         BNZ   VGETLOP             INVALID POSTING A/C                          
         OC    ABALEL,ABALEL                                                    
         BZ    VGETLOP                                                          
*                                                                               
VGETLO4  CLI   LDGTOFFP,C'1'       IS OFFICE IN A/C FILTERS?                    
         BL    VGETLO20                                                         
         CLI   LDGTLVA,L'ACKEYACC-3 IS THIS A ONE LEVEL LEDGER?                 
         BE    VGETLO20                                                         
         LA    R3,LDGTLVD                                                       
         LA    R0,LDGTLVD+1-LDGTLVA                                             
         CLI   0(R3),0                                                          
         BNE   *+10                                                             
         BCTR  R3,0                                                             
         BCT   R0,*-10             ESTABLISH LOWEST LEVEL                       
         LA    RE,VGETLO20         SET RETURN ADDRESS                           
         EJECT                                                                  
***********************************************************************         
* GRAB SOME STORAGE FOR INTERMEDIATE IO, ADDRESSED BY R4              *         
* NTRY - R0=NUMBER OF LEVELS                                          *         
*        R3=A(L'LOWEST LEVEL)                                         *         
***********************************************************************         
         SPACE 1                                                                
VGETLO6  NTR1  WORK=(R4,LIOS)                                                   
         MVC   SAVEKEY1,KEY        SAVE ACCOUNT KEY                             
         MVC   FULL,AIO            SAVE IO AREA                                 
         ST    R4,AIO              SET NEW IO AREA                              
*                                                                               
VGETLO8  MVC   KEY,SPACES                                                       
         IC    RF,0(R3)            TAKE L'LEVEL                                 
         LA    RF,2(RF)            ADD CUL (-1 FOR EXECUTE)                     
         EX    RF,*+4                                                           
         MVC   KEY(0),SAVEKEY1     SET ACCOUNT KEY                              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(42),KEYSAVE     TEST IF RECORD FOUND                         
         BE    *+6                                                              
         DC    H'0'                DUMP IF CANNOT FIND HIGHER LEVEL             
         MVI   ELCODE,ACSTELQ      GET STATUS ELEMENT                           
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                BIG TROUBLE IF NO STATUS                     
         USING ACSTATD,R6                                                       
*                                                                               
VGETLO10 MVC   BYTE(1),LDGTOFFP    EXTRACT FILTER POSITION FOR OFFICE           
         NI    BYTE,X'07'          X'F1'...X'F5' -> X'01'...X'05'               
         SR    RE,RE                                                            
         IC    RE,BYTE                                                          
         IC    RE,FILDISP-1(RE)                                                 
         LA    RE,ACSTATD(RE)                                                   
         CLI   0(RE),C' '          TEST OFFICE FILTER SET AT THIS LEVEL         
         BH    VGETLO12                                                         
         BCTR  R3,0                R3=A(L'PREVIOUS LEVEL)                       
         BCT   R0,VGETLO8          TRY AGAIN ONE LEVEL HIGHER                   
         B     VGETLO14                                                         
*                                                                               
VGETLO12 L     R1,AOFFBLK                                                       
         USING OFFALD,R1           R1=A(OFFAL CONTROL BLOCK)                    
         MVC   OFFAOFFC(1),0(RE)   EXTRACT OFFICE CODE                          
         MVI   OFFAOFFC+1,C' '                                                  
*                                                                               
VGETLO14 MVC   AIO,FULL                                                         
         MVC   KEY,SAVEKEY1        RESTORE KEY                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                RE-READ ACCOUNT                              
         B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
VGETLO20 L     R1,AOFFBLK                                                       
         USING OFFALD,R1           R1=A(OFFAL CONTROL BLOCK)                    
         MVC   OFFAREC,AIO                                                      
         MVC   OFFAOPOS,LDGTOFFP   SET LEDGER VALUES FOR OFFAL                  
         MVC   OFFALDGL,LDGTLVA                                                 
         MVC   OFFALDGT,LDGTCLOS                                                
         MVI   OFFAACT,OFFATST                                                  
         GOTO1 OFFAL               CALL OFFAL FOR OFFICE/SECURITY CHECK         
         BNE   VGETLOS                                                          
         MVC   RECOFFC,OFFAOFFC    SET OFFICE CODE                              
*                                                                               
VGETLOX  CR    RB,RB                                                            
         B     XIT                                                              
         DROP  R1,R2                                                            
         SPACE 1                                                                
VGETLOL  MVI   ERROR,INVLEDG                                                    
         B     VGETLON                                                          
VGETLOA  MVI   ERROR,INVACCT                                                    
         B     VGETLON                                                          
VGETLOP  MVI   ERROR,INVPOST                                                    
         B     VGETLON                                                          
VGETLOS  MVI   ERROR,SECLOCK                                                    
*                                                                               
VGETLON  OI    GENSTAT2,USMYERSY                                                
         LTR   RB,RB                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*             GET THE DUE DATE + DAYS FROM THE $INT CONTROL PROFILE   *         
*             AT ENTRY , CUL MUST BE HEXCOMP THEN SJ. ALSO, CLICODE   *         
*             AND CLIOFFC MUST BE SET (CLIENT CODE AND CLIENTS OFFICE)*         
***********************************************************************         
         SPACE 1                                                                
VGETDUE  DS    0H                                                               
         XC    WORK,WORK                  CLEAR WORK                            
         XC    PROGPROF,PROGPROF          CLEAR PROFILE AREA                    
         XC    EFFDUEDA,EFFDUEDA          THE + DUE DAYS                        
         LA    R2,2                       TWO PASSES TO GETPROF                 
         SPACE 1                                                                
*                                         ** GET COMPANY PROFILE **             
         SPACE 1                                                                
VGETDUE1 MVC   WORK(4),=C'AINT'           A=SYSTEM INT IS PROG ONLINE           
         NI    WORK,X'BF'                 LOWER CASE SYSTEM FOR ON-LINE         
         MVC   WORK+4(1),CUL              HEXCOMP CODE                          
         MVC   WORK+12(2),TWAAGY          AGENCY ALPHA                          
         SPACE 1                                                                
         GOTO1 GETPROF,DMCB,WORK,PROGPROF,DATAMGR                               
         BCT   R2,*+14                                                          
         MVC   EFFDUEDA,PROGPROF          SAVE THE ++DUE DAYS                   
         B     XIT                                                              
         SPACE 2                                                                
*                                         ** GET CLIENT PROFILE **              
         XC    WORK,WORK                                                        
         MVC   WORK+4(L'CUL),CUL                                                
         MVC   WORK+7(3),CLICODE                                                
         CLC   CLIOFFC,SPACES             NO OFFICE CODE                        
         BE    VGETDUE1                                                         
         CLC   CLIOFFC+1(L'CLIOFFC-1),SPACES                                    
         BNE   VGETDUE3                                                         
         MVI   WORK+10,C'*'               ONE BYTE OFFICE CODE                  
         MVC   WORK+11(1),CLIOFFC                                               
         B     VGETDUE1                                                         
VGETDUE3 MVI   WORK+10,C'+'               TWO BYTE OFFICE CODE                  
         MVC   WORK+14(L'CLIOFFC),CLIOFFC                                       
         B     VGETDUE1                                                         
         EJECT                                                                  
*************************************************************                   
*        VALIDATE OFFICE                                    *                   
*************************************************************                   
         SPACE 3                                                                
VVALOFF  DS    0H                                                               
         GOTO1 ANY                                                              
         MVC   EFFOFF,WORK         EFFECTIVE OFFICE                             
         MVC   EFFOFFC,WORK        EFFECTIVE OFFICE GROUP                       
         SPACE 1                                                                
         LA    R4,KEY                                                           
         USING ACOGKEY,R4                                                       
         XC    ACOGKEY,ACOGKEY     OFFICE/OFFICE GROUP RECORDS                  
         MVI   ACOGRTYP,ACOGEQU    THESE ARE X'2C' RECORDS                      
         MVI   ACOGSREC,ACOGOFF    SUB RECORD X'04'                             
         MVC   ACOGCUL,CUL                                                      
         MVC   ACOGCODE,EFFOFFC                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         GOTO1 ANYNAME                                                          
         B     XIT                                                              
         EJECT                                                                  
*************************************************************                   
*        VALIDATE ACCOUNT                                   *                   
*************************************************************                   
VVALACCT DS    0H                                                               
         GOTO1 SETHEIR             SET HEIRARCHY AND VALIDATE LEDGER            
         GOTO1 ANY                 VERIFY THAT THERE IS INPUT                   
         MVC   RECCODE,WORK                                                     
         SPACE 1                                                                
         LA    R6,LLEVA            LENGTH OF LEVEL A                            
         LA    R0,1                LEVEL NUMBER                                 
         SPACE 1                                                                
VAC02    CLC   5(1,R2),0(R6)       FIND HIERARCHY LEVEL FOR ACCT CODE.          
         BNH   VAC04                                                            
         LA    R6,1(R6)            BUMP TO LENGTH OF NEXT LEVEL                 
         AH    R0,=H'1'            LEVEL NUMBER                                 
         CH    R0,=H'4'            ARE WE UP TO 4TH LEVEL?                      
         BNH   VAC02               NO -CONTINUE                                 
         MVI   ERROR,TOOLONG                                                    
         B     ERREND                                                           
         SPACE 1                                                                
*                                  MUST BE A LOW LEVEL ACCOUNT                  
VAC04    CH    R0,=H'4'            IF IT'S THE 4TH LEVEL,                       
         BE    VAC08                                                            
         CLI   1(R6),0             OR THE NEXT LEVEL LENGTH IS ZERO.            
         BE    VAC08                                                            
         CLI   OPTION2,C'Y'        HIGH LEVEL ACCOUNT INPUT PERMITTED?          
         BE    VAC08                                                            
         MVI   ERROR,WRNGLVAC      IT ISN'T.                                    
         B     ERREND                                                           
         SPACE 1                                                                
VAC08    LA    R6,LLEVA            MAKE SURE HIGHER LEVELS EXIST                
         MVI   ERROR,NOHIGHER      SET ERROR MESSAGE                            
         SPACE 1                                                                
VAC10    MVC   KEY,SPACES                                                       
         MVC   KEY(L'CUL),CUL                                                   
         ZIC   R1,0(R6)            LENGTH OF HIGHER LEVELS                      
         SPACE 1                                                                
         CH    R0,=H'1'            IS THIS THE LOWEST LEVEL                     
         BNE   VAC12               NO - CONTINUE                                
         MVI   ERROR,INVALID       SET ERROR MESSAGE                            
         CLC   5(1,R2),0(R6)       COMPARE LEVEL LEN TO SCREN INPUT LEN         
         BNL   VAC12               IF SCREEN INPUT LENGTH IS LESS               
         ZIC   R1,5(R2)            USE THAT LENGTH                              
         SPACE 1                                                                
VAC12    BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   KEY+L'CUL(0),8(R2)      R2 = A(ACCT CODE FIELD HEADER).          
         MVI   ERROR,NOTFOUND          ERROR MESSAGE                            
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(42),KEY                                                  
         BNE   ERREND              ERROR ACCOUNT AT THIS LEVEL MISSING          
         SPACE 1                                                                
         LA    R6,1(R6)            LENGTH OF NEXT LEVEL                         
         BCT   R0,VAC10            GO BACK AND VALIDATE                         
         SPACE 1                                                                
         CLI   OPTION2,C'Y'        HIGH LEVEL ACCOUNT INPUT PERMITTED?          
         BE    VAC16                                                            
         MVI   ELCODE,ACBLELQ                                                   
         BAS   RE,GETELIO          CHECK FOR A BALANCE ELEMENT                  
         BE    VAC16                                                            
         OI    GENSTAT2,USMYERSY   USE MY ERROR LOWER THAN 60                   
         MVI   ERROR,INVPOST       INVALID ACCOUNT FOR POSTING                  
         B     ERREND                                                           
         SPACE 1                                                                
VAC16    LA    R3,RCVNAME                                                       
         MVC   0(L'RCVNAME,R3),SPACES                                           
         GOTO1 NAMEIT              GET NAME  ELEMENT                            
         SPACE 1                                                                
VAC18    BAS   RE,SECCHECK         (SECURITY CHECK)                             
         GOTO1 ANYNAME                                                          
         B     XIT                                                              
         EJECT                                                                  
**********************************************************                      
*              VALIDATE CLIENT                           *                      
**********************************************************                      
         SPACE 3                                                                
VVALCLI  DS    0H                                                               
         GOTO1 SETHEIR             CHECK THE HEIRARCHY                          
         GOTO1 ANY                 SHOULD ALWAYS BE INPUT                       
         MVC   CLICODE,WORK                                                     
         MVI   ERROR,TOOLONG                                                    
         CLC   5(1,R2),LCLI                                                     
         BH    ERREND                                                           
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'CUL),CUL                                                   
         MVC   KEY+L'CUL(L'CLICODE),CLICODE    READ FOR THE CLIENT              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         SPACE 1                                                                
         BAS   RE,SECCHECK         (SECURITY CHECK)                             
         CLI   TWAACCS,C'*'        TEST FOR OFFICE SECURITY                     
         BE    VVALCL4             YES-DO NOT CHECK AT CLIENT LEVEL             
         CLI   TWAACCS,C'$'                                                     
         BE    VVALCL4                                                          
         BAS   RE,ACCCHECK         CHECK ANYTHING ELSE                          
         SPACE 1                                                                
VVALCL4  GOTO1 ANYNAME                                                          
         SPACE 1                                                                
         MVC   COSTCODE,SPACES     GET COSTING CODE AND OFFICE                  
         MVC   COSTNAME,SPACES                                                  
         MVI   ELCODE,ACPRELQ                                                   
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACPROFD,R6                                                       
         MVC   CLIOFFC,ACPROFFC                                                 
         MVC   EFFOFFC,ACPROFFC                                                 
         TM    COMPSTA1,X'10'      ON COSTING?                                  
         BZ    VVALCL6             NO                                           
         CLC   ACPRCOST,SPACES     HAVE A COST CODE?                            
         BNH   VVALCL6             NO                                           
         MVC   COSTCODE(L'ACPRCOST),ACPRCOST                                    
VVALCL6  DS    0H                                                               
         GOTO1 GETDUE              GET + DUE DATES                              
         B     XIT                                                              
         EJECT                                                                  
**************************************************************                  
*              PRODUCT ROUTINES                              *                  
**************************************************************                  
         SPACE 3                                                                
VVALPROD DS    0H                                                               
         GOTO1 ANY                 SHOULD ALWAYS BE INPUT                       
         MVC   PRODCODE,WORK                                                    
         MVI   ERROR,TOOLONG                                                    
         CLC   5(1,R2),LPRO                                                     
         BH    ERREND                                                           
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'CUL),CUL                                                   
         MVC   KEY+L'CUL(L'CLICODE),CLICODE                                     
         ZIC   R1,LCLI                                                          
         LA    R1,KEY+L'CUL(R1)                                                 
         MVC   0(L'PRODCODE,R1),PRODCODE    READ FOR THE PRODUCT                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         LA    R3,PRONAME                                                       
         MVC   0(L'PRONAME,R3),SPACES                                           
         GOTO1 NAMEIT                                                           
         MVI   ELCODE,ACPRELQ      GET PROFILE ELEMENT                          
         BAS   RE,GETELIO                                                       
         BE    *+10                                                             
         SR    R6,R6               SET R6 TO ZERO IF NOT FOUND                  
         B     VVALPR2                                                          
         USING ACPROFD,R6                                                       
         MVC   PRODOFFC,ACPROFFC                                                
         CLC   ACPROFFC,SPACES                                                  
         BNH   *+10                                                             
         MVC   EFFOFFC,ACPROFFC    PRODUCT LEVEL OFFICE                         
         SPACE 1                                                                
VVALPR2  BAS   RE,SECCHECK         (SECURITY CHECK)                             
         BAS   RE,ACCCHECK         (ACCESS CHECK)                               
         GOTO1 ANYNAME                                                          
         GOTO1 VALCOS                                                           
VVALPRX  B     XIT                                                              
         EJECT                                                                  
**************************************************************                  
*              INCOME ANALYSIS ROUTINE                       *                  
* (ASSUMES THAT AIO IS POINTING AT SI INCOME ACCOUNT RECORD) *                  
* GET THE 12 ACCOUNT FROM X'2C' OR X'30'                     *                  
**************************************************************                  
         SPACE 3                                                                
VVINCM   DS    0H                                                               
         TM    COMPSTA1,X'10'      IS AGENCY ON COST ACCOUNTING?                
         BZ    XIT                 NO - GET OUT                                 
         MVC   A12CODE,SPACES                                                   
         MVC   A12NAME,SPACES                                                   
         MVC   A11CODE,SPACES                                                   
         MVC   A11NAME,SPACES                                                   
         MVC   A12CODE(L'COMPANY),COMPANY  YES- MOVE COMP INTO 12               
         SPACE 1                                                                
         MVI   ELCODE,ACSPELQ      SPECIAL POSTING ACCOUNT EL X'2C'             
         BAS   RE,GETELIO                                                       
         BNE   VVINCM4                                                          
         USING ACSPECD,R6                                                       
VVINCM2  CLI   ACSPTYP,ACSPOAN     IS THIS AN ANALYSIS ACCOUNT?                 
         BNE   VVINCM2A                                                         
         MVC   A12CODE+1(2),=C'12'                                              
         MVC   A12CODE+3(L'A12CODE-3),ACSPACCT                                  
         MVC   A11CODE,A12CODE                                                  
         MVI   A11CODE+2,C'1'                                                   
         B     VVINCM6                                                          
VVINCM2A BAS   RE,NEXTEL                                                        
         BE    VVINCM2                                                          
         SPACE 1                                                                
VVINCM4  MVI   ELCODE,ACSTELQ      STATUS ELEMENT X'30'                         
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                30 ELEM MUST EXIST                           
         USING ACSTATD,R6                                                       
         MVC   A12CODE+1(2),=C'12'                                              
         MVC   A12CODE+3(L'ACSTCOST),ACSTCOST                                   
         MVC   A11CODE,A12CODE                                                  
         MVI   A11CODE+2,C'1'                                                   
         SPACE 1                                                                
VVINCM6  MVC   KEY,SPACES                                                       
         MVC   KEY(L'A12CODE),A12CODE                                           
         GOTO1 GETLOW                                                           
         BNE   VVINCM8                                                          
         MVC   A12NAME,RECNAME                                                  
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'A11CODE),A11CODE                                           
         GOTO1 GETLOW                                                           
         BNE   VVINCM8                                                          
         MVC   A11NAME,RECNAME                                                  
         B     XIT                                                              
VVINCM8  MVC   CONHEAD(L'BADANAL),BADANAL   BAD ANALYSIS CODE MSG               
         B     MYEND                                                            
         EJECT                                                                  
**************************************************************                  
*              COST ACCOUNT ROUTINES                         *                  
* (ASSUMES THAT AIO IS POINTING AT EITHER CLI OR PRD RECORD  *                  
*  AND R6=A(PROFILE ELEMENT))                                *                  
**************************************************************                  
         SPACE 3                                                                
VVALCOS  DS    0H                                                               
         USING ACPROFD,R6                                                       
         TM    COMPSTA1,X'10'              ON COST ACCOUNTING?                  
         BZ    VVALCOSX                    NO - GET OUT                         
         LTR   R6,R6                       TEST ANY PROFILE ELEMENT             
         BZ    VVALCOS6                    NO- CHECK IF ONE WAS PASSED          
         SPACE 1                                                                
         CLC   ACPRCOST,SPACES             DO WE HAVE A COST CODE               
         BNH   VVALCOS6                    NO - SEE IF ONE WAS PASSED           
         MVC   COSTCODE(L'ACPRCOST),ACPRCOST                                    
         SPACE 1                                                                
VVALCOS6 CLC   COSTCODE,SPACES             DO WE HAVE A COST CODE?              
         BNH   VVALCOSX                    NO - EXIT                            
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'COSTCODE),COSTCODE                                         
         GOTO1 GETLOW                                                           
         BNE   ERREND                                                           
         MVC   COSTNAME,RECNAME                                                 
VVALCOSX B     XIT                                                              
         EJECT                                                                  
***********************************************************                     
*              VALIDATE MEDIA                             *                     
***********************************************************                     
         SPACE 3                                                                
VVALMED  DS    0H                                                               
         GOTO1 ANY                 SHOULD ALWAYS BE INPUT                       
         MVC   KEY,SPACES                                                       
         MVC   MEDNAME,SPACES                                                   
         CLI   5(R2),2             IF 2 BYTE MEDIA CODE FROM SI                 
         BE    VVALMED2                                                         
         MVI   ERROR,NOTFOUND      ERROR MESSAGE                                
         CLI   5(R2),5             5 BYTES 'MI=XX'                              
         BNE   ERREND                                                           
         CLC   WORK(3),=C'MI='     ELSE IT MUST BE MI RECORD                    
         BNE   ERREND                                                           
         MVC   MEDIA,WORK+3        ANY FILLED IN WORK WITH INPUT                
         MVI   KEY,X'08'           TRY AND READ THE MEDIA RECORD                
         MVC   KEY+1(1),COMPANY                                                 
         MVC   KEY+2(L'MEDIA),MEDIA                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVI   ERROR,NOTFOUND      ERROR MESSAGE                                
         CLC   KEY(5),KEYSAVE                                                   
         BNE   ERREND                                                           
         SPACE 1                                                                
         MVI   ELCODE,ACMIELQ                                                   
         BAS   RE,GETELIO                   GET MEDIA ELEMENT                   
         BE    *+12                                                             
         MVI   ERROR,NOTFOUND               ERROR MESSAGE                       
         B     ERREND                                                           
         USING ACMID,R6                                                         
         MVC   A12CODE,SPACES                                                   
         MVC   A11CODE,SPACES                                                   
         MVC   A12CODE(L'COMPANY),COMPANY                                       
         MVC   A12CODE+L'COMPANY(2),=C'12'                                      
         MVC   A12CODE+L'COMPANY+2(L'ACMICOST),ACMICOST                         
         MVC   A11CODE,A12CODE                                                  
         MVC   A11CODE+L'COMPANY(2),=C'11'                                      
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'COMPANY),COMPANY             AND INCOME ACCOUNT            
         MVC   KEY+L'COMPANY(L'ACMICOMM),ACMICOMM                               
         MVC   MEDIACOD,KEY                       SAVE INCOME ACCOUNT           
         MVC   MEDNAME(L'ACMIDESC),ACMIDESC       SAVE THE DESCRIPTION          
         GOTO1 GETLOW                                                           
         BNE   ERREND                                                           
*        MVC   MEDNAME,RECNAME                                                  
         TM    COMPSTA1,X'10'               ON COST ACCOUNTING?                 
         BZ    XIT                          NO - GET OUT                        
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'A12CODE),A12CODE                                           
         GOTO1 GETLOW                                                           
         BNE   VVALMED1                                                         
         MVC   A12NAME,RECNAME                                                  
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'A11CODE),A11CODE                                           
         GOTO1 GETLOW                                                           
         BNE   VVALMED1                                                         
         MVC   A11NAME,RECNAME                                                  
         B     XIT                                                              
VVALMED1 MVC   CONHEAD(L'BADANAL),BADANAL   BAD ANALYSIS CODE MSG               
         B     MYEND                                                            
         SPACE 2                                                                
VVALMED2 DS    0H                                                               
         MVC   MEDIA,WORK          ANY FILLED IN WORK WITH INPUT                
         MVI   ERROR,INVALID                                                    
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'SI'                                                  
         MVC   KEY+3(L'MEDIA),MEDIA      READ FOR THE SI ACCOUNT                
         MVC   MEDIACOD,KEY              SAVE INCOME ACCOUNT                    
         SPACE 1                                                                
VVALMED4 GOTO1 GETLOW                                                           
         BNE   ERREND                                                           
         MVC   MEDNAME,RECNAME                                                  
         SPACE 1                                                                
         TM    COMPSTA1,X'10'               ON COST ACCOUNTING?                 
         BZ    XIT                          NO - GET OUT                        
         GOTO1 VINCM                        GO GET THE 12 ACCOUNT               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************                     
*              VALIDATE ESTIMATE NUMBER                   *                     
***********************************************************                     
         SPACE 3                                                                
VVALEST  DS    0H                                                               
         GOTO1 ANY                 SHOULD ALWAYS BE INPUT                       
         MVC   ESTIMATE,WORK                                                    
         MVI   ERROR,NOTNUM                                                     
         SPACE 1                                                                
*        ZIC   R1,5(R2)            CHECK FOR NUMERIC                            
*        BCTR  R1,0                                                             
*        MVC   WORK(6),=6X'F0'                                                  
*        EX    R1,*+8                                                           
*        B     *+10                                                             
*        MVZ   WORK(0),8(R2)                                                    
*        CLC   WORK(6),=6X'F0'                                                  
*        BNE   ERREND                                                           
         SPACE 1                                                                
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),3             MUST BE AT LEAST 3 BYTES                     
         BL    ERREND                                                           
         OI    6(R2),X'80'         TRANSMIT                                     
         B     XIT                                                              
         EJECT                                                                  
**************************************************************                  
*              VALIDATE ESTIMATE PERIOD INPUT                *                  
**************************************************************                  
         SPACE 3                                                                
VVALPERI DS    0H                                                               
         GOTO1 ANY                 INPUT REQUIRED                               
         MVI   ERROR,INVDATE                                                    
         GOTO1 DATVAL,DMCB,(2,8(R2)),WORK                                       
         ICM   R3,15,0(R1)         R3=L'FIRST DATE                              
         BZ    ERREND                                                           
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(0,WORK),(1,PERIODS)  START OF PERIOD PACKED         
         LA    R1,8(R2)                                                         
         LA    R3,0(R3,R1)                                                      
         CLI   0(R3),C'-'          NEXT CHAR. MUST BE HYPHEN                    
         BNE   ERREND                                                           
         SPACE 1                                                                
         GOTO1 DATVAL,DMCB,(2,1(R3)),WORK+6                                     
         OC    DMCB,DMCB                                                        
         BZ    ERREND                                                           
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,PERIODE)  END OF PERIOD PACKED         
         OI    6(R2),X'80'         TRANSMIT                                     
         CLC   PERIODE,PERIODS                                                  
         BNL   *+12                                                             
         MVI   ERROR,BADDATES      END CANNOT BE LOWER THAN START               
         B     ERREND                                                           
         CLC   PERIODE(2),NEXTP    OR HIGHER THAN A YEAR FROM TODAY             
         BH    ERREND                                                           
         SPACE 1                                                                
         GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         ZIC   R3,15(R1)                 NUMBER OF MONTHS IN THE PERIOD         
         STH   R3,PERINUM                                                       
*                                                                               
         CLI   OVERLAY,X'17'       COMING FROM ESTIMATE REPORT?                 
         BE    XIT                 YES, DON'T CARE ABOUT LIMIT                  
         CH    R3,=H'12'                                                        
         BNH   XIT                                                              
         MVI   ERROR,ONEYEAR       PERIOD NOT LONGER THAN 12 MONTHS             
         B     ERREND                                                           
         EJECT                                                                  
**************************************************************                  
*              CHECK IF OK TO DELETE RECORD                  *                  
**************************************************************                  
         SPACE 3                                                                
VCANWEDL DS    0H                                                               
         CLI   MODE,RECDEL             CHECK JUST BEFORE THE DELETE             
         BNE   XIT                                                              
         CLC   TWAALIAS,=C'DDSECRET'   LOOK FOR SPECIAL ALIAS                   
         BE    XIT                     WHEN DELETE IS ALLOWED                   
         LA    R2,CONACTH                                                       
         MVI   ERROR,RECNTDEL                                                   
         B     ERREND                  OTHERWISE WE'RE OUT OF LUCK              
         EJECT                                                                  
**************************************************************                  
*              ASSUMES THE NEXT FIELD IS THE NAME            *                  
**************************************************************                  
         SPACE 3                                                                
VANYNAME DS    0H                                                               
         CLI   OPTION,C'Y'         IF OPTION IS SET TO C'Y'                     
         BNE   XIT                                                              
         BAS   RE,BUMP             POSITION R2 TO NEXT FIELD HEADER             
         GOTO1 NAMEOUT,DMCB,AIO,WORK                                            
         B     XIT                                                              
         EJECT                                                                  
***************************************************************                 
*              ADD A NAME ELEMENT                             *                 
***************************************************************                 
         SPACE 3                                                                
VNAMEIN  MVI   ELCODE,ACNMELQ      X'20'                                        
         GOTO1 REMELEM             REMOVE THE CURRENT NAME ELEMENT              
         LA    R6,ELEMENT                                                       
         USING ACNAMED,R6                                                       
         MVI   ACNMEL,ACNMELQ                                                   
         ZIC   R1,5(R2)            (INPUT NAME LENGTH)                          
         LA    R1,2(R1)                                                         
         STC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACNMNAME(0),8(R2)                                                
         DROP  R6                                                               
         GOTO1 ADDELEM             ADD A NEW NAME ELEMENT                       
         B     XIT                                                              
         SPACE 3                                                                
**********************************************************                      
*              READ NAME AND MOVE IT TO SCREEN           *                      
**********************************************************                      
         SPACE 3                                                                
VNAMEOUT DS    0H                                                               
         MVI   ELCODE,ACNMELQ      NAME TO SCREEN HEADER                        
         BAS   RE,GETELIO                                                       
         BNE   XIT                                                              
         USING ACNAMED,R6                                                       
         ZIC   R3,0(R2)            GET FIELD LENGTH                             
         SH    R3,=H'8'             MINUS HEADER                                
         TM    1(R2),X'02'          IF EXTENEDED HEADER, SUBTRACT IT            
         BNO   *+8                                                              
         SH    R3,=H'3'                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES                                                   
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         CR    R1,R3                                                            
         BNH   *+6                                                              
         LR    R1,R3                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),ACNMNAME                                                 
         OI    6(R2),X'80'                                                      
         DROP  R6                                                               
         B     XIT                                                              
         EJECT                                                                  
**********************************************************                      
*              READ NAME AND MOVE IT INTO ADDR IN R3     *                      
**********************************************************                      
         SPACE 3                                                                
VNAMEIT  DS    0H                                                               
         MVI   ELCODE,ACNMELQ                                                   
         BAS   RE,GETELIO                  GET NAME ELEMENT                     
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACNAMED,R6                                                       
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),ACNMNAME             SAVE NAME                           
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE FACILITATES THE ACTION OF SWITCHING, WITHIN A SINGLE   *         
* TRANSACTION, BETWEEN ONE OVERLAY AND ANOTHER BY CHANGING THE RECORD,*         
* ACTION, AND KEY FIELDS ON THE SCREEN AND CALLING GENCON AGAIN. FIRST*         
* IT SAVES THE CURRENT TWA IN TEMPSTR AND PUSHES THE CURRENT OVERLAY  *         
* NUMBER ONTO A STACK.  THEN IT CHANGES THE RECORD, ACTION AND KEY    *         
* FIELDS TO THE DATA SPECIFIED IN THE PARAMETER LIST. FINALLY, IT SETS*         
* THE FLAG 'GOAGAIN' TO 'YES' AND TAKES AN ERROR EXIT BACK TO GENCON  *         
* WHICH RETURNS BACK TO THE CONTROLLER. THE CONTROLLER THEN RECOGNIZES*         
* THE FLAG AND CALLS GENCON AGAIN. THE PARAMTER LIST LOOKS AS FOLLOWS:*         
*                                                                     *         
*       RECORD,ACTION,KEY1,KEY2,KEY3,KEY4,...,0                       *         
*                                                                     *         
***********************************************************************         
         SPACE 3                                                                
CALL0    DS    0H                                                               
         LR    R4,R1               ENTRY POINT FOR VTRANSF                      
         B     CALL6                                                            
*                                                                               
CALL     LR    R4,R1               SAVE POINTER TO PARMS                        
         ZIC   R3,CALLSP           GET STACK POINTER                            
         LR    R2,R3               R2=ORIGINAL STACK POINTER VALUE              
         LA    RF,CALLSTK(R3)      RF=A(NEXT POSITION)                          
         MVC   0(1,RF),OVERLAY     SLOT IN OVERLAY NUMBER                       
         LA    R3,1(R3)                                                         
         STC   R3,CALLSP                                                        
         CLI   CALLSP,L'CALLSTK    TEST MORE THAN ALLOWED NEST LEVELS           
         BNH   CALL2                                                            
         LA    R2,CONRECH          RETURN CURSOR TO RECORD FIELD                
         MVI   ERROR,BADNEST                                                    
         B     ERREND                                                           
*                                                                               
CALL2    SRL   R2,1                DIVIDE ORIGINAL LEVEL BY TWO                 
         LA    R2,3(R2)            ADD BACK THREE TWA PAGES                     
         SLL   R2,32-8             MOVE TO HIGH ORDER BYTE                      
         ICM   R2,3,TERM                                                        
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'TEMPSTR',(R2),ATIA,0              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CALL4    STC   R3,BYTE             SAVE STACK LEVEL                             
         L     RE,ATIA             RE=DESTINATION                               
         LA    RF,3072             MOVE/CLEAR HALF A TWA                        
         LA    R0,CONRECH          START AT RECORD HEADER                       
         LA    R1,SAVAREA-CONRECH  MOVE RECORD HEADER UNTIL TWA0 SAVE           
         TM    BYTE,X'01'          TEST FOR ODD NUMBER                          
         BO    *+6                 YES                                          
         AR    RE,RF               NO-MOVE TO SECOND HALF OF TWA                
         MVCL  RE,R0                                                            
         GOTO1 DATAMGR,DMCB,(0,=C'DMWRT'),=C'TEMPSTR',(R2),ATIA,0               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CALL6    XC    CONREC,CONREC       SET NEW RECORD TYPE                          
         OI    CONRECH+6,X'80'                                                  
         OI    CONRECH+4,X'80'     INPUT THIS TIME                              
         NI    CONRECH+4,X'DF'                                                  
         L     RF,0(R4)                                                         
         ZIC   RE,0(R4)                                                         
         STC   RE,CONRECH+5                                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CONREC(0),0(RF)                                                  
*                                                                               
CALL8    LA    R4,4(R4)            BUMP TO SECOND PARM                          
         XC    CONACT,CONACT       SET NEW ACTION TYPE                          
         OI    CONACTH+6,X'80'                                                  
         NI    CONACTH+4,X'DF'                                                  
         OI    CONACTH+4,X'80'                                                  
         L     RF,0(R4)                                                         
         ZIC   RE,0(R4)                                                         
         STC   RE,CONACTH+5                                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CONACT(0),0(RF)                                                  
*                                                                               
CALL10   LA    R4,4(R4)            BUMP TO THIRD PARM                           
         XC    CONKEY,CONKEY       SET NEW KEY FIELDS                           
         OI    CONKEYH+6,X'80'                                                  
         NI    CONKEYH+4,X'DF'                                                  
         OI    CONKEYH+4,X'80'                                                  
         LA    R2,BLOCK            BUILD KEY FIELD FROM THIRD PARM ON           
         LR    R3,R2                                                            
*                                                                               
CALL12   L     RF,0(R4)            ADD PARM TO KEY FIELD                        
         ZIC   RE,0(R4)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RF)       EXTRACT KEY FIELD                            
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         OC    0(0,R2),SPACES      SPACE PAD THE FIELD                          
*                                                                               
         LA    R2,0(RE,R2)         R2=A(LAST BYTE IN PARM)                      
         LA    RE,1(RE)            RESTORE PARM LENGTH                          
         CLI   0(R2),C' '          TEST FOR SIGNIFICANT CHARACTER               
         BH    CALL14              FOUND ONE                                    
         BCTR  R2,0                BACK UP TO PREVIOUS BYTE                     
         BCT   RE,*-10                                                          
*                                                                               
         LA    R2,1(R2)            ADVANCE TO FIRST POSITION                    
         MVI   0(R2),C','          INSERT POSITIONAL COMMA                      
*                                                                               
CALL14   LA    R4,4(R4)            BUMP TO NEXT PARM                            
         OC    0(4,R4),0(R4)       TEST FOR END OF PARM LIST                    
         BZ    CALL15              YES                                          
*                                                                               
         CLI   0(R2),C','          TEST LAST PARM ENDED WITH COMMA              
         BE    *+12                                                             
         LA    R2,1(R2)            NO-SO ADD ONE AFTER PARM                     
         MVI   0(R2),C','                                                       
*                                                                               
         LA    R2,1(R2)                                                         
         B     CALL12                                                           
*                                                                               
CALL15   LA    R2,1(R2)                                                         
         SR    R2,R3               COMPUTE LENGTH OF STRING                     
         LA    R0,L'CONKEY                                                      
         CR    R2,R0                                                            
         BL    *+6                                                              
         LR    R2,R0               DO NOT MOVE MORE THAN L'CONKEY               
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   CONKEY(0),BLOCK                                                  
         LA    R2,1(R2)            RESTORE LENGTH                               
         STC   R2,CONKEYH+5                                                     
*                                                                               
CALLX    MVI   GOAGAIN,C'Y'        SET FLAG TO CALL GENCON AGAIN                
         MVC   CALLER,OVERLAY      LET OVERLAY KNOW WHO CALLED IT               
         SR    R2,R2                                                            
         GOTO1 ERREX2                                                           
         EJECT                                                                  
**********************************************************************          
* THIS ROUTINE RESTORES THE TWA AND OVERLAY NUMBER/SCREEN TO         *          
* THE VALUE ON TOP OF THE OVERLAY STACK.  IT THEN SETS THE 'GOAGAIN' *          
* FLAG TO 'YES' AND TAKES AN ERROR EXIT BACK TO GENCON.  WHEN        *          
* THE CONTROLLER REGAINS CONTROL, IT CALLS GENCON AGAIN WITH THE     *          
* RESTORED SCREEN.                                                   *          
**********************************************************************          
         SPACE 3                                                                
RETURN   DS    0H                                                               
         ZIC   R3,CALLSP           GET STACK POINTER                            
         BCTR  R3,0                DECREMENT POINTER TO POP STACK               
         STC   R3,CALLSP                                                        
         LA    RE,CALLSTK(R3)                                                   
         MVC   RETURNED,LASTOV     NOTE OVERLAY RETURNING FROM                  
         MVC   OVERLAY,0(RE)       EXTRACT NEW OVERLAY NUMBER                   
*                                                                               
RETURN2  LR    R2,R3                                                            
         SRL   R2,1                DIVIDE LEVEL BY TWO                          
         LA    R2,3(R2)            START AT TWA PAGE 3                          
         SLL   R2,32-8             MOVE PAGE TO HOB                             
         ICM   R2,3,TERM                                                        
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'TEMPSTR',(R2),ATIA,0              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RETURN4  LA    RE,CONRECH                                                       
         LA    RF,SAVAREA-CONRECH  MOVE RECORD HEADER UP TO SAVE AREA           
         L     R0,ATIA                                                          
         LR    R1,RF                                                            
         LA    R3,1(R3)            RESTORE ORIGINAL LEVEL                       
         STC   R3,BYTE                                                          
         TM    BYTE,X'01'          TEST FOR ODD LEVEL                           
         BO    *+8                 YES                                          
         A     R0,=F'3072'         NO-MUST BE IN SECOND HALF OF PAGE            
         MVCL  RE,R0                                                            
*                                                                               
RETURN6  LA    R2,CONRECH                                                       
         BAS   RE,BUMP                                                          
         CLI   0(R2),0             TEST FOR EOS                                 
         BNE   *-8                                                              
         MVC   1(2,R2),=X'0101'    SET INDICATOR TO XMIT WHOLE SCREEN           
         LA    RE,OVSCRTAB         RE=A(OVERLAY/SCREEN TABLE)                   
*                                                                               
RETURN7  CLC   OVERLAY,0(RE)                                                    
         BE    RETURN8                                                          
         LA    RE,L'OVSCRTAB(RE)                                                
         CLI   0(RE),X'FF'         TEST FOR EOT                                 
         BNE   RETURN7                                                          
         DC    H'0'                                                             
*                                                                               
RETURN8  MVC   TWASCR,1(RE)        SET SCREEN NUMBER RESTORED                   
         MVI   GOAGAIN,C'Y'        SET TO CALL GENCON AGAIN                     
         SR    R2,R2                                                            
         GOTO1 ERREX2                                                           
         SPACE 2                                                                
OVSCRTAB DS    0CL2                X'EDIT OVERLAY'  X'SCREEN OVERLAY'           
         SPACE 1                                                                
         DC    X'10',X'F0'         PROFILE DISPLAY                              
         DC    X'11',X'F1'         ESTIMATE DISPLAY                             
         DC    X'12',X'F2'         CHECK HEADER                                 
         DC    X'13',X'F3'         CHECK ESTIMATE                               
         DC    X'15',X'F5'         CHECK LIST                                   
         DC    X'14',X'F4'         ESTIMATE ADJUSTMENT                          
         DC    X'30',X'E0'         PROFILE LIST                                 
         DC    X'FF'                                                            
         EJECT                                                                  
*************************************************************                   
*          CLEAR AND FOUT FIELDS                            *                   
*                                                           *                   
*        ON ENTRY                                           *                   
*        P1    BYTE 0    = 0 UNPROTECTED FIELDS             *                   
*                        = 1 PROTECTED FIELDS               *                   
*              BYTES 1-3 = A(START FIELD HEADER)            *                   
*        P2    BYTES 1-3 = A(END FIELD HEADER)              *                   
*************************************************************                   
         SPACE 3                                                                
CLEARF   DS    0H                                                               
         LM    R2,R3,0(R1)                                                      
         SR    RE,RE                                                            
         LA    R4,X'10'            BRANCH CONDITION                             
         LA    R5,MOVESPA          CLEAR FIELD INSTRUCTION                      
         CLI   0(R1),0             TEST FOR UNPROTECTED FIELDS                  
         BE    *+12                YES                                          
         LA    R4,X'80'            SET BRANCH CONDITION AND CLEAR               
         LA    R5,ZEROFLD          INSTRUCTION FOR PROTECTED FIELDS             
         SPACE 1                                                                
CLEARF2  IC    RE,0(R2)            LENGTH OF FIELD PLUS HEADER                  
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         EX    R4,TSTBRAN          BRANCH ACCORDINGLY                           
         LR    R1,RE                                                            
         SH    R1,=H'9'            SET EXECUTE LENGTH FOR FIELD DATA            
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    *+8                                                              
         SH    R1,=H'8'            LESS 8 MORE FOR EXTENDED FIELD               
         EX    R1,0(R5)            CLEAR FIELD                                  
         OI    6(R2),X'80'         SET TO TRANSMIT                              
         SPACE 1                                                                
CLEARF4  LA    R2,0(RE,R2)                                                      
         CR    R2,R3               TEST IF END FIELD REACHED                    
         BL    CLEARF2             NO-CONTINUE                                  
         B     XIT                 YES-ALL DONE                                 
         SPACE 2                                                                
MOVESPA  MVC   8(0,R2),SPACES                                                   
ZEROFLD  XC    8(0,R2),8(R2)                                                    
TSTBRAN  BC    0,CLEARF4                                                        
         EJECT                                                                  
*********************************************************                       
* ERRCUR - ERROR EXIT WITH CURSOR POSITIONING           *                       
*                                                       *                       
* AT ENTRY, R2=A(FIELD HEADER) AND ERRNDX=INDEX INTO    *                       
*           FIELD FOR CURSOR (OPTIONAL)                 *                       
*********************************************************                       
         SPACE 3                                                                
ERRCUR   DS    0H                                                               
         OI    6(R2),X'80'                                                      
         CLI   OFFLINE,C'Y'        TEST IF OFFLINE                              
         BE    VERRCUR2            YES-ALL DONE NOW                             
         L     R1,ATIOB                                                         
         USING TIOBD,R1                                                         
         LR    RF,R2                                                            
         S     RF,ATWA             RF=DISP. TO ERROR FIELD HEADER               
         STCM  RF,3,TIOBCURD                                                    
         MVC   TIOBCURI,ERRNDX                                                  
         OI    TIOBINDS,TIOBSETC                                                
         SPACE 1                                                                
VERRCUR2 OI    6(R2),X'40'                                                      
         OI    CONHEADH+6,X'80'    XMIT BACK MESSAGE                            
         CLI   ERROR,X'FE'                                                      
         BE    VERRCUR4                                                         
         GOTO1 ERREX                                                            
*                                                                               
VERRCUR4 GOTO1 ERREX2                                                           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************                     
* CHKACC - CHECK ACCESS TO RECORD/ACTION PAIRS            *                     
* AT ENTRY, DUB(2)=RECORD NUMBER/ACTION NUMBER            *                     
* ON EXIT,  CC=EQ FOR ACCESS, CC=NEQ FOR NO ACCESS        *                     
***********************************************************                     
         SPACE 3                                                                
CHKACC   DS    0H                                                               
         L     R4,ARECACT          R4=A(RECORD/ACTION TABLE)                    
         ZIC   R3,LRECACT          R3=L'RECORD/ACTION TABLE ENTRY               
*                                                                               
CHKACC2  CLI   0(R4),X'FF'         TEST FOR EOT                                 
         BNE   *+6                                                              
         DC    H'0'                DUMP IF NOT IN TABLE                         
         CLI   0(R4),X'03'         TEST FOR RECORD/ACTION ENTRY                 
         BNE   CHKACC3             NO                                           
         CLC   DUB(2),1(R4)        MATCH ON RECORD/ACTION                       
         BE    CHKACC4                                                          
*                                                                               
CHKACC3  LA    R4,0(R3,R4)         NEXT TABLE ENTRY                             
         B     CHKACC2                                                          
*                                                                               
CHKACC4  OC    SECMASKS,SECMASKS   TEST FOR ACCESS CONTROL                      
         BZ    CHKACCY             NO-RETURN 'YES'                              
         OC    12(4,R4),12(R4)     TEST ANY SECURITY FOR RECORD/ACTION          
         BZ    CHKACCY                                                          
         MVC   FULL,SECMASKS                                                    
         NC    FULL,12(R4)         TEST IF ANY BITS IN COMMON                   
         BZ    CHKACCN             NO-NO ACCESS                                 
*                                                                               
CHKACCY  CR    RB,RB               SET CC=EQ FOR 'YES'                          
         B     XIT                                                              
*                                                                               
CHKACCN  LTR   RB,RB               SET CC=NEQ FOR 'NO'                          
         B     XIT                                                              
         EJECT                                                                  
**************************************************************                  
*              VALIDATE ESTIMATE PERIOD INPUT                *                  
*              CAN BE START/END OR JUST -END                 *                  
**************************************************************                  
         SPACE 3                                                                
VVALPERE DS    0H                                                               
         GOTO1 ANY                 INPUT REQUIRED                               
         MVI   ERROR,INVDATE                                                    
         XC    PERIODS,PERIODS     CLEAR START IN CASE NONE ENTERED             
         GOTO1 DATVAL,DMCB,(2,8(R2)),WORK                                       
         ICM   R3,15,0(R1)         R3=L'FIRST DATE                              
         BZ    VALPER2                                                          
         GOTO1 DATCON,DMCB,(0,WORK),(1,PERIODS)  START OF PERIOD PACKED         
*                                                                               
VALPER2  LA    R1,8(R2)                                                         
         LA    R3,0(R3,R1)                                                      
         CLI   0(R3),C'-'          NEXT CHAR. MUST BE HYPHEN                    
         BNE   ERREND                                                           
*                                                                               
         GOTO1 DATVAL,DMCB,(2,1(R3)),WORK+6                                     
         OC    DMCB,DMCB                                                        
         BZ    ERREND                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,PERIODE)  END OF PERIOD PACKED         
         OI    6(R2),X'80'                                                      
         CLC   PERIODE,PERIODS                                                  
         BNL   *+12                                                             
         MVI   ERROR,BADDATES      END CANNOT BE LOWER THAN START BUT           
         B     ERREND                                                           
*                                                                               
         MVI   ERROR,TOOSOON                                                    
         CLC   PERIODE(2),LASTP    MUST BE 1 YEAR FROM TODAY,OR PRIOR           
         BH    ERREND                                                           
         B     XIT                                                              
         EJECT                                                                  
*************************************************************                   
*              SECURITY AND ACCESS CHECKING                 *                   
*************************************************************                   
         SPACE 3                                                                
SECCHECK NTR1                      CHECK SECURITY LEVEL AGAINST TWA             
         CLI   TWAAUTH+1,0                                                      
         BE    XIT                                                              
         CLI   OFFLINE,C'Y'                                                     
         BE    XIT                                                              
         MVI   ELCODE,ACSTELQ      STATUS ELEMENT                               
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACSTATD,R6                                                       
         CLC   ACSTSECY+1(1),TWAAUTH+1                                          
         BNH   XIT                                                              
         MVI   ERROR,SECLOCK       RECORD HAS HIGHER SECURITY THAN TWA          
         B     ERREND                                                           
         SPACE 3                                                                
ACCCHECK NTR1  ,                                                                
         B     XIT                 **CHECK IS NO-OPED FOR NOW ***               
         CLC   TWAACCS,SPACES      TEST FOR ANY LIMIT ACCESS                    
         BNH   XIT                 NO                                           
         CLC   TWAACCS(2),SPACES   TEST FOR ANY OLD ACCESS                      
         BNH   OFFCHECK                                                         
         CLI   TWAACCS,C'*'        TEST FOR SINGLE OFFICE CONTROL               
         BE    OFFCHECK                                                         
         CLI   TWAACCS,C'$'        TEST FOR LIST CONTROL                        
         BE    OFFCHECK                                                         
         CLC   TWAACCS(2),CLICODE  APPLY CLIENT CODE SECURITY                   
         BE    XIT                                                              
         B     NOACCEX                                                          
         SPACE 1                                                                
OFFCHECK L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         MVC   OFFAOFFC,EFFOFFC                                                 
         MVC   OFFAOPOS,LEDGTOFF   LEDGER OFFICE POSITION                       
         MVC   OFFAREC,AIO                                                      
         MVI   OFFAACT,OFFATST                                                  
         GOTO1 OFFAL                                                            
         BE    XIT                 SECURITY IS OK                               
         SPACE 1                                                                
NOACCEX  MVI   ERROR,SECLOCK                                                    
         B     ERREND                                                           
         DROP  R1                                                               
         EJECT                                                                  
*              COMMON EXIT ROUTINES                                             
         SPACE 3                                                                
BUMP     ZIC   RF,0(R2)            GET TO NEXT SCREEN FIELD                     
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
BUMPTOUN ZIC   RF,0(R2)            GET TO NEXT UNPROTECTED FIELD                
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
         SPACE 1                                                                
MYEND    MVI   ERROR,X'FE'                                                      
         B     ERREND                                                           
         SPACE 1                                                                
ERREND   GOTO1 VERRCUR                                                          
         B     XIT                                                              
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
* LIST OF SCREENS FROM WHICH RETURNS ARE ALLOWED                                
*                                                                               
RETTAB   DS    0XL1                                                             
         DC    X'F0'               PROFILE                                      
         DC    X'F1'               ESTIMATE                                     
         DC    X'F2'               CHECK HEADER (REVIEW,QUIT,UPDATE)            
         DC    X'F3'               CHECK ESTIMATE                               
         DC    X'F4'               ESTIMATE ADJUSTMENTS                         
RETURNS  EQU   (*-RETTAB)/L'RETTAB                                              
*                                                                               
BADANAL  DC    CL42'*ERROR* - INVALID COSTING ANALYSIS POINTER'                 
         EJECT                                                                  
*              CONSTANTS TABLES ETC                                             
         SPACE 3                                                                
RELO     DS    A                                                                
*YSVCON  DS    0V                                                               
*        DC    AL4(0)                                                           
*VTYPES  EQU   (*-SYSVCON)/4                                                    
*                                  TABLE OF CORE-RESIDENT MODULES               
CORETAB  DS    0X                           (DDCOREQUS)                         
         DC    AL1(QGENCON)                                                     
         DC    AL1(QADDTRN)                                                     
         DC    AL1(QOFFAL)                                                      
         DC    AL1(QTSAR)                                                       
CORES    EQU   (*-CORETAB)                                                      
*                                                                               
* TABLE OF DISPLACEMENT OF FILTER FIELDS INTO STATUS ELEMENT                    
*                                                                               
FILDISP  DC    AL1(ACSTFILT-ACSTATD)                                            
         DC    AL1(ACSTFILT+1-ACSTATD)                                          
         DC    AL1(ACSTANAL-ACSTATD)                                            
         DC    AL1(ACSTSUB-ACSTATD)                                             
         DC    AL1(ACSTFLT5-ACSTATD)                                            
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              TABLES OF RECORDS ACTIONS AND COMBINATIONS                       
         SPACE 3                                                                
RECACTS  DS    0CL16                                                            
         SPACE 1                                                                
* X'01'  ENTRIES ARE AVAILABLE RECORDS                                          
*                                                                               
* CL8 EXPANDED RECORD NAME                                                      
* CL1 RECORD NUMBER (01 IS RESERVED)                                            
* CL1 PHASE NUMBER FOR DATA DICTIONARY - LEAVE AS ZERO FOR NOW                  
* CL1 PHASE NUMBER FOR HELP SCREEN - LEAVE AS ZERO FOR NOW                      
* XL4 SECURITY MASK BITS (******* LEAVE AS ZERO FOR NOW *****)                  
         SPACE 1                                                                
         DC    X'01',C'HELP    ',AL1(00),X'0000',AL1(0,0,0,0)                   
         DC    X'01',C'PROFILE ',AL1(PR),X'0000',AL1(0,0,0,0)                   
         DC    X'01',C'ESTIMATE',AL1(ES),X'0000',AL1(0,0,0,0)                   
         DC    X'01',C'CHECK   ',AL1(CK),X'0000',AL1(0,0,0,0)                   
         SPACE 3                                                                
* X'02' ENTRIES ARE AVAILABLE ACTIONS                                           
*                                                                               
* CL8 EXPANDED ACTION NAME                                                      
* CL1 ACTION NUMBER                                                             
* CL1 ACTION EQUATE                                                             
* CL1 SPARE                                                                     
* XL4 SECURITY MASK BITS                                                        
         SPACE 1                                                                
         DC    X'02',C'HELP    ',AL1(00,00,00)                                  
         DC    AL1(0,0,0,0)                                                     
         DC    X'02',C'ADD     ',AL1(ACTNADD,01,00)                             
         DC    AL1(0,0,0,0)                                                     
         DC    X'02',C'CHANGE  ',AL1(ACTNCHA,02,00)                             
         DC    AL1(0,0,0,0)                                                     
         DC    X'02',C'ADJUST  ',AL1(ACTNCHA,11,00)                             
         DC    AL1(0,0,0,0)                                                     
         DC    X'02',C'DISPLAY ',AL1(ACTNDIS,03,00)                             
         DC    AL1(0,0,0,0)                                                     
         DC    X'02',C'DELETE  ',AL1(ACTNDEL,04,00)                             
         DC    AL1(0,0,0,0)                                                     
         DC    X'02',C'SELECT  ',AL1(ACTNSEL,03,00)                             
         DC    AL1(0,0,0,0)                                                     
         DC    X'02',C'RESTORE ',AL1(ACTNRES,04,00)                             
         DC    AL1(0,0,0,0)                                                     
         DC    X'02',C'HEADER  ',AL1(ACTNHED,ACTNHED,00)                        
         DC    AL1(0,0,0,0)                                                     
         DC    X'02',C'ESTIMATE',AL1(ACTNEST,ACTNEST,00)                        
         DC    AL1(0,0,0,0)                                                     
         DC    X'02',C'REPORT  ',AL1(ACTNRPT,ACTNRPT,00)                        
         DC    AL1(0,0,0,0)                                                     
         DC    X'02',C'LIST    ',AL1(ACTNLIST,ACTNLIST,00)                      
         DC    AL1(0,0,0,0)                                                     
         DC    X'02',C'REVIEW  ',AL1(ACTNREV,ACTNREV,00)                        
         DC    AL1(0,0,0,0)                                                     
         DC    X'02',C'QUIT    ',AL1(ACTNQUIT,ACTNQUIT,00)                      
         DC    AL1(0,0,0,0)                                                     
         DC    X'02',C'UPDATE  ',AL1(ACTNUPD,ACTNUPD,00)                        
         DC    AL1(0,0,0,0)                                                     
         DC    X'02',C'DRAFT   ',AL1(ACTNDFT,ACTNDFT,00)                        
         DC    AL1(0,0,0,0)                                                     
         DC    X'02',C'FILTER  ',AL1(ACTNFILT,ACTNFILT,00)                      
         DC    AL1(0,0,0,0)                                                     
         DC    X'02',C'PEEL    ',AL1(ACTNPEEL,ACTNPEEL,00)                      
         DC    AL1(0,0,0,0)                                                     
         EJECT                                                                  
* DIRECTORY OF PHASES FOR SELECTED RECORD/ACTION                                
         SPACE 3                                                                
* X'03' ENTRIES ARE OK REC/ACT COMBOS                                           
* CL1 RECORD NUMBER                                                             
* CL1 ACTION EQUATE                                                             
* CL1 PHASE NUMBER FOR SCREEN      (SC)                                         
* CL1 PHASE NUMBER FOR EDIT        (ED)                                         
* CL1 PHASE NUMBER FOR SPECS       (SP)                                         
* CL1 PHASE NUMBER FOR REPORT      (RP)                                         
* CL1 WHEN OK BITS 80=SCREEN 40=NOW 20=SOON 10=OV 08=DDS 01=I MAINT             
* CL2 CODE FOR REPORTS                                                          
* CL2 CODE FOR EOD HANDLING                                                     
* XL4 SECURITY MASK BITS                                                        
         SPACE 1                                                                
*                                 SC  SP  OK                                    
*                                   ED  RP                                      
         DC    X'03',AL1(PR,01),X'F010000080',C'    '  PROFILE  ADD             
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(PR,02),X'F010000080',C'    '  PROFILE  CHANGE          
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(PR,03),X'F010000080',C'    '  PROFILE  DIS/SEL         
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(PR,04),X'F010000080',C'    '  PROFILE  DEL/RES         
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(PR,10),X'E030000081',C'    '  PROFILE  LIST            
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(PR,22),X'F616001638',C'E6E6'  PROFILE PEEL             
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(ES,02),X'F111000080',C'    '  ESTIMATE CHANGE          
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(ES,03),X'F111000080',C'    '  ESTIMATE DIS/SEL         
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(ES,12),X'F717001738',C'E2E2'  ESTIMATE REPORT          
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(ES,11),X'F414000080',C'    '  ESTIMATE ADJUST          
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(CK,07),X'F212000081',C'    '  CHECK    HEADER          
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(CK,08),X'F312000081',C'    '  CHECK   ESTIMATE         
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(CK,10),X'F512000081',C'    '  CHECK    LIST            
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(CK,12),X'F212000081',C'    '  CHECK    REVIEW          
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(CK,14),X'F212000081',C'    '  CHECK    QUIT            
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(CK,16),X'F212000081',C'CJCJ'  CHECK    UPDATE          
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(CK,18),X'F212000081',C'CJCJ'  CHECK    DRAFT           
         DC    AL1(0,0,0,0)                                                     
         DC    X'03',AL1(CK,20),X'F212000081',C'CJCJ'  CHECK    FILTER          
         DC    AL1(0,0,0,0)                                                     
         DC    X'FF'                                                            
         EJECT                                                                  
         SPACE 2                                                                
       ++INCLUDE ACINTWORKD                                                     
         EJECT                                                                  
*              OTHER DSECTS ARE HIDDEN IN HERE                                  
         SPACE 3                                                                
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*ACGENBOTH                                                                      
*ACGENFILE                                                                      
*CTGENFILE                                                                      
*FATIOB                                                                         
*DDACCFACS                                                                      
*DDCOMFACS                                                                      
*DDCOREQUS                                                                      
*DMDTFIS                                                                        
         PRINT ON                                                               
         ORG     CONTAGH                                                        
       ++INCLUDE ACINTF0D                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDACCFACS                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDTWADCOND                                                     
*********INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'065ACINT00   05/09/08'                                      
         END                                                                    
