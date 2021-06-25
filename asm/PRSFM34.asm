*          DATA SET PRSFM34    AT LEVEL 033 AS OF 07/23/18                      
*PHASE T41C34A                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*               T41C34 - SUBMEDIA MAINT/LIST                                    
*                                                                               
* SMUR 04/18  SPEC-17729 NEW MEDIA (D)IGITAL AUDIO                              
*                                                                               
* BPLA 06/15    SUPPORT FOR MEDIA B, V, W                                       
*                                                                               
* KWAN 03/14/14 Supporting new media L (Social)                                 
*                                                                               
* KWAN 11/12/04 CREATE NEW SUBMEDIA SFM RECORD                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T41C00 (SFM CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, CHG, DISP, SEL, LIST, REP               *         
*                                                                     *         
*  INPUTS       SCREEN T41CB0 (MAINTENANCE)                           *         
*               SCREEN T41CC4 (LIST)                                  *         
*                                                                     *         
*  OUTPUTS                                                            *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- WORK                                            *         
*               R4 -- WORK                                            *         
*               R5 -- WORK                                            *         
*               R6 -- WORK                                            *         
*               R7 -- WORK                                            *         
*               R8 -- SPOOLD                                          *         
*               R9 -- SYSD                                            *         
*               RA -- TWA                                             *         
*               RB -- FIRST BASE                                      *         
*               RC -- GEND                                            *         
*               RD -- SYSTEM                                          *         
*               RE -- SYSTEM                                          *         
*               RF -- SYSTEM                                          *         
*                                                                     *         
*  I/O AREAS                                                          *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T41C34 - SUBMEDIA MAINT/LIST'                                   
*                                                                               
T41C34   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C34                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         BRAS  RE,INITIALZ                                                      
*                                                                               
* IF FOLLOWING COMPARE IS NOT MADE, PF12 WILL NOT DISPLAY FIRST                 
* SELECTED RECORD (I.E. PF12 IS USED INSTEAD OF NORMAL ENTER)                   
*                                                                               
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   *+8                                                              
         MVI   PFAID,0             SET PFKEY SAME AS ENTER                      
*                                                                               
         CLI   ACTNUM,ACTSEL       SELECT? (CKING FOR PF KEYS)                  
         BNE   CKMODE                                                           
         TM    GENSTAT2,NEXTSEL                                                 
         JO    EXIT                                                             
         CLI   PFAID,12            PF 12 OR 24 FOR RETURN?                      
         BE    RTN                                                              
         CLI   PFAID,24                                                         
         BNE   STY                                                              
RTN      OI    GENSTAT2,NEXTSEL+RETEQSEL                                        
         MVI   PFAID,0                                                          
         J     EXIT                                                             
*                                                                               
STY      OI    GENSTAT2,RETEQSEL   SET TO RETURN THIS SELECTION                 
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKMODE   CLI   ACTNUM,ACTDEL       ACTION DELETE?                               
         JE    RECACERR                                                         
         CLI   ACTNUM,ACTREST      ACTION RESTORE?                              
         JE    RECACERR                                                         
*                                                                               
         CLI   PFAID,0             PFKEY IS PRESSED?                            
         BE    *+12                NO                                           
         BRAS  RE,CKPFKEYS                                                      
         JNE   PFKEYERR            INVALID PFKEY IS PRESSED                     
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
*                                                                               
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL                                    
*                                                                               
EXIT XIT1                                                                       
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VK       DS    0H                  VALIDATE KEY ROUTINE                         
         LA    R2,SMDMEDH          POINT TO MEDIA FLD                           
         CLI   LSTREPSW,C'Y'       LIST OR REPORT?                              
         BNE   *+8                                                              
         LA    R2,LSTMEDH                                                       
         CLI   5(R2),0             ANY INPUTS?                                  
         BNE   VK10                                                             
         CLI   ACTNUM,ACTADD       ADD?                                         
         BE    MSSNGERR                                                         
         MVI   QMED,C'B'           SET TO FIRST MEDIA (B,D,I,M,N,S...)          
         XC    LSTMEDN,LSTMEDN                                                  
         OI    LSTMEDNH+6,X'80'                                                 
         B     VK30                                                             
*                                                                               
VK10     GOTO1 VALIMED             MEDIA IS REQUIRED EVEN FOR LIST              
*                                                                               
         CLI   LSTREPSW,C'Y'       LIST OR REPORT?                              
         BNE   VK20                                                             
         MVC   LSTMEDN,MEDNM                                                    
         OI    LSTMEDNH+6,X'80'                                                 
         B     VK30                                                             
*                                                                               
VK20     MVC   SMDMEDN,MEDNM                                                    
         OI    SMDMEDNH+6,X'80'                                                 
*                                                                               
VK30     LA    R2,SMDSMDH          POINT TO SUBMEDIA FLD                        
         CLI   LSTREPSW,C'Y'       LIST OR REPORT?                              
         BNE   *+8                                                              
         LA    R2,LSTSMDH                                                       
         CLI   5(R2),0             ANY INPUTS?                                  
         BNE   VK40                                                             
         CLI   ACTNUM,ACTADD       ADD?                                         
         BE    MSSNGERR                                                         
         LA    RE,SMDCODTB         POINT TO SUBMEDIA CODE TABLE                 
         MVC   SVSUBMCD,0(RE)      SET TO FIRST SUBMEDIA CODE                   
         B     VK80                                                             
*                                                                               
VK40     CLI   5(R2),1                                                          
         BH    TOOLGERR                                                         
         LA    RE,SMDCODTB                                                      
VK42     CLI   0(RE),X'FF'                                                      
         BE    INVFDERR                                                         
         CLC   0(1,RE),8(R2)       INPUT MATCH THAT OF TABLE ENTRY?             
         BE    *+12                                                             
         LA    RE,1(RE)            POINT TO NEXT ENTRY IN TABLE                 
         B     VK42                                                             
         MVC   SVSUBMCD,8(R2)                                                   
*                                                                               
VK80     XC    SVKEY,SVKEY                                                      
         LA    R6,SVKEY                                                         
         USING PSMDKEY,R6                                                       
*                                                                               
         MVC   PSMDKAGY,AGENCY                                                  
         MVC   PSMDKMED,QMED                                                    
         MVI   PSMDKRCD,PSMDKR1Q   RECORD CODE - 1ST IDENTIFIER                 
         MVI   PSMDKRC2,PSMDKR2Q   RECORD CODE - 2ND IDENTIFIER                 
         MVC   PSMDKSMD,SVSUBMCD                                                
*                                                                               
         MVC   LS_MEDCD,QMED                                                    
         MVC   LS_SMDCD,SVSUBMCD                                                
*                                                                               
VKX      XC    KEY,KEY                                                          
         MVC   KEY(25),SVKEY                                                    
         MVC   WKKEY,SVKEY                                                      
         MVC   AIO,AIO1            RECORD WILL BE READ INTO AIO1                
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VR       DS    0H                  VALIDATE DATA FOR PROCTER $ GAMBLE           
         L     R6,AIO                                                           
         MVI   ELCODE,PSMDELCQ                                                  
         GOTOR REMELEM                                                          
*                                                                               
VR10     XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING PSMDELEM,R6                                                      
         MVI   PSMDELCD,PSMDELCQ   MAIN ELEM CODE                               
         MVI   PSMDELEN,PSMDELNQ   ELEM LENGTH                                  
*                                                                               
         LA    R2,SMDDESCH         SUBMEDIA DESCRIPTION                         
         CLI   5(R2),0                                                          
         BE    MSSNGERR                                                         
         SR    RE,RE                                                            
         IC    RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PSMDDESC(0),8(R2)                                                
         OC    PSMDDESC,SPACES     SPACE PADDED                                 
*                                                                               
         GOTOR ADDELEM             ADD X'02' ELEM TO CLT RECORD                 
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,PSMDELCQ                                                  
         GOTOR GETEL                                                            
         BE    *+6                                                              
         DC    H'0'                MAKE SURE 1ST ELEM IS ADDED                  
*                                                                               
VRX      DS    0H                                                               
         B     DR                  RECORD VALIDATED, REDISPLAY IT               
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DR       DS    0H                  DISPLAY RECORD                               
         L     R6,AIO                                                           
         CLC   3(2,R6),SMEDRCOD    SUBMEDIA RECORD CODE?                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PSMDELEM,R6                                                      
         MVI   ELCODE,PSMDELCQ     1ST SUBMEDIA ELEM CODE                       
         GOTOR GETEL                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    SMDDESC,SMDDESC                                                  
         MVC   SMDDESC,PSMDDESC                                                 
         OI    SMDDESCH+6,X'80'                                                 
*                                                                               
DRX      DS    0H                                                               
         LA    R2,CONACTH          NEED TO RESET FLD POINTER                    
         CLI   ACTNUM,ACTSEL       ACTION SEL?                                  
         JNE   EXIT                                                             
         XC    CONHEAD,CONHEAD                                                  
         CLI   THISLSEL,C'C'       SEL CODE IS CHANGE ON LIST?                  
         JE    EXIT                                                             
         CLI   MODE,VALREC         MODE IS VALREC?                              
         JE    EXIT                                                             
         CLI   ACTNUM,ACTCHA       ACTION IS CHANGE?                            
         JE    EXIT                                                             
         MVC   CONHEAD(L'SELMSG01),SELMSG01                                     
         OI    CONHEADH+6,X'80'                                                 
         J     TRAPERR2                                                         
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DK       DS    0H                  DISPLAY KEY                                  
         L     R6,AIO                                                           
         CLC   3(2,R6),SMEDRCOD    SUBMEDIA RECORD CODE?                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PSMDKEY,R6                                                       
*                                                                               
         XC    SMDMED,SMDMED                                                    
         MVC   SMDMED(L'PSMDKMED),PSMDKMED                                      
         MVI   SMDMEDH+5,1         INPUT LENGTH                                 
         OI    SMDMEDH+6,X'80'     TRANSMIT MEDIA CODE                          
         LA    R2,SMDMEDH                                                       
         MVI   USEIONUM,2          USE AIO2 FOR VALIDATE MEDIA                  
         GOTO1 VALIMED                                                          
         XC    SMDMEDN,SMDMEDN                                                  
         MVC   SMDMEDN,MEDNM                                                    
         OI    SMDMEDNH+6,X'80'    TRANSMIT MEDIA NAME                          
*                                                                               
         XC    SMDSMD,SMDSMD                                                    
         MVC   SMDSMD(L'PSMDKSMD),PSMDKSMD                                      
         MVI   SMDSMDH+5,1         INPUT LENGTH                                 
         OI    SMDSMDH+6,X'80'     TRANSMIT MEDIA CODE                          
*                                                                               
DKX      DS    0H                                                               
         MVC   AIO,AIO1            AIO1 HAS REC TO BE DISPLAYED                 
         MVI   USEIONUM,1          RESET TO AIO1                                
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
LR       DS    0H                  LIST RECORDS                                 
         OC    KEY,KEY                                                          
         BNZ   LR20_HI             CONTINUE LISTING                             
         MVC   KEY(25),WKKEY                                                    
*                                                                               
LR20_HI  GOTO1 HIGH                FIRST RECORD                                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     LR30                                                             
*                                                                               
LR20_SEQ GOTO1 SEQ                 FIRST RECORD                                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LR30     CLC   KEY+3(2),SMEDRCOD   SUBMEDIA RECORD CODE?                        
         BE    LR60                                                             
         MVC   KEY,WKKEY                                                        
*                                                                               
         LA    RE,MEDCODTB                                                      
LR42     CLI   0(RE),X'FF'         END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                TABLE IS NO GOOD                             
         CLC   0(1,RE),KEY+2                                                    
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     LR42                                                             
         CLI   1(RE),X'FF'         LAST MEDIA CODE IN TABLE?                    
         BE    LRX                                                              
         MVC   KEY+2(1),1(RE)                                                   
         MVC   WKKEY+2(1),1(RE)    UPDATE WORKING KEY                           
         B     LR20_HI                                                          
*                                                                               
LR60     XC    LISTAR,LISTAR       FILL IN LIST LINE                            
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         CLI   33(R6),PSMDELCQ     SUBMEDIA ELEM PRESENT?                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,KEY                                                           
         USING PSMDKEY,R6                                                       
         MVC   LSMEDCOD,PSMDKMED                                                
         MVC   LSSMDCOD,PSMDKSMD                                                
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,PSMDELCQ                                                  
         GOTOR GETEL                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PSMDELEM,R6                                                      
         MVC   LSSMDDES,PSMDDESC                                                
         GOTOR NEXTEL                                                           
         BNE   *+6                                                              
         DC    H'0'                SHOULD NOT FIND ANOTHER ONE                  
*                                                                               
         GOTO1 LISTMON             DISPLAY LINE                                 
*                                                                               
         CLI   MODE,PRINTREP       REPORTING USING LIST ACTION?                 
         BNE   LR20_SEQ                                                         
*                                                                               
         BRAS  RE,GETREPLN         GET REPORT LINE                              
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT LINE                                   
         B     LR20_SEQ                                                         
*                                                                               
LRX      DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PR       DS    0H                  PRINT RECORDS                                
         OC    KEY,KEY                                                          
         BNZ   PR20_HI             CONTINUE LISTING                             
         MVC   KEY(25),WKKEY                                                    
*                                                                               
PR20_HI  GOTO1 HIGH                FIRST RECORD                                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY+3(2),SMEDRCOD   SUBMEDIA RECORD CODE?                        
         BE    PR60                                                             
         MVC   KEY,WKKEY                                                        
*                                                                               
PR40     LA    RE,MEDCODTB                                                      
PR42     CLI   0(RE),X'FF'         END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                TABLE IS NO GOOD                             
         CLC   0(1,RE),KEY+2                                                    
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     PR42                                                             
         CLI   1(RE),X'FF'         LAST MEDIA CODE IN TABLE?                    
         BE    PRX                                                              
         MVC   KEY+2(1),1(RE)                                                   
         MVC   WKKEY+2(1),1(RE)    UPDATE WORKING KEY                           
         B     PR20_HI                                                          
*                                                                               
PR60     MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         CLI   33(R6),PSMDELCQ     SUBMEDIA ELEM PRESENT?                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BRAS  RE,GETREPLN         GET REPORT LINE                              
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT LINE                                   
         B     PR40                                                             
*                                                                               
PRX      DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SMDCODTB DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'                          
         DC    X'FF'                                                            
*                                                                               
MEDCODTB DC    C'BDILMNOSTVW'         MEDIA CODE TABLE                          
         DC    X'FF'                                                            
*                                                                               
SMEDRCOD DC    AL1(PSMDKR1Q,PSMDKR2Q)                                           
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
TRAPERR2 GOTO1 ERREX2                                                           
*                                                                               
MSSNGERR MVI   ERROR,001                                                        
         J     TRAPERR                                                          
*                                                                               
INVFDERR MVI   ERROR,002                                                        
         J     TRAPERR                                                          
*                                                                               
RECACERR MVI   ERROR,012           INVALID RECORD ACTION ERROR                  
         LA    R2,CONACTH          POINT TO ACTION FLD                          
         J     TRAPERR                                                          
*                                                                               
TOOLGERR MVI   ERROR,071           INPUT TOO LONG ERROR                         
         J     TRAPERR                                                          
*                                                                               
PFKEYERR MVI   ERROR,088           INVALID PFKEY                                
         LA    R2,CONACTH          POINT TO ACTION FLD                          
         J     TRAPERR                                                          
*                                                                               
SELMSG01 DC    C'Record displayed - hit Pf12 to return or next sel'             
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INITIALZ NTR1  BASE=*,LABEL=*      INITIALIZE WORKING STORAGES                  
*                                                                               
         CLI   TRANSSW,C'Y'        TRANSFERRED INTO PROGRAM?                    
         BNE   INITI50                                                          
         CLI   PFAID,0             PF KEY PRESSED?                              
         BE    INITI50             NO                                           
*                                                                               
         OC    KEY(25),KEY         HAVE KEY?                                    
         BZ    INITI50                                                          
         LA    RE,KEY                                                           
         CLI   3(RE),PSMDELCQ      SUBMEDIA ECORD CODE?                         
         BNE   INITI50                                                          
         USING PSMDKEY,RE                                                       
         LA    R2,SMDMEDH          MEDIA FLD ON MAINT SCR                       
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   *+8                                                              
         LA    R2,LSTMEDH          POINT TO MEDIA FLD ON LIST SCR               
         MVC   8(1,R2),PSMDKMED                                                 
         MVI   5(R2),1             INPUT LENGTH                                 
         OI    6(R2),X'80'         TRANSMIT                                     
         DROP  RE                                                               
*                                                                               
INITI50  OI    GENSTAT4,NODELLST   NO DELETE ON LIST                            
         MVI   ACTELOPT,C'N'       NO ACTIVITY ELEM WILL BE ADDED               
         MVI   NLISTS,14           14 LINES ON LIST SCREEN                      
         CLI   PHSCREEN,X'AD'      SUBMEDIA MAINT SCR?                          
         BNE   *+8                                                              
         OI    SMDDESCH+6,X'81'    CHG TO MODIFIED FLD (GAIN CONTROL)           
*                                                                               
         MVI   LSTREPSW,0                                                       
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   *+8                                                              
         MVI   LSTREPSW,C'Y'       ACTION IS LIST OR REPORT                     
*                                                                               
         CLI   ACTNUM,ACTSEL       SELECT FROM LIST?                            
         BNE   INITI54                                                          
         CLI   PHSCREEN,X'AD'      SUBMEDIA MAINT SCREEN?                       
         BE    *+6                                                              
         DC    H'0'                WRONG SCREEN!                                
         MVC   SMDBOTL+PF12POSQ(L'PF12TXT),PF12TXT                              
         OI    SMDBOTLH+6,X'80'                                                 
*                                                                               
INITI54  LA    RE,HEDSPECS         SET UP HEADHOOK AND SPECS                    
         ST    RE,SPECS                                                         
         LA    RE,HOOK                                                          
         ST    RE,HEADHOOK                                                      
         OC    MEDNM,SPACES                                                     
         CLC   MEDNM,SPACES        MEDIA NAME PRESENT?                          
         BE    INITIX                                                           
         MVC   H2+15(10),MEDNM                                                  
         MVC   H2+10(01),QMED                                                   
*                                                                               
INITIX   J     EXIT                                                             
*                                                                               
HOOK     NTR1                      HEADLINE ROUTINES                            
*                                                                               
HOOKX    J     EXIT                                                             
*                                                                               
HEDSPECS SSPEC H2,01,C'Media'                                                   
         SSPEC H1,56,C' SubMedia Report'                                        
         SSPEC H2,56,C'-----------------'                                       
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,99,RUN                                                        
         SSPEC H5,99,REPORT                                                     
         SSPEC H1,01,REQUESTOR                                                  
         SSPEC H6,01,C'Media   SubMedia Code/Description'                       
         SSPEC H7,01,C'-----   -------------------------'                       
         DC    H'0'                                                             
*                                                                               
PF12POSQ EQU   L'SMDBOTL-L'PF12TXT                                              
PF12TXT  DC    C'Pf12=Return/NextSel'                                           
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETREPLN NTR1  BASE=*,LABEL=*      R6 POINTS TO RECORD                          
*                                                                               
         XC    P1,P1               CLEAR PRINT LINE                             
         LA    R5,P1                                                            
         USING REP_LINE,R5                                                      
*                                                                               
         LA    R6,KEY                                                           
         USING PSMDKEY,R6                                                       
         MVC   R_MEDCOD,PSMDKMED                                                
         MVC   R_SMDMED,PSMDKSMD                                                
*                                                                               
         MVI   ELCODE,PSMDELCQ     SUBMEDIA ELEM ID                             
         L     R6,AIO                                                           
         GOTOR GETEL                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PSMDELEM,R6                                                      
         MVC   R_SMDDES,PSMDDESC                                                
*                                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R6,R5                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKPFKEYS NTR1  BASE=*,LABEL=*      CKING FOR PK KEYS                            
*                                                                               
         CLI   PFAID,2             PF2, CLIENT2?                                
         BE    CKPFK10                                                          
         CLI   PFAID,5             PF5, CLT LIST?                               
         BE    CKPFK10                                                          
         CLI   PFAID,6             PF6, PRD LIST?                               
         BE    CKPFK10                                                          
*                                                                               
         CLI   PFAID,12            PF12, EXIT/RETURN?                           
         JE    SETCCEQ                                                          
         CLI   PFAID,24            PF24, EXIT/RETURN?                           
         JE    SETCCEQ                                                          
*                                                                               
         J     SETCCNEQ            VALID PFKEY IS NOT ENTERED                   
*                                                                               
CKPFK10  XC    WORK,WORK           ESTABLISH AS XCTL ELEMENT                    
         LA    R1,WORK                                                          
         USING GLVXFRSY,R1                                                      
*                                                                               
         MVC   GLVXFRSY,=C'PRI'    SET FROM SYSTEM                              
         MVC   GLVXFRPR,=C'SFM'    SET FROM PROGRAM                             
         MVC   GLVXTOSY,=C'PRI'    SET TO   SYSTEM                              
         MVC   GLVXTOPR,=C'SFM'    SET TO   PROGRAM                             
         OI    GLVXFLG1,GLV1RETN                                                
         OI    GLVXFLG1,GLV1RETG                                                
*                                                                               
* SEND XCTL ELM                                                                 
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK,14,GLVXCTL                           
*                                                                               
         MVC   DUB,SPACES          PREPARE IT FOR RECORD FLD                    
*                                                                               
         CLI   PFAID,2             RECORD IS SUBMEDIA?                          
         BNE   CKPFK15                                                          
         MVC   DUB,=C'SUBMEDIA'                                                 
         B     CKPFK25                                                          
*                                                                               
CKPFK15  J     SETCCNEQ            NO OTHER PFKEY DEFINED YET                   
*                                                                               
* SET RECORD FLD                                                                
*                                                                               
CKPFK25  GOTO1 VGLOBBER,DMCB,=C'PUTD',DUB,8,GLVXREC                             
*                                                                               
         MVC   DUB,SPACES          PREPARE IT FOR ACTION FLD                    
*                                                                               
         CLI   PFAID,2             SUBMEDIA MAINT?                              
         BNE   CKPFK30                                                          
         MVC   DUB,=C'LIST    '                                                 
         B     CKPFK40                                                          
*                                                                               
CKPFK30  DS    0H                                                               
         J     SETCCNEQ            NO OTHER PFKEY DEFINED YET                   
*                                                                               
* SET ACTION FLD                                                                
*                                                                               
CKPFK40  GOTO1 VGLOBBER,DMCB,=C'PUTD',DUB,8,GLVXACT                             
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTF',SMDMEDH,,GLVPRKEY   KEY                   
         GOTO1 VGLOBBER,DMCB,=C'PUTF',SMDMEDH,,GLVPRMD    MEDIA                 
*                                                                               
         J     SETCCEQ                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
*                                                                               
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMADD          SUBMEDIA MAINT SCREEN                        
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMAED          SUBMEDIA LIST SCREEN                         
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE PRSFMWORKD                                                     
         ORG   SYSSPARE            WORKING AREA                                 
*                                                                               
SVWORK   DS    XL64                GENERAL WORKING STORAGE                      
*                                                                               
SVDMINBT DS    X                                                                
SVDMOUTB DS    X                                                                
*                                                                               
WKTMPFLD DS    XL11                HDR IS 8 AND 3 INPUT CHARS                   
WKTMPKEY DS    XL(L'KEY)                                                        
WKTMPAIO DS    XL(L'AIO)                                                        
WKTMPION DS    XL(L'USEIONUM)                                                   
*                                                                               
WKKEY    DS    XL25                                                             
*                                                                               
LSTREPSW DS    C                   C'Y' IF ACTION IS LIST OR REPORT             
SVSUBMCD DS    CL(L'PSMDKSMD)                                                   
*                                                                               
LS_MEDCD DS    CL(L'PSMDKMED)      LIST FILTER FOR MEDIA                        
LS_SMDCD DS    CL(L'PSMDKSMD)      LIST FILTER FOR SUBMEDIA                     
*                                                                               
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPGENSMED         SUBMEDIA RECORD                              
         EJECT                                                                  
*                                                                               
       ++INCLUDE FLDIND            FIELD INDICATOR EQUATES                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE FAFACTS           MASTER SYS INFO BLOCK                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGLVXCTLD        GLOBBER TRANSFER CONTROLS                    
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGLOBEQUS        DSECT FOR GLOBBER                            
         EJECT                                                                  
*                                                                               
       ++INCLUDE FASECRETD                                                      
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSMEDCOD DS    CL(L'PSMDKMED)      MEDIA CODE                                   
         DS    CL7                                                              
LSSMDCOD DS    CL(L'PSMDKSMD)      SUBMEDIA CODE                                
         DS    CL3                                                              
LSSMDDES DS    CL(L'PSMDDESC)      DESCRIPTION                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
REP_LINE DSECT                     REPORT DSECT FOR PRINTING LINES              
*                                                                               
R_MEDCOD DS    CL(L'PSMDKMED)      MEDIA CODE                                   
         DS    CL7                                                              
R_SMDMED DS    CL(L'PSMDKSMD)      SUBMEDIA CODE                                
         DS    XL3                                                              
R_SMDDES DS    CL(L'PSMDDESC)      DESCRIPTION                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033PRSFM34   07/23/18'                                      
         END                                                                    
