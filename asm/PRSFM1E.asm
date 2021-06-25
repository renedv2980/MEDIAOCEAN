*          DATA SET PRSFM1E    AT LEVEL 247 AS OF 10/28/20                      
*PHASE T41C1EA                                                                  
*INCLUDE PPBROWSE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*               T41C1E - PRODUCT MAINT/LIST                                     
*                                                                               
* KWAN 03/05/20 SPEC-42067 NEW OPTION FILED: BILL=NO                            
*                                                                               
* MHER 05/01/19 Add field for SAP product code                                  
*                                                                               
* KWAN 10/11/16 Remove traffic user restriction for Division changes            
*                                                                               
* KWAN 03/03/15 Do not allow one character product code                         
*                                                                               
* KWAN 07/05/12 GETFACT - use address passed back by GETFACT call               
*                                                                               
* KWAN 04/13/11 Trap to dump product record on bad elements                     
*                                                                               
* KWAN 05/17/10 New logic for Main PST element                                  
*                                                                               
* SMYE 11/06/09 DISALLOW EMBEDDED BLANKS IN NEW PRODUCT CODES (ADDS)            
*                                                                               
* KWAN 07/02/07 DIVISION DISPLAY FIX                                            
*                                                                               
* KWAN 10/19/06 BILL ON PLANNED COST EFFECTIVE DATE                             
*                                                                               
* KWAN 06/01/06 ENABLE LIST FILTER FOR REPORT ACTION                            
*                                                                               
* KWAN 11/10/05 CORRECT PRD CODE FOR AUTORE WHEN NEW PRD IS ADDED               
*                                                                               
* KWAN 08/16/05 NO VALIDATION FOR ACC OFFICE CODE IF BLANK                      
*                                                                               
* KWAN 06/03/05 BROWSE FUNCTION & SPECIAL CHARS FIX ON ADD                      
*                                                                               
* KWAN 02/02/05 NEED TO GENERATE AN AUTO P41 T/A REPORT                         
*                                                                               
* KWAN 01/07/05 CK MAX I/O WHEN FILTERING LARGE AMT OF PRD DATA                 
*                                                                               
* KWAN 10/06/04 BUG FIX FOR ADDING NEW PRD (DIV FIELD ERROR)                    
*                                                                               
* KWAN 09/08/04 BUG FIX FOR ACC OFFICE CODE & ATTENTION OF                      
*                                                                               
* KWAN 08/23/04 BUG FIXES (PRD AAA)                                             
*                                                                               
* KWAN 06/21/04 ENHANCEMENT TO CODES BY PASSING CLIENT SECURITY                 
*                                                                               
* KWAN 06/02/04 LEGAL WARNING FIELD ACCESS                                      
*                                                                               
* KWAN 04/28/04 ADD USER1, USER2 AND LEGALW AS FILTER KEYWORD                   
*                                                                               
* KWAN 03/15/04 ADD ADJ CODE TO LIST AS FILTER KEYWORD                          
*                                                                               
* KWAN 01/08/04 APPLY FEILD CONTROL FEATURE FOR OPTION FIELDS                   
*                                                                               
* KWAN 01/03/03 CONVERT PRODUCT FROM FIL TO SFM                                 
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T41C00 (SFM CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, CHG, DISP, SEL, LIST, REP               *         
*                                                                     *         
*  INPUTS       SCREEN T41CA0 (MAINTENANCE)                           *         
*               SCREEN T41CA1 (LIST)                                  *         
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
*               R7 -- SECOND BASE REGISTER                            *         
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
         TITLE 'T41C1E - PRODUCT MAINT/LIST'                                    
*                                                                               
T41C1E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C1E,R7,RR=R3                                                
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         ST    R3,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         BRAS  RE,INITIALZ         INITIALIZE WORKING STORAGES                  
         CLI   MODE,NEWSCR         SCR JUST BEING LOADED?                       
         JNE   *+12                                                             
         BRAS  RE,SETSAP                                                        
         J     EXIT                YES, DISPLAY INIT'D SCR                      
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
*                                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+8                                                              
         BRAS  RE,DK                                                            
*                                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,XRECADD        AFTER ADD                                    
         BE    PPTRS                                                            
         CLI   MODE,XRECPUT        AFTER PUT                                    
         BE    PPTRS                                                            
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
*                                                                               
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL                                    
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VK       DS    0H                  VALIDATE KEY ROUTINE                         
         LA    R2,PRDMEDH          POINT TO MEDIA FLD                           
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   *+8                                                              
         LA    R2,LSTMEDH                                                       
         GOTO1 VALIMED             MEDIA IS REQUIRED EVEN FOR LIST              
*                                                                               
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    VK10                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BE    VK10                                                             
         MVC   PRDMEDN,MEDNM                                                    
         OI    PRDMEDNH+6,X'80'    NOT DISPLAYED FOR LIST AND REPORT            
*                                                                               
VK10     XC    SVKEY,SVKEY                                                      
         LA    R6,SVKEY                                                         
         USING PPRDKEY,R6                                                       
*                                                                               
         MVC   PPRDKAGY,AGENCY                                                  
         MVC   PPRDKMED,QMED                                                    
         MVI   PPRDKRCD,X'06'      RECORD CODE FOR PRODUCT                      
*                                                                               
         XC    CLTLSFLT,CLTLSFLT   CLEAR LIST FILTER ON CLT                     
         LA    R2,PRDCLTH          POINT TO CLIENT FLD                          
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   *+8                                                              
         LA    R2,LSTCLTH                                                       
         CLI   5(R2),0                                                          
         BNE   VK20                                                             
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                YES                                          
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         JNE   MSSNGERR            FIELD IS REQUIRED                            
*                                                                               
         OC    TWAACCS(2),TWAACCS  ANY LIMIT ACCESS?                            
         BZ    VK20                NO                                           
         CLI   CONWHENH+5,0        NOW, SOON, OR OV?                            
         BE    VK20                NO                                           
*                                                                               
         J     CLTRQERR            SECURITY - CLIENT REQUIRED                   
*                                                                               
VK20     CLI   8(R2),C'='          BROWSE?                                      
         BNE   VK24                                                             
*                                                                               
         GOTO1 =V(PPBROWSE),DMCB,ACOMFACS,SYSRD,(R2),                  +        
               0,(QMED,C' CLT'),0,RR=RELO                                       
         DC    H'0'                                                             
*                                                                               
VK24     CLI   5(R2),3             MAX 3 CHARS                                  
         JH    INVFDERR                                                         
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                FOR LIST, NO NEED TO VALICLT                 
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   VK30                FOR REPORT, NO NEED TO VALICLT               
         CLI   5(R2),0             NO INPUT?                                    
         BE    VK50                                                             
         CLC   =C'ALL',8(R2)       ALL CLIENT?                                  
         BE    VK50                YES, LEAVE CLIENT FILTER NULLS               
         SR    RE,RE                                                            
         IC    RE,5(R2)            INPUT LENGTH                                 
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CLTLSFLT(0),8(R2)   GET CLIENT CODE                              
         OC    CLTLSFLT,SPACES     SPACE PADDED                                 
         B     VK50                                                             
*                                                                               
VK30     CLC   =C'ALL',8(R2)                                                    
         JE    INVFDERR            CLT CODE "ALL" IS NOT ALLOWED                
         GOTO1 VALICLT                                                          
         MVC   PPRDKCLT,QCLT                                                    
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    VK50                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BE    VK50                                                             
         MVC   PRDCLTN,CLTNM                                                    
         OI    PRDCLTNH+6,X'80'    NOT DISPLAYED FOR LIST AND REPORT            
*                                                                               
VK50     XC    PRDLSFLT,PRDLSFLT   CLEAR LIST FILTER ON PRD                     
         LA    R2,PRDPRDH          POINT TO PRD FLD                             
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   *+8                                                              
         LA    R2,LSTPRDH                                                       
         CLI   5(R2),0                                                          
         BNE   VK60                                                             
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                YES                                          
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         JNE   MSSNGERR            FIELD IS REQUIRED                            
*                                                                               
* PRD SECURITY CAN BE CKED HERE!                                                
*                                                                               
VK60     CLI   8(R2),C'='          BROWSE?                                      
         BNE   VK64                                                             
*                                                                               
         LA    RF,QCLT                                                          
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   *+8                                                              
         LA    RF,CLTLSFLT                                                      
         OC    0(3,RF),0(RF)       CLIENT CODE PRESENT?                         
         BNZ   VK62                                                             
         LA    R2,PRDCLTH          POINT TO PRD FLD                             
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   *+8                                                              
         LA    R2,LSTCLTH                                                       
         J     MSSNGERR            CANNOT BROWSE PRD W/O CLT CODE               
*                                                                               
VK62     GOTO1 =V(PPBROWSE),DMCB,ACOMFACS,SYSRD,(R2),                  +        
               (0,0(RF)),(QMED,C' PRD'),0,RR=RELO                               
         DC    H'0'                                                             
*                                                                               
VK64     CLI   5(R2),3             MAX 3 CHARS                                  
         JH    INVFDERR                                                         
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                FOR LIST, NO NEED TO VALIPRD                 
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   VK70                FOR REPORT, NO NEED TO VALIPRD               
         CLI   5(R2),0             NO INPUT?                                    
         BE    VK90                                                             
         CLC   =C'ALL',8(R2)       ALL PRD?                                     
         BE    VK90                YES, LEAVE PRD FILTER NULLS                  
         SR    RE,RE                                                            
         IC    RE,5(R2)            INPUT LENGTH                                 
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PRDLSFLT(0),8(R2)   GET CLIENT CODE                              
         OC    PRDLSFLT,SPACES     SPACE PADDED                                 
         B     VK90                                                             
*                                                                               
VK70     CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BNE   VK80                                                             
         CLI   5(R2),1             At leat two characters?                      
         JNH   INVFDERR                                                         
         CLC   =C'ALL',8(R2)                                                    
         JE    INVFDERR            CANNOT ADD PRD CODE "ALL"                    
         SR    RE,RE                                                            
         IC    RE,5(R2)            INPUT LENGTH                                 
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PPRDKPRD(0),8(R2)   GET PRD CODE                                 
         OC    PPRDKPRD,SPACES     SPACE PADDED                                 
         MVC   QPRD,PPRDKPRD       FOR AUTOREQ                                  
*                                                                               
         CLI   PPRDKPRD+1,C' '                                                  
         BH    VK70F               CONTINUE                                     
         CLI   PPRDKPRD+2,C' '                                                  
         BH    INVFDERR            "MIDDLE" EMBEDDED BLANK FOUND                
*                                                                               
VK70F    DS    0H                                                               
*                                                                               
         SR    RE,RE                                                            
         IC    RE,5(R2)                                                         
         LA    RF,8(R2)                                                         
         CLI   0(RF),C'*'          MAY START WITH *                             
         BE    VK74                                                             
         CLI   0(RF),C'A'          MUST START WITH A OR GREATER                 
         JL    INVFDERR                                                         
*                                                                               
VK72     CLI   0(RF),C' '          EMBEDDED BLANK IS OK                         
         BE    VK74                                                             
         CLI   0(RF),C'0'                                                       
         BE    VK74                                                             
         TM    0(RF),X'C0'                                                      
         JNO   INVFDERR                                                         
         MVC   DUB(1),0(RF)                                                     
         NI    DUB,X'0F'                                                        
         CLI   DUB,X'01'                                                        
         JL    INVFDERR                                                         
         CLI   DUB,X'09'                                                        
         JH    INVFDERR                                                         
*                                                                               
VK74     LA    RF,1(RF)                                                         
         BCT   RE,VK72                                                          
         B     VK90                                                             
*                                                                               
VK80     GOTO1 VALIPRD                                                          
         MVC   PPRDKPRD,QPRD                                                    
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    VK90                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BE    VK90                                                             
         MVC   PRDPRDN,PRDNM                                                    
         OI    PRDPRDNH+6,X'80'    NOT DISPLAYED FOR LIST AND REPORT            
*                                                                               
VK90     LA    R0,F_FLDS                                                        
         LHI   R1,F_FLDLNQ         LENGTH OF FILTER FLDS STORAGE                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   VKX                                                              
         LA    R2,LSTFLTH          POINT TO FITLER FLD ON SCR                   
         CLI   5(R2),0                                                          
         BE    VKX                                                              
*                                                                               
         BRAS  RE,CKFLTFLD         VALIDATE FLT FLDS                            
         JNE   TRAPERR2            ERROR MSG IS SET IN ROUTINE                  
*                                                                               
VKX      MVC   KEY,SVKEY                                                        
         MVC   AIO,AIO1            RECORD WILL BE READ INTO AIO1                
*                                                                               
         J     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VR       DS    0H                  VALIDATE RECORD                              
         LA    R0,WKPOFFCD                                                      
         LHI   R1,WKSVLNQ          LENGTH OF SAVING WRK STORAGE                 
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               SAME AS XCEF, ONLY SHORTER                   
*                                                                               
         CLI   ACTNUM,ACTSEL       SEL?                                         
         BNE   VR05                                                             
*                                                                               
* FOLLOWING CODE TO PREVENT USERS FROM DESTROYING RECORD(S) THEY                
* DO NOT HAVE ACCESS TO (VALICLT IN DK MODE IS NOT GOOD ENOUGH)                 
*                                                                               
         MVC   SVWORK(L'KEY),KEY                                                
         MVC   SVWORK+L'KEY(L'AIO),AIO                                          
         MVI   USEIONUM,2                                                       
         LA    R2,PRDCLTH                                                       
         GOTO1 VALICLT             NEED TO REVALIDATE CLT FOR SECURITY          
         MVI   USEIONUM,1                                                       
         MVC   KEY,SVWORK          RESTORE KEY                                  
         MVC   AIO,SVWORK+L'KEY    RESTORE AIO                                  
*                                                                               
VR05     L     R6,AIO                                                           
         XC    WKPOFFCD,WKPOFFCD   PRD OFFICE CODE                              
         XC    WKPTRACD,WKPTRACD   PRD TRAFFICE CODE                            
         XC    WKADJCD,WKADJCD     ADJACENCY CODES                              
         XC    WKPRDOAN,WKPRDOAN   OTHER AGY NAME CODE                          
         XC    WKPRBILL,WKPRBILL   PRD BILLING PROFILE                          
         XC    WKPRSAP,WKPRSAP     SAP CODE                                     
*                                                                               
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         JNE   VR08                                                             
         XC    0(250,R6),0(R6)                                                  
         XC    250(200,R6),250(R6)                                              
         USING PPRDREC,R6                                                       
         MVC   PPRDKEY,KEY                                                      
         MVC   PPRDLEN,=H'33'      Make sure IO area & length are good          
         J     VR10                                                             
*                                                                               
VR08     MVI   ELCODE,X'06'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                FIRST ELEM MUST BE THERE!                    
         USING PPRDELEM,R6                                                      
         MVC   WKPOFFCD,PPRDOFFC   SAVE PRD OFFICE CODE                         
         MVC   WKPTRACD,PPRDTRAF   SAVE PRD TRAFFICE CODE                       
         MVC   WKADJCD,PPRDEXCL    SAVE ADJACENCY CODES                         
         MVC   WKPRDOAN,PPRDOAN    SAVE OTHER AGY NAME CODE                     
         MVC   WKPRBILL,PPRDBILP   SAVE PRD BILLING PROFILE                     
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'06'                                                     
         GOTO1 REMELEM             REMOVE X'06' ELEM (MAIN ELEM)                
*                                                                               
VR10     XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING PPRDELEM,R6                                                      
         MVI   0(R6),X'06'         MAIN ELEM CODE                               
         MVI   1(R6),X'C8'         ELEM LENGTH                                  
*                                                                               
* INIT PRD BILLING PROGFILE TO SAVED VALUES ON CHG                              
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    *+10                                                             
         MVC   PPRDBILP,WKPRBILL   PRBILL IS HANDLED IN PRSFM1F                 
                                                                                
*                                                                               
         LA    R2,PRDPNAMH         PRD NAME                                     
         GOTO1 ANY                                                              
         CLI   5(R2),6             EXACTLY SIX CHARS (DELETE)?                  
         BNE   VR14H                                                            
         CLC   8(6,R2),=C'DELETE'  DELETING PRODUCT?                            
         BNE   VR14H                                                            
         CLI   ACTNUM,ACTADD                                                    
         JE    INVFDERR            CANNOT ADD PRD W/ "DELETED" IN NAME          
         BRAS  RE,DEL_PRD                                                       
         J     TRAPERR2            ERROR OR COMPLETION MSG IS SET               
*                                                                               
VR14H    XC    PPRDNAME,PPRDNAME                                                
         SR    RE,RE                                                            
         IC    RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PPRDNAME(0),PRDPNAM                                              
*                                                                               
         LA    R2,PRDBRNH          BILL RECEIPT NAME(LINE 1)                    
         GOTO1 ANY                                                              
         XC    PPRDBILL,PPRDBILL                                                
         SR    RE,RE                                                            
         IC    RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PPRDBILL(0),PRDBRN                                               
*                                                                               
         XC    PPRDBIL2,PPRDBIL2   BILL RECEIPT NAME(LINE 2)                    
         CLI   PRDBRN2H+5,0                                                     
         BE    VR18                                                             
         SR    RE,RE                                                            
         IC    RE,PRDBRN2H+5                                                    
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PPRDBIL2(0),PRDBRN2                                              
*                                                                               
VR18     LA    R2,PRDAL1H          ADDRESS (LINE 1)                             
         GOTO1 ANY                                                              
         XC    PPRDLIN1,PPRDLIN1                                                
         SR    RE,RE                                                            
         IC    RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PPRDLIN1(0),PRDAL1                                               
*                                                                               
         LA    R2,PRDAL2H          ADDRESS (LINE 2)                             
         GOTO1 ANY                                                              
         XC    PPRDLIN2,PPRDLIN2                                                
         SR    RE,RE                                                            
         IC    RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PPRDLIN2(0),PRDAL2                                               
*                                                                               
         XC    PPRDATTN,PPRDATTN   ATTENTION OF                                 
         CLI   PRDATTNH+5,0                                                     
         BE    VR20                                                             
         SR    RE,RE                                                            
         IC    RE,PRDATTNH+5                                                    
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PPRDATTN(0),PRDATTN                                              
*                                                                               
VR20     MVI   PPRDEXC,0           EXCLUSION CLASS                              
         LA    R2,PRDEXCLH                                                      
         CLI   5(R2),0                                                          
         BE    VR22                                                             
         CLI   5(R2),9             MAX IS 9                                     
         JH    INVFDERR                                                         
         LA    R1,PRDEXCL          POINT TO INPUT FLD                           
         MVI   DUB,C'V'            FORMAT CALL                                  
         MVC   DUB+01(01),5(R2)    INPUT LENGTH                                 
         BRAS  RE,CK_EXCL          EXCLUSION CLASS VALIDATION                   
         JNE   INVFDERR                                                         
         MVC   PPRDEXC,DUB         VALIDATED EXCL CLASS BIT(S)                  
*                                                                               
VR22     LA    R2,PRDNOTRH         TRAFFIC? FLD                                 
         NI    PPRDSTAT,X'FF'-X'20'                                             
         CLI   5(R2),0             ANY INPUTS?                                  
         BE    VR24                                                             
         CLI   8(R2),C'Y'          VALUE IS "Y"? (LEAVE BIT OFF)                
         BE    VR24                                                             
         CLI   8(R2),C'N'          VALUE IS "N"?                                
         JNE   INVFDERR                                                         
         OI    PPRDSTAT,X'20'      SET BIT ON, TRAFFIC? = N                     
*                                                                               
VR24     LA    R2,PRDADJSH         ADJACENCY CODES                              
         XC    PPRDEXCL,PPRDEXCL                                                
         CLI   5(R2),0                                                          
         BE    VR26                                                             
         CLI   5(R2),3                                                          
         JH    INVFDERR                                                         
*                                                                               
         SR    RE,RE                                                            
         IC    RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PPRDEXCL(0),PRDADJS                                              
*                                                                               
* CHECK FOR DUPLICATES                                                          
*                                                                               
         CLC   PPRDEXCL(1),PPRDEXCL+1                                           
         JE    DUPEDERR                                                         
         CLC   PPRDEXCL(1),PPRDEXCL+2                                           
         JE    DUPEDERR                                                         
         OC    PPRDEXCL+1(2),PPRDEXCL+1                                         
         BZ    VR30                                                             
         CLC   PPRDEXCL+1(1),PPRDEXCL+2                                         
         JE    DUPEDERR                                                         
*                                                                               
VR26     CLI   SAPAGY,C'Y'                                                      
         BNE   VR30                                                             
*                                                                               
         XC    WORK,WORK                                                        
         LA    R2,PRDSAPH          SAP CODE NOT REQUIRED!                       
         CLI   5(R2),0                                                          
         JE    VR26X                                                            
         GOTO1 ANY                                                              
VR26X    MVC   WKPRSAP,WORK                                                     
*                                                                               
VR30     LA    R2,PRDGSTH          GST CODE                                     
         MVI   PPRDGST,0                                                        
         TM    1(R2),X'20'         GST CODE FLD IS PROTECTED?                   
         BO    VR32                YES, NO VALIDATION IS NEEDED                 
         CLI   5(R2),0                                                          
         BE    VR32                NO INPUTS                                    
         LA    RF,VR_GSTC                                                       
VR30H    CLI   0(RF),X'FF'         END OF TABLE?                                
         JE    INVFDERR                                                         
         CLC   0(1,RF),PRDGST      GST CODE DEFINED IN TABLE?                   
         BE    *+12                                                             
         LA    RF,1(RF)                                                         
         B     VR30H                                                            
         MVC   PPRDGST,PRDGST      MOVE VALIDATED GST TAX CODE                  
*                                                                               
VR32     LA    R2,PRDDIVH          DIVISION NUMBER                              
* * * *  L     RF,ATWA                                                          
* * * *  TM    12(RF),X'08'        TRAFFIC USER AUTHORIZED?                     
* * * *  BZ    VR32D                                                            
* * * *  CLI   5(R2),0             NO INPUT?                                    
* * * *  BE    VR34                                                             
* * * *  OC    8(L'PRDDIV,R2),8(R2)                                             
* * * *  BZ    VR34                                                             
* * * *  B     NOTAUTER            NOT AUTHORIZED TO CHANGE THIS FLD            
VR32D    XC    PRDDIVN,PRDDIVN     CLR DIV NAME                                 
         OI    PRDDIVNH+6,X'80'                                                 
         CLI   SVCPROF+00,C'1'     DIVISIONS REQUIRED?                          
         BE    VR32H                                                            
         CLI   SVCPROF+00,C'2'     PUB DRD REQUIRED?                            
         BNE   VR32M                                                            
VR32H    OC    8(L'PRDDIV,R2),8(R2)                                             
         BZ    MSSNGERR                                                         
         TM    4(R2),X'08'         NUMERIC?                                     
         BZ    NOTNUMER                                                         
         GOTO1 ANY                                                              
         BRAS  RE,PACK             DUB=0 (PACKED) IF INPUT NOT NUMBERIC         
         OI    DUB+07,X'0F'                                                     
         UNPK  PPRDDIV(03),DUB+06(02)                                           
         MVC   SVWORK(L'KEY),KEY                                                
         XC    KEY+07(L'KEY-07),KEY+07                                          
         MVI   KEY+03,X'03'        DIVISION RECORD CODE                         
         MVC   KEY+07(03),PPRDDIV                                               
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(25),KEY                                                  
         JNE   DIVNFERR            DIV RECORD NOT FOUND ERROR                   
         MVC   FULL,AIO                                                         
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,FULL            RESTORE ORIGINAL AIO                         
         L     RE,AIO3                                                          
         CLI   33(RE),X'03'        FIRST DIV ELEM PRESENT?                      
         BE    *+6                                                              
         DC    H'0'                IT MUST BE THERE!                            
         USING PDIVREC,RE                                                       
         MVC   PRDDIVN,PDIVNAME    DIVISION NAME                                
         OI    PRDDIVNH+6,X'80'                                                 
         MVC   KEY,SVWORK          RESTORE KEY                                  
         B     VR34                                                             
         DROP  RE                                                               
*                                                                               
VR32M    CLI   5(R2),0                                                          
         JNE   DIVNSERR            DIV NOT SETUP IN CLT PROFILE ERROR           
         XC    PPRDDIV,PPRDDIV                                                  
*                                                                               
VR34     MVI   PPRDOFFC,0          PRD OFFICE CODE                              
         MVI   PPRDTRAF,0          PRD TRAFFIC CODE                             
         TM    PRDOFFH+1,X'20'     PRD OFFICE CODE FLD IS PROTECTED?            
         BO    VR36                YES                                          
         MVC   PRDOFX,SPACES                                                    
         OI    PRDOFXH+6,X'80'     CLR HEX DISPLAYS IF ANY (OFFICE)             
         MVC   PRDTRAX,SPACES                                                   
         OI    PRDTRAXH+6,X'80'    CLR HEX DISPLAYS IF ANY (TRAFFIC)            
         LA    R2,PRDOFFH                                                       
         CLI   5(R2),0             ANY INPUTS?                                  
         BE    VR34H                                                            
         BRAS  RE,VR_OTCHR         CK FOR SPECIAL VALID CHARS                   
         MVC   BYTE,PRDOFF                                                      
         LA    R2,PRDOFXH          FOR DISPLAYING HEX VALUES                    
         BRAS  RE,VR_OTHXD                                                      
         MVC   PPRDOFFC,PRDOFF     PUT VALIDATED OFFICE CODE IN ELEM            
VR34H    LA    R2,PRDTRAFH                                                      
         CLI   5(R2),0             ANY INPUTS?                                  
         BE    VR36                                                             
         BRAS  RE,VR_OTCHR         CK FOR SPECIAL VALID CHARS                   
         MVC   BYTE,PRDTRAF                                                     
         LA    R2,PRDTRAXH         FOR DISPLAYING HEX VALUES                    
         BRAS  RE,VR_OTHXD                                                      
         MVC   PPRDTRAF,PRDTRAF    PUT VALIDATED TRAFFIC CODE IN ELEM           
*                                                                               
VR36     XC    PPRDACCT,PPRDACCT   ACCOUNT NUMBER                               
         LA    R2,PRDACCTH                                                      
         OC    8(L'PRDACCT,R2),8(R2)                                            
         BZ    VR38                                                             
         CLI   5(R2),0                                                          
         BE    VR38                                                             
         TM    4(R2),X'08'         NUMERIC?                                     
         BNO   VR36K                                                            
         BRAS  RE,PACK                                                          
         STCM  R0,7,PPRDACCT+1                                                  
         MVI   PPRDACCT,X'FF'      INDICATOR FOR 3 BYTES BINARY ACC#            
         B     VR38                                                             
VR36K    CLI   5(R2),4                                                          
         JH    INVFDERR            IF NOT NUMERIC, ONLY 4 CHARS ALLOWED         
         MVC   PPRDACCT,PRDACCT                                                 
         OC    PPRDACCT,SPACES     SPACE PADDED                                 
*                                                                               
* CANNOT CHANGE OAN PRD TO A NON-OAN PRD                                        
*                                                                               
VR38     XC    PPRDOAN,PPRDOAN                                                  
         XC    PRDOANN,PRDOANN     CLR OAN NAME (PROTECTED)                     
         OI    PRDOANNH+6,X'80'                                                 
         LA    R2,PRDOANH                                                       
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BNE   *+16                                                             
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VR40                                                             
         B     VR38K               CK OAN INPUT IS VALID                        
         CLI   5(R2),0             ANY INPUT?                                   
         BE    *+18                NONE, NEED TO CK PREVIOUS VALUE              
         OC    WKPRDOAN,WKPRDOAN   PREVIOUS OAN CODE PRESENT?                   
         BZ    *+18                CANNOT ADD OAN CODE                          
         B     VR38K               CK OAN INPUT IS VALID                        
         OC    WKPRDOAN,WKPRDOAN   PREVIOUS OAN CODE PRESENT?                   
         BZ    VR40                NO OAN CODE IS ENTERED                       
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'VR_MSG01),VR_MSG01                                     
         OI    CONHEADH+6,X'80'                                                 
         J     TRAPERR2                                                         
*                                                                               
* CK IF OTHER AGY NAME REC IS ON FILE                                           
*                                                                               
VR38K    MVC   SVWORK(L'KEY),KEY                                                
         XC    KEY+03(L'KEY-03),KEY+03                                          
         MVI   KEY+03,X'16'                                                     
         SR    RE,RE                                                            
         IC    RE,5(R2)            INPUT LENGTH                                 
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   KEY+04(00),PRDOAN                                                
         OC    KEY+04(02),SPACES   NO NULLS                                     
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE     OAN CODE ON FILE?                            
         JNE   RECNFERR                                                         
*                                                                               
         MVC   DUB(L'AIO),AIO      SAVE ORIGINAL AIO                            
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     RE,AIO3                                                          
         USING POTHREC,RE                                                       
         MVC   PRDOANN(L'POTHNAME),POTHNAME                                     
         OI    PRDOANNH+6,X'80'                                                 
         MVC   PPRDOAN,POTHCODE    GET VALIDATED OAN CODE                       
         DROP  RE                                                               
*                                                                               
         MVC   KEY,SVWORK          RESTORE KEY                                  
         MVC   AIO,DUB             RESTORE ORIGINAL AIO                         
*                                                                               
VR40     DS    0H                  FOR FUTURE USES                              
*                                                                               
         GOTO1 ADDELEM             MAIN ELEM IS VALIDATED AND ADDED             
         DROP  R6                  FOR PPRDELEM                                 
*                                                                               
         OC    PRDLWT,PRDLWT       LW ROTATION FLD PRESENT?                     
         BZ    VR50                                                             
         MVI   ELCODE,X'40'                                                     
         GOTO1 REMELEM             REMOVE X'40' ELEM (LEGAL WARNING)            
         LA    R2,PRDLWROH         LW ROTATION (LEGAL WARNING)                  
         OC    PRDLWRO,PRDLWRO                                                  
         BZ    VR50                                                             
*                                                                               
         LA    R6,ELEM             BUILT ELEM AND ADD IT TO PRD REC             
         XC    ELEM,ELEM                                                        
         USING PPRDLWEL,R6                                                      
*                                                                               
         CLC   8(11,R2),=C'A1,B2,C3,D4'                                         
         BNE   *+14                                                             
         MVC   PPRDROTA,=C'ABCD'                                                
         B     VR48K                                                            
         CLC   8(11,R2),=C'B1,C2,D3,A4'                                         
         BNE   *+14                                                             
         MVC   PPRDROTA,=C'BCDA'                                                
         B     VR48K                                                            
         CLC   8(11,R2),=C'C1,D2,A3,B4'                                         
         BNE   *+14                                                             
         MVC   PPRDROTA,=C'CDAB'                                                
         B     VR48K                                                            
         CLC   8(11,R2),=C'D1,A2,B3,C4'                                         
         BNE   *+14                                                             
         MVC   PPRDROTA,=C'DABC'                                                
         B     VR48K                                                            
*                                                                               
         J     INVFDERR            NO OTHER COMBINATIONS YET                    
*                                                                               
VR48K    MVI   0(R6),X'40'         LW ROTATION ELEM CODE                        
         MVI   1(R6),08            LW ROATAION ELEM LENGTH                      
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
VR50     LA    R2,PRDAOFFH         PRD ACCOUNT OFFICE CODE                      
         MVI   ELCODE,X'35'                                                     
         GOTO1 REMELEM             REMOVE X'35' ELEM (ACC OFFICE CODE)          
         CLI   5(R2),0             ANY INPUTS?                                  
         BNH   VR52                                                             
         BRAS  RE,CKAOFF                                                        
         JNE   TRAPERR2            ERROR IS SET IN CONHEAD                      
         LA    R6,ELEM                                                          
         GOTO1 ADDELEM             ELEM IS BUILT IN CKAOFF ROUTINE              
*                                                                               
* CKING FOR PRD USER FLDS                                                       
*                                                                               
VR52     L     RF,ATWA                                                          
         TM    12(RF),X'08'        AUTHORIZED?                                  
         BZ    VR52D               NO CHANGES TO USER FLDS                      
         LA    R2,PRDDSC1H                                                      
         CLI   PRDDSC1H+5,0        NO INPUT?                                    
         BNE   NOTAUTER            NOT AUTHORIZED TO CHANGE THIS FLD            
         LA    R2,PRDDSC2H                                                      
         CLI   PRDDSC2H+5,0        NO INPUT?                                    
         BNE   NOTAUTER            NOT AUTHORIZED TO CHANGE THIS FLD            
         B     VR54                                                             
*                                                                               
VR52D    XC    SVPUSER1,SVPUSER1                                                
         XC    SVPUSER2,SVPUSER2                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'08'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VR52F                                                            
         USING PPRDUDEF,R6                                                      
         MVC   SVPUSER1,PUSER1     SAVE FOR FIELD CONTROL                       
         MVC   SVPUSER2,PUSER2     SAVE FOR FIELD CONTROL                       
         DROP  R6                                                               
*                                                                               
VR52F    MVI   ELCODE,X'08'                                                     
         GOTO1 REMELEM             REMOVE X'08' ELEM (USER DEF FLDS)            
         LA    R6,ELEM             BUILT ELEM AND ADD IT TO PRD REC             
         XC    ELEM,ELEM                                                        
         USING PPRDUDEF,R6                                                      
         MVI   ELEM+00,X'08'       ELEM CODE                                    
         MVI   ELEM+01,2+32+16     ELEM LENGTH                                  
*                                                                               
         MVC   PUSER1,SVPUSER1                                                  
         CLI   SECPUD1,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    VR52H                                                            
         CLI   SECPUD1,C'Y'        READ ONLY (FIELD SECURITY)?                  
         BE    VR52H                                                            
         LA    R2,PRDDSC1H                                                      
         LA    R3,SVP1USER                                                      
         BRAS  RE,VR_USERF                                                      
         MVC   PUSER1,WORK         GET VALIDATED USER FLD 1 IF PRESENT          
*                                                                               
VR52H    MVC   PUSER2,SVPUSER2                                                  
         CLI   SECPUD2,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    VR52K                                                            
         CLI   SECPUD2,C'Y'        READ ONLY (FIELD SECURITY)?                  
         BE    VR52K                                                            
         LA    R2,PRDDSC2H                                                      
         LA    R3,SVP2USER                                                      
         BRAS  RE,VR_USERF                                                      
         MVC   PUSER2,WORK         GET VALIDATED USER FLD 2 IF PRESENT          
*                                                                               
* CK IF ELEM IS NOT EMPTY                                                       
*                                                                               
VR52K    OC    ELEM+2(L'PUSER1+L'PUSER2),ELEM+2                                 
         BZ    VR54                                                             
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
VR54     LA    R2,PRDPSTH                                                       
         MVI   ELCODE,X'25'                                                     
         BRAS  RE,VAL_PST                                                       
         JNE   INVFDERR                                                         
                                                                                
         LA    R2,PRDMPSH                                                       
         MVI   ELCODE,PPRDMPEQ                                                  
         BRAS  RE,VAL_PST                                                       
         JNE   INVFDERR                                                         
*                                                                               
VR58     L     RF,ATWA                                                          
         TM    12(RF),X'08'        TRAFFIC USER AUTHORIZED?                     
         BZ    VR58D               YES, NO NEED TO CK INTERFACE CODE            
         LA    R2,PRDINFCH                                                      
         CLI   PRDINFCH+5,0        NO INPUT?                                    
         BE    VR60                                                             
         B     NOTAUTER            NOT AUTHORIZED TO CHANGE THIS FLD            
VR58D    MVI   ELCODE,X'30'                                                     
         GOTO1 REMELEM             REMOVE X'30' ELEM (INTERFACE ELEM)           
         LA    R2,PRDINFCH                                                      
         CLI   5(R2),0             ANY INPUTS?                                  
         BNE   VR58H                                                            
         CLI   F0PROF+5,C'P'       ALLOWED FOR PRD?                             
         JE    MSSNGERR            YES, MISSING ERROR                           
         CLI   F0PROF+5,C'B'       ALLOWED FOR BOTH PRD AND CLT?                
         JE    MSSNGERR            YES, MISSING ERROR                           
*                                                                               
VR58H    LA    R6,ELEM             BUILT ELEM AND ADD IT TO PRD REC             
         XC    ELEM,ELEM                                                        
         USING PPRDICEL,R6                                                      
         MVI   0(R6),X'30'         INTERFACE CODE ELEM CODE                     
         MVI   1(R6),2+10          INTERFACE CODE ELEM LENGTH                   
         SR    RE,RE                                                            
         IC    RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PPRDINFC(0),8(R2)                                                
         OC    PPRDINFC,SPACES     SPACE PADDED                                 
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
VR60     CLI   SECPCED,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    VR65                                                             
         CLI   SECPCED,C'Y'        READ ONLY (FIELD SECURITY)?                  
         BE    VR65                                                             
         BRAS  RE,BPCEFFDT         BILL ON PLANNED COST EFFECTIVE DATE          
         JNE   TRAPERR2                                                         
*                                                                               
VR65     MVI   ELCODE,PPSAPELQ     X'51'                                        
         GOTO1 REMELEM                                                          
*                                                                               
         CLI   SAPAGY,C'Y'                                                      
         JNE   VR70                                                             
*                                  X'51'                                        
         MVI   ELEM,PPSAPELQ                                                    
         MVI   ELEM+1,12                                                        
         MVC   ELEM+2(10),WKPRSAP                                               
         GOTO1 ADDELEM                                                          
*                                                                               
VR70     BRAS  RE,CKOPTFLD         Check option field                           
         JNE   TRAPERR2                                                         
*                                                                               
VRX      J     DR                  RECORD VALIDATED, REDISPLAY IT               
*                                                                               
* CK FOR INVALID CHARS FOR PRD OFFICE AND TRAFFIC CODES                         
*                                                                               
VR_OTCHR CLI   8(R2),C'='                                                       
         JE    INVFDERR                                                         
         CLI   8(R2),C'-'                                                       
         JE    INVFDERR                                                         
         CLI   8(R2),C','                                                       
         JE    INVFDERR                                                         
         CLI   8(R2),C'.'                                                       
         JE    INVFDERR                                                         
         BR    RE                                                               
*                                                                               
* HEX DISPLAY FOR PRD OFFICE AND TRAFFIC CODE                                   
*                                                                               
VR_OTHXD ST    RE,FULL             SAVE RETURN ADDRESS                          
         CLI   BYTE,X'40'          SHOW HEX VALUE?                              
         BNH   VR_OTX              NO                                           
         CLI   BYTE,X'D0'          SHOW HEX VALUE?                              
         BE    VR_OT50             YES                                          
         CLI   BYTE,X'E0'          SHOW HEX VALUE?                              
         BE    VR_OT50             YES                                          
         CLI   BYTE,C'A'           SHOW HEX VALUE?                              
         BL    VR_OT50             YES                                          
         CLI   BYTE,C'9'           SHOW HEX VALUE?                              
         BNH   VR_OTX              NO                                           
*                                                                               
VR_OT50  GOTO1 HEXOUT,DMCB,BYTE,8(R2),1                                         
         OI    6(R2),X'80'                                                      
*                                                                               
VR_OTX   L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
* ROUTINE TO CK FOR USER DEFINITION FLDS                                        
*                                                                               
* INPUT:  R2 = A(INPUT FIELD)                                                   
*         R3 = USER DEFINITION FLD BLOCK FROM CLT RECORD                        
*                                                                               
* OUTPUT: WORK = DATA OR NULLS                                                  
*                                                                               
VR_USERF DS    0H                                                               
         USING UDEFD,R3                                                         
         ST    RE,FULL             SAVE RETURN ADDRESS                          
         XC    WORK,WORK                                                        
         OC    0(L'SVP1USER,R3),0(R3)                                           
         BZ    VR_USRX                                                          
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   VR_USR10                                                         
         TM    UFLG1,X'80'         INPUT REQUIRED?                              
         BNO   VR_USRX                                                          
         J     MSSNGERR                                                         
*                                                                               
VR_USR10 CLC   5(1,R2),ULEN        INPUT LENGTH IS VALID?                       
         JH    LONGERR                                                          
         CLI   UTYPE,C' '          ACCEPT ANY INPUT?                            
         BNH   VR_USR60                                                         
         CLI   UTYPE,C'N'          NUMERIC?                                     
         BNE   VR_USR40                                                         
         TM    4(R2),X'08'         INPUT MUST BE NUMERIC                        
         BO    VR_USR60                                                         
         SR    R1,R1                                                            
         IC    R1,5(R2)            BUT ALLOW '- /'                              
         LA    R4,8(R2)                                                         
*                                                                               
VR_USR12 CLI   0(R4),C'0'                                                       
         BL    VR_USR14                                                         
         CLI   0(R4),C'9'                                                       
         BNH   VR_USR16                                                         
*                                                                               
VR_USR14 CLI   0(R4),C' '                                                       
         BE    VR_USR16                                                         
         CLI   0(R4),C'/'                                                       
         BE    VR_USR16                                                         
         CLI   0(R4),C'-'                                                       
         JNE   INVFDERR                                                         
*                                                                               
VR_USR16 LA    R4,1(R4)                                                         
         BCT   R1,VR_USR12                                                      
         B     VR_USR60                                                         
*                                                                               
VR_USR40 CLI   UTYPE,C'C'          ALPHA TYPE?                                  
         BNE   VR_USR50                                                         
         SR    R1,R1                                                            
         IC    R1,5(R2)            ALLOW ALL INPUT EXCEPT NUMBERS               
         LA    R4,8(R2)                                                         
*                                                                               
VR_USR44 CLI   0(R4),C'0'                                                       
         BL    VR_USR48                                                         
         CLI   0(R4),C'9'                                                       
         JNH   INVFDERR                                                         
*                                                                               
VR_USR48 LA    R4,1(R4)                                                         
         BCT   R1,VR_USR44                                                      
         B     VR_USR60                                                         
*                                                                               
VR_USR50 CLI   UTYPE,C'D'          DATE TYPE?                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATVAL,DMCB,(0,8(R2)),WKCDATE                                    
         OC    DMCB(4),DMCB                                                     
         JZ    INVDTERR                                                         
         L     R4,0(R1)            L'DATE                                       
         SR    R1,R1                                                            
         IC    R1,5(R2)            L'INPUT                                      
         SR    R1,R4                                                            
         JNZ   INVFDERR                                                         
*                                                                               
VR_USR60 SR    R1,R1                                                            
         IC    R1,5(R2)            GET INPUT LENGTH                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),8(R2)       MOVE INPUT INTO WORK                         
*                                                                               
VR_USRX  L     RE,FULL                                                          
         BR    RE                                                               
         DROP  R3                                                               
*                                                                               
* TABLE OF VALID GST CODES                                                      
*                                                                               
VR_GSTC  DC    C'S'                                                             
         DC    C'X'                                                             
         DC    C'Z'                                                             
         DC    X'FF'                                                            
*                                                                               
* TABLE FOR EXCLUSION CLASSES                                                   
* 1ST BYTE = EXCL CLASS CODE, 2ND BYTE = BIT EQUATE                             
*                                                                               
VR_EXCTB DC    X'C280'             (B)EER    (X'80' BIT)                        
         DC    X'E640'             (W)INE    (X'40' BIT)                        
         DC    X'D320'             (L)IQUOR  (X'20' BIT)                        
         DC    X'E310'             (T)OBACCO (X'10' BIT)                        
         DC    X'C308'             (C)IG     (X'08' BIT)                        
         DC    X'0000'             END OF TABLE                                 
*                                                                               
VR_MSG01 DC    C'Cannot add/remove OAN code'                                    
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DR       DS    0H                  DISPLAY RECORD                               
         L     R6,AIO                                                           
         CLI   3(R6),X'06'         PRD RECORD CODE?                             
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PPRDELEM,R6                                                      
         MVI   ELCODE,X'06'        FIRST PRD ELEM CODE                          
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    PRDPNAM,PRDPNAM     PRD NAME                                     
         MVC   PRDPNAM(L'PPRDNAME),PPRDNAME                                     
         OI    PRDPNAMH+6,X'80'                                                 
*                                                                               
         XC    PRDBRN,PRDBRN       BILL RECEIPT NAME - LINE 1                   
         MVC   PRDBRN(L'PPRDBILL),PPRDBILL                                      
         OI    PRDBRNH+6,X'80'                                                  
*                                                                               
         XC    PRDBRN2,PRDBRN2     BILL RECEIPT NAME - LINE 2                   
         MVC   PRDBRN2(L'PPRDBIL2),PPRDBIL2                                     
         OI    PRDBRN2H+6,X'80'                                                 
*                                                                               
         XC    PRDAL1,PRDAL1       ADDRESS LINE 1                               
         MVC   PRDAL1(L'PPRDLIN1),PPRDLIN1                                      
         OI    PRDAL1H+6,X'80'                                                  
*                                                                               
         XC    PRDAL2,PRDAL2       ADDRESS LINE 2                               
         MVC   PRDAL2,PPRDLIN2                                                  
         OI    PRDAL2H+6,X'80'                                                  
*                                                                               
         XC    PRDATTN,PRDATTN     ATTENTION OF                                 
         MVC   PRDATTN(L'PPRDATTN),PPRDATTN                                     
         OI    PRDATTNH+6,X'80'                                                 
*                                                                               
         XC    PRDADJS,PRDADJS     ADJACENCY CODES                              
         MVC   PRDADJS(L'PPRDEXCL),PPRDEXCL                                     
         OI    PRDADJSH+6,X'80'                                                 
*                                                                               
         MVI   PRDNOTR,C'Y'        "TRAFFIC?" Y/N (DEFAULT IS Y)                
         USING PPRDELEM,R6                                                      
         TM    PPRDSTAT,X'20'      IS TRAFFIC? = N?                             
         BZ    *+8                                                              
         MVI   PRDNOTR,C'N'                                                     
         OI    PRDNOTRH+6,X'80'                                                 
*                                                                               
         TM    SVCLSTAT,X'20'      CLT ALLOWS OFFC & TRFF CNTL?                 
         BZ    DR08                NO                                           
         XC    PRDOFF,PRDOFF                                                    
         XC    PRDOFX,PRDOFX                                                    
         CLI   PPRDOFFC,0          OFFICE CODE PRESENT?                         
         BE    DR06H                                                            
         MVC   PRDOFF(L'PPRDOFFC),PPRDOFFC                                      
         LA    R2,PRDOFXH                                                       
         MVC   BYTE,PPRDOFFC                                                    
         BRAS  RE,DR_OTHEX         DISP OFFICE CODE IN HEX                      
*                                                                               
DR06H    XC    PRDTRAF,PRDTRAF                                                  
         XC    PRDTRAX,PRDTRAX                                                  
         CLI   PPRDTRAF,0          TRAFFIC CODE PRESENT?                        
         BE    DR06U                                                            
         MVC   PRDTRAF(L'PPRDTRAF),PPRDTRAF                                     
         LA    R5,PRDTRAXH                                                      
         MVC   BYTE,PPRDTRAF                                                    
         BRAS  RE,DR_OTHEX         DISP TRAFFIC CODE IN HEX                     
*                                                                               
DR06U    OI    PRDOFFH+6,X'80'                                                  
         OI    PRDOFXH+6,X'80'                                                  
         OI    PRDTRAFH+6,X'80'                                                 
         OI    PRDTRAXH+6,X'80'                                                 
*                                                                               
DR08     XC    PRDEXCL,PRDEXCL     EXCLUSION CLASS                              
         LA    R1,PRDEXCL                                                       
         MVC   BYTE,PPRDEXC                                                     
         MVI   DUB,C'F'            FORMAT CALL                                  
         BRAS  RE,CK_EXCL          FORMAT EXCLUSION CLASS DISPLAY               
         OI    PRDEXCLH+6,X'80'    TRANSMITT TO SCREEN                          
*                                                                               
         CLI   WNATION,C'C'        CANADIAN?                                    
         BNE   *+14                                                             
         MVC   PRDGST(L'PPRDGST),PPRDGST                                        
         OI    PRDGSTH+6,X'80'                                                  
*                                                                               
         XC    PRDACCT,PRDACCT     ACCOUNT NUMBER                               
         CLI   PPRDACCT,X'FF'                                                   
         BNE   DR12H                                                            
         MVC   FULL,PPRDACCT                                                    
         MVI   FULL,0                                                           
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PRDACCT(5),DUB+5(3)                                              
         B     DR12U                                                            
DR12H    OC    PPRDACCT,PPRDACCT   ANYTHING IN ACCOUNT NUMBER?                  
         BZ    DR12U                                                            
         MVC   PRDACCT(L'PPRDACCT),PPRDACCT                                     
DR12U    OI    PRDACCTH+6,X'80'                                                 
*                                                                               
         XC    PRDDIV,PRDDIV       CLR DIV CODE                                 
         XC    PRDDIVN,PRDDIVN     CLR DIV NAME                                 
         OC    PPRDDIV,PPRDDIV     DIV CODE PRESENT?                            
         BZ    DR14U                                                            
         MVC   PRDDIV(L'PPRDDIV),PPRDDIV                                        
         MVC   SVWORK(L'KEY),KEY                                                
         XC    KEY+07(L'KEY-07),KEY+07                                          
         MVI   KEY+03,X'03'        DIVISION RECORD CODE                         
         MVC   KEY+07(03),PPRDDIV                                               
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(25),KEY     RECORD ON FILE?                              
         BE    DR14K                                                            
         MVC   PRDDIVN(19),=CL19'* DIV NOT ON FILE *'                           
         B     DR14U                                                            
*                                                                               
DR14K    MVC   DUB(L'AIO),AIO      SAVE AIO POINTER                             
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     RE,AIO2                                                          
         USING PDIVREC,RE                                                       
         MVC   PRDDIVN(L'PDIVNAME),PDIVNAME                                     
         DROP  RE                                                               
         MVC   KEY,SVWORK          RESTORE KEY                                  
         MVC   AIO,DUB             RESTORE AIO POINTER                          
*                                                                               
DR14U    OI    PRDDIVH+6,X'80'                                                  
         OI    PRDDIVNH+6,X'80'                                                 
*                                                                               
         LA    R2,PRDOANH          FOR POSITIONING CURSOR ON ERROR              
         XC    PRDOAN,PRDOAN       OTHER AGENCY NAME (CODE)                     
         XC    PRDOANN,PRDOANN     OTHER AGENCY NAME                            
         OC    PPRDOAN,PPRDOAN                                                  
         BZ    DR16U                                                            
         MVC   PRDOAN(L'PPRDOAN),PPRDOAN                                        
         MVC   SVWORK(L'KEY),KEY                                                
         XC    KEY+03(L'KEY-03),KEY+03                                          
         MVI   KEY+03,X'16'        OTHER AGY NAME REC CODE                      
         MVC   KEY+04(02),PRDOAN                                                
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE     OAN CODE ON FILE?                            
         JNE   INVFDERR                                                         
         MVC   DUB(L'AIO),AIO      SAVE AIO POINTER                             
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     RE,AIO2                                                          
         USING POTHREC,RE                                                       
         MVC   PRDOANN(L'POTHNAME),POTHNAME                                     
         MVC   KEY,SVWORK          RESTORE KEY                                  
         MVC   AIO,DUB             RESTORE AIO POINTER                          
         DROP  RE                                                               
*                                                                               
DR16U    OI    PRDOANH+6,X'80'                                                  
         OI    PRDOANNH+6,X'80'                                                 
*                                                                               
         DROP  R6                  FROM PPRDELEM                                
*                                                                               
         LA    R2,PRDLWROH         LEGAL WARNING                                
         XC    PRDLWRO,PRDLWRO                                                  
         CLI   SECLGWR,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    DR18U                                                            
         BRAS  RE,FMTLEGWG         RESULT WILL BE IN ELEM                       
         BNE   DR18U                                                            
         MVC   8(11,R2),ELEM                                                    
DR18U    OI    6(R2),X'80'                                                      
*                                                                               
         XC    PRDINFC,PRDINFC     INTERFACE CODE                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DR20U                                                            
         USING PPRDICEL,R6                                                      
         MVC   PRDINFC(L'PPRDINFC),PPRDINFC                                     
DR20U    OI    PRDINFCH+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
         XC    PRDDSC1,PRDDSC1     USER DEFINITION FLDS                         
         XC    PRDDSC2,PRDDSC2                                                  
         L     R6,AIO                                                           
         MVI   ELCODE,X'08'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DR22U                                                            
         USING PPRDUDEF,R6                                                      
         CLI   SECPUD1,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    DR22M                                                            
         OC    SVP1USER,SVP1USER                                                
         BZ    *+10                                                             
         MVC   PRDDSC1(L'PUSER1),PUSER1                                         
DR22M    CLI   SECPUD2,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    DR22U                                                            
         OC    SVP2USER,SVP2USER                                                
         BZ    *+10                                                             
         MVC   PRDDSC2(L'PUSER2),PUSER2                                         
DR22U    OI    PRDDSC1H+6,X'80'                                                 
         OI    PRDDSC2H+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
         CLI   WNATION,C'C'        CANADIAN?                                    
         BNE   DR26                                                             
         XC    PRDPST,PRDPST       PST                                          
         L     R6,AIO                                                           
         MVI   ELCODE,X'25'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DR24U                                                            
         LA    R4,ELEM                                                          
         USING PSTBLKD,R4                                                       
         XC    ELEM,ELEM           CLEAR INTERFACE BLOCK                        
         MVI   PSTACT,PSTFMTQ      ACTION = FORMAT                              
         LA    R1,2(R6)                                                         
         ST    R1,PSTADIN          INPUT ADDRESS                                
         XC    SVWORK,SVWORK                                                    
         LA    R1,SVWORK                                                        
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,ACOMFACS                                                 
         XC    DMCB(12),DMCB       GET PST ADDRESS                              
         MVC   DMCB+4(4),=X'D9000A6B'                                           
         GOTO1 CALLOV,DMCB                                                      
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R4)                                                   
         MVC   PRDPST,SVWORK       OUTPUT                                       
DR24U    OI    PRDPSTH+6,X'80'                                                  
         DROP  R4                                                               
*                                                                               
         XC    PRDMPS,PRDMPS       Main PST                                     
         L     R6,AIO                                                           
         MVI   ELCODE,PPRDMPEQ                                                  
         BRAS  RE,GETEL                                                         
         BNE   DR25U                                                            
         LA    R4,ELEM                                                          
         USING PSTBLKD,R4                                                       
         XC    ELEM,ELEM           CLEAR INTERFACE BLOCK                        
         MVI   PSTACT,PSTFMTQ      ACTION = FORMAT                              
         LA    R1,2(R6)                                                         
         ST    R1,PSTADIN          INPUT ADDRESS                                
         XC    SVWORK,SVWORK                                                    
         LA    R1,SVWORK                                                        
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,ACOMFACS                                                 
         XC    DMCB(12),DMCB       GET PST ADDRESS                              
         MVC   DMCB+4(4),=X'D9000A6B'                                           
         GOTO1 CALLOV,DMCB                                                      
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R4)                                                   
         MVC   PRDMPS,SVWORK       OUTPUT                                       
DR25U    OI    PRDMPSH+6,X'80'                                                  
         DROP  R4                                                               
*                                                                               
DR26     XC    PRDAOFF,PRDAOFF     PRD ACC OFFICE CODE                          
         L     R6,AIO                                                           
         MVI   ELCODE,X'35'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DR26U                                                            
         USING PPRDAOEL,R6                                                      
         MVC   PRDAOFF(L'PPRDAOFC),PPRDAOFC                                     
         OC    PPRDACCA,PPRDACCA   ACC OFFICE AGENCY CODE PRESENT?              
         BZ    DR26U                                                            
         MVC   WORK+0(2),PPRDAOFC                                               
         MVI   WORK+2,C'/'                                                      
         MVC   WORK+3(2),PPRDACCA                                               
         MVC   PRDAOFF,WORK                                                     
DR26U    OI    PRDAOFFH+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
         XC    PRDSAP,PRDSAP                                                    
         L     R6,AIO                                                           
         MVI   ELCODE,PPSAPELQ     X'51'                                        
         BRAS  RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   PRDSAP,2(R6)                                                     
         OI    PRDSAPH+6,X'80'                                                  
*                                                                               
DR28     BRAS  RE,D_BPCEDT         DISPLAY BILL ON PC EFFECTIVE DATE            
*                                                                               
DR30     BRAS  RE,D_OPTFLD         Dispay option fields                         
*                                                                               
DRX      BRAS  RE,CKPRDREC                                                      
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    DRX_10              NO NEED TO RESTORE ON ADD                    
         GOTO1 HIGH                RESTORE SEQ FOR GENCON                       
         MVC   DUB(L'AIO),AIO      AIO HAS UPDATED REC                          
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC              NEEDED FOR GENCON'S PUTREC                   
         MVC   AIO,DUB             RESTORE ORIGINAL AIO                         
         BRAS  RE,CKPRDREC                                                      
DRX_10   LA    R2,CONACTH          RESET FLD POINTER                            
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
* DISPLAYING HEX PRD OFFICE/TRAFFIC                                             
* BYTE = OFFICE OR TRAFFIC CODE                                                 
* R2   = OFFICE OR TRAFFIC HEX FLD                                              
*                                                                               
DR_OTHEX ST    RE,FULL                                                          
         CLI   BYTE,X'40'          SHOW HEX VALUE?                              
         BNH   DR_OTHX             NO                                           
         CLI   BYTE,X'D0'          SHOW HEX VALUE?                              
         BE    DR_OTH50            YES                                          
         CLI   BYTE,X'E0'          SHOW HEX VALUE?                              
         BE    DR_OTH50            YES                                          
         CLI   BYTE,C'A'           SHOW HEX VALUE?                              
         BL    DR_OTH50            YES                                          
         CLI   BYTE,C'9'           SHOW HEX VALUE?                              
         BNH   DR_OTHX             NO                                           
*                                                                               
DR_OTH50 GOTO1 HEXOUT,DMCB,BYTE,8(R2),1                                         
*                                                                               
DR_OTHX  L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
CKPRDREC LR    R0,RE                                                            
         L     RE,AIO                                                           
         CLI   3(RE),X'06'         Product record code?                         
         JE    *+6                                                              
         DC    H'0'                                                             
         CLI   33(RE),X'06'        First Product elem present?                  
         JE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
LR       DS    0H                  LIST RECORDS                                 
         GOTOR CLRLSSCR            CLEAR LIST SCREEN                            
*                                                                               
         XC    SVCLTLSC,SVCLTLSC                                                
         CLI   KEY+3,X'06'         STILL PRD RECORD?                            
         BNE   LR10                IF NOT, NEED TO WRAP AROUND AGAIN            
         OC    KEY,KEY                                                          
         BNZ   LR20_HI             CONTINUE LISTING                             
LR10     MVC   KEY,SVKEY           KEY BUILT FROM VK                            
*                                                                               
LR20_HI  GOTOR CKMAXIO             CK FOR MAX I/O                               
         BNE   LR94_LM                                                          
         GOTO1 HIGH                FIRST RECORD                                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     LR40                                                             
*                                                                               
LR30_SEQ GOTOR CKMAXIO             CK FOR MAX I/O                               
         BNE   LR94_LM                                                          
         GOTO1 SEQ                 NEXT RECORD                                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LR40     CLC   KEY(4),KEYSAVE      SAME AGY/MED/RECORD CODE?                    
         BNE   LRX                                                              
*                                                                               
         CLC   KEY+04(3),CLTLSFLT  CLT IS HIGHER THAN FILTER?                   
         BL    LR30_SEQ                                                         
         CLC   KEY+07(3),PRDLSFLT  PRD IS HIGHER THAN FILTER?                   
         BL    LR30_SEQ                                                         
*                                                                               
         XC    LISTAR,LISTAR       PREPARING LIST LINE                          
         MVC   SVWORK(L'KEY),KEY   SAVING PRD KEY                               
*                                                                               
* BY PASS RECS WITH LIMIT ACCESS (NOT TO LIST THEM)                             
*                                                                               
         MVI   ERROPT,C'Y'         RETURN TO APPL INSTEAD OF GENCON ERR         
         XC    WKTMPFLD,WKTMPFLD                                                
         MVI   WKTMPFLD+0,8+3      TOTAL LENGTH OF TEMP FLD                     
         MVI   WKTMPFLD+5,3        INPUT LENGTH OF TEMP FLD                     
         MVC   WKTMPFLD+8(3),KEY+4 CLT CODE TO BE VALIDATED FOR SEC             
         LA    R2,WKTMPFLD                                                      
         MVC   WKTMPAIO,AIO                                                     
         MVC   WKTMPION,USEIONUM                                                
         MVI   USEIONUM,3          VALICLT WILL USE AIO3                        
         XC    CLTNM,CLTNM         IT WILL BE SET IF CLT IS OK                  
         GOTO1 VALICLT                                                          
*                                                                               
         OC    CLTNM,CLTNM         CLT CODE PASS SECURITY?                      
         BNZ   LR42                                                             
         MVC   KEY,SVWORK          RESTORE PRD KEY                              
         SR    RE,RE                                                            
         ICM   RE,7,KEY+4          CLIENT CODE                                  
         AHI   RE,1                WILL BUMP TO NEXT CLIENT CODE                
         STCM  RE,7,KEY+4                                                       
         MVC   AIO,WKTMPAIO                                                     
         MVC   USEIONUM,WKTMPION                                                
         B     LR20_HI             TRY NEXT CLIENT                              
*                                                                               
LR42     MVC   KEY,SVWORK          RESTORE PRD KEY                              
         MVC   AIO,WKTMPAIO                                                     
         MVC   USEIONUM,WKTMPION                                                
         GOTO1 HIGH                RESTORE SEQUENCE FOR PRD READING             
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'06'        1ST PRD REC ELEM CODE                        
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                PRD REC IS NO GOOD!                          
         CLI   LISTNUM,0           1ST LIST LINE?                               
         BNE   *+10                                                             
         MVC   LSCLTC,KEY+04       1ST LIST LINE NEED TO HAVE CLT CODE          
         MVC   LSPRDC(L'LSPRDC),KEY+07                                          
         MVI   LSPRDC+L'LSPRDC,C'/'                                             
         USING PPRDELEM,R6                                                      
         MVC   LSPRD(L'PPRDNAME),PPRDNAME                                       
*                                                                               
* FILTERING ON FLITER FLDS                                                      
*                                                                               
         OC    F_FLDS(F_FLDLNQ),F_FLDS                                          
         BNZ   LR50                                                             
*                                                                               
* NO FITLER VALUE(S) FOUND, DISPLAY DEFAULT LIST DATA                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'35'        PRD ACC OFFICE CODE ELEM CODE                
         BRAS  RE,GETEL                                                         
         BNE   LR44                                                             
         USING PPRDAOEL,R6                                                      
         MVC   LSAOF,PPRDAOFC      PRD ACC OFFICE CODE                          
         OC    PPRDACCA,PPRDACCA                                                
         BZ    LR44                                                             
         MVI   LSAOFASP,C'/'       SEPARATOR                                    
         MVC   LSAGY,PPRDACCA      PRD ACC OFFICE AGY                           
         DROP  R6                                                               
*                                                                               
LR44     L     R6,AIO                                                           
         MVI   ELCODE,X'06'        1ST PRD REC ELEM CODE                        
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                PRD REC IS NO GOOD!                          
         USING PPRDELEM,R6                                                      
         MVC   LSOFC,PPRDOFFC      PRD OFFICE CODE                              
         MVC   LSTOF,PPRDTRAF      PRD TRAFFIC CODE                             
*                                                                               
         LA    R6,PPRDBILP                                                      
         USING BILPROF,R6                                                       
         MVI   DUB,02              DUB+01 WILL HAVE BIT INPUTS                  
         MVC   DUB+01(L'BILBASA),BILBASA                                        
         BRAS  RE,CK_BASEA         CK BASE FORMULA A                            
         BNE   LR90                                                             
         MVC   LSBASEA,DUB         FORMATTED FORMULA (5 CHARS)                  
         B     LR90                                                             
         DROP  R6                                                               
*                                                                               
LR50     DS    0H                  CKING FOR INDIVIDUAL FILTER(S)               
         USING PPRDELEM,R6                                                      
*                                                                               
         L     R0,AIO2             TO BUILD LISTING DATA                        
         LHI   R1,F_SMAXQ*L_DLNQ   LENGTH OF LISTING DATA TABLE                 
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         CLI   F_OFC+L'F_OFC,0                                                  
         BE    LR52                                                             
         SR    RE,RE                                                            
         IC    RE,F_OFC+L'F_OFC    SCANNER ENTRY POSITION NUMBER                
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),03           LENGTH OF TITLE AND DASHES                   
         MVC   01(03,RF),=C'OFC'                                                
         MVI   L_DLNQ/2+00(RF),03  DEFAULT LENGTH OF FLT DATA                   
         CLI   F_OFC,C' '          FLT KEYWORD ONLY?                            
         BE    LR51P                                                            
         CLI   F_OFC,C'*'          WILDCARD?                                    
         BNE   LR51H                                                            
         OC    PPRDOFFC,PPRDOFFC   ANYTHING IN OFFICE CODE?                     
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
         B     LR51P                                                            
LR51H    CLC   F_OFC,PPRDOFFC      MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
LR51P    SR    RE,RE                                                            
         IC    RE,F_OFC+L'F_OFC    SCANNER ENTRY POSITION NUMBER                
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),L'PPRDOFFC                                       
         MVC   L_DLNQ/2+01(L'PPRDOFFC,RF),PPRDOFFC                              
*                                                                               
LR52     CLI   F_TOF+L'F_TOF,0                                                  
         BE    LR53                                                             
         SR    RE,RE                                                            
         IC    RE,F_TOF+L'F_TOF    SCANNER ENTRY POSITION NUMBER                
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),03           LENGTH OF TITLE AND DASHES                   
         MVC   01(03,RF),=C'TOF'                                                
         MVI   L_DLNQ/2+00(RF),03  DEFAULT LENGTH OF FLT DATA                   
         CLI   F_TOF,C' '          FLT KEYWORD ONLY?                            
         BE    LR52P                                                            
         CLI   F_TOF,C'*'          WILDCARD?                                    
         BNE   LR52H                                                            
         OC    PPRDTRAF,PPRDTRAF   ANYTHING IN TRAFFIC OFFICE CODE?             
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
         B     LR52P                                                            
LR52H    CLC   F_TOF,PPRDTRAF      MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
LR52P    SR    RE,RE                                                            
         IC    RE,F_TOF+L'F_TOF    SCANNER ENTRY POSITION NUMBER                
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),L'PPRDTRAF                                       
         MVC   L_DLNQ/2+01(L'PPRDTRAF,RF),PPRDTRAF                              
*                                                                               
LR53     CLI   F_NOTRAF+L'F_NOTRAF,0                                            
         BE    LR54                                                             
         SR    RE,RE                                                            
         IC    RE,F_NOTRAF+L'F_NOTRAF                                           
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),08           LENGTH OF TITLE AND DASHES                   
         MVC   01(08,RF),=C'Traffic?'                                           
         MVI   L_DLNQ/2+00(RF),08  DEFAULT LENGTH OF FLT DATA                   
         CLI   F_NOTR_W,C'*'       WILDCARD?                                    
         BE    LR53D                                                            
         CLI   F_NOTRAF,0          FLT KEYWORD ONLY?                            
         BE    LR53P                                                            
         B     LR53H                                                            
LR53D    TM    PPRDSTAT,X'20'      NO TRAFFIC BIT IS ON?                        
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
         B     LR53P                                                            
LR53H    MVC   BYTE,PPRDSTAT                                                    
         NI    BYTE,X'FF'-X'20'                                                 
         CLC   F_NOTRAF,BYTE       MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
LR53P    SR    RE,RE                                                            
         IC    RE,F_NOTRAF+L'F_NOTRAF                                           
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),01                                               
         MVI   L_DLNQ/2+01(RF),C'N'                                             
         TM    PPRDSTAT,X'20'                                                   
         BZ    *+8                                                              
         MVI   L_DLNQ/2+01(RF),C'Y'                                             
*                                                                               
LR54     CLI   F_BASEA+L'F_BASEA,0                                              
         BE    LR55                                                             
         SR    RE,RE                                                            
         IC    RE,F_BASEA+L'F_BASEA                                             
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),05           LENGTH OF TITLE AND DASHES                   
         MVC   01(08,RF),=C'BaseA'                                              
         MVI   L_DLNQ/2+00(RF),05  DEFAULT LENGTH OF FLT DATA                   
         LA    RE,PPRDBILP                                                      
         USING BILPROF,RE                                                       
         MVC   BYTE,BILBASA                                                     
         XC    DUB,DUB             FOR FORMATTING BASE A FORMULA                
         DROP  RE                                                               
         CLI   F_BASA_W,C'*'       WILDCARD?                                    
         BE    LR54E                                                            
         CLI   F_BASEA,0           FLT KEYWORD ONLY?                            
         BE    LR54K                                                            
         B     LR54H                                                            
LR54E    MVI   DUB,02              DUB+01 WILL HAVE BIT INPUTS                  
         MVC   DUB+01(01),BYTE     BIT FORMAT OF BASE A                         
         BRAS  RE,CK_BASEA         CK BASE FORMULA A                            
         BNE   LR30_SEQ            NOTHING, SKIP IT                             
         B     LR54P               DUB WILL HAVE FORMMATED BASE A               
LR54H    CLC   BYTE,F_BASEA        MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
LR54K    MVI   DUB,02              DUB+01 WILL HAVE BIT INPUTS                  
         MVC   DUB+01(01),BYTE     BIT FORMAT OF BASE A                         
         BRAS  RE,CK_BASEA         CK BASE FORMULA A                            
LR54P    SR    RE,RE                                                            
         IC    RE,F_BASEA+L'F_BASEA                                             
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),05                                               
         MVC   L_DLNQ/2+01(05,RF),DUB                                           
*                                                                               
LR55     CLI   F_OAN+L'F_OAN,0                                                  
         BE    LR56                                                             
         SR    RE,RE                                                            
         IC    RE,F_OAN+L'F_OAN    SCANNER ENTRY POSITION NUMBER                
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),03           LENGTH OF TITLE AND DASHES                   
         MVC   01(03,RF),=C'OAN'                                                
         MVI   L_DLNQ/2+00(RF),03  DEFAULT LENGTH OF FLT DATA                   
         CLI   F_OAN,C' '          FLT KEYWORD ONLY?                            
         BE    LR55P                                                            
         CLI   F_OAN,C'*'          WILDCARD?                                    
         BNE   LR55H                                                            
         OC    PPRDOAN,PPRDOAN     ANYTHING IN OTHER AGY NAME?                  
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
         B     LR55P                                                            
LR55H    CLC   F_OAN,PPRDOAN       MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
LR55P    SR    RE,RE                                                            
         IC    RE,F_OAN+L'F_OAN    SCANNER ENTRY POSITION NUMBER                
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),L'PPRDOAN                                        
         MVC   L_DLNQ/2+01(L'PPRDOAN,RF),PPRDOAN                                
*                                                                               
LR56     CLI   F_EXCL+L'F_EXCL,0                                                
         BE    LR57                                                             
         SR    RE,RE                                                            
         IC    RE,F_EXCL+L'F_EXCL  SCANNER ENTRY POSITION NUMBER                
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),10           LENGTH OF TITLE AND DASHES                   
         MVC   01(10,RF),=C'Excl Class'                                         
         MVI   L_DLNQ/2+00(RF),10  DEFAULT LENGTH OF FLT DATA                   
         XC    WORK,WORK           OUTPUT BLK FOR EXCL CLASS DISPLAY            
         CLI   F_EXCL_W,C'*'       WILDCARD?                                    
         BE    LR56C                                                            
         CLI   F_EXCL,0            FLT KEYWORD ONLY?                            
         BE    LR56E                                                            
         B     LR56H                                                            
LR56C    OC    PPRDEXC,PPRDEXC     ANYTHING IN EXCLUSION CLASS?                 
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
LR56E    LA    R1,WORK                                                          
         MVC   BYTE,PPRDEXC                                                     
         MVI   DUB,C'F'            FORMAT CALL                                  
         BRAS  RE,CK_EXCL          FORMAT EXCLUSION CLASS DISPLAY               
         B     LR56P                                                            
LR56H    CLC   PPRDEXC,F_EXCL      MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
         B     LR56E               YES, FORMAT IT                               
LR56P    SR    RE,RE                                                            
         IC    RE,F_EXCL+L'F_EXCL  SCANNER ENTRY POSITION NUMBER                
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),10                                               
         MVC   L_DLNQ/2+01(10,RF),WORK                                          
*                                                                               
LR57     CLI   F_GST+L'F_GST,0                                                  
         BE    LR58                                                             
         SR    RE,RE                                                            
         IC    RE,F_GST+L'F_GST    SCANNER ENTRY POSITION NUMBER                
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),03           LENGTH OF TITLE AND DASHES                   
         MVC   01(03,RF),=C'GST'                                                
         MVI   L_DLNQ/2+00(RF),03  DEFAULT LENGTH OF FLT DATA                   
         CLI   F_GST,C' '          FLT KEYWORD ONLY?                            
         BE    LR57P                                                            
         CLI   F_GST,C'*'          WILDCARD?                                    
         BNE   LR57H                                                            
         OC    PPRDGST,PPRDGST     ANYTHING IN GST CODE?                        
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
         B     LR57P                                                            
LR57H    CLC   F_GST,PPRDGST       MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
LR57P    SR    RE,RE                                                            
         IC    RE,F_GST+L'F_GST    SCANNER ENTRY POSITION NUMBER                
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),L'PPRDGST                                        
         MVC   L_DLNQ/2+01(L'PPRDGST,RF),PPRDGST                                
*                                                                               
         DROP  R6                  MAIN PRD ELEM USING STOPS HERE!              
*                                                                               
LR58     CLI   F_PST+L'F_PST,0                                                  
         BE    LR59                                                             
         SR    RE,RE                                                            
         IC    RE,F_PST+L'F_PST    SCANNER ENTRY POSITION NUMBER                
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),L'PPRDPSTC   LENGTH OF TITLE AND DASHES                   
         MVC   01(03,RF),=C'PST'                                                
         MVI   L_DLNQ/2+00(RF),L'PPRDPSTC                                       
         L     R6,AIO                                                           
         MVI   ELCODE,X'25'        PRD PST ELEM CODE                            
         USING PPRDPST,R6                                                       
         CLI   F_PST,C' '          FLT KEYWORD ONLY?                            
         BNE   LR58F                                                            
         BRAS  RE,GETEL                                                         
         BE    *+8                                                              
         LA    R6,SPACES           SO SPACES WILL BE DISPLAYED                  
         B     LR58P                                                            
LR58F    L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   LR30_SEQ            DO NOT HAVE IT, NOTHING TO FILTER ON         
         CLI   F_PST,C'*'          WILDCARD?                                    
         BNE   LR58H                                                            
         OC    PPRDPSTC,PPRDPSTC   ANYTHING IN PST CODE?                        
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
         B     LR58P                                                            
LR58H    CLC   F_PST,PPRDPSTC      MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
LR58P    SR    RE,RE                                                            
         IC    RE,F_PST+L'F_PST    SCANNER ENTRY POSITION NUMBER                
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),L'PPRDPSTC                                       
         MVC   L_DLNQ/2+01(L'PPRDPSTC,RF),PPRDPSTC                              
         DROP  R6                                                               
*                                                                               
LR59     CLI   F_AOF+L'F_AOF,0                                                  
         BE    LR60                                                             
         SR    RE,RE                                                            
         IC    RE,F_AOF+L'F_AOF    SCANNER ENTRY POSITION NUMBER                
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),03           LENGTH OF TITLE AND DASHES                   
         MVC   01(03,RF),=C'AOF'                                                
         MVI   L_DLNQ/2+00(RF),03  DEFAULT LENGTH OF FLT DATA                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'35'        PRD ACC OFF/AGY ELEM CODE                    
         USING PPRDAOEL,R6                                                      
         CLI   F_AOF,C' '          FLT KEYWORD ONLY?                            
         BNE   LR59F                                                            
         BRAS  RE,GETEL                                                         
         BE    *+8                                                              
         LA    R6,SPACES           SO SPACES WILL BE DISPLAYED                  
         B     LR59P                                                            
LR59F    L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   LR30_SEQ            DO NOT HAVE IT, NOTHING TO FILTER ON         
         CLI   F_AOF,C'*'          WILDCARD?                                    
         BNE   LR59H                                                            
         OC    PPRDAOFC,PPRDAOFC   ANYTHING IN PRD ACC OFFICE CODE?             
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
         OC    PPRDAOFC,SPACES     FOR MIXES OF NULL & SPACE (NO CHGS!)         
         CLC   PPRDAOFC,SPACES                                                  
         BE    LR30_SEQ            SPACES, SKIP IT TOO                          
         B     LR59P                                                            
LR59H    CLC   F_AOF,PPRDAOFC      MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
LR59P    SR    RE,RE                                                            
         IC    RE,F_AOF+L'F_AOF    SCANNER ENTRY POSITION NUMBER                
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),L'PPRDAOFC                                       
         MVC   L_DLNQ/2+01(L'PPRDAOFC,RF),PPRDAOFC                              
         DROP  R6                                                               
*                                                                               
LR60     CLI   F_AGY+L'F_AGY,0                                                  
         BE    LR61                                                             
         SR    RE,RE                                                            
         IC    RE,F_AGY+L'F_AGY    SCANNER ENTRY POSITION NUMBER                
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),07           LENGTH OF TITLE AND DASHES                   
         MVC   01(07,RF),=C'AOF/Agy'                                            
         MVI   L_DLNQ/2+00(RF),L'PPRDAOFC+2+L'PPRDACCA                          
         L     R6,AIO                                                           
         MVI   ELCODE,X'35'        PRD ACC OFF/AGY ELEM CODE                    
         USING PPRDAOEL,R6                                                      
         CLI   F_AGY,C' '          FLT KEYWORD ONLY?                            
         BNE   LR60F                                                            
         BRAS  RE,GETEL                                                         
         BE    *+8                                                              
         LA    R6,SPACES           SO SPACES WILL BE DISPLAYED                  
         B     LR60P                                                            
LR60F    L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   LR30_SEQ            DO NOT HAVE IT, NOTHING TO FILTER ON         
         CLI   F_AGY,C'*'          WILDCARD?                                    
         BNE   LR60H                                                            
         OC    PPRDACCA,PPRDACCA   ANYTHING IN PRD ACC OFFICE CODE?             
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
         B     LR60P                                                            
LR60H    CLC   F_AGY,PPRDACCA      MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
LR60P    SR    RE,RE                                                            
         IC    RE,F_AGY+L'F_AGY    SCANNER ENTRY POSITION NUMBER                
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),L'PPRDAOFC+2+L'PPRDACCA                          
         MVC   L_DLNQ/2+01(L'PPRDAOFC,RF),PPRDAOFC                              
         MVC   L_DLNQ/2+05(L'PPRDACCA,RF),PPRDACCA                              
         DROP  R6                                                               
*                                                                               
LR61     CLI   F_ADJCDS+L'F_ADJCDS,0                                            
         BE    LR62                                                             
         SR    RE,RE                                                            
         IC    RE,F_ADJCDS+L'F_ADJCDS                                           
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),04           LENGTH OF TITLE AND DASHES                   
         MVC   01(04,RF),=C'AdjC'                                               
         MVI   L_DLNQ/2+00(RF),4   DEFAULT LENGTH OF FLT DATA                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'06'        MAIN PRD ELEM CODE                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE THERE                              
         USING PPRDELEM,R6                                                      
         CLI   F_ADJCDS,C' '       FLT KEYWORD ONLY?                            
         BE    LR61P                                                            
         CLI   F_ADJCDS,C'*'       WILDCARD?                                    
         BNE   LR61H                                                            
         OC    PPRDEXCL,PPRDEXCL   ANYTHING IN PRD ADJACENCY CODE?              
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
         B     LR61P                                                            
LR61H    CLC   F_ADJCDS,PPRDEXCL   MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
LR61P    SR    RE,RE                                                            
         IC    RE,F_ADJCDS+L'F_ADJCDS                                           
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),L'PPRDEXCL                                       
         MVC   L_DLNQ/2+01(L'PPRDEXCL,RF),PPRDEXCL                              
         DROP  R6                                                               
*                                                                               
LR62     CLI   F_DIV+L'F_DIV,0                                                  
         BE    LR63                                                             
         SR    RE,RE                                                            
         IC    RE,F_DIV+L'F_DIV                                                 
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),L'PPRDDIV    LENGTH OF TITLE AND DASHES                   
         MVC   01(03,RF),=C'Div'                                                
         MVI   L_DLNQ/2+00(RF),3   DEFAULT LENGTH OF FLT DATA                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'06'        MAIN PRD ELEM CODE                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE THERE                              
         USING PPRDELEM,R6                                                      
         CLI   F_DIV,C' '          FLT KEYWORD ONLY?                            
         BE    LR62P                                                            
         CLI   F_DIV,C'*'          WILDCARD?                                    
         BNE   LR62H                                                            
         OC    PPRDDIV,PPRDDIV     ANYTHING IN DIVISION CODE?                   
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
         B     LR62P                                                            
LR62H    CLC   F_DIV,PPRDDIV       MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
LR62P    SR    RE,RE                                                            
         IC    RE,F_DIV+L'F_DIV                                                 
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),L'PPRDDIV                                        
         MVC   L_DLNQ/2+01(L'PPRDDIV,RF),PPRDDIV                                
         DROP  R6                                                               
*                                                                               
LR63     CLI   F_BILLF+L'F_BILLF,0                                              
         BE    LR64                                                             
         SR    RE,RE                                                            
         IC    RE,F_BILLF+L'F_BILLF                                             
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),20           LENGTH OF TITLE AND DASHES                   
         MVC   01(12,RF),=C'Bill Formula'                                       
         MVI   L_DLNQ/2+00(RF),20  DEFAULT LENGTH OF FLT DATA                   
*                                                                               
         BRAS  RE,FMTBILLF         RESULT WILL BE IN ELEM                       
*                                                                               
         CLI   F_BILLF,C' '        FLT KEYWORD ONLY?                            
         BE    LR63P                                                            
         CLI   F_BILLF,C'*'        WILDCARD?                                    
         BNE   LR64                                                             
         CLC   ELEM(4),=C'NONE'                                                 
         BE    LR30_SEQ            NOTHING, SKIP THIS ONE                       
LR63P    SR    RE,RE                                                            
         IC    RE,F_BILLF+L'F_BILLF                                             
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),20                                               
         MVC   L_DLNQ/2+01(20,RF),ELEM                                          
*                                                                               
LR64     CLI   F_LEGALW+L'F_LEGALW,0                                            
         BE    LR65                                                             
         SR    RE,RE                                                            
         IC    RE,F_LEGALW+L'F_LEGALW                                           
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),11           LENGTH OF TITLE AND DASHES                   
         MVC   01(11,RF),=C'LegalWarngs'                                        
         MVI   L_DLNQ/2+00(RF),11  DEFAULT LENGTH OF FLT DATA                   
*                                                                               
         BRAS  RE,FMTLEGWG         RESULT WILL BE IN ELEM                       
*                                                                               
         CLI   F_LEGALW,C' '       FLT KEYWORD ONLY?                            
         BE    LR64P                                                            
         CLI   F_LEGALW,C'*'       WILDCARD?                                    
         BNE   LR65                                                             
         OC    ELEM,ELEM                                                        
         BZ    LR30_SEQ            NOTHING, SKIP THIS ONE                       
LR64P    SR    RE,RE                                                            
         IC    RE,F_LEGALW+L'F_LEGALW                                           
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),11                                               
         MVC   L_DLNQ/2+01(11,RF),ELEM                                          
*                                                                               
LR65     BRAS  RE,L_USRFLD         DO USER DEFINITION FLDS LIST                 
         BNE   LR30_SEQ            FAILED FILTERING, SKIP THIS ONE              
*                                                                               
LR66     DS    0H                  FOR FUTURE FILTER FLDS                       
*                                                                               
         LA    R2,LSFSTART         POINT TO LISTING AREA                        
         BRAS  RE,LR_DFLTF         DISPLAY FLT FLDS                             
*                                                                               
LR90     CLC   SVCLTLSC,KEY+04     CLT CODE ALREADY DISPLAYED?                  
         BE    *+16                YES, NO NEED TO DISPLAY IT AGAIN             
         MVC   SVCLTLSC,KEY+04     SET CLT CODE ON LIST                         
         MVC   LSCLTC,KEY+04       NO, DISPLAY IT ON LIST LINE                  
*                                                                               
         CLI   LISTNUM,0           1ST LIST LINE?                               
         BNE   LR94_LM                                                          
         MVC   LSCLTC,KEY+04       1ST LIST LINE NEED TO HAVE CLT CODE          
*                                                                               
LR94_LM  GOTO1 LISTMON             DISPLAY LINE                                 
*                                                                               
         GOTOR SET_LSER            SET ERROR FOR LIST                           
*                                                                               
         CLI   MODE,PRINTREP       REPORTING USING LIST ACTION?                 
         BNE   LR98                                                             
         MVC   H5+00(07),=C'Filters'                                            
         MVC   H5+08(L'LSTFLT),LSTFLT                                           
         MVC   H7+05(L'LSTTL2),LSTTL2                                           
         MVC   H8+05(L'LSTUL2),LSTUL2                                           
         MVC   H7+05+L'LSTTL2+02(L'LSTTL3),LSTTL3                               
         MVC   H8+05+L'LSTUL2+02(L'LSTUL3),LSTUL3                               
         XC    P1,P1                                                            
         MVC   P1+05(L'LISTAR),LISTAR                                           
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT LINE                                   
         TM    CNTLSW,C_MAXIOQ     MAX I/O REACHED?                             
         BZ    LR30_SEQ                                                         
         XC    P1,P1                                                            
         MVC   P1(L'CONHEAD),CONHEAD                                            
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT LINE                                   
*                                                                               
LR98     TM    CNTLSW,C_MAXIOQ     MAX I/O REACHED?                             
         BNZ   LRX                                                              
*                                                                               
         B     LR30_SEQ            GET NEXT RECORD                              
*                                                                               
LRX      DS    0H                                                               
         OC    F_FLDS(F_FLDLNQ),F_FLDS                                          
         BZ    LRX_10                                                           
         LA    R4,LSTTL3                                                        
         USING LS_TTL3,R4                                                       
         CLI   LTFSTART,0          TITLES DISPLAYED?                            
         BH    *+12                                                             
         LA    R2,WORK             POINT TO LISTING AREA (DUMMY USE)            
         BRAS  RE,LR_DFLTF         DISPLAY FLT FLDS                             
         OI    LSTTL3H+6,X'80'                                                  
         OI    LSTUL3H+6,X'80'     REDISPLAY TITLE LINES                        
         DROP  R4                                                               
*                                                                               
LRX_10   TM    CNTLSW,C_MAXIOQ     MAX I/O REACHED?                             
         BZ    EXIT                                                             
         LA    R2,LSTFLTH          PLACE CURSOR ON FILTER FLD                   
         B     TRAPERR2                                                         
*                                                                               
L_DLNQ   EQU   100                 DATA TITLE AND DATA ITSELF                   
L_DASHES DC    50C'-'                                                           
*                                                                               
* R2 POINTS TO DISPLAYING AREA, AIO2 HAS TABLE OF FLT VALUES                    
*                                                                               
LR_DFLTF ST    RE,FULL             SAVE RETURN ADDRESS                          
         LA    RE,LSTTL3                                                        
         USING LS_TTL3,RE                                                       
         LA    RE,LTFSTART         POINT TO TITLE START                         
         XC    0(LTFFLDQ,RE),0(RE)                                              
         DROP  RE                                                               
         LA    RF,LSTUL3                                                        
         USING LS_TTL3,RF                                                       
         LA    RF,LTFSTART         POINT TO TITLE (UNDERLINE) START             
         XC    0(LTFFLDQ,RF),0(RF)                                              
         DROP  RF                                                               
*                                                                               
         LR    R1,RE                                                            
         AHI   R1,LTFFLDQ-3        3 CHARS BEFORE END OF DISPLAYING FLD         
*                                                                               
         L     R3,AIO2             POINT TO TABLE OF LISTING DATA               
         SR    R4,R4               LOOP COUNTER                                 
*                                                                               
LR_DFF10 CHI   R4,F_SMAXQ          NUMBER OF SCANNER ENTRIES REACHED?           
         BNL   LR_DFFX                                                          
         OC    0(L_DLNQ,R3),0(R3)  ANYTHING TO BE LISTED?                       
         BNZ   LR_DFF20                                                         
         LA    R3,L_DLNQ(R3)       NEXT ENTRY IN LISTING DATA TABLE             
         AHI   R4,1                LOOP COUNTER UP BY ONE                       
         B     LR_DFF10                                                         
*                                                                               
LR_DFF20 SR    R5,R5                                                            
         IC    R5,0(R3)            LENGTH OF TITLE                              
         CHI   R5,0                                                             
         BH    *+6                                                              
         DC    H'0'                INCORRECT LENGTH!                            
*                                                                               
         AR    RE,R5                                                            
         CR    RE,R1               STILL HAVE ROOM TO DISPLAY?                  
         BNL   LR_DFF50                                                         
         SR    RE,R5                                                            
*                                                                               
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),1(R3)       TITLE                                        
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),L_DASHES    TITLE (UNDERLINED)                           
         AHI   R5,1+2              1 FROM EX AND 2 SPACES                       
         AR    RE,R5                                                            
         AR    RF,R5                                                            
         IC    R5,L_DLNQ/2(R3)     LENGTH OF LIST FILTERING DATA                
         CHI   R5,0                                                             
         BH    *+6                                                              
         DC    H'0'                INCORRECT LENGTH!                            
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),L_DLNQ/2+1(R3)                                           
         IC    R5,0(R3)            LENGTH OF TITLE                              
         AHI   R5,2                2 SPACES                                     
         AR    R2,R5               POINT TO NEXT LISTING DATA AREA              
         LA    R3,L_DLNQ(R3)       NEXT ENTRY IN LISTING DATA TABLE             
         AHI   R4,1                LOOP COUNTER UP BY ONE                       
         B     LR_DFF10                                                         
*                                                                               
LR_DFF50 SR    RE,R5                                                            
         MVC   0(03,RE),=C'...'    CANNOT DISPLAY ANYMORE DATA                  
         MVC   0(03,RF),=C'...'                                                 
         MVC   0(03,R2),=C'...'                                                 
*                                                                               
LR_DFFX  L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PR       DS    0H                  PRINT RECORDS                                
         B     LR                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PPTRS    DS    0H                  REC IS JUST ADDED OR CHANGED                 
         BRAS  RE,CKPPTRS          CK FOR PASSIVE POINTERS                      
*                                                                               
         BRAS  RE,PUTREQRC         TO REQUEST AUTO T/A REPORTS                  
         BE    PPTRS_X                                                          
         LHI   R2,65               CANNOT GENERATE REQ FOR T/A REPORT           
         BRAS  RE,GET_ITXT                                                      
         LA    R2,CONACTH                                                       
         J     TRAPERR2            ERROR OR COMPLETION MSG IS SET               
*                                                                               
PPTRS_X  L     RE,AIO                                                           
         MVC   KEY,0(RE)                                                        
         J     DR                  REDISPLAY RECORD                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
PACK     SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BCR   8,RE                EXIT ON ZERO LENGTH                          
         TM    4(R2),X'08'                                                      
         BCR   8,RE                OR NON NUMERIC                               
         BCTR  R1,R0                                                            
         EX    R1,VARPACK                                                       
         CVB   R0,DUB                                                           
         BR    RE                                                               
*                                                                               
VARPACK  PACK  DUB,8(0,R2)                                                      
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
NOTNUMER MVI   ERROR,003                                                        
         J     TRAPERR                                                          
*                                                                               
RECACERR MVI   ERROR,012           INVALID RECORD ACTION ERROR                  
R2CONACT LA    R2,CONACTH          POINT TO ACTION FLD                          
         J     TRAPERR                                                          
*                                                                               
RECNFERR MVI   ERROR,053           RECORD NOT FOUND                             
         J     TRAPERR                                                          
*                                                                               
INVCLERR MVI   ERROR,062           INVALID CLIENT                               
         J     TRAPERR                                                          
*                                                                               
INVDTERR MVI   ERROR,068           INVALID DATE                                 
         J     TRAPERR                                                          
*                                                                               
LONGERR  MVI   ERROR,071           INPUT IS TOO LONG                            
         J     TRAPERR                                                          
*                                                                               
CLTRQERR MVI   ERROR,085           SPECIFIC CLT REQUIRED (SECURITY)             
         J     TRAPERR                                                          
*                                                                               
PFKEYERR MVI   ERROR,088           INVALID PFKEY                                
         J     R2CONACT                                                         
*                                                                               
CLTACERR MVI   ERROR,089           CLIENT LIMIT ACCESS ERROR                    
         J     TRAPERR                                                          
*                                                                               
DIVNSERR MVI   ERROR,092           DIV NOT SETUP IN CLT PROFILE                 
         J     TRAPERR                                                          
*                                                                               
DIVNFERR MVI   ERROR,093           DIV RECORD NOT FOUND                         
         J     TRAPERR                                                          
*                                                                               
PTOFDERR MVI   ERROR,095           PRD ASSIGNED Y/N CANNOT SET TO 'N'           
         J     TRAPERR                                                          
*                                                                               
DUPEDERR MVI   ERROR,179           DUPLICATE ENTRIES ERROR MSG                  
         J     TRAPERR                                                          
*                                                                               
NOTAUTER LHI   R0,271              NOT AUTHORIZED TO CHANGE ERROR               
         B     GTTXTMSG                                                         
*                                                                               
GTTXTMSG XC    FULL,FULL                                                        
         MVI   FULL+1,25           SYSTEM                                       
         L     RF,FULL                                                          
         GOTOR GETTXT,DMCB+12,(R0),0,(C'E',DMCB),0,0,(RF)                       
         OI    CONHEADH+6,X'80'                                                 
         J     TRAPERR2                                                         
*                                                                               
SELMSG01 DC    C'Record displayed - hit Pf12 to return or next sel'             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DK       NTR1  BASE=*,LABEL=*      DISPLAY KEY                                  
*                                                                               
         L     R6,AIO                                                           
         CLI   3(R6),X'06'         PRD RECORD CODE?                             
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PPRDKEY,R6                                                       
         MVC   PRDMED,PPRDKMED                                                  
         MVC   PRDCLT,PPRDKCLT                                                  
         MVC   PRDPRD,PPRDKPRD                                                  
         DROP  R6                                                               
*                                                                               
         MVI   USEIONUM,2          USE AIO2 FOR MED/CLT/PRD/EST RECS            
*                                                                               
         OI    PRDMEDH+6,X'80'     TRANSMIT MEDIA CODE                          
         XC    PRDMEDN,PRDMEDN                                                  
         OI    PRDMEDNH+6,X'80'    CLEARED MEDIA NAME                           
         MVI   PRDMEDH+5,1         INPUT LENGTH                                 
         LA    R2,PRDMEDH                                                       
         GOTO1 VALIMED                                                          
         MVC   PRDMEDN,MEDNM                                                    
         OI    PRDMEDNH+6,X'80'    TRANSMIT MEDIA NAME                          
*                                                                               
         OI    PRDCLTH+6,X'80'     TRANSMIT CLIENT CODE                         
         XC    PRDCLTN,PRDCLTN                                                  
         OI    PRDCLTNH+6,X'80'    CLEARED CLIENT NAME                          
         MVI   PRDCLTH+5,3         INPUT LENGTH                                 
         LA    R2,PRDCLTH                                                       
         GOTO1 VALICLT                                                          
         MVC   PRDCLTN,CLTNM                                                    
         OI    PRDCLTNH+6,X'80'    TRANSMIT CLIENT NAME                         
*                                                                               
         OI    PRDPRDH+6,X'80'     TRANSMIT PRD CODE                            
         XC    PRDPRDN,PRDPRDN                                                  
         OI    PRDPRDNH+6,X'80'    CLEARED PRD NAME                             
         MVI   PRDPRDH+5,3         INPUT LENGTH                                 
         LA    R2,PRDPRDH                                                       
         GOTO1 VALIPRD                                                          
         MVC   PRDPRDN,PRDNM                                                    
         OI    PRDPRDNH+6,X'80'    TRANSMIT PRD NAME                            
*                                                                               
DKX      DS    0H                                                               
         MVC   AIO,AIO1            AIO1 HAS REC TO BE DISPLAYED                 
         MVI   USEIONUM,1          RESET TO AIO1                                
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VAL_PST  NTR1 BASE=*,LABEL=*       Validate PST codes                           
*                                                                               
         TM    1(R2),X'20'         PST FLD IS PROTECTED?                        
         JO    V_PST_X             YES, NO VALIDATION IS NEEDED                 
*                                                                               
         GOTO1 REMELEM             REMOVE X'25' ELEM (PST ELEM)                 
         CLI   5(R2),0                                                          
         JE    V_PST_X                                                          
*                                                                               
         XC    SVWORK,SVWORK       Init interface block                         
         XC    ELEM,ELEM           Init PST element                             
         LA    R6,SVWORK                                                        
         USING PSTBLKD,R6                                                       
         MVI   PSTACT,PSTVALQ      ACTION = VALIDATE                            
         LA    R1,0(R2)                                                         
         ST    R1,PSTADIN          INPUT ADDRESS                                
         LA    R1,ELEM+02                                                       
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,ACOMFACS                                                 
         XC    DMCB(12),DMCB       GET PST ADDRESS                              
         MVC   DMCB+4(4),=X'D9000A6B'                                           
         GOTO1 CALLOV,DMCB                                                      
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R6)                                                   
         CLI   PSTERR,0                                                         
         JNE   V_PST_ER                                                         
         DROP  R6                                                               
*                                                                               
         CLI   ELCODE,X'25'        PST?                                         
         JNE   V_PST20                                                          
         CLI   ELEM+2+0,C' '       Have tax code in British Columbia?           
         JH    V_PST_ER                                                         
         CLI   ELEM+2+4,C' '       Have tax code in Ontario?                    
         JH    V_PST_ER                                                         
*                                                                               
V_PST20  CLI   ELCODE,X'25'        PST elem?                                    
         JNE   V_PST30                                                          
         MVI   ELEM+00,X'25'                                                    
         MVI   ELEM+01,2+10                                                     
         J     V_PST40                                                          
*                                                                               
V_PST30  CLI   ELCODE,PPRDMPEQ     Main PST elem?                               
         JNE   V_PST38                                                          
         MVI   ELEM+00,PPRDMPEQ                                                 
         MVI   ELEM+01,PPRDMPLQ                                                 
         J     V_PST40                                                          
*                                                                               
V_PST38  DC    H'0'                Invalid element code                         
*                                                                               
V_PST40  GOTO1 ADDELEM                                                          
*                                                                               
V_PST_X  J     SETCCEQ                                                          
V_PST_ER J     SETCCNEQ                                                         
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BPCEFFDT NTR1 BASE=*,LABEL=*       BILL ON PLANNED COST EFFECTIVE DATE          
*                                                                               
* * * *  XC    WORK,WORK           GET B2B PROFILE                              
* * * *  XC    SVWORK,SVWORK                                                    
* * * *  MVC   WORK(4),=C'PB2B'                                                 
* * * *  NI    WORK,X'BF'          LOWER CASE                                   
* * * *  MVC   WORK+4(2),AGENCY                                                 
* * * *  MVC   WORK+6(1),QMED                                                   
* * * *  MVC   WORK+7(3),QCLT                                                   
* * * *  CLI   SVCPROF+30,C' '                                                  
* * * *  BNH   *+14                                                             
* * * *  MVI   WORK+10,C'*'                                                     
* * * *  MVC   WORK+11(1),SVCPROF+30                                            
* * * *  GOTO1 GETPROF,DMCB,WORK,SVWORK,DATAMGR                                 
*                                                                               
         XC    SVWORK,SVWORK                                                    
         LA    R2,PRDPCEDH                                                      
         MVI   ELCODE,PPBPCECQ                                                  
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   BPCED10                                                          
         ZIC   RE,1(R6)            SAVE OLD ELEM                                
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SVWORK(0),0(R6)                                                  
         B     BPCED20                                                          
                                                                                
BPCED10  CLI   5(R2),0             ANY INPUTS?                                  
         BNH   BPCED_X                                                          
         B     BPCED30                                                          
*                                                                               
BPCED20  GOTO1 REMELEM             REMOVE BILL ON PC EFF. DATE ELEM             
         CLI   5(R2),0             ANY INPUTS?                                  
         BH    BPCED30                                                          
         XC    FULL,FULL           SET YEAR AND MONTH TO X'0000'                
         B     BPCED60                                                          
*                                                                               
BPCED30  GOTO1 PERVAL,DMCB,(5(R2),8(R2)),(X'40',ELEM)                           
         CLI   DMCB+4,X'01'                                                     
         BE    BPCED_E4                                                         
         USING PERVALD,R6                                                       
         LA    R6,ELEM                                                          
         CLI   PVALBSTA+2,1        FIRST DAY OF MONTH?                          
         BH    BPCED_E5                                                         
         MVC   FULL(3),PVALBSTA                                                 
*                                                                               
BPCED60  XC    ELEM,ELEM                                                        
         USING PPRDBPCE,R6                                                      
         LA    R6,ELEM                                                          
         MVI   PPBPCELC,PPBPCECQ                                                
         MVI   PPBPCELN,PPBPCELQ                                                
         MVC   PPBPCEFF,FULL       BINARY YEAR & MONTH                          
         BRAS  RE,GET_BPID                                                      
         MVC   PPBPCPID,SV_B_PID                                                
         MVC   PPBPCCHG,BTODAY                                                  
*                                                                               
         OC    SVWORK,SVWORK       HAVE OLD ELEM?                               
         BZ    BPCED70                                                          
         CLC   PPBPCEFF,SVWORK+(PPBPCEFF-PPRDBPCE)                              
         BNE   BPCED70                                                          
         MVC   ELEM,SVWORK         NOT A REAL CHANGE                            
*                                                                               
BPCED70  GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
BPCED_X  J     SETCCEQ                                                          
*                                                                               
BPCED_E3 LHI   R2,97               DATE IS OUTSIDE OF ESTIMATE RANGE            
         B     BPCED_EX                                                         
BPCED_E4 LHI   R2,193              INVALID DATE EXPRESSION                      
         B     BPCED_EX                                                         
BPCED_E5 LHI   R2,294              EFFECTIVE DATE FORMAT IS MMM/YY              
*                                                                               
BPCED_EX BRAS  RE,GET_ETXT                                                      
         LA    R2,PRDPCEDH         POINT TO BILL ON PLANNED COST FLD            
         J     SCCNEQ_R                                                         
*                                                                               
SCCEQ_R  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SCCNEQ_R LTR   RB,RB               NOT EQUAL                                    
X_R2     XIT1  REGS=(R2)           CURSOR POSITION                              
*                                                                               
GET_BPID LR    R0,RE                                                            
         XC    SV_B_PID,SV_B_PID   BINARY PID                                   
         L     RF,ACOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTOR (RF),DMCB,(2,0),0,0                                              
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   SVSECAGY,FATAGYSC   ALSO NEEDED TO GET CORRECT PID               
         TM    FATFLAG,X'08'       CHECK IF SECET CODE IS THERE                 
         JZ    *+10                                                             
         MVC   SV_B_PID,FAPASSWD   SAVE PASSWORD ID NUMBER                      
         LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R1                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
D_BPCEDT NTR1  BASE=*,LABEL=*      DISPLAY BILL ON PC EFFECTIVE DATE            
*                                                                               
         XC    PRDPCED,PRDPCED                                                  
         XC    PRDPCAC,PRDPCAC                                                  
         CLI   SECPCED,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    D_BPC_X                                                          
         L     R6,AIO                                                           
         MVI   ELCODE,PPBPCECQ                                                  
         BRAS  RE,GETEL                                                         
         BNE   D_BPC_X                                                          
         USING PPRDBPCE,R6                                                      
         OC    PPBPCEFF,PPBPCEFF   HAVE YEAR AND MONTH?                         
         BZ    D_BPC20                                                          
         MVC   DUB(L'PPBPCEFF),PPBPCEFF                                         
         MVI   DUB+L'PPBPCEFF,1                                                 
         GOTO1 DATCON,DMCB,(3,DUB),(6,PRDPCED)                                  
*                                                                               
D_BPC20  OC    PPBPCPID,PPBPCPID   HAVE PID?                                    
         BZ    D_BPC_X                                                          
         MVC   PRDPCAC(L'BPC_TXT1),BPC_TXT1                                     
         BRAS  RE,GET_BPID                                                      
         MVC   HALF,PPBPCPID                                                    
         BRAS  RE,GET_CPID                                                      
         OC    WORK,WORK                                                        
         BZ    D_BPC_X                                                          
         MVC   PRDPCAC+L'BPC_TXT1+1(L'SAPALPID),WORK                            
         OC    PPBPCCHG,PPBPCCHG                                                
         BZ    D_BPC_X                                                          
         LA    RF,PRDPCAC+(L'PRDPCAC-1)                                         
         BRAS  RE,LAST_CHR                                                      
         LR    R2,RF                                                            
         MVI   2(R2),C'('                                                       
         GOTO1 DATCON,DMCB,(3,PPBPCCHG),(10,3(R2))                              
         LA    RF,PRDPCAC+(L'PRDPCAC-1)                                         
         BRAS  RE,LAST_CHR                                                      
         MVI   1(RF),C')'                                                       
*                                                                               
D_BPC_X  OI    PRDPCEDH+6,X'80'                                                 
         OI    PRDPCACH+6,X'80'                                                 
         J     EXIT                                                             
*                                                                               
LAST_CHR CLI   0(RF),0                                                          
         JE    *+8                                                              
         CLI   0(RF),C' '                                                       
         JH    *+10                                                             
         BCTR  RF,0                                                             
         J     *-18                                                             
         BR    RE                                                               
*                                                                               
BPC_TXT1 DC    C'Last Chg:'                                                     
*                                                                               
         LTORG                                                                  
         DROP  RB,R6                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKOPTFLD NTR1  BASE=*,LABEL=*      Check option field                           
*                                                                               
         LA    R2,PRDOPTH                                                       
         CLI   5(R2),0             Any input?                                   
         JE    CKOPTFX                                                          
*                                                                               
         LA    R3,F_SMAXQ          Max number of scanner entries                
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),((R3),AIO3)                                    
         CLI   DMCB+4,0                                                         
         JE    CKOPT_E2            Scanner returned error                       
         XC    HALF,HALF                                                        
         MVC   HALF+1(1),DMCB+4    Number of entries                            
         LA    R5,1                Loop counter                                 
*                                                                               
         L     R4,AIO3                                                          
         USING SCANBLKD,R4                                                      
*                                                                               
CKOPTF20 CH    R5,HALF             ALL SCANNER FLDS PROCESSED?                  
         JH    CKOPTFX                                                          
*                                                                               
CKOPTF30 CLC   =C'BILL',SC1STFLD   BILL=NO?                                     
         JNE   CKOPTF40                                                         
         CLI   SC1STLEN,4                                                       
         JNE   CKOPT_E2                                                         
         CLI   SC2NDLEN,3                                                       
         JH    CKOPT_E2                                                         
         MVI   ELCODE,X'06'                                                     
         L     R6,AIO                                                           
         USING PPRDELEM,R6                                                      
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                Bad product record                           
         NI    PPRDSTAT,X'FF'-X'40'                                             
         CLC   SC2NDFLD(3),=C'YES'                                              
         JE    CKOPTF90                                                         
         CLC   SC2NDFLD(2),=C'NO'                                               
         JNE   CKOPT_E2                                                         
         OI    PPRDSTAT,X'40'      BILL=NO is enabled                           
         J     CKOPTF90                                                         
*                                                                               
CKOPTF40 DS    0H                  For additional options                       
*                                                                               
         J     CKOPT_E2                                                         
*                                                                               
CKOPTF90 LA    R4,SCBLKLQ(R4)      Point to next SCANNER block                  
         AHI   R5,1                Loop counter, bump up by 1                   
         J     CKOPTF20            Check for next SCANNER entry                 
*                                                                               
CKOPTFX  J     SETCCEQ                                                          
*                                                                               
CKOPT_E1 LHI   R2,179              Duplicate entries error                      
         J     CKOPTF_E                                                         
*                                                                               
CKOPT_E2 LHI   R2,191              Invalid option                               
         J     CKOPTF_E                                                         
*                                                                               
CKOPTF_E BRAS  RE,GET_ETXT                                                      
         LA    R2,PRDOPTH          Place cursor on option field                 
         J     SCCNEQ_R                                                         
*                                                                               
         LTORG                                                                  
         DROP  RB,R6,R4                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
D_OPTFLD NTR1  BASE=*,LABEL=*      Display option field                         
*                                                                               
         XC    PRDOPT,PRDOPT       Clear entire option line                     
         LA    R1,PRDOPT                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,X'06'                                                     
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                Bad product record                           
         USING PPRDELEM,R6                                                      
         TM    PPRDSTAT,X'40'      BILL=NO?                                     
         JZ    D_OPT20                                                          
         MVC   0(7,R1),=C'BILL=NO'                                              
         AHI   R1,7                                                             
*                                                                               
D_OPT20  DS    0H                  For additional options                       
*                                                                               
         OI    PRDOPTH+6,X'80'     Display option line                          
*                                                                               
D_OPTFX  J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R6                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GET_CPID NTR1  BASE=*,LABEL=*      GET CHAR PID                                 
*                                                                               
         XC    WORK,WORK           INIT REPLY BLOCK                             
         L     RF,AIO3                                                          
         USING CT0REC,RF           ESTABLISH KEY AS PERSON AUTH REC             
         XC    CT0KEY,CT0KEY       INIT KEY                                     
         MVI   CT0KTYP,CT0KTEQU    SET RECORD TYPE                              
         MVC   CT0KNUM,HALF        SET PID                                      
         MVC   CT0KAGY,SVSECAGY                                                 
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',(RF),(RF)                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO3                                                          
         LA    RE,CT0DATA          POINT TO FIRST ELEMENT                       
         SR    R0,R0                                                            
         USING SAPALEL,RE                                                       
G_CPID02 CLI   SAPALEL,0           CHECK FOR END OF RECORD                      
         BE    G_CPID_X                                                         
         CLI   SAPALEL,SAPALELQ    MATCH ON ELEMENT CODE?                       
         BE    *+14                                                             
         IC    R0,SAPALLN          GET ELEMENT LENGTH                           
         AR    RE,R0               BUMP TO NEXT ELEMENT                         
         B     G_CPID02            GO FIND NEXT ELEMENT                         
*                                                                               
         MVC   WORK(L'SAPALPID),SAPALPID                                        
*                                                                               
G_CPID_X J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,RE,RF                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DEL_PRD  NTR1  BASE=*,LABEL=*      DELETING PRD RECORD (KEY IS PRD KEY)         
*                                                                               
         MVC   SVWORK(L'KEY),KEY                                                
         MVC   SVWORK+L'KEY(L'AIO),AIO                                          
         MVC   AIO,AIO2                                                         
*                                                                               
         LA    R4,TABRTYP1         TABLE OF REC TYPES TO CHECK                  
DEL_P10  CLI   0(R4),0             END OF TABLE?                                
         BE    DEL_P30                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(10),SVWORK                                                   
         MVC   KEY+3(1),0(R4)                                                   
         GOTO1 HIGH                                                             
         B     DEL_P16                                                          
*                                                                               
DEL_P14  GOTO1 SEQ                                                              
DEL_P16  CLC   KEY(10),KEYSAVE                                                  
         BNE   DEL_P20                                                          
         CLI   KEY+3,X'08'         BILL W/ EST=0?                               
         BNE   DEL_PERR                                                         
         OC    KEY+10(2),KEY+10    OLD "ESTIMATE SUMMARY"?                      
         BZ    DEL_P14                                                          
         B     DEL_PERR                                                         
*                                                                               
DEL_P20  LA    R4,1(R4)            NEXT RECORD TYPE                             
         B     DEL_P10                                                          
*                                                                               
DEL_P30  LA    R4,TABRTYP2         TABLE OF REC TYPES TO DELETE                 
DEL_P34  CLI   0(R4),0             END OF TABLE?                                
         BE    DEL_P50                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(10),SVWORK                                                   
         MVC   KEY+3(1),0(R4)                                                   
         GOTO1 HIGH                                                             
         B     DEL_P38                                                          
*                                                                               
DEL_P36  GOTO1 SEQ                                                              
*                                                                               
DEL_P38  CLC   KEY(10),KEYSAVE                                                  
         BNE   DEL_P42                                                          
         CLI   KEY+3,X'08'         BILLING?                                     
         BNE   DEL_P40                                                          
         OC    KEY+10(2),KEY+10    WITH EST=0?                                  
         BZ    DEL_P40                                                          
*                                                                               
* SHOULD NOT HAPPEN, SINCE THIS BILLING RECORD SHOULD HAVE CAUSED               
* A DELETION ERROR IN EARLIER RECORD TYPES CHECKING CODES                       
*                                                                               
         DC    H'0'                                                             
*                                                                               
DEL_P40  OI    KEY+25,X'80'        DELETE POINTER                               
         GOTO1 WRITE                                                            
         B     DEL_P36                                                          
*                                                                               
DEL_P42  LA    R4,1(R4)            NEXT RECORD TYPE                             
         B     DEL_P34                                                          
*                                                                               
DEL_P50  XC    KEY,KEY                                                          
         MVC   KEY(7),SVWORK                                                    
         MVI   KEY+3,X'10'         CHK FOR PRD CONTRACTS                        
         GOTO1 HIGH                                                             
         B     DEL_P54                                                          
*                                                                               
DEL_P52  GOTO1 SEQ                                                              
*                                                                               
DEL_P54  CLC   KEY(7),KEYSAVE                                                   
         BNE   DEL_P60                                                          
*                                                                               
         GOTO1 GETREC                                                           
         TM    8(R1),X'40'         NON RECOVERABLE DISK ERRPR?                  
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIO                                                           
         USING PCONREC,R3                                                       
         CLC   PCONPRD,SVWORK+7    SAME PRD CODE?                               
         BNE   DEL_P52                                                          
         OI    PCONCTRL,X'80'                                                   
*                                                                               
         GOTO1 PUTREC              DELETE CONTRACT RECORD                       
         TM    8(R1),X'40'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OI    KEY+25,X'80'        DELETE DIRECTORY KEY                         
         GOTO1 WRITE                                                            
         B     DEL_P52                                                          
         DROP  R3                                                               
*                                                                               
DEL_P60  XC    KEY,KEY                                                          
         MVC   KEY(10),SVWORK                                                   
         MVI   KEY+3,X'3B'         CHK FOR PRD GROUP DIR REC                    
         GOTO1 HIGH                                                             
         B     DEL_P64                                                          
*                                                                               
DEL_P62  GOTO1 SEQ                                                              
*                                                                               
DEL_P64  CLC   KEY(10),KEYSAVE                                                  
         BNE   DEL_P70                                                          
         OI    KEY+25,X'80'        DELETE '3B' PRD GRP DIR REC                  
         GOTO1 WRITE                                                            
         B     DEL_P62                                                          
*                                                                               
DEL_P70  XC    KEY,KEY                                                          
         MVC   KEY(7),SVWORK                                                    
         MVI   KEY+3,X'3E'         CHK FOR PRD GROUP DIR REC                    
         GOTO1 HIGH                                                             
         B     DEL_P74                                                          
*                                                                               
DEL_P72  GOTO1 SEQ                                                              
*                                                                               
DEL_P74  CLC   KEY(7),KEYSAVE                                                   
         BNE   DEL_P80                                                          
         LA    R3,KEY                                                           
         USING GRPGKEY,R3                                                       
         CLC   SVWORK+7(3),GRPGVAL SAME PRD CODE?                               
         BNE   DEL_P72                                                          
         OI    GRPGCNTL,X'80'      DELETE '3E' PRD GRP DIR REC                  
         GOTO1 WRITE                                                            
         B     DEL_P72                                                          
         DROP  R3                                                               
*                                                                               
DEL_P80  DS    0H                  PASSIVE POINTER DELETIONS                    
         CLI   WKPOFFCD,0                                                       
         BE    *+18                NO PRD OFFICE PPOINTER TO DELETE             
         MVI   HALF+0,X'A3'                                                     
         MVC   HALF+1(1),WKPOFFCD                                               
         BAS   RE,DEL_PPTR         DELETE PRD OFFICE PASSIVE POINTER            
*                                                                               
         CLI   WKPTRACD,0                                                       
         BE    *+18                NO PRD TRAFFIC PPOINTER TO DELETE            
         MVI   HALF+0,X'A4'                                                     
         MVC   HALF+1(1),WKPTRACD                                               
         BAS   RE,DEL_PPTR         DELETE PRD TRAFFIC PASSIVE POINTER           
*                                                                               
* LAST STEP IS TO DELETE PRD RECORD                                             
*                                                                               
         MVC   KEY,SVWORK                                                       
         MVC   AIO,SVWORK+L'KEY                                                 
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R3,AIO                                                           
         USING PPRDREC,R3                                                       
         OI    PPRDCNTL,X'80'                                                   
         GOTO1 PUTREC                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(10),SVWORK                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                PRD DIRECTORY KEY MUST BE THERE!             
         OI    KEY+25,X'80'                                                     
         GOTO1 WRITE                                                            
         DROP  R3                                                               
*                                                                               
         LHI   R2,63               RECORD DELETED, ENTER NEXT REQUEST           
         BRAS  RE,GET_ITXT                                                      
*                                                                               
DEL_PX   J     SETCCEQ                                                          
*                                                                               
DEL_PERR LHI   R2,RECNDLER                                                      
         BRAS  RE,GET_ETXT         GET ERROR MSG                                
         J     SETCCNEQ                                                         
*                                                                               
GET_ITXT LR    R0,RE                                                            
         XC    FULL,FULL                                                        
         MVI   FULL+1,25           SYSTEM                                       
         L     RF,FULL                                                          
         GOTOR GETTXT,DMCB+12,(R2),0,(C'I',DMCB),0,0,(RF)                       
         OI    CONHEADH+6,X'80'                                                 
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
* COMMON ROUTINE TO DELETE PASSIVE POINTERS                                     
* HALF+0 = RECORD TYPE                                                          
* HALF+1 = OFFICE OR TRAFFIC CODE                                               
*                                                                               
DEL_PPTR ST    RE,FULL                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(3),SVWORK       AGENCY AND MEDIA                             
         MVC   KEY+3(1),HALF+0     PASSIVE POINTER REC CODE                     
         MVC   KEY+4(3),SVWORK+4   CLIENT                                       
         MVC   KEY+7(1),HALF+1     OFFICE OR TRAFFICE CODE                      
         MVC   KEY+8(3),SVWORK+7   PRODUCT                                      
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE     PASSIVE POINTER FOUND?                       
         BNE   DEL_PPTX            NO                                           
         OI    KEY+25,X'FF'        DELETE POINTER                               
         GOTO1 WRITE                                                            
DEL_PPTX L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
* TABLE OF RECORD TYPES NEED TO BE CHECKED BEFORE PRD DELETION                  
*                                                                               
TABRTYP1 DC    X'07'               EST                                          
         DC    X'08'               BILLING                                      
         DC    X'20'               BUY                                          
         DC    X'14'               AGY OF RECORD (AOR)                          
         DC    X'00'               END OF TABLE                                 
*                                                                               
* TABLE OF RECORD TYPES NEED TO BE DELETED ALONG WITH PRD DELETION              
*                                                                               
TABRTYP2 DC    X'08'               BILLS (NO EST)                               
         DC    X'15'               ADS                                          
         DC    X'18'               BUDGET                                       
         DC    X'0A'               PG ESTIMATE RECORD                           
         DC    X'0B'               GF ESTIMATE RECORD                           
         DC    X'00'               END OF TABLE                                 
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* FORMAT BILL FORMULA                                                           
* ELEM -  C'NONE' (IF NO FORMULA IS PRESENT)                                    
* ELEM -  CL20 FORMULA                                                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FMTBILLF NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   ELEM,SPACES                                                      
         MVC   ELEM(4),=C'NONE'                                                 
*                                                                               
         L     R6,AIO              AIO STILL POINTS TO PRD REC                  
         MVI   ELCODE,X'06'        MAIN PRD ELEM CODE                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE THERE                              
         USING PPRDELEM,R6                                                      
*                                                                               
         LA    R4,PPRDBILP                                                      
         USING BILPROF,R4                                                       
         CLI   BILCMSW,C'C'        SEE IF COMMISSION ONLY                       
         BNE   *+8                                                              
         OI    BILBASA,X'10'                                                    
         LA    R3,ELEM                                                          
         LA    R5,BILBASA                                                       
         BAS   RE,FMTBAS                                                        
         LA    R3,ELEM+10                                                       
         OC    BILADJ,BILADJ                                                    
         BNZ   FMTBF20                                                          
         CLI   BILBASA,0                                                        
         BE    FMTBF40                                                          
         MVC   ELEM+6(2),=C'+0'                                                 
         B     FMTBF40                                                          
*                                                                               
FMTBF20  LA    R3,ELEM+7                                                        
*                                                                               
         EDIT  (B3,BILADJ),(8,0(R3)),4,ALIGN=LEFT,DROP=2                        
         MVI   ELEM+6,C'+'                                                      
         TM    BILADJ,X'80'                                                     
         BZ    *+8                                                              
         MVI   ELEM+6,C'-'                                                      
         LA    R3,ELEM+14                                                       
         CLI   0(R3),C' '                                                       
         BH    FMTBF30                                                          
         BCT   R3,*-8                                                           
*                                                                               
FMTBF30  LA    R3,2(R3)                                                         
FMTBF40  LA    R5,BILBASB                                                       
         BAS   RE,FMTBAS                                                        
*                                                                               
         J     EXIT                                                             
*                                                                               
FMTBAS   ST    RE,FULL                                                          
         LA    RE,BASLST                                                        
         LA    RF,BASLSTN                                                       
FB2      CLC   0(1,R5),0(RE)                                                    
         BE    FB3                                                              
         LA    RE,6(RE)                                                         
         BCT   RF,FB2                                                           
         B     FBX                                                              
FB3      MVC   0(5,R3),1(RE)                                                    
FBX      L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
BASLST   DS    0C                                                               
         DC    X'01',C'G    '                                                   
         DC    X'02',C'N    '                                                   
         DC    X'05',C'G-CD '                                                   
         DC    X'06',C'N-CD '                                                   
         DC    X'08',C'AC   '                                                   
         DC    X'11',C'CG   '                                                   
         DC    X'12',C'CN   '                                                   
         DC    X'15',C'CG-CD'                                                   
         DC    X'16',C'CN-CD'                                                   
         DC    X'18',C'CAC  '                                                   
BASLSTN  EQU   (*-BASLST)/6        NUMBER IN LIST                               
*                                                                               
         LTORG                                                                  
         DROP  RB,R6,R4                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* FORMAT LEGAL WARNING ROTATION                                                 
* ELEM -  X'00'   (IF NO LEGAL WARNING DATA PRESENT)                            
* ELEM -  CL11 DATA                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FMTLEGWG NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    ELEM,ELEM                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,X'40'        LEGAL WARNING ELEM CODE                      
         BRAS  RE,GETEL                                                         
         JNE   SETCCNEQ            NOT FOUND                                    
*                                                                               
         USING PPRDLWEL,R6                                                      
         CLC   PPRDROTA,=C'ABCD'                                                
         BNE   *+10                                                             
         MVC   ELEM(11),=C'A1,B2,C3,D4'                                         
         CLC   PPRDROTA,=C'BCDA'                                                
         BNE   *+10                                                             
         MVC   ELEM(11),=C'B1,C2,D3,A4'                                         
         CLC   PPRDROTA,=C'CDAB'                                                
         BNE   *+10                                                             
         MVC   ELEM(11),=C'C1,D2,A3,B4'                                         
         CLC   PPRDROTA,=C'DABC'                                                
         BNE   *+10                                                             
         MVC   ELEM(11),=C'D1,A2,B3,C4'                                         
*                                                                               
         J     SETCCEQ                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB,R6                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* PUT OUT USER DEFINITION FLDS ON LIST LINE IF FLT KEYWORDS ARE PRESENT         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
L_USRFLD NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,FMTUDEFD         RESULT WILL BE IN ELEM                       
*                                                                               
         CLI   F_USER1+L'F_USER1,0                                              
         BE    L_UFS40                                                          
         SR    RE,RE                                                            
         IC    RE,F_USER1+L'F_USER1                                             
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),32           LENGTH OF TITLE AND DASHES                   
         MVC   01(16,RF),=C'User Def Field 1'                                   
         MVI   L_DLNQ/2+00(RF),32  DEFAULT LENGTH OF FLT DATA                   
*                                                                               
         CLI   F_USER1,C' '        FLT KEYWORD ONLY?                            
         BE    L_UFS20                                                          
         CLI   F_USER1,C'*'        WILDCARD?                                    
         BNE   L_UFS40                                                          
         OC    ELEM(32),ELEM                                                    
         JZ    SETCCNEQ            NOTHING, SKIP THIS ONE                       
L_UFS20  SR    RE,RE                                                            
         IC    RE,F_USER1+L'F_USER1                                             
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),32                                               
         MVC   L_DLNQ/2+01(32,RF),ELEM                                          
*                                                                               
L_UFS40  CLI   F_USER2+L'F_USER2,0                                              
         BE    L_UFS60                                                          
         SR    RE,RE                                                            
         IC    RE,F_USER2+L'F_USER2                                             
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),16           LENGTH OF TITLE AND DASHES                   
         MVC   01(16,RF),=C'User Def Field 2'                                   
         MVI   L_DLNQ/2+00(RF),16  DEFAULT LENGTH OF FLT DATA                   
*                                                                               
         CLI   F_USER2,C' '        FLT KEYWORD ONLY?                            
         BE    L_UFS42                                                          
         CLI   F_USER2,C'*'        WILDCARD?                                    
         BNE   L_UFS60                                                          
         OC    ELEM+32(16),ELEM+32                                              
         JZ    SETCCNEQ            NOTHING, SKIP THIS ONE                       
L_UFS42  SR    RE,RE                                                            
         IC    RE,F_USER2+L'F_USER2                                             
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),16                                               
         MVC   L_DLNQ/2+01(16,RF),ELEM+32                                       
*                                                                               
L_UFS60  DS    0H                  FOR FUTURE USES                              
*                                                                               
         J     SETCCEQ             EXIT WITH CC EQUAL                           
*                                                                               
FMTUDEFD DS   0H                                                                
         ST   RE,FULL                                                           
         XC   ELEM,ELEM                                                         
         L    R6,AIO                                                            
         MVI  ELCODE,X'08'         PRD USER DEF FLDS ELEM CODE                  
         BRAS RE,GETEL                                                          
         JNE  F_UFSX                                                            
         MVC  ELEM(50-2),2(R6)     USER1 AND USER2                              
F_UFSX   L    RE,FULL                                                           
         BR   RE                                                                
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* R1     = POINTS TO INPUT/OUTPUT AREA                                          
* BYTE   = EXCLUSION CLASS IN BIT FORMAT (NOT REQ'D FOR  C'V' CALLS)            
* DUB+00 = C'V' FOR VALIDATING, C'F' FOR FORMATTING                             
* DUB+01 = LENGTH OF INPUT FOR C'V' CALLS. IF NOT PRESENT, COMMAS ARE           
*          NOT PART OF INPUT (BUT INPUT MUST END WITH SPACES).                  
* DUB+00 = WILL RETURN EXCL CLASS BIT(S) FOR C'V' CALLS                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CK_EXCL  NTR1  BASE=*,LABEL=*      CKING FOR EXCLUSION CLASS                    
*                                                                               
         LA    R4,CKEXC_T1         POINT TO TRANSLATION TABLE                   
*                                                                               
         CLI   DUB,C'F'            FORMATTING EXCLUSION CLASS?                  
         BE    CKEXCL10                                                         
         CLI   DUB,C'V'            VALIDATING EXCLUSION CLASS?                  
         BE    CKEXCL50                                                         
         DC    H'0'                NO OTHER MODE AT THIS TIME                   
*                                                                               
CKEXCL10 CLI   0(R4),0             END OF TABLE?                                
         JE    SETCCEQ                                                          
         MVC   FULL(1),BYTE                                                     
         NC    BYTE,0(R4)          SET BIT TO BE COMPARED                       
         CLC   BYTE,FULL           EXCL CLASS BIT IS ON?                        
         BE    CKEXCL20            NO                                           
         MVC   0(1,R1),1(R4)       TRANSLATED BIT                               
         CLI   BYTE,0              ANY MORE EXCL CLASS BITS ON?                 
         JE    SETCCEQ             NO                                           
         MVI   1(R1),C','                                                       
         LA    R1,2(R1)            FOR NEXT EXCL CLASS CHAR                     
CKEXCL20 LA    R4,3(R4)            NEXT ENTRY IN TABLE                          
         CLI   BYTE,0              ANY MORE EXCL CLASS BITS ON?                 
         JE    SETCCEQ             NO                                           
         B     CKEXCL10                                                         
*                                                                               
CKEXCL50 SR    RE,RE                                                            
         IC    RE,DUB+01           INPUT LENGTH                                 
         XC    DUB,DUB             FOR RETURNING VALIDATED BIT(S)               
         CHI   RE,0                INPUT CONTAIN NO COMMAS?                     
         BE    CKEXCL60                                                         
*                                                                               
CKEXCL52 LA    R4,CKEXC_T1         POINT TO TRANSLATION TABLE                   
CKEXCL54 CLI   0(R4),0             END OF TABLE?                                
         JE    SETCCNEQ            ERROR, NOT IN TABLE                          
         CLC   0(1,R1),1(R4)                                                    
         BE    CKEXCL56                                                         
         LA    R4,3(R4)            NEXT ENTRY IN TABLE                          
         B     CKEXCL54                                                         
CKEXCL56 OC    DUB(1),2(R4)        TURN EXCLUSION CLASS BIT ON                  
         AHI   RE,-2                                                            
         LTR   RE,RE               MORE INPUT TO PROCESS?                       
         JNP   SETCCEQ                                                          
         CLI   1(R1),C','                                                       
         JNE   SETCCNEQ            INVALID FORMAT FOUND IN INPUT                
         LA    R1,2(R1)            NEXT EXCLUSION CLASS INPUT CHAR              
         B     CKEXCL52                                                         
*                                                                               
CKEXCL60 LA    R4,CKEXC_T1         POINT TO TRANSLATION TABLE                   
CKEXCL64 CLI   0(R4),0             END OF TABLE?                                
         JE    SETCCNEQ            ERROR, NOT IN TABLE                          
         CLC   0(1,R1),1(R4)                                                    
         BE    CKEXCL66                                                         
         LA    R4,3(R4)            NEXT ENTRY IN TABLE                          
         B     CKEXCL64                                                         
CKEXCL66 OC    DUB(1),2(R4)        TURN EXCLUSION CLASS BIT ON                  
         LA    R1,1(R1)            POINT TO NEXT EXCL CLASS INPUT CHAR          
         CLI   0(R1),C' '          NO MORE TO PROCESS?                          
         JE    SETCCEQ                                                          
         B     CKEXCL60                                                         
*                                                                               
* EXCULSION CLASS TABLE: 1ST BYTE = COMPLIMENT OF EXCL CLASS CODE               
*                        2ND BYTE = BIT TO CHAR TRANSLATION                     
*                        3RD BYTE = EXCL CLASS BIT EQUATES                      
*                                                                               
CKEXC_T1 DC    AL1(X'FF'-X'80'),C'B',X'80'                B = BEER              
         DC    AL1(X'FF'-X'40'),C'W',X'40'                W = WINE              
         DC    AL1(X'FF'-X'20'),C'L',X'20'                L = LIQUOR            
         DC    AL1(X'FF'-X'10'),C'T',X'10'                T = TOBACCO           
         DC    AL1(X'FF'-X'08'),C'C',X'08'                C = CIG               
         DC    X'00'                                      END OF TABLE          
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKFLTFLD NTR1  BASE=*,LABEL=*      CKING FOR FILTER KEYWORD(S)                  
*                                                                               
         LA    R3,F_SMAXQ          MAX NUM OF SCANNER ENTRIES                   
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),((R3),AIO3)                                    
         CLI   DMCB+4,0                                                         
         BE    CKFFD_E2            SCANNER RETURNED ERROR                       
         XC    HALF,HALF                                                        
         MVC   HALF+1(1),DMCB+4    NUMBER OF ENTRIES                            
         LA    R5,1                LOOP COUNTER                                 
*                                                                               
         L     R4,AIO3                                                          
         USING SCANBLKD,R4                                                      
*                                                                               
CKFFD20  CH    R5,HALF             ALL SCANNER FLDS PROCESSED?                  
         JH    SETCCEQ             YES, DONE (SET CC TO EQ)                     
*                                                                               
CKFFD30  CLC   =C'OFC',SC1STFLD                                                 
         BNE   CKFFD32                                                          
         CLI   SC1STLEN,3                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,L'F_OFC                                                 
         BH    CKFFD_E2                                                         
         MVC   F_OFC,SC2NDFLD      OFFICE CODE FLT VALUE                        
         CLI   F_OFC+L'F_OFC,0                                                  
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_OFC+L'F_OFC                                                 
         B     CKFFD90                                                          
*                                                                               
CKFFD32  CLC   =C'TOF',SC1STFLD                                                 
         BNE   CKFFD34                                                          
         CLI   SC1STLEN,3                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,L'F_TOF                                                 
         BH    CKFFD_E2                                                         
         MVC   F_TOF,SC2NDFLD      TRAFFIC OFFICE FLT VALUE                     
         CLI   F_TOF+L'F_TOF,0                                                  
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_TOF+L'F_TOF                                                 
         B     CKFFD90                                                          
*                                                                               
CKFFD34  CLC   =C'NOTRAFF',SC1STFLD                                             
         BNE   CKFFD36                                                          
         CLI   SC1STLEN,7                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,0          JUST FILTER?                                 
         BNH   CKFFD34M            NOTE: NULL(S), MEANS FLT ONLY                
         CLI   SC2NDLEN,3          YES OR Y?                                    
         BH    CKFFD_E2                                                         
         CLI   SC2NDFLD,C'*'       WILDCARD?                                    
         BNE   *+12                                                             
         MVI   F_NOTR_W,C'*'       IN THIS CASE, WILL DO SAME AS "Y"            
         B     CKFFD34M                                                         
         CLI   SC2NDFLD,C'Y'       YES OR Y?                                    
         BNE   *+8                                                              
         OI    F_NOTRAF,X'20'      BIT FOR NOTRAFF=Y                            
CKFFD34M CLI   F_NOTRAF+L'F_NOTRAF,0                                            
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_NOTRAF+L'F_NOTRAF                                           
         B     CKFFD90                                                          
*                                                                               
CKFFD36  CLC   =C'BASEA',SC1STFLD                                               
         BNE   CKFFD38                                                          
         CLI   SC1STLEN,5                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,0          JUST FILTER?                                 
         BNH   CKFFD36M            NOTE: NULL(S), MEANS FLT ONLY                
         CLI   SC2NDLEN,5                                                       
         BH    CKFFD_E2                                                         
         CLI   SC2NDFLD,C'*'       WILDCARD?                                    
         BNE   *+12                                                             
         MVI   F_BASA_W,C'*'                                                    
         B     CKFFD36M                                                         
         MVI   DUB,01              DUB+01 WILL HAVE CHAR INPUTS                 
         MVC   DUB+01(05),SC2NDFLD                                              
         BRAS  RE,CK_BASEA         CK BASE FORMULA A                            
         BNE   CKFFD_E2                                                         
         MVC   F_BASEA,DUB+05      GET BIT FORMAT OF BASE A                     
CKFFD36M CLI   F_BASEA+L'F_BASEA,0                                              
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_BASEA+L'F_BASEA                                             
         B     CKFFD90                                                          
*                                                                               
CKFFD38  CLC   =C'OAN',SC1STFLD                                                 
         BNE   CKFFD40                                                          
         CLI   SC1STLEN,3                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,L'F_OAN                                                 
         BH    CKFFD_E2                                                         
         MVC   F_OAN,SC2NDFLD      OTHER AGY NAME FLT VALUE                     
         CLI   F_OAN+L'F_OAN,0                                                  
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_OAN+L'F_OAN                                                 
         B     CKFFD90                                                          
*                                                                               
CKFFD40  CLC   =C'EXCL',SC1STFLD                                                
         BNE   CKFFD42                                                          
         CLI   SC1STLEN,4                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,0          JUST FILTER?                                 
         BNH   CKFFD40M            NOTE: NULL(S), MEANS FLT ONLY                
         CLI   SC2NDLEN,5                                                       
         BH    CKFFD_E2                                                         
         CLI   SC2NDFLD,C'*'       WILDCARD?                                    
         BNE   *+12                                                             
         MVI   F_EXCL_W,C'*'                                                    
         B     CKFFD40M                                                         
         LA    R1,SC2NDFLD         POINT TO INPUT FLD                           
         MVI   DUB,C'V'            VALIDATION CALL                              
         MVI   DUB+01,0            INPUT LENGTH (ZERO MEANS NO COMMAS)          
         BRAS  RE,CK_EXCL          EXCLUSION CLASS VALIDATION                   
         BNE   CKFFD_E2                                                         
         MVC   F_EXCL,DUB          BIT FORMAT OF EXCLUSION CLASS                
CKFFD40M CLI   F_EXCL+L'F_EXCL,0                                                
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_EXCL+L'F_EXCL                                               
         B     CKFFD90                                                          
*                                                                               
CKFFD42  CLC   =C'GST',SC1STFLD                                                 
         BNE   CKFFD44                                                          
         CLI   SC1STLEN,3                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,L'F_GST                                                 
         BH    CKFFD_E2                                                         
         MVC   F_GST,SC2NDFLD      GST FLT VALUE                                
         CLI   F_GST+L'F_GST,0                                                  
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_GST+L'F_GST                                                 
         B     CKFFD90                                                          
*                                                                               
CKFFD44  CLC   =C'PST',SC1STFLD                                                 
         BNE   CKFFD46                                                          
         CLI   SC1STLEN,3                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,L'F_PST                                                 
         BH    CKFFD_E3            INPUT IS TOO LONG                            
         MVC   F_PST,SC2NDFLD      PST FLT VALUE                                
         CLI   F_PST+L'F_PST,0                                                  
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_PST+L'F_PST                                                 
         B     CKFFD90                                                          
*                                                                               
CKFFD46  CLC   =C'AOF',SC1STFLD                                                 
         BNE   CKFFD48                                                          
         CLI   SC1STLEN,3                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,L'F_AOF                                                 
         BH    CKFFD_E2                                                         
         MVC   F_AOF,SC2NDFLD      ACC OFFICE CODE FLT VALUE                    
         CLI   F_AOF+L'F_AOF,0                                                  
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_AOF+L'F_AOF                                                 
         B     CKFFD90                                                          
*                                                                               
CKFFD48  CLC   =C'AGY',SC1STFLD                                                 
         BNE   CKFFD50                                                          
         CLI   SC1STLEN,3                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,L'F_AGY                                                 
         BH    CKFFD_E2                                                         
         MVC   F_AGY,SC2NDFLD      ACC OFFICE AGY FLT VALUE                     
         CLI   F_AGY+L'F_AGY,0                                                  
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_AGY+L'F_AGY                                                 
         B     CKFFD90                                                          
*                                                                               
CKFFD50  CLC   =C'ADJC',SC1STFLD                                                
         BNE   CKFFD52                                                          
         CLI   SC1STLEN,4                                                       
         BNE   CKFFD_E2                                                         
         MVC   F_ADJCDS,SPACES     INIT TO SPACES (KEYWORD ONLY)                
         CLI   SC2NDLEN,L'F_ADJCDS                                              
         BH    CKFFD_E2                                                         
         CLI   SC2NDLEN,0          NO FILTER INPUT?                             
         BNH   CKFFD50H                                                         
         XC    F_ADJCDS,F_ADJCDS   FOR TRAILING NULLS                           
         SR    RE,RE                                                            
         IC    RE,SC2NDLEN                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   F_ADJCDS(0),SC2NDFLD                                             
CKFFD50H CLI   F_ADJCDS+L'F_ADJCDS,0                                            
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_ADJCDS+L'F_ADJCDS                                           
         B     CKFFD90                                                          
*                                                                               
CKFFD52  CLC   =C'DIV',SC1STFLD                                                 
         BNE   CKFFD54                                                          
         CLI   SC1STLEN,3                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,L'F_DIV                                                 
         BH    CKFFD_E2                                                         
         MVC   F_DIV,SC2NDFLD      DIVISION CODE                                
         TM    SC2NDVAL,SCNUMQ     NUMERIC?                                     
         BZ    CKFFD52H                                                         
         EDIT  (B4,SC2NDNUM),(3,F_DIV),0,FILL=0,ALIGN=RIGHT                     
CKFFD52H CLI   F_DIV+L'F_DIV,0                                                  
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_DIV+L'F_DIV                                                 
         B     CKFFD90                                                          
*                                                                               
CKFFD54  CLC   =C'BILLF',SC1STFLD                                               
         BNE   CKFFD56                                                          
         CLI   SC1STLEN,5                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,L'F_BILLF                                               
         BH    CKFFD_E2                                                         
         MVC   F_BILLF,SPACES      INIT TO SPACES (KEYWORD ONLY)                
         CLI   SC2NDLEN,0          NO FILTER INPUT?                             
         BE    CKFFD54K                                                         
         CLI   SC2NDFLD,C'*'       ONLY ALLOWING WILDCARD                       
         BNE   CKFFD_E2                                                         
         MVC   F_BILLF,SC2NDFLD    BILL FORMULA FILTER                          
CKFFD54K CLI   F_BILLF+L'F_BILLF,0                                              
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_BILLF+L'F_BILLF                                             
         B     CKFFD90                                                          
*                                                                               
CKFFD56  CLC   =C'LEGALW',SC1STFLD                                              
         BNE   CKFFD58                                                          
         CLI   SECLGWR,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    CKFFD_E4                                                         
         CLI   SC1STLEN,6                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,L'F_LEGALW                                              
         BH    CKFFD_E2                                                         
         MVC   F_LEGALW,SPACES     INIT TO SPACES (KEYWORD ONLY)                
         CLI   SC2NDLEN,0          NO FILTER INPUT?                             
         BE    CKFFD56K                                                         
         CLI   SC2NDFLD,C'*'       ONLY ALLOWING WILDCARD                       
         BNE   CKFFD_E2                                                         
         MVC   F_LEGALW,SC2NDFLD   LEGAL WARNING ROTATION FILTER                
CKFFD56K CLI   F_LEGALW+L'F_LEGALW,0                                            
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_LEGALW+L'F_LEGALW                                           
         B     CKFFD90                                                          
*                                                                               
CKFFD58  CLC   =C'USER1',SC1STFLD                                               
         BNE   CKFFD60                                                          
         CLI   SECPUD1,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    CKFFD_E4                                                         
         CLI   SC1STLEN,5                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,L'F_USER1                                               
         BH    CKFFD_E2                                                         
         MVC   F_USER1,SPACES      INIT TO SPACES (KEYWORD ONLY)                
         CLI   SC2NDLEN,0          NO FILTER INPUT?                             
         BE    CKFFD58K                                                         
         CLI   SC2NDFLD,C'*'       ONLY ALLOWING WILDCARD                       
         BNE   CKFFD_E2                                                         
         MVC   F_USER1,SC2NDFLD    USER DEFINITION FLD 1 FILTER                 
CKFFD58K CLI   F_USER1+L'F_USER1,0                                              
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_USER1+L'F_USER1                                             
         B     CKFFD90                                                          
*                                                                               
CKFFD60  CLC   =C'USER2',SC1STFLD                                               
         BNE   CKFFD62                                                          
         CLI   SECPUD2,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    CKFFD_E4                                                         
         CLI   SC1STLEN,5                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,L'F_USER2                                               
         BH    CKFFD_E2                                                         
         MVC   F_USER2,SPACES      INIT TO SPACES (KEYWORD ONLY)                
         CLI   SC2NDLEN,0          NO FILTER INPUT?                             
         BE    CKFFD60K                                                         
         CLI   SC2NDFLD,C'*'       ONLY ALLOWING WILDCARD                       
         BNE   CKFFD_E2                                                         
         MVC   F_USER2,SC2NDFLD    USER DEFINITION FLD 2 FILTER                 
CKFFD60K CLI   F_USER2+L'F_USER2,0                                              
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_USER2+L'F_USER2                                             
         B     CKFFD90                                                          
*                                                                               
CKFFD62  DS    0H                  FOR FUTURE FILTER KEYWORDS                   
*                                                                               
         B     CKFFD_E2            KEYWORD IS NOT DEFINED                       
*                                                                               
CKFFD90  LA    R4,SCBLKLQ(R4)      POINT TO NEXT SCANNER BLK                    
         AHI   R5,1                LOOP COUNTER UP BY ONE                       
         B     CKFFD20             CK FOR NEXT SCANNER BLK                      
*                                                                               
         J     SETCCEQ                                                          
         DROP  R4                                                               
*                                                                               
CKFFD_E1 XC    F_FLDS(F_FLDLNQ),F_FLDS                                          
         J     DUPEDERR                                                         
*                                                                               
CKFFD_E2 LHI   R2,INVFUSGE                                                      
         BRAS  RE,GET_ETXT                                                      
         J     SETCCNEQ                                                         
*                                                                               
CKFFD_E3 XC    F_FLDS(F_FLDLNQ),F_FLDS                                          
         J     LONGERR                                                          
*                                                                               
CKFFD_E4 LHI   R2,FLTKWNTA                                                      
         BRAS  RE,GET_ETXT                                                      
         J     SETCCNEQ                                                         
*                                                                               
GET_ETXT LR    R0,RE                                                            
         XC    FULL,FULL                                                        
         MVI   FULL+1,25           SYSTEM                                       
         L     RF,FULL                                                          
         GOTOR GETTXT,DMCB+12,(R2),0,(C'E',DMCB),0,0,(RF)                       
         OI    CONHEADH+6,X'80'                                                 
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* DUB+00 = 01 MEANS CKING FOR CHAR FORMAT OF BASE A FORMULA                     
*          02 MEANS CKING FOR BIT FORMAT OF BASE A FORMULA                      
*                                                                               
* DUB+01 = CHAR FORMAT OF BASE A FORMULA (5 CHARS MAX) OR                       
*          BIT FORMAT OF BASE A FORMULA                                         
*                                                                               
* DUB WILL RETURN BOTH FORMAT OF BASE A FORMULA (5 CHARS + 1 BYTE)              
* IF NO BASE A FORMULA IS FOUND, DUB WILL RETURN NULLS                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CK_BASEA NTR1  BASE=*,LABEL=*      CKING FOR FORMULA BASE A                     
*                                                                               
         CLI   DUB+00,01           CKING FOR CHAR INPUT?                        
         BE    CK_BA20                                                          
         CLI   DUB+00,02           CKING FOR BIT INPUT?                         
         BE    CK_BA40                                                          
         DC    H'0'                INVALID PARAMETER!                           
*                                                                               
CK_BA20  LA    RF,CK_BA_T1         POINT TO FORMULA TABLE                       
CK_BA20H CLI   0(RF),X'FF'         END?                                         
         BE    CK_BA30                                                          
         CLC   0(5,RF),DUB+01      FLT VALUE MATCH THAT IN TABLE?               
         BE    *+12                                                             
         LA    RF,6(RF)            NEXT ENTRY                                   
         B     CK_BA20H                                                         
         MVC   DUB(06),0(RF)       5 CHARS AND 1 BYTE INDICATOR                 
         J     SETCCEQ                                                          
*                                                                               
CK_BA30  XC    DUB,DUB             RETURN NULLS AND SET CC NOT EQ               
         J     SETCCNEQ                                                         
*                                                                               
* IF PRD'S BASE A IS ZERO, USE PRD AAA'S BASE A                                 
*                                                                               
CK_BA40  CLI   DUB+01,00           BASE A FORMULA IS ZERO?                      
         BNE   CK_BA40H                                                         
         MVC   WORK(L'KEY),KEY                                                  
         MVC   WORK+L'KEY(L'AIO),AIO                                            
         CLI   KEY+03,X'06'        PRD KEY?                                     
         BE    *+6                                                              
         DC    H'0'                BAD KEY                                      
         XC    KEY+07(L'KEY-07),KEY+07                                          
         MVC   KEY+07(03),=C'AAA'                                               
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE     PRD AAA IS SET UP?                           
         BE    *+14                                                             
CK_BA40F XC    DUB,DUB             FOR SETTING CC CODE                          
         B     CK_BA90                                                          
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO3                                                          
         MVI   ELCODE,X'06'        1ST PRD ELEM CODE                            
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PPRDELEM,R6                                                      
         LA    R6,PPRDBILP                                                      
         USING BILPROF,R6                                                       
         CLI   BILBASA,0           BASE A IS SET UP IN PRD AAA?                 
         BE    CK_BA40F            NO, SET CC AND GET OUT                       
         MVC   DUB+01(L'BILBASA),BILBASA                                        
         DROP  R6                                                               
*                                                                               
         MVC   KEY,WORK            RESTORE KEY                                  
         MVC   AIO,WORK+L'KEY      RESTORE ORIGINAL AIO                         
         GOTO1 HIGH                RESTORE READING SEQ                          
         GOTO1 GETREC                                                           
*                                                                               
CK_BA40H LA    RF,CK_BA_T1         POINT TO FORMULA TABLE                       
CK_BA40K CLI   0(RF),X'FF'         END?                                         
         BE    CK_BA30             NOT VALID BASE A FORMULA                     
         CLC   5(1,RF),DUB+01      BIT IS SAME AS THAT IN TABLE?                
         BE    *+12                                                             
         LA    RF,6(RF)            NEXT ENTRY                                   
         B     CK_BA40K                                                         
         MVC   DUB(06),0(RF)       5 CHARS AND 1 BYTE INDICATOR                 
         CLC   =C'G    ',DUB                                                    
         BNE   *+14                                                             
         MVC   DUB(05),=C'GROSS'   USE ENTIRE WORD                              
         J     SETCCEQ                                                          
         CLC   =C'N    ',DUB                                                    
         JNE   SETCCEQ                                                          
         MVC   DUB(05),=C'NET  '   USE ENTIRE WORD                              
         J     SETCCEQ                                                          
*                                                                               
CK_BA90  MVC   KEY,WORK            RESTORE KEY                                  
         MVC   AIO,WORK+L'KEY      RESTORE ORIGINAL AIO                         
         GOTO1 HIGH                RESTORE READING SEQ                          
         GOTO1 GETREC                                                           
         OC    DUB,DUB                                                          
         JZ    SETCCNEQ                                                         
         J     SETCCEQ                                                          
*                                                                               
CK_BA_T1 DC    C'G    ',X'01'      GROSS                                        
         DC    C'GROSS',X'01'      GROSS                                        
         DC    C'N    ',X'02'      NET                                          
         DC    C'NET  ',X'02'      NET                                          
         DC    C'CD   ',X'04'      CASH DISCOUNT                                
         DC    C'G-CD ',X'05'      GROSS-CASH DISCOUNT                          
         DC    C'N-CD ',X'06'      NET-CASH DISCOUNT                            
         DC    C'AC   ',X'08'      AGEY COMMISSION                              
         DC    X'FF'               END OF TABLE                                 
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
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
         CLI   3(RE),X'06'         PRD RECORD CODE?                             
         BNE   INITI50                                                          
         USING PPRDKEY,RE                                                       
         LA    R2,PRDMEDH          MEDIA FLD ON MAINT SCR                       
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   *+8                                                              
         LA    R2,LSTMEDH          POINT TO MEDIA FLD ON LIST SCR               
         MVC   8(1,R2),PPRDKMED                                                 
         MVI   5(R2),1             INPUT LENGTH                                 
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         LA    R2,PRDCLTH          CLIENT FLD ON MAINT SCR                      
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   *+8                                                              
         LA    R2,LSTCLTH          POINT TO CLIENT FLD ON LIST SCR              
         MVC   8(3,R2),PPRDKCLT                                                 
         MVI   5(R2),3             INPUT LENGTH                                 
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         LA    R2,PRDPRDH          PRD FLD ON MAINT SCR                         
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   *+8                                                              
         LA    R2,LSTPRDH          POINT TO PRD FLD ON LIST SCR                 
         MVC   8(3,R2),PPRDKPRD                                                 
         MVI   5(R2),3             INPUT LENGTH                                 
         OI    6(R2),X'80'         TRANSMIT                                     
         DROP  RE                                                               
*                                                                               
INITI50  OI    GENSTAT4,NODELLST   NO DELETE ON LIST                            
         MVI   ACTELOPT,C'N'       NO ACTIVITY ELEM WILL BE ADDED               
         MVI   NLISTS,14           14 LINES ON LIST SCREEN                      
         XC    IO_COUNT,IO_COUNT   I/O COUNTER                                  
         MVI   CNTLSW,0            CONTROL SWITCH                               
*                                                                               
         XC    SECVALS(SECVALSL),SECVALS                                        
         L     RF,ATWA                                                          
         OC    4(2,RF),4(RF)       TEST AGENCY ON NEW SECURITY                  
         BNZ   *+14                                                             
         OC    6(2,RF),6(RF)       TEST ANY LIMIT ACCESS                        
         BZ    INITI52X                                                         
*                                                                               
         LA    R2,SECFLDS          R2=A(FIELD SECURITY DISPLACEMENTS)           
         LHI   R0,SECFLDSN         R0=N'SECURITY FIELDS                         
INITI52E GOTOR SECRET,DMCB,('SECPFLDP',ASECBLK),1(R2)                           
         BE    INITI52H                                                         
         LA    RF,C'Y'             C'Y'=READ ONLY                               
         BH    *+8                                                              
         LA    RF,C'N'             C'N'=NO ACCESS                               
         SR    RE,RE                                                            
         IC    RE,0(R2)            RE=DISPLACEMENT TO SECURITY VALUE            
         LA    RE,SECVALS(RE)                                                   
         STC   RF,0(RE)            YES - DISPLAY AND CHANGE                     
INITI52H AHI   R2,L'SECFLDS        BUMP TO NEXT DISPLACEMENT                    
         BCT   R0,INITI52E         DO FOR NUMBER OF SECURITY FIELDS             
INITI52X DS    0H                                                               
*                                                                               
         CLI   PHSCREEN,X'A1'      PRD LIST SCR?                                
         BNE   INITI51                                                          
         XC    LSTTL3++20+4+2(LTFFLDQ),LSTTL3+20+4+2                            
         XC    LSTUL3++20+4+2(LTFFLDQ),LSTUL3+20+4+2                            
         LA    RE,F_FLDLNQ                                                      
         CHI   RE,255                                                           
         BNH   *+6                                                              
         DC    H'0'                TOTAL FILTER FLDS CANNOT EXCEED 255!         
         OC    F_FLDS(F_FLDLNQ),F_FLDS                                          
         BNZ   INITI50U                                                         
T        USING LS_TTL3,LSTTL3                                                   
         MVC   T.LTOFC(03),=C'OFC'                                              
         MVC   T.LTAOF(03),=C'AOF'                                              
         MVI   T.LTAOFASP,C'/'                                                  
         MVC   T.LTAGY(03),=C'Agy'                                              
         MVC   T.LTTOF(03),=C'TOF'                                              
         MVC   T.LTBASEA(05),=C'BaseA'                                          
         DROP  T                                                                
U        USING LS_TTL3,LSTUL3                                                   
         MVC   U.LTOFC(03),=C'---'                                              
         MVC   U.LTAOF(03),=C'---'                                              
         MVI   U.LTAOFASP,C'-'                                                  
         MVC   U.LTAGY(03),=C'---'                                              
         MVC   U.LTTOF(03),=C'---'                                              
         MVC   U.LTBASEA(05),=C'-----'                                          
         DROP  U                                                                
INITI50U OI    LSTTL3H+6,X'80'                                                  
         OI    LSTUL3H+6,X'80'                                                  
*                                                                               
INITI51  CLI   PHSCREEN,X'A0'      PRD MAINT SCREEN?                            
         BNE   INITI56                                                          
         XC    PRDBOTL+PF12POSQ(L'PF12TXT),PRDBOTL+PF12POSQ                     
         OI    PRDBOTLH+6,X'80'                                                 
         CLI   MODE,NEWSCR         NEW SCR MODE?                                
         BE    INITI56                                                          
         CLI   MODE,VALKEY         VALIDATING KEY?                              
         BE    INITI56                                                          
         CLI   THISLSEL,C'C'       SEL CODE IS CHANGE ON LIST?                  
         BE    INITI54M                                                         
         CLI   MODE,VALREC         MODE IS VALREC?                              
         BE    INITI54M                                                         
         CLI   ACTNUM,ACTCHA       ACTION IS CHANGE?                            
         BNE   INITI56                                                          
INITI54M OI    PRDPNAMH+6,X'81'    CHG TO MODIFIED FLD (GAIN CONTROL)           
*                                                                               
INITI56  CLI   ACTNUM,ACTSEL       SELECT FROM LIST?                            
         BNE   INITI58                                                          
         CLI   PHSCREEN,X'A0'      PRD MAINT SCREEN?                            
         BE    *+6                                                              
         DC    H'0'                WRONG SCREEN!                                
         MVC   PRDBOTL+PF12POSQ(L'PF12TXT),PF12TXT                              
         OI    PRDBOTLH+6,X'80'                                                 
*                                                                               
INITI58  LA    RE,HEDSPECS         SET UP HEADHOOK AND SPECS                    
         ST    RE,SPECS                                                         
         LA    RE,HOOK                                                          
         ST    RE,HEADHOOK                                                      
         MVC   H2+15(10),MEDNM                                                  
         MVC   H2+10(01),QMED                                                   
*                                                                               
         CLI   ACTNUM,ACTLIST      ACTION IS LIST?                              
         BE    INITIX                                                           
         CLI   ACTNUM,ACTREP       ACTION IS LIST?                              
         BE    INITIX                                                           
         CLI   PHSCREEN,X'A0'      PRD MAINT SCREEN?                            
         BE    *+6                                                              
         DC    H'0'                WRONG SCREEN!                                
*                                                                               
         CLI   WNATION,C'C'        CANADIAN?                                    
         BNE   INITI60                                                          
         MVC   PRDGSTA+00(3),=C'GST'                                            
         MVI   PRDGSTA+03,C' '                                                  
         MVC   PRDGSTA+04(4),=C'Code'                                           
         OI    PRDGSTAH+6,X'80'                                                 
         NI    PRDGSTH+1,X'FF'-X'20'                                            
         OI    PRDGSTH+6,X'80'                                                  
*                                                                               
         MVC   PRDPSTA+00(3),=C'PST'                                            
         OI    PRDPSTAH+6,X'80'                                                 
         NI    PRDPSTH+1,X'FF'-X'20'                                            
         OI    PRDPSTH+6,X'80'                                                  
*                                                                               
         MVC   PRDMPSA+00(8),=C'Main PST'                                       
         OI    PRDMPSAH+6,X'80'                                                 
         NI    PRDMPSH+1,X'FF'-X'20'                                            
         OI    PRDMPSH+6,X'80'                                                  
*                                                                               
INITI60  MVC   PRDDIVA(15),=C'Division Number'                                  
         OI    PRDDIVAH+6,X'80'                                                 
         XC    PRDDIVN,PRDDIVN                                                  
         OI    PRDDIVNH+6,X'80'                                                 
         NI    PRDDIVH+1,X'FF'-X'20'                                            
*                                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         TM    FATSTAT6,TST6STRO   STEREO MODE?                                 
         BZ    *+8                                                              
         NI    PRDDIVH+1,X'FF'-X'10'                                            
         DROP  R1                                                               
*                                                                               
         OI    PRDDIVH+6,X'80'                                                  
         CLI   SVCPROF+00,C'0'     NO DIVISION REQUIRED FOR BUYING?             
         BNE   INITI60X                                                         
         XC    PRDDIVA,PRDDIVA     CLEAR DIVISION NUMBER TITLE                  
         OI    PRDDIVAH+6,X'80'                                                 
         XC    PRDDIV,PRDDIV                                                    
         OI    PRDDIVH+1,X'20'     PROTECT FIELD                                
         MVI   PRDDIVH+5,0         CLEAR INPUT LENGTH                           
         OI    PRDDIVH+6,X'80'                                                  
INITI60X DS    0H                                                               
*                                                                               
INITI64  TM    SVCLSTAT,X'20'      CLT ALLOWS OFFC & TRFF CNTL?                 
         BZ    INITI64X            NO, NO NEED TO ENABLE FLDS                   
         MVC   PRDOFFT(6),=C'Office'                                            
         OI    PRDOFFTH+6,X'80'                                                 
         NI    PRDOFFH+1,X'FF'-X'20'                                            
         OI    PRDOFFH+6,X'80'                                                  
         MVC   PRDTRAT(7),=C'Traffic'                                           
         OI    PRDTRATH+6,X'80'                                                 
         NI    PRDTRAFH+1,X'FF'-X'20'                                           
         OI    PRDTRAFH+6,X'80'                                                 
INITI64X DS    0H                                                               
*                                                                               
* IF FLD CONTROL NOT PRESENT, STILL NEED TO CK FOR HEX CONTROL                  
*                                                                               
         OC    SECVALS(SECVALSL),SECVALS                                        
         BZ    INITI66X                                                         
*                                                                               
         LA    RF,SECPCED                                                       
         LA    R2,PRDPCETH                                                      
         BRAS  RE,PRCFLD                                                        
*                                                                               
         LA    RF,SECPUD1                                                       
         LA    R2,PRDDEF1H                                                      
         BRAS  RE,PRCFLD                                                        
         XC    PRDDEF1,PRDDEF1                                                  
         CLI   SECPUD1,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    *+10                                                             
         MVC   PRDDEF1,SVP1USER    NEED TO DISP USER DEF 1 TITLE                
         OI    PRDDEF1H+6,X'80'                                                 
*                                                                               
         LA    RF,SECPUD2                                                       
         LA    R2,PRDDEF2H                                                      
         BRAS  RE,PRCFLD                                                        
         XC    PRDDEF2,PRDDEF2                                                  
         CLI   SECPUD2,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    *+10                                                             
         MVC   PRDDEF2,SVP2USER    NEED TO DISP USER DEF 2 TITLE                
         OI    PRDDEF2H+6,X'80'                                                 
         B     INITI74             FCON APPLIED, NO NEED TO CK HEX              
INITI66X DS    0H                                                               
*                                                                               
INITI70  XC    PRDDEF1,PRDDEF1                                                  
         OI    PRDDSC1H+1,X'20'    DEFAULT IS PROTECTED                         
         L     RF,ATWA                                                          
         TM    12(RF),X'08'        AUTHORIZED?                                  
         BO    *+8                                                              
         NI    PRDDSC1H+1,X'FF'-X'20'                                           
         OC    SVP1USER,SVP1USER   PRD USER DEF 1 FLD PRESENT?                  
         BZ    *+14                                                             
         MVC   PRDDEF1,SVP1USER                                                 
         B     *+8                                                              
         OI    PRDDSC1H+1,X'20'    PROTECT, SINCE DEF FLD IS NOT THERE          
         OI    PRDDSC1H+6,X'80'                                                 
         OI    PRDDEF1H+6,X'80'                                                 
*                                                                               
         XC    PRDDEF2,PRDDEF2                                                  
         OI    PRDDSC2H+1,X'20'    DEFAULT IS PROTECTED                         
         L     RF,ATWA                                                          
         TM    12(RF),X'08'        AUTHORIZED?                                  
         BO    *+8                                                              
         NI    PRDDSC2H+1,X'FF'-X'20'                                           
         OC    SVP2USER,SVP2USER   PRD USER DEF 1 FLD PRESENT?                  
         BZ    *+14                                                             
         MVC   PRDDEF2,SVP2USER                                                 
         B     *+8                                                              
         OI    PRDDSC2H+1,X'20'    PROTECT, SINCE DEF FLD IS NOT THERE          
         OI    PRDDSC2H+6,X'80'                                                 
         OI    PRDDEF2H+6,X'80'                                                 
*                                                                               
INITI74  DS    0H                                                               
*                                                                               
         BRAS  RE,SETLWFLD         SET UP LEGAL WARNING FLD                     
*                                                                               
INITIX   J     EXIT                                                             
*                                                                               
HOOK     NTR1                      HEADLINE ROUTINES                            
*                                                                               
HOOKX    J     EXIT                                                             
*                                                                               
HEDSPECS SSPEC H1,01,REQUESTOR                                                  
         SSPEC H2,01,C'Media'                                                   
         SSPEC H1,56,C' Product Report'                                         
         SSPEC H2,56,C'----------------'                                        
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,99,RUN                                                        
         SSPEC H5,99,REPORT                                                     
         SSPEC H6,99,C' '                                                       
         DC    H'0'                                                             
*                                                                               
PF12POSQ EQU   L'PRDBOTL-L'PF12TXT                                              
PF12TXT  DC    C'Pf12=Return/NextSel'                                           
*                                                                               
* RF POINTS FLD SECURITY VALUE                                                  
* R2 POINTS TITLE FLD TO BE PROCESSED                                           
*                                                                               
PRCFLD   DS    0H                  CLR TITLE & PROTECT FLDS                     
         SR    R1,R1                                                            
         IC    R1,0(R2)            TOTAL FLD LENGTH                             
         CLI   0(RF),0             FULL ACCESS?                                 
         BNE   PRCF20                                                           
         AR    R2,R1               TO TO INPUT FLD                              
         NI    1(R2),X'FF'-X'20'   UNPROTECT INPUT FLD                          
         B     PRCFLDX                                                          
PRCF20   CLI   0(RF),C'Y'          READ ONLY?                                   
         BE    PRCF30                                                           
         CLI   0(RF),C'N'          NO ACCESS?                                   
         BE    *+6                                                              
         DC    H'0'                INVALID SECURITY VALUE!                      
         SHI   R1,8+1              MINUS OVERHEAD AND ONE FOR EX                
         CHI   R1,0                                                             
         BNL   *+6                                                              
         DC    H'0'                BAD TITLE FLD LENGTH                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),X'80'         TRANSMIT CLEARED TITLE FLD                   
PRCF30   IC    R1,0(R2)                                                         
         AR    R2,R1               BUMP FROM TITLE TO INPUT FLD                 
         OI    1(R2),X'20'         PROTECT INPUT FLD                            
PRCFLDX  OI    6(R2),X'80'         TRANSMIT PROTECTED INPUT FLD                 
         BR    RE                                                               
*                                                                               
* SECURITY FIELDS TABLE - DISPLACEMENT (1 BYTE) AND FLD # (1 BYTE)              
*                                                                               
SECFLDS  DS    0XL2                ** DISPS. TO SECURITY VALUES **              
         DC    AL1(SECPUD1-SECVALS,006)                                         
         DC    AL1(SECPUD2-SECVALS,007)                                         
         DC    AL1(SECLGWR-SECVALS,015)                                         
         DC    AL1(SECPCED-SECVALS,017)                                         
*                                                                               
SECFLDSN EQU   (*-SECFLDS)/L'SECFLDS                                            
*                                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                  FROM INITIALZ                                
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETREPLN NTR1  BASE=*,LABEL=*      R6 POINTS TO RECORD                          
*                                                                               
         LA    R5,P1                                                            
         USING REP_LINE,R5                                                      
*                                                                               
         MVC   R_PRD,07(R6)        PRD CODE FROM KEY                            
*                                                                               
         USING PPRDELEM,R6                                                      
         MVI   ELCODE,X'06'        PRD ELEM ID                                  
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                FIRST ELEM MUST BE THERE!                    
*                                                                               
         MVC   R_PRDNM,PPRDNAME    CLIENT NAME                                  
         MVC   R_PRDBNM,PPRDBILL   BILL RECEIPT NAME                            
         MVC   R_PRDLN1,PPRDLIN1   ADDRESS - LINE 1                             
         MVC   R_PRDLN2,PPRDLIN2   ADDRESS - LINE 2                             
         MVC   R_PRDATN,PPRDATTN   ATTENTION OF                                 
*                                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  R6,R5,RB                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKPFKEYS NTR1  BASE=*,LABEL=*      CKING FOR PK KEYS                            
*                                                                               
         CLI   PFAID,3             PF3, CLT MAINT?                              
         BE    CKPFK10                                                          
         CLI   PFAID,5             PF5, CLT LIST?                               
         BE    CKPFK10                                                          
         CLI   PFAID,6             PF6, PRD LIST?                               
         BE    CKPFK10                                                          
         CLI   PFAID,7             PF7, EST LIST?                               
         BE    CKPFK10                                                          
         CLI   PFAID,8             PF8, PRD BILL (PRBILL) MAINT?                
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
         CLI   PFAID,3             RECORD IS CLT (FOR MAINT)?                   
         BNE   CKPFK15                                                          
         MVC   DUB,=C'CLIENT  '                                                 
         B     CKPFK25                                                          
*                                                                               
CKPFK15  CLI   PFAID,5             RECORD IS CLT (FOR LIST)?                    
         BNE   CKPFK16                                                          
         MVC   DUB,=C'CLIENT  '                                                 
         B     CKPFK25                                                          
*                                                                               
CKPFK16  CLI   PFAID,6             RECORD IS PRD (FOR LIST)?                    
         BNE   CKPFK17                                                          
         MVC   DUB,=C'PRODUCT '                                                 
         B     CKPFK25                                                          
*                                                                               
CKPFK17  CLI   PFAID,7             RECORD IS EST (FOR LIST)?                    
         BNE   CKPFK18                                                          
         MVC   DUB,=C'ESTIMATE'                                                 
         B     CKPFK25                                                          
*                                                                               
CKPFK18  CLI   PFAID,8             RECORD IS PRD BILL (FOR MAINT)?              
         BNE   CKPFK19                                                          
         MVC   DUB,=C'PRBILL  '                                                 
         B     CKPFK25                                                          
*                                                                               
CKPFK19  DS    0H                  FOR FUTURE PFKEYS                            
*                                                                               
         J     SETCCNEQ            NO OTHER PFKEY DEFINED YET                   
*                                                                               
* SET RECORD FLD                                                                
*                                                                               
CKPFK25  GOTO1 VGLOBBER,DMCB,=C'PUTD',DUB,8,GLVXREC                             
*                                                                               
         MVC   DUB,SPACES          PREPARE IT FOR ACTION FLD                    
*                                                                               
         CLI   PFAID,3             CLT MAINT?                                   
         BE    *+12                                                             
         CLI   PFAID,8             PRD BILL MAINT?                              
         BNE   CKPFK30                                                          
         MVC   DUB,=C'CHANGE  '                                                 
         CLI   ACTNUM,ACTCHA       CHANGE ACTION?                               
         BE    CKPFK40                                                          
         CLI   THISLSEL,C'C'       SEL CODE IS CHANGE ON LIST?                  
         BE    CKPFK40                                                          
         CLI   MODE,VALREC         MODE IS VALREC?                              
         BE    CKPFK40                                                          
         CLI   MODE,RECPUT         MODE IS PUTREC? (STILL CHG)                  
         BE    CKPFK40                                                          
         CLI   MODE,XRECPUT        MODE IS XPUTREC?                             
         BE    CKPFK40                                                          
         MVC   DUB,=C'DISPLAY '                                                 
         B     CKPFK40                                                          
*                                                                               
CKPFK30  CLI   PFAID,5             CLT LIST?                                    
         BNE   CKPFK31                                                          
CKPFK30H MVC   DUB,=C'LIST    '                                                 
         B     CKPFK40                                                          
*                                                                               
CKPFK31  CLI   PFAID,6             PRD LIST?                                    
         BNE   CKPFK32                                                          
         B     CKPFK30H                                                         
*                                                                               
CKPFK32  CLI   PFAID,7             EST LIST?                                    
         BNE   CKPFK33                                                          
         B     CKPFK30H                                                         
*                                                                               
CKPFK33  DS    0H                  FOR FUTURE PFKEYS                            
*                                                                               
         J     SETCCNEQ            NO OTHER PFKEY DEFINED YET                   
*                                                                               
* SET ACTION FLD                                                                
*                                                                               
CKPFK40  GOTO1 VGLOBBER,DMCB,=C'PUTD',DUB,8,GLVXACT                             
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTF',PRDMEDH,,GLVPRKEY   KEY                   
         GOTO1 VGLOBBER,DMCB,=C'PUTF',PRDMEDH,,GLVPRMD    MEDIA                 
         GOTO1 VGLOBBER,DMCB,=C'PUTF',PRDCLTH,,GLVPRCLT   CLIENT                
         GOTO1 VGLOBBER,DMCB,=C'PUTF',PRDPRDH,,GLVPRPRD   PRODUCT               
*                                                                               
         J     SETCCEQ                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB                  FROM CKPFKEYS                                
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* LEGAL WARNING IS ALLOWED FOR H9 & SJ (HEX SECURITY IS NOT SUPPORTED)          
*                                                                               
* IF FCON FOR LEGAL WARNING IS WRITE ACCESS, IT MEANS ONLY LEGAL                
* WARNING FLD IS ALLOWED TO BE CHANGED, ALL OTHER FLDS WILL BE                  
* PROTECTED ("W" IN THIS CASE IS REALLY AN EXLUCSIVE WRITE ACCESS)              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SETLWFLD NTR1  BASE=*,LABEL=*      SETUP LEGAL WARNING FLD                      
*                                                                               
         CLC   AGENCY,=C'H9'       STARCOM?                                     
         BE    SETLWF30                                                         
         CLC   AGENCY(2),=C'SJ'                                                 
         BE    SETLWF30                                                         
SETLWF20 XC    PRDLWT,PRDLWT                                                    
         OI    PRDLWTH+6,X'80'     TRANSMIT                                     
         XC    PRDLWRO,PRDLWRO                                                  
SETLWF26 OI    PRDLWROH+6,X'A0'    PROTECT FLD FOR NXT & TRANSMIT               
         B     SETLWFX                                                          
*                                                                               
SETLWF30 CLC   =C'LEGAL',CONREC                                                 
         BE    SETLWF50                                                         
         CLC   =C'LEGALW',CONREC                                                
         BNE   SETLWF80                                                         
*                                                                               
SETLWF50 LA    R2,PRDFRSTH         FIRST FLD ON PRD SCR                         
         LA    R3,PRDENDX          LAST FLD ON PRD SCR                          
*                                                                               
SETLWF60 CR    R2,R3               LAST SCREEN FIELD YET?                       
         BH    SETLWF80            YES                                          
         TM    6(R2),X'20'         ALREADY CHG'D TO PROTECTED?                  
         BO    *+8                                                              
         OI    6(R2),X'20'         PROTECTED FLD FOR NEXT INPUT                 
         SR    RF,RF                                                            
         IC    RF,0(R2)                                                         
         AR    R2,RF                                                            
         B     SETLWF60            DONE NEXT FLD ON SCR                         
*                                                                               
SETLWF80 CLI   SECLGWR,C'N'        NO ACCESS?                                   
         BE    SETLWF20                                                         
         CLI   SECLGWR,C'Y'        READ ACCESS?                                 
         BE    SETLWF26                                                         
         CLI   SECLGWR,0           WRITE ACCESS?                                
         BE    *+6                                                              
         DC    H'0'                INVALID FCON VALUE                           
*                                                                               
         LA    R2,PRDLWROH         NEED TO UNPROTECT LW ROTATION FLD            
         NI    6(R2),X'FF'-X'20'                                                
*                                                                               
         OI    PRDLWROH+6,X'81'    CHG TO MODIFIED FLD (GAIN CONTROL)           
*                                                                               
SETLWFX  DS    0H                                                               
         J     SETCCEQ             EQUAL                                        
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKAOFF   NTR1  BASE=*,LABEL=*      CK ACCOUNTING OFFICE CODE FOR PRD            
*                                                                               
         LA    R4,ELEM             POINTS TO ELEM TO BE BUILT                   
         XC    ELEM,ELEM                                                        
         USING PPRDAOEL,R4                                                      
         MVI   0(R4),X'35'         ACCOUNT OFFICE CODE ELEM CODE                
         MVI   1(R4),10            ELEM LENGTH                                  
*                                                                               
         MVI   COMPCD,0                                                         
         MVI   ASW,0               SET TO X'01' IF SWITCHED TO ACC              
         XC    POWCODE,POWCODE                                                  
         XC    ACCOFF,ACCOFF                                                    
         MVI   SVACCOFC,C'Y'       SET 2 CHARS ACC OFFICE REQUIRED              
         CLI   SVAGYPF+11,C'A'                                                  
         JE    *+16                                                             
         CLI   SVAGYPF+11,C'B'                                                  
         JE    *+8                                                              
         MVI   SVACCOFC,C'N'                                                    
*                                                                               
* GET SYS NUM TO SWITCH BACK TO                                                 
*                                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   SYSSW,FASYS                                                      
         DROP  R1                                                               
*                                                                               
         SR    R1,R1                                                            
         IC    R1,PRDAOFFH+5       INPUT LENGTH                                 
*                                                                               
         LA    R2,PRDAOFF+0                                                     
         CLI   PRDAOFFH+5,0                                                     
         BE    CKAOF50             INPUT LENGTH IS 0                            
         LA    R2,PRDAOFF+1                                                     
         CLI   PRDAOFFH+5,1                                                     
         BE    CKAOF50             INPUT LENGTH IS 1                            
         LA    R2,PRDAOFF+2                                                     
         CLI   PRDAOFFH+5,2                                                     
         BE    CKAOF50             INPUT LENGTH IS 2                            
*                                                                               
         LA    R2,PRDAOFF          POINT TO INPUT FLD                           
         LA    R3,5                MAX OF 5 CHARS INPUT                         
         SR    R1,R1                                                            
CKAOF30  CLI   0(R2),C','                                                       
         BE    CKAOF50                                                          
         CLI   0(R2),C'/'                                                       
         BE    CKAOF50                                                          
         CLI   0(R2),C' '                                                       
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         LA    R2,1(R2)                                                         
         BCT   R3,CKAOF30                                                       
*                                                                               
CKAOF50  DS    0H                  CK FOR LENGTH                                
         LA    R2,1(R2)            POINT TO END OF AGENCY CODE + 1              
         STC   R1,OFFLEN                                                        
         MVC   ACCOFF(2),PRDAOFF                                                
         CLI   OFFLEN,2                                                         
         BH    CKAOF90             ERR: LENGTH ERROR                            
         BE    CKAOF55                                                          
         CLI   SVACCOFC,C'Y'                                                    
         BE    CKAOF91             ERR: 2 CHARS REQUIRED                        
         MVI   ACCOFF+1,C' '                                                    
         CLI   OFFLEN,0                                                         
         BNE   CKAOF80             SEE IF 2 CHARS MATCH                         
         XC    PPRDAOFC,PPRDAOFC                                                
         XC    PPRDACCA,PPRDACCA                                                
         MVC   PPRDAOFC,SVCPROF+30 DEFAULT TO CLT OFFICE IF <2 CHARS            
         MVI   PPRDAOFC+1,C' '                                                  
         J     SETCCEQ                                                          
*                                                                               
CKAOF55  CLI   SVACCOFC,C'Y'                                                    
         BNE   CKAOF92             ERR: 1 CHAR REQUIRED                         
         B     CKAOF80                                                          
*                                                                               
CKAOF60  DS    0H                  VALIDATE AGAINST AGY HDR CODE LIST           
         LA    R3,SVACCAGY                                                      
         LA    R1,4                                                             
CKAOF60H CLC   0(2,R3),POWCODE     MATCH?                                       
         BE    CKAOF65                                                          
         CLI   0(R3),C' '          END OF TABLE?                                
         BNH   CKAOF93             ERR: INVALID AGENCY CODE                     
         LA    R3,2(R3)                                                         
         BCT   R1,CKAOF60H                                                      
         B     CKAOF93             ERR: INVALID AGENCY CODE                     
*                                                                               
CKAOF65  MVC   DATADISP,=H'28'     FIND SE NUMBER FOR SPECIFIED                 
         XC    CTKEY,CTKEY         ACC AGY CODE                                 
         LA    RE,CTKEY                                                         
         USING CT5REC,RE                                                        
         MVI   CT5KTYP,CT5KTYPQ    RECORD TYPE '5'                              
         MVC   CT5KALPH,POWCODE                                                 
         DROP  RE                                                               
*                                                                               
         L     R6,AIO3                                                          
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',CTKEY,(R6)                
*                                                                               
         CLI   8(R1),0             ERRORS?                                      
         BNE   CKAOF94             ERR: INVALID ACC AGY CODE                    
*                                                                               
         MVI   ELCODE,X'21'        GET SE NUM FOR ACC FILE                      
         BRAS  RE,GETEL                                                         
         BE    *+12                                                             
         B     CKAOF94             ERR: INVALID ACC AGY CODE                    
CKAOF65H BRAS  RE,NEXTEL                                                        
*                                                                               
         USING CTSYSD,R6                                                        
         CLI   CTSYSEL,X'21'       STILL X'21' EL                               
         BNE   CKAOF94             ERR: INVALID ACC AGY CODE                    
         CLI   CTSYSNUM,X'06'      ACC??                                        
         BNE   CKAOF65H                                                         
*                                                                               
         MVC   COMPCD(1),CTSYSAGB  AGY BINARY CD                                
         XC    DMCB(8),DMCB        YES                                          
         MVC   DMCB(1),CTSYSSE     SE NUM                                       
         GOTO1 SWITCH,DMCB         SWITCH TO THAT ACC SYSTEM                    
         B     CKAOF85                                                          
*                                                                               
CKAOF80  DS    0H                                                               
         CLI   0(R2),C' '                                                       
         BNH   *+14                                                             
         MVC   POWCODE,0(R2)                                                    
         B     CKAOF60             SWITCH TO DIFFERENT ACC SYSTEM               
*                                                                               
         CLI   SVACCOFC,C'Y'       2 CHARS REQUIRED?                            
         BNE   CKAOF88             IF NOT THEN SKIP SWITCH                      
*                                                                               
* SWITCH TO ACC SYSTEM                                                          
*                                                                               
         GOTO1 SWITCH,DMCB,=C'ACC',0                                            
*                                                                               
CKAOF85  CLI   4(R1),2             SYSTEM NOT OPEN?                             
         BE    CKAOF95             ERR: SYS IS NOT OPEN                         
*                                                                               
         CLI   4(R1),1             ANY OTHER ERRORS?                            
         BE    CKAOF96             ERR: CANNOT SWITCH TO ACC SYS                
*                                                                               
         MVI   ASW,X'01'           SUCCESSFULLY SWITCHED TO ACC                 
*                                                                               
         CLI   COMPCD,0            ALREADY HAVE AGENCY BINARY CODE?             
         BNE   *+18                                                             
         CLI   0(R1),0             NO RETURNED CODE?                            
         BE    CKAOF97             ERR: ACC COMPANY REC NOT FOUND               
         MVC   COMPCD,0(R1)        SAVE RETURNED AGENCY BINARY CODE             
*                                                                               
         MVC   WKACCKEY,SPACES     READ COMPANY REC                             
         MVC   WKACCKEY(1),COMPCD  RETURNED AGENCY BINARY CODE                  
*                                                                               
         L     R6,AIO3                                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',WKACCKEY,(R6)                
         CLI   8(R1),0                                                          
         BNE   CKAOF97             ERR: ACC COMPANY REC NOT FOUND               
*                                                                               
         CLI   SVACCOFC,C'Y'       2 CHAR REQUIRED?                             
         BNE   CKAOF88                                                          
         LA    R6,WKACCKEY         NEW OFFICE - LOOK FOR OFFICE REC             
         USING OFFRECD,R6                                                       
         MVC   WKACCKEY,SPACES                                                  
         MVI   OFFKTYP,OFFKTYPQ    X'01'                                        
         MVC   OFFKCPY,COMPCD                                                   
         MVC   OFFKOFF(2),ACCOFF                                                
         DROP  R6                                                               
*                                                                               
         L     R6,AIO3                                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',WKACCKEY,(R6)                
         CLI   8(R1),0                                                          
         BNE   CKAOF98             ERR: INVALID ACC OFFICE CODE                 
*                                                                               
CKAOF88  DS    0H                  OFFICE CODE IS GOOD                          
         MVC   PPRDAOFC,ACCOFF     SAVE OFFICE CODE                             
         MVC   PPRDACCA,POWCODE    SAVE AGY CODE                                
         CLI   ASW,0               SWITCHED TO AN ACC SYSTEM?                   
         JE    SETCCEQ                                                          
         BRAS  RE,RETURN                                                        
         J     SETCCEQ                                                          
*                                                                               
CKAOF90  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(32),=C'** ERROR - INVALID OFFICE LENGTH'                 
         OI    CONHEADH+6,X'80'                                                 
         J     SETCCNEQ                                                         
*                                                                               
CKAOF91  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(39),=C'** ERROR - 2 CHARS OFFICE CODE REQUIRED'          
         B     CKAOF99X                                                         
*                                                                               
CKAOF92  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(38),=C'** ERROR - 1 CHAR OFFICE CODE REQUIRED'           
         B     CKAOF99X                                                         
*                                                                               
CKAOF93  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(31),=C'** ERROR - INVALID AGENCY CODE'                   
         B     CKAOF99X                                                         
*                                                                               
CKAOF94  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(31),=C'** ERROR - INVALID ACC AGY CODE'                  
         B     CKAOF99X                                                         
*                                                                               
CKAOF95  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(33),=C'** ERROR - ACC SYSTEM IS NOT OPEN'                
         B     CKAOF99X                                                         
*                                                                               
CKAOF96  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(38),=C'** ERROR - CANNOT SWITCH TO ACC SYSTEM'           
         B     CKAOF99X                                                         
*                                                                               
CKAOF97  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(36),=C'** ERROR - ACC COMPANY REC NOT FOUND'             
         BRAS  RE,RETURN                                                        
         B     CKAOF99X                                                         
*                                                                               
CKAOF98  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(31),=C'** ERROR - INVALID ACC OFF CODE'                  
         BRAS  RE,RETURN                                                        
*                                                                               
CKAOF99X OI    CONHEADH+6,X'80'                                                 
         J     SETCCNEQ                                                         
*                                                                               
RETURN   LR    R5,RE               SAVE RETURN REGISTER                         
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SYSSW       ORIGINAL SYS                                 
         GOTO1 SWITCH,DMCB         SWITCH BACK                                  
         CLI   4(R1),0             ANY ERRORS?                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DATADISP,=H'33'     FOR PRINT                                    
         LR    RE,R5                                                            
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB,R4                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKPPTRS  NTR1  BASE=*,LABEL=*      CK FOR PASSIVE POINTERS                      
*                                                                               
         MVC   SVDMINBT,DMINBTS    SAVE ORIGINAL DM IN/OUT BYTES                
         MVC   SVDMOUTB,DMOUTBTS                                                
         MVI   DMINBTS,X'08'       SET TO PASS DELETES                          
         MVI   DMOUTBTS,0          SUPPRESS DATAMGR ERRORS                      
         MVC   SVWORK(L'KEY),KEY   SAVE KEY                                     
         MVC   SVWORK+L'KEY(4),AIO SAVE ORIGINAL AIO                            
         MVC   DUB+4(4),KEY+27     DISK ADDRESS                                 
*                                                                               
         MVC   AIO,AIO3                                                         
         ICM   R6,15,SVWORK+L'KEY  POINT TO ORIGINAL AIO                        
         MVI   ELCODE,X'06'        PRD MAIN ELEM CODE                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE!                               
         USING PPRDELEM,R6                                                      
*                                                                               
         CLC   WKADJCD,PPRDEXCL    ADJACENCY CODE CHANGED?                      
         BE    CKPPT50                                                          
         OC    WKADJCD,WKADJCD     SAVED ADJACENCY CODE PRESENT?                
         BZ    CKPPT20                                                          
*                                                                               
* OLD ADJ CODE(S) PRESENT, NEED TO REMOVE PASSIVE POINTER(S)                    
*                                                                               
         LA    R4,3                THREE ADJ CODE TO BE PROCESSED               
         LA    R3,WKADJCD                                                       
CKPPT10  CLI   0(R3),0             ANY CODE TO BE ADDED AS POINTER?             
         BE    CKPPT15                                                          
         XC    KEY,KEY                                                          
         ICM   RE,15,SVWORK+L'KEY  POINT ORIGINAL AIO                           
         USING PPRDKEY,RE                                                       
         MVC   KEY(07),PPRDKEY      AGY/MED/RTYPE/CLT                           
         MVI   KEY+03,PADJKIDQ      RECORD CODE                                 
         MVC   KEY+07(01),0(R3)     ADJ CODE                                    
         MVC   KEY+08(03),PPRDKPRD  PRD                                         
         DROP  RE                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                PASSIVE POINTER MUST BE FOUND                
         OI    KEY+25,X'FF'        DELETE POINTER                               
         GOTO1 WRITE                                                            
*                                                                               
CKPPT15  LA    R3,1(R3)            POINT TO NEXT CODE                           
         BCT   R4,CKPPT10                                                       
*                                                                               
CKPPT20  OC    PPRDEXCL,PPRDEXCL   NEW ADAJENCY CODE PRESENT?                   
         BZ    CKPPT50                                                          
*                                                                               
* NEW ADJ CODE(S) PRESENT, NEED TO ADD PASSIVE POINTER(S)                       
*                                                                               
         LA    R4,3                                                             
         LA    R3,PPRDEXCL                                                      
CKPPT25  CLI   0(R3),0                                                          
         BE    CKPPT40                                                          
         XC    KEY,KEY                                                          
         ICM   RE,15,SVWORK+L'KEY  POINT ORIGINAL AIO                           
         USING PPRDKEY,RE                                                       
         MVC   KEY(07),PPRDKEY      AGY/MED/RTYPE/CLT                           
         MVI   KEY+03,PADJKIDQ      RECORD CODE                                 
         MVC   KEY+07(01),0(R3)     ADJ CODE                                    
         MVC   KEY+08(03),PPRDKPRD  PRD                                         
         DROP  RE                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE     FOUND?                                       
         BE    CKPPT30             IF YES, UNDELETE IT, ELSE ADD IT             
         XC    KEY,KEY                                                          
         MVC   KEY(25),KEYSAVE                                                  
         MVC   KEY+27(4),DUB+4     DISK ADDRESS                                 
         GOTO1 ADD                                                              
         B     CKPPT40                                                          
*                                                                               
CKPPT30  MVI   KEY+25,0            UNDELETE POINTER                             
         MVC   KEY+27(4),DUB+4                                                  
         GOTO1 WRITE                                                            
*                                                                               
CKPPT40  LA    R3,1(R3)                                                         
         BCT   R4,CKPPT25          NEXT ADJACENCY CODE                          
*                                                                               
* CK IF PRD OFFICE OR TRAFFIC CODE PASSIVE POINTERS NEED TO BE ADDED            
*                                                                               
CKPPT50  MVC   DUB+0(1),WKPOFFCD   OLD PRD OFFICE CODE                          
         MVC   DUB+1(1),PPRDOFFC   NEW PRD OFFICE CODE                          
         MVI   DUB+2,POFPKIDQ      PRD OFFICE PTR REC CODE, X'A3'               
         BRAS  RE,ADDOTPP                                                       
*                                                                               
         MVC   DUB+0(1),WKPTRACD   OLD PRD TRAFFIC CODE                         
         MVC   DUB+1(1),PPRDTRAF   NEW PRD TRAFFIC CODE                         
         MVI   DUB+2,PTRPKIDQ      PRD TRAFFIC PTR REC CODE, X'A4'              
         BRAS  RE,ADDOTPP                                                       
*                                                                               
         MVC   DMINBTS,SVDMINBT    RESTORE DMINBTS AND DMOUTBTS                 
         MVC   DMOUTBTS,SVDMOUTB                                                
         MVC   KEY,SVWORK          RESTORE KEY                                  
         MVC   AIO,SVWORK+L'KEY    RESTORE AIO                                  
*                                                                               
         J     EXIT                                                             
         DROP  R6                  FOR PPRDELEM                                 
*                                                                               
ADDOTPP  ST    RE,FULL             SAVE RETURN ADDRESS                          
         CLI   ACTNUM,ACTADD       ADD?                                         
         BE    ADDOTP52            CK IF NEW CODE NEED TO BE ADDED              
         CLC   DUB+0(1),DUB+1      OFFICE/TRAFFICE CODES CHANGED?               
         BE    ADDOTP58            NO, DON'T BOTHER WITH PPOINTER               
         CLI   DUB+0,0             OLD CODE IS PRESENT?                         
         BE    ADDOTP52            NO, CK NEW CODE                              
*                                                                               
         XC    KEY,KEY                                                          
         ICM   RE,15,SVWORK+L'KEY  POINT ORIGINAL AIO                           
         USING PPRDKEY,RE                                                       
         MVC   KEY+0(3),PPRDKAGY   AGENCY AND MEDIA                             
         MVC   KEY+3(1),DUB+2      PASSIVE POINTER REC CODE                     
         MVC   KEY+4(3),PPRDKCLT   CLT                                          
         MVC   KEY+7(1),DUB+0      OLD OFFICE OR OLD TRAFFICE CODE              
         MVC   KEY+8(3),PPRDKPRD   PRODUCT                                      
         DROP  RE                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                PASSIVE POINTER MUST BE FOUND                
         OI    KEY+25,X'FF'        DELETE POINTER                               
         GOTO1 WRITE                                                            
*                                                                               
ADDOTP52 CLI   DUB+1,0             NEW CODE IS PRESENT?                         
         BE    ADDOTP58                                                         
*                                                                               
         XC    KEY,KEY                                                          
         ICM   RE,15,SVWORK+L'KEY  POINT ORIGINAL AIO                           
         USING PPRDKEY,RE                                                       
         MVC   KEY(03),PPRDKEY     AGENCY AND MEDIA                             
         MVC   KEY+03(01),DUB+2    PASSIVE POINTER REC CODE                     
         MVC   KEY+04(03),PPRDKCLT                                              
         MVC   KEY+07(01),DUB+1    NEW OFFICE OR NEW TRAFFICE CODE              
         MVC   KEY+08(03),PPRDKEY+7                                             
         DROP  RE                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE     ALREADY EXIST IN DIRECTORY?                  
         BE    ADDOTP56            YES                                          
*                                                                               
         XC    KEY,KEY             IF NOT FOUND, ADD PPOINTER                   
         MVC   KEY(25),KEYSAVE                                                  
         MVC   KEY+27(04),DUB+4    DISK ADDRESS                                 
         GOTO1 ADD                                                              
         B     ADDOTP58                                                         
*                                                                               
ADDOTP56 MVI   KEY+25,0            UNDELETE POINTER                             
         MVC   KEY+27(04),DUB+4                                                 
         GOTO1 WRITE                                                            
*                                                                               
ADDOTP58 L     RE,FULL             GET RETURN ADDRESS                           
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* BUMP I/O COUNTER AND CHECK FOR MAX I/O                                        
*                                                                               
* CC NOT EQUAL - MAX I/O REACHED                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKMAXIO  NTR1  BASE=*,LABEL=*                                                   
         TM    WHEN,X'20'          SOON?                                        
         JNZ   SETCCEQ                                                          
         TM    WHEN,X'10'          OV?                                          
         JNZ   SETCCEQ                                                          
*                                                                               
         LH    RF,IO_COUNT                                                      
         CHI   RF,300              CHECK EVERY 300 I/O'S                        
         BL    CKMXIO50                                                         
*                                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         ICM   R3,3,FATMAXIO       MAX IO'S                                     
         MHI   R3,MXIOPCT          GET % OF MAX                                 
         D     R2,=F'100'                                                       
         CLM   R3,3,FATIOCNT       STILL LESS THAN MAX%?                        
         BH    *+12                                                             
         OI    CNTLSW,C_MAXIOQ                                                  
         J     SETCCNEQ            MAX I/O EXCEEDED                             
*                                                                               
         SR    RF,RF               RESET COUNT FOR ANOTHER 300                  
*                                                                               
CKMXIO50 AHI   RF,1                                                             
         STH   RF,IO_COUNT                                                      
         J     SETCCEQ                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB,R1                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CLRLSSCR NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         SR    RE,RE                                                            
         IC    RE,NLISTS           NUMBER OF LIST LINES                         
         LA    RF,LSTSELH          POINT TO FIRST LINE                          
         SR    R1,R1                                                            
CLRLS20  XC    8(03,RF),8(RF)                                                   
         OI    6(RF),X'80'                                                      
         IC    R1,0(RF)                                                         
         AR    RF,R1                                                            
         XC    8(74,RF),8(RF)                                                   
         OI    6(RF),X'80'                                                      
         IC    R1,0(RF)                                                         
         AR    RF,R1                                                            
         BCT   RE,CLRLS20                                                       
*                                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* SET ERROR FOR LIST SCREEN, ERROR WILL BE RETURNED IN CONHEAD                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SET_LSER NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    CNTLSW,C_MAXIOQ     MAX I/O REACHED?                             
         BZ    SLSER50                                                          
         LHI   R2,64               TOO MANNY RECORDS                            
         BRAS  RE,GET_ITXT                                                      
         J     EXIT                                                             
*                                                                               
SLSER50  DS    0H                  FOR FUTURE ERRORS                            
*                                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PUTREQRC NTR1  BASE=*,LABEL=*      PUT A REQUEST CARD FOR T/A REPORT            
*                                                                               
         XC    QCTL,QCTL                                                        
         MVC   QAREA,SPACES                                                     
         MVC   QAREA(2),=C'41'                                                  
         MVC   QAREA+2(2),AGENCY                                                
         MVC   QAREA+4(1),QMED                                                  
         MVC   QAREA+5(3),QCLT                                                  
         MVC   QAREA+11(3),QPRD                                                 
         MVC   QAREA+68(7),=C'AUTOREQ'                                          
*                                                                               
         MVI   QCTL+10,41                                                       
         MVI   QCTL+14,106                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'PREQUEST',QCTL,QCTL                    
*                                                                               
         TM    DMCB+8,X'FF'                                                     
         JZ    SETCCEQ                                                          
         J     SETCCNEQ                                                         
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
SETSAP   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING CT5REC,R4                                                        
*                                                                               
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,AGENCY                                                  
         DROP  R4                                                               
*                                                                               
         L     R0,AIO              SAVE CURRENT AIO                             
         MVC   AIO,AIO2            USE IO2 AS IO1 USED BY KEYMERGE!             
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',WORK,AIO                  
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   SAPAGY,C'N'                                                      
         L     R4,AIO                                                           
         LA    R4,CT5DATA-CT5REC(R4)                                            
         SR    RE,RE                                                            
*                                                                               
SETSAP2  CLI   0(R4),0                                                          
         JE    SETSAP10                                                         
         CLI   0(R4),X'B4'                                                      
         BE    SETSAP4                                                          
         IC    RE,1(R4)                                                         
         AR    R4,RE                                                            
         B     SETSAP2                                                          
*                                                                               
         USING CTAGDD,R4                                                        
*                                                                               
SETSAP4  TM    CTAGOPTS,CTAGSAP    TEST SAP AGY                                 
         JZ    SETSAP10                                                         
         MVI   SAPAGY,C'Y'                                                      
         J     SETSAPX                                                          
         DROP  R4                                                               
                                                                                
*=========================================================                      
* NOT AN SAP AGENCY - CLEAR TITLE AND PROTECT SAP FIELD                         
*=========================================================                      
                                                                                
SETSAP10 XC    PRDSAP,PRDSAP       SAP INPUT FIELD                              
         OI    PRDSAPH+6,X'80'                                                  
         OI    PRDSAPH+1,X'20'     SET TO PROTECTED                             
*                                                                               
         XC    PRDSAPI,PRDSAPI     SAP TITLE                                    
         OI    PRDSAPIH+6,X'80'                                                 
*                                                                               
SETSAPX  ST    R0,AIO              RESTORE SAVED AIO                            
         J     EXIT                                                             
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
       ++INCLUDE PRSFMA0D          PRODUCT MAINT SCREEN                         
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMA1D          PRODUCT LIST SCREEN                          
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE PRSFMWORKD                                                     
         ORG   SYSSPARE            WORKING AREA                                 
*                                                                               
CLTLSFLT DS    CL3                 LIST FILTER ON CLT CODE                      
PRDLSFLT DS    CL3                 LIST FILTER ON PRD CODE                      
SVWORK   DS    XL64                GENERAL WORKING STORAGE                      
SVCLTLSC DS    CL3                 SAVE CLIENT CODE ON LIST                     
WKCDATE  DS    CL6                 WORKING CHARACTER DATE (MMDDYY)              
*                                                                               
SVDMINBT DS    X                                                                
SVDMOUTB DS    X                                                                
*                                                                               
CNTLSW   DS    XL1                 CONTROL SWTICH                               
C_MAXIOQ EQU   X'08'               MAX I/O IS REACHED                           
*                                                                               
IO_COUNT DS    H                   I/O COUNTER                                  
MXIOPCT  EQU   30                  % OF MAX IO ALLOWED                          
*                                                                               
SVSECAGY DS    XL2                                                              
SV_B_PID DS    XL2                                                              
*                                                                               
SAVERE   DS    F                   FOR SAVING RETURN ADDRESSES                  
QCTL     DS    CL26                                                             
QAREA    DS    CL80                                                             
*                                                                               
WKTMPFLD DS    XL11                HDR IS 8 AND 3 INPUT CHARS                   
WKTMPKEY DS    XL(L'KEY)                                                        
WKTMPAIO DS    XL(L'AIO)                                                        
WKTMPION DS    XL(L'USEIONUM)                                                   
*                                                                               
WKSVSTRT EQU   *                                                                
WKPOFFCD DS    XL(L'PPRDOFFC)      PRD OFFICE CODE                              
WKPTRACD DS    XL(L'PPRDTRAF)      PRD TRAFFIC CODE                             
WKADJCD  DS    CL(L'PPRDEXCL)      ADJACENCY CODES                              
WKPRDOAN DS    CL(L'PPRDOAN)       OTHER AGY NAME CODE                          
WKPRBILL DS    CL(L'PPRDBILP)      PRD BILLING PROFILE                          
WKPRSAP  DS    CL(L'PPSAPCODE)     SAP CODE                                     
WKSVLNQ  EQU   *-WKSVSTRT                                                       
*                                                                               
* 1ST FLD IS FILTER THEN FOLLOWED BY 1 BYTE POSITION COUNTER                    
* WILDCARD IND USED TO AVOID BIT CONFLICT, ALSO FOR NEGATIVE FILTERING          
*                                                                               
F_FLDS   DS    0X                  FILTER FLDS START                            
*                                                                               
F_OFC    DS    XL(L'PPRDOFFC)      PRD OFFICE CODE FILTER                       
         DS    X                                                                
F_TOF    DS    XL(L'PPRDTRAF)      PRD TRAFFIC OFFICE CODE FILTER               
         DS    X                                                                
F_NOTRAF DS    XL(L'PPRDSTAT)      TRAFFIC?  Y/N (X'20' = NO TRAFFIC)           
         DS    X                                                                
F_NOTR_W DS    C                   WILDCARD CHAR (AVOID BITS CONFLICT)          
F_BASEA  DS    XL(L'BILBASA)       PRD BILLING FORMULA PROF - BASE A            
         DS    X                                                                
F_BASA_W DS    C                   WILDCARD CHAR (AVOID BITS CONFLICT)          
F_OAN    DS    XL(L'PPRDOAN)       OTHER AGY NAME                               
         DS    X                                                                
F_EXCL   DS    XL(L'PPRDEXC)       EXCLUSION CLASS BIT(S)                       
         DS    X                                                                
F_EXCL_W DS    C                   WILDCARD CHAR (AVOID BITS CONFLICT)          
F_GST    DS    XL(L'PPRDGST)       CANADIAN GST TAX CODE                        
         DS    X                                                                
F_PST    DS    XL(L'PPRDPSTC)      CANCDIAN PST CODE                            
         DS    X                                                                
F_AOF    DS    XL(L'PPRDAOFC)      PRD ACC OFFICE CODE FILTER                   
         DS    X                                                                
F_AGY    DS    XL(L'PPRDACCA)      PRD ACC OFFICE AGY FILTER                    
         DS    X                                                                
F_ADJCDS DS    XL(L'PPRDEXCL)      PRD EXLUSION CODE (ADJACENCY CODES)          
         DS    X                                                                
F_DIV    DS    XL(L'PPRDDIV)       DIVISION CODE                                
         DS    X                                                                
F_BILLF  DS    X                   BILL FORMULA (WILDCARD ONLY)                 
         DS    X                                                                
F_LEGALW DS    X                   LEGAL WARNING (WILDCARD ONLY)                
         DS    X                                                                
F_USER1  DS    X                   USER DEF 1 (WILDCARD ONLY)                   
         DS    X                                                                
F_USER2  DS    X                   USER DEF 2 (WILDCARD ONLY)                   
         DS    X                                                                
F_FLDLNQ EQU   *-F_FLDS                                                         
*                                                                               
* SECURITY VALUES ARE TRANSLATED AS FOLLOW:                                     
* C'Y'   READ ONLY (FIELD WILL BE PROTECTED)                                    
* C'N'   NO ACCESS (FIELD WILL BE HIDDEN AND PROTECTED)                         
* X'00'  WRITE                                                                  
*                                                                               
SECVALS  DS    0X                  ** FIELD SECURITY VALUES **                  
*                                                                               
SECPUD1  DS    C                   PRD USER DEFINITION FLD 1                    
SECPUD2  DS    C                   PRD USER DEFINITION FLD 2                    
SECLGWR  DS    C                   LEGAL WARNING                                
SECPCED  DS    C                   BILL ON PC EFFECTIVE DATE                    
*                                                                               
SECVALSL EQU   *-SECVALS           MAX IS 255                                   
*                                                                               
* STORAGE AREA TO BE USED BY CKAOFF ROUTINE                                     
*                                                                               
AOFFWKSS EQU   *                                                                
SVACCOFC DS    CL1                 SET TO 'Y' IF SVAGPF12 IS A OR B             
COMPCD   DS    CL1                                                              
ASW      DS    XL1                 SET TO X'01' IF I SWITCHED TO ACC            
WKACCKEY DS    CL42                                                             
CTKEY    DS    CL28                                                             
SENUM    DS    XL1                                                              
SYSSW    DS    XL1                                                              
POWCODE  DS    CL2                                                              
ACCOFF   DS    CL2                                                              
OFFLEN   DS    XL1                                                              
AOFFWKSX EQU   *-AOFFWKSS                                                       
*                                                                               
SVPUSER1 DS    CL(L'PUSER1)        SAVED USER DESCRIPTION 1                     
SVPUSER2 DS    CL(L'PUSER2)        SAVED USER DESCRIPTION 2                     
*                                                                               
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PCLTREC           CLIENT RECORD                                
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPRDREC           PRODUCT RECORD                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE PDIVREC           DIVISION RECORD                              
         EJECT                                                                  
*                                                                               
       ++INCLUDE PBILPROF          BILLING FORMULA PROFILE                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE PCONREC           CONTRACT RECORD                              
         EJECT                                                                  
*                                                                               
       ++INCLUDE PGENGRP           CLIENT/PRODUCT/PUB GROUP RECORDS             
         EJECT                                                                  
*                                                                               
       ++INCLUDE FLDIND            FIELD INDICATOR EQUATES                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDPSTBLK          BLOCK FOR PST VALIDATION CALL                
         EJECT                                                                  
*                                                                               
       ++INCLUDE FAFACTS           MASTER SYS INFO BLOCK                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE CTGENFILE         DSECT FOR CONTROL FILE RECORDS               
         EJECT                                                                  
*                                                                               
       ++INCLUDE ACGENFILE         DSECT FOR OFFICE RECORDS                     
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGLVXCTLD        GLOBBER TRANSFER CONTROLS                    
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGLOBEQUS        DSECT FOR GLOBBER                            
         EJECT                                                                  
*                                                                               
       ++INCLUDE POTHAGY           OTHER AGENCY NAME & ADDRESS REC              
         EJECT                                                                  
*                                                                               
       ++INCLUDE PADJPRDPP         PRD ADJACENCY CODE PASSIVE POINTER           
         EJECT                                                                  
*                                                                               
       ++INCLUDE POFFPRDPP         PRD OFFICE PASSIVE POINTER                   
         EJECT                                                                  
*                                                                               
       ++INCLUDE PTRAPRDPP         PRD TRAFFIC PASSIVE POINTER                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDSCANBLKD        DSECT FOR SCANNER                            
F_SMAXQ  EQU   10                  MAX NUM OF FILTER SCANNER ENTRIES            
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDPERVALD         DSECT FOR PERVAL                             
         EJECT                                                                  
*                                                                               
       ++INCLUDE FASECRETD                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE SEACSFILE         SECURITY SYSTEM RECORD                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE FAUTL             USER TERMINAL LIST                           
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSCLTC   DS    CL(L'PCLTKCLT)      CLT CODE                                     
         DS    CL2                                                              
LSPRDC   DS    CL(L'PPRDKPRD)      PRD CODE                                     
         DS    C                   /                                            
LSPRD    DS    CL(L'PPRDNAME)      ENTIRE PRODUCT NAME                          
         DS    CL2                                                              
*                                                                               
LSFSTART DS    0X                  FILTER DATA START HERE                       
*                                                                               
LSOFC    DS    CL(L'PPRDOFFC)      PRD OFFICE CODE                              
         DS    CL3                                                              
LSAOF    DS    CL(L'PPRDAOFC)      PRD ACCOUNT OFFICE CODE                      
LSAOFASP DS    CL1                 /                                            
LSAGY    DS    CL(L'PPRDACCA)      PRD ACCOUNT OFFICE AGY                       
         DS    CL3                                                              
LSTOF    DS    CL(L'PPRDTRAF)      TOF                                          
         DS    CL3                                                              
LSBASEA  DS    CL5                 BASE FORMULA A (G,N,G-CD,N-CD,AC)            
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
REP_LINE DSECT                     REPORT DSECT FOR PRINTING LINES              
*                                                                               
R_PRD    DS    CL(L'PPRDKPRD)      PRD CODE                                     
         DS    XL2                                                              
R_PRDNM  DS    CL(L'PPRDNAME)      PRD NAME                                     
         DS    XL2                                                              
R_PRDBNM DS    CL(L'PPRDBILL)      BILL RECEIPT NAME                            
         DS    XL2                                                              
R_PRDLN1 DS    CL(L'PPRDLIN1)      ADDRESS - LINE 1                             
         DS    XL2                                                              
R_PRDLN2 DS    CL(L'PPRDLIN2/2)    ADDRESS - LINE 2                             
         DS    XL2                                                              
R_PRDATN DS    CL(L'PPRDATTN/2)    ATTENTION OF                                 
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
UDEFD    DSECT                     USER DEFINITION FLDS                         
UDEF     DS    CL20                                                             
UTYPE    DS    CL1                                                              
ULEN     DS    XL1                                                              
UFLG1    DS    XL1                                                              
UFLG2    DS    XL1                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
LS_TTL3  DSECT                     DEFAULT LIST TITLE LINES                     
*                                                                               
LTPRDC   DS    CL(L'PPRDKPRD)      PRD CODE                                     
         DS    C                   /                                            
LTPRD    DS    CL(L'PPRDNAME)      PRD NAME                                     
         DS    CL2                                                              
*                                                                               
LTFSTRTQ EQU   *-LS_TTL3+3+2+3+2   SEL  CLT  (10 CHARS)                         
LTFSTART DS    0X                  FILTER TITLE START HERE                      
*                                                                               
LTOFC    DS    CL3                 OFC                                          
         DS    CL1                                                              
LTAOF    DS    CL3                 AOF                                          
LTAOFASP DS    C                   /                                            
LTAGY    DS    CL3                 AGY                                          
         DS    CL1                                                              
LTTOF    DS    CL3                 TOF                                          
         DS    CL1                                                              
LTBASEA  DS    CL5                 BASEA                                        
*                                                                               
LTFFLDQ  EQU   80-1-LTFSTRTQ       LENGTH OF FLT DISPLAY AREA                   
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'247PRSFM1E   10/28/20'                                      
         END                                                                    
