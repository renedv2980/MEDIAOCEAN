*          DATA SET PRSFM1C    AT LEVEL 248 AS OF 08/31/17                      
*PHASE T41C1CA                                                                  
*INCLUDE PPBROWSE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*               T41C1C - CLIENT MAINT/LIST                                      
*                                                                               
* KWAN 03/02/15 Do not allow one character client code                          
*                                                                               
* KWAN 07/05/12 GETFACT - use address passed back by GETFACT call               
*                                                                               
* SMYE 08/18/10 FIX HANDLING OF 5-DIGIT INTERFACE NUMBER (PCLTNUM)              
*                                                                               
* SMYE 07/07/10 ALLOW ONLY ALPHANUMERIC NEW CLIENT CODES (ADDS)                 
*                                                                               
* KWAN 05/14/10 New logic for Main PST element                                  
*                                                                               
* SMYE 11/06/09 DISALLOW EMBEDDED BLANKS IN NEW CLIENT CODES (ADDS)             
*                                                                               
* BPLA 04/08    DON'T CLEAR CLTFIN - DOING SO CAUSED ERRONEOUS ERRORS           
*                                                                               
* BPLA 10/07    ALLOW FILTERING ON UCOMM BILL CONTROL                           
*                                                                               
* KWAN 11/19/07 FIX ACC OFFICE VALIDATION WHEN SWITCHING TO ACC SYSTEM          
*                                                                               
* KWAN 07/23/07 VALIDATE ACC OFFICE CODE PER ACC'S COMPANY RECORD               
*                                                                               
* SMYE 05/07    FIX BUG IN PRD ASSIGN VALIDATION AT VR66K AND CORRECT           
*               "DISPLACEMENT WITH NO BASE REGISTER" ERROR AT VR                
*                                                                               
* KWAN 05/31/06 ENABLE LIST FILTER FOR REPORT ACTION                            
*                                                                               
* BOBY 04/06    FIX BUG IN ACC OFFICE VALIDATION                                
*                                                                               
* SMYE 11/05    ADD TRAFFIC OFFICE TO 2-CHAR MODIFICATIONS                      
*                                                                               
* KWAN 10/13/05 TWO CHARACTER OFFICE CODE MODIFICATIONS                         
*                                                                               
* KWAN 06/16/05 FOR ZENITH, NEED TO CHECK CONTROL FILE BEFORE ADDING            
*                                                                               
* KWAN 06/03/05 BROWSE FUNCTION & SPECIAL CHARS FIX ON ADD                      
*                                                                               
* KWAN 03/16/05 CK MAX I/O WHEN FILTERING LARGE AMT OF CLT DATA                 
*                                                                               
* KWAN 02/02/05 NEED TO GENERATE AN AUTO P41 T/A REPORT                         
*                                                                               
* KWAN 01/24/05 FIX PST AND GST CODE DISPLAY                                    
*                                                                               
* KWAN 11/19/02 CONVERT CLIENT FROM FIL TO SFM                                  
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
         TITLE 'T41C1C - CLIENT MAINT/LIST'                                     
*                                                                               
T41C1C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C1C,R7,RR=R3                                                
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         USING OFFICED,OFCBLK                                                   
*                                                                               
         BRAS  RE,INITIALZ                                                      
         CLI   MODE,NEWSCR         SCR JUST BEING LOADED?                       
         JE    EXIT                YES, DISPLAY INIT'D SCR                      
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
         CLI   MODE,XRECADD        AFTER ADD                                    
         BE    PPTRS                                                            
         CLI   MODE,XRECPUT        AFTER PUT                                    
         BE    PPTRS                                                            
*                                                                               
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
         LA    R2,CLTMEDH          POINT TO MEDIA FLD                           
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
         MVC   CLTMEDN,MEDNM                                                    
         OI    CLTMEDNH+6,X'80'    NOT DISPLAYED FOR LIST AND REPORT            
*                                                                               
VK10     XC    SVKEY,SVKEY                                                      
         LA    R6,SVKEY                                                         
         USING PCLTKEY,R6                                                       
*                                                                               
         MVC   PCLTKAGY,AGENCY                                                  
         MVC   PCLTKMED,QMED                                                    
         MVI   PCLTKRCD,X'02'      RECORD CODE FOR CLIENT                       
*                                                                               
         XC    CLTLSFLT,CLTLSFLT   CLEAR CLT LIST FILTER                        
         LA    R2,CLTCLTH          POINT TO CLIENT FLD                          
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
         BE    VK70                                                             
         CLC   =C'ALL',8(R2)       ALL CLIENT?                                  
         BE    VK70                YES, LEAVE CLIENT FILTER NULLS               
         ZIC   RE,5(R2)            INPUT LENGTH                                 
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CLTLSFLT(0),8(R2)   GET CLIENT CODE                              
         OC    CLTLSFLT,SPACES     SPACE PADDED                                 
         B     VK70                                                             
*                                                                               
VK30     CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BNE   VK40                                                             
*                                                                               
         CLI   5(R2),1             At leat two characters?                      
         JNH   INVFDERR                                                         
         CLC   =C'ALL',8(R2)                                                    
         JE    INVFDERR            CANNOT ADD CLT CODE "ALL"                    
         ZIC   RE,5(R2)            INPUT LENGTH                                 
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PCLTKCLT(0),8(R2)   GET CLIENT CODE                              
         OC    PCLTKCLT,SPACES     SPACE PADDED                                 
         MVC   QCLT,PCLTKCLT       FOR AUTOREQ                                  
*                                                                               
         CLI   PCLTKCLT+1,C' '                                                  
         BH    VK30D               CONTINUE                                     
         CLI   PCLTKCLT+2,C' '                                                  
         BH    INVFDERR            "MIDDLE" EMBEDDED BLANK FOUND                
*                                                                               
VK30D    DS    0H                                                               
         CLI   8(R2),C' '                                                       
         JNH   INVFDERR            1ST CHARACTER CANNOT BE BLANK                
*                                                                               
         ZIC   R1,5(R2)            INPUT LENGTH AS LUP COUNTER                  
         LA    R4,8(R2)            POINT TO CLIENT CODE ENTRY                   
*                                                                               
* ONLY A THRU Z AND 0 THRU 9 ARE VALID CHARACTERS OF CLIENT CODE                
*                                                                               
VK30L    DS    0H                                                               
         CLI   0(R4),C'A'                                                       
         JL    INVFDERR             INVALID 1ST CHARACTER                       
         CLI   0(R4),C'I'                                                       
         BNH   VK30X                OK                                          
         CLI   0(R4),C'J'                                                       
         JL    INVFDERR             INVALID 1ST CHARACTER                       
         CLI   0(R4),C'R'                                                       
         BNH   VK30X                OK                                          
         CLI   0(R4),C'S'                                                       
         JL    INVFDERR             INVALID 1ST CHARACTER                       
         CLI   0(R4),C'Z'                                                       
         BNH   VK30X                OK                                          
         CLI   0(R4),C'0'                                                       
         JL    INVFDERR             INVALID 1ST CHARACTER                       
         CLI   0(R4),C'9'                                                       
         JH    INVFDERR             INVALID 1ST CHARACTER                       
*                                                                               
VK30X    DS    0H                                                               
         LA    R4,1(R4)                                                         
         BCT   R1,VK30L                                                         
*                                                                               
VK36     CLC   SVCTAGY,=C'  '      CONTROL FILE ID PRESENT?                     
         BNH   VK70                                                             
         BRAS  RE,CKCTFCLT         CK CONTROL FILE CLIENT                       
         BE    VK70                                                             
         L     R2,FULL             ERROR MSG NUMBER                             
         BRAS  RE,GET_ETXT                                                      
         LA    R2,CLTCLTH                                                       
         J     TRAPERR2            ERROR OR COMPLETION MSG IS SET               
*                                                                               
VK40     GOTO1 VALICLT                                                          
         MVC   PCLTKCLT,QCLT                                                    
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    VK70                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BE    VK70                                                             
         MVC   CLTCLTN,CLTNM                                                    
         OI    CLTCLTNH+6,X'80'    NOT DISPLAYED FOR LIST AND REPORT            
*                                                                               
VK70     LA    R0,F_FLDS                                                        
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
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VR       DS    0H                  VALIDATE DATA FOR PROCTER $ GAMBLE           
         CLI   ACTNUM,ACTSEL       SEL?                                         
         BNE   VR05                                                             
*                                                                               
* FOLLOWING CODE TO PREVENT USERS FROM DESTROYING RECORD(S) THEY                
* DO NOT HAVE ACCESS TO (VALICLT IN DK MODE IS NOT GOOD ENOUGH)                 
*                                                                               
         MVC   SVWORK(L'KEY),KEY                                                
         MVC   SVWORK+L'KEY(L'AIO),AIO                                          
         MVI   USEIONUM,2                                                       
         LA    R2,CLTCLTH                                                       
         GOTO1 VALICLT             NEED TO REVALIDATE CLT FOR SECURITY          
         MVI   USEIONUM,1                                                       
         MVC   KEY,SVWORK          RESTORE KEY                                  
         MVC   AIO,SVWORK+L'KEY    RESTORE AIO                                  
         GOTO1 GETREC              RESTORE GENCON'S PUTREC SEQUENCE             
*                                                                               
VR05     L     R6,AIO                                                           
         XC    WKCLTFIN,WKCLTFIN   CLT FINANCIAL BYTE                           
         XC    WKCLSTAT,WKCLSTAT   CLT STATUS BYTE                              
         XC    WKCLPROF,WKCLPROF   CLT PROFILE VALUES                           
         XC    SVCOFFC,SVCOFFC     CLT OFFICE CODE (ORIGINAL VALUE)             
*                                                                               
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    VR10                                                             
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                FIRST ELEM MUST BE THERE!                    
         USING PCLTELEM,R6                                                      
         MVC   WKCLTFIN,PCLTFIN    CLT FINANCIAL BYTE                           
         MVC   WKCLSTAT,PCLTSTAT   CLT STATUS BYTE                              
         MVC   WKCLPROF,PCLTPROF   CLT PROFILE VALUES                           
         MVC   SVCOFFC,PCLTOFF     CLT OFFICE CODE (ORIGINAL VALUE)             
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'02'                                                     
         GOTO1 REMELEM             REMOVE X'02' ELEM (MAIN ELEM)                
         MVI   ELCODE,X'10'                                                     
         GOTO1 REMELEM             REMOVE X'10' ELEM (STD COMMENT NUMB)         
         MVI   ELCODE,X'11'                                                     
         GOTO1 REMELEM             REMOVE X'11' ELEM (I/O COMMENT NUMB)         
         MVI   ELCODE,X'25'                                                     
         GOTO1 REMELEM             REMOVE X'25' ELEM (PST CODE ELEM)            
         MVI   ELCODE,PCLTMPEQ                                                  
         GOTO1 REMELEM             Remove Main PST element                      
         MVI   ELCODE,X'30'                                                     
         GOTO1 REMELEM             REMOVE X'30' ELEM (DRD CLT ELEM)             
*                                                                               
VR10     XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING PCLTELEM,R6                                                      
         MVI   0(R6),X'02'         MAIN ELEM CODE                               
         MVI   1(R6),X'AA'         ELEM LENGTH                                  
*                                                                               
         LA    R2,CLTCNAMH         CLIENT NAME                                  
         GOTO1 ANY                                                              
         XC    PCLTNAME,PCLTNAME                                                
         MVC   PCLTNAME,CLTCNAM                                                 
*                                                                               
         LA    R2,CLTBRNH          BILL RECEIPT NAME                            
         XC    PCLTBNAM,PCLTBNAM                                                
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   PCLTBNAM,CLTBRN                                                  
*                                                                               
         LA    R2,CLTAL1H          ADDRESS LINE 1                               
         XC    PCLTLIN1,PCLTLIN1                                                
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   PCLTLIN1,CLTAL1                                                  
*                                                                               
         LA    R2,CLTAL2H          ADDRESS LINE 2                               
         XC    PCLTLIN2,PCLTLIN2                                                
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   PCLTLIN2,CLTAL2                                                  
*                                                                               
         LA    R2,CLTATTNH         ATTENTION OF                                 
         XC    PCLTATTN,PCLTATTN                                                
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   PCLTATTN,CLTATTN                                                 
*                                                                               
         LA    R2,CLTOFFH          CLIENT OFFICE                                
         XC    PCLTOFF,PCLTOFF                                                  
         CLI   5(R2),0                                                          
         JNE   VR20                                                             
         CLI   SVAGYPF+11,C'B'                                                  
         JE    *+12                                                             
         CLI   SVAGYPF+11,C'1'                                                  
         JNE   VR40                                                             
         JE    MSSNGERR            IF 1 OR B, OFFICE IS REQUIRED                
         DC    H'0'                                                             
*                                                                               
VR20     CLI   5(R2),2             MAX OF 2 CHARS?                              
         JH    INVFDERR                                                         
*                                                                               
         GOTO1 ANY                                                              
         XC    OFCBLK,OFCBLK       INIT OFFICER BLOCK                           
         MVI   OFCSYS,C'P'         PRINT SYSTEM                                 
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC2,WORK                                                     
         GOTO1 OFFICER,DMCB,(C'2',OFFICED),(0,ACOMFACS)                         
         MVC   PCLTOFF,OFCOFC      SAVE 1 BYTE INTERNAL OFFICE CODE             
         TM    OFCINDS,OFCINOLA    USING 2 CHAR OFFICES?                        
         BNZ   VR20D                                                            
         CLI   0(R1),0                                                          
         JNE   INVFDERR                                                         
         B     VR40                                                             
*                                                                               
VR20D    CLI   CLTOFF,C'='         CK FOR SPECIAL CHARACTERS                    
         JE    INVFDERR                                                         
         CLI   CLTOFF,C'-'                                                      
         JE    INVFDERR                                                         
         CLI   CLTOFF,C','                                                      
         JE    INVFDERR                                                         
         CLI   CLTOFF,C'.'                                                      
         JE    INVFDERR                                                         
*                                                                               
         CLI   CLTOFF,X'40'        SHOW HEX VALUE?                              
         BNH   VR40                NO                                           
         CLI   CLTOFF,X'D0'        SHOW HEX VALUE?                              
         BE    VR20H               YES                                          
         CLI   CLTOFF,X'E0'        SHOW HEX VALUE?                              
         BE    VR20H               YES                                          
         CLI   CLTOFF,C'A'         SHOW HEX VALUE?                              
         BL    VR20H               YES                                          
         CLI   CLTOFF,C'9'         SHOW HEX VALUE?                              
         BNH   VR40                NO                                           
*                                                                               
VR20H    GOTO1 HEXOUT,DMCB,CLTOFF,CLTOFN,L'CLTOFF                               
         OI    CLTOFNH+6,X'80'                                                  
*                                                                               
* CK FOR OFFICE LIMIT ACCESS, SO THAT WRONG OFFICE CODE WON'T BE ADDED          
*                                                                               
VR40     CLI   T41CFFD+1,C'*'      DDS TERM                                     
         BE    VR42                                                             
         CLI   T41CFFD+6,C'*'      OFFICE LIMIT ACCESS                          
         BNE   VR42                                                             
         CLC   PCLTOFF(1),T41CFFD+7                                             
         JNE   CLTACERR                                                         
*                                                                               
VR42     LA    R2,CLTAOFFH         ACC OFFICE CODE                              
         BRAS  RE,CKAOFF                                                        
         JNE   TRAPERR2            ERROR SET IN CONHEAD                         
*                                                                               
         LA    R2,CLTNUMH                                                       
         XC    PCLTNUM,PCLTNUM                                                  
         CLI   5(R2),0             INPUT?                                       
         BNE   VR44                                                             
         CLI   F0PROF+5,C'C'       ALLOWED FOR CLIENT?                          
         JE    MSSNGERR                                                         
         CLI   F0PROF+5,C'B'       ALLOWED FOR BOTH PRODUCT AND CLIENT?         
         JE    MSSNGERR                                                         
*                                                                               
         B     VR46                                                             
*                                                                               
VR44     TM    4(R2),X'08'         TEST FOR NUMERICS                            
         BNO   VR44H                                                            
         CLC   =C'H9',AGENCY       FOR STARCOM ONLY                             
         BE    VR44H                                                            
         BRAS  RE,PACK                                                          
         OI    DUB+7,X'0F'                                                      
         BRAS  RE,EXCEPTN          TEST FOR EXCEPTION AGENCIES                  
         BE    LOADIT              EXCEPTION AGY FOUND LOAD ROUTINE             
*                                                                               
         CLI   5(R2),4             NON EXCEPTION, LENGTH IS 3 OR LESS?          
         JNL   INVFDERR                                                         
         UNPK  PCLTNUM,DUB+6(2)                                                 
         B     VR46                                                             
*                                                                               
* ALPHA/NUMERIC INPUT                                                           
*                                                                               
VR44H    BRAS  RE,EXCEPTN          TEST FOR EXCEPTION AGENCIES                  
         BE    VR44M               EXCEPTION AGENCY FOUND ERROR                 
         CLI   5(R2),3             IF NON EXCEPTION LEN M/B 3 OR LESS           
         JH    INVFDERR                                                         
         B     *+12                                                             
VR44M    CLI   5(R2),2                                                          
         JH    INVFDERR            MUST BE 2 CHARS                              
*                                                                               
         MVC   PCLTNUM,CLTNUM                                                   
         OC    PCLTNUM,SPACES                                                   
*                                                                               
* INIT CLT PROFILE TO C'0' IF ADDING, ELSE USE SAVED VALUES                     
*                                                                               
VR46     MVI   PCLTPROF,C'0'                                                    
         MVC   PCLTPROF+1(L'PCLTPROF-1),PCLTPROF                                
         CLI   ACTNUM,ACTADD                                                    
         BE    *+10                                                             
         MVC   PCLTPROF,WKCLPROF                                                
*                                                                               
         MVI   PCLTGST,0                                                        
         LA    R2,CLTGSTH          EDIT GST CODE                                
         CLI   WNATION,C'C'        CANADIAN                                     
         BNE   VR48                                                             
         CLI   5(R2),0                                                          
         BE    MSSNGERR                                                         
         LA    RF,VALGSTC          POINT TO VALID GST CODE TABLE                
VR46H    CLI   0(RF),255                                                        
         JE    INVFDERR                                                         
         CLC   0(1,RF),CLTGST                                                   
         BE    *+12                                                             
         LA    RF,1(RF)                                                         
         B     VR46H                                                            
*                                                                               
         MVC   PCLTGST,CLTGST      MOVE GST TAX CODE                            
*                                                                               
VR48     BRAS  RE,CKFINCLT         FINANCIAL CLT?                               
*        BNE   VR50                                                             
         LA    R2,CLTFINH          EDIT FINANCIAL FLD                           
         CLI   5(R2),0                                                          
         BE    VR48M                                                            
         CLI   8(R2),C'Y'                                                       
         BNE   *+12                                                             
         MVI   PCLTFIN,C'Y'                                                     
         B     VR48P                                                            
         CLI   8(R2),C'N'                                                       
         JNE   INVFDERR                                                         
*                                                                               
VR48M    XC    PCLTFIN,PCLTFIN                                                  
*                                                                               
VR48P    CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    VR50                                                             
         CLC   WKCLTFIN,PCLTFIN    CLT FINANCIAL BYTE CHANGED?                  
         JNE   INVFDERR            INVALID (CANNOT CHANGE IT)                   
*                                                                               
VR50     LA    R2,CLTBILH          EDIT BILLING GROUP FLD                       
         XC    PCLTBLGP,PCLTBLGP                                                
         CLI   5(R2),0                                                          
         BE    *+10                NO INPUT, EDIT NEXT FLD                      
         MVC   PCLTBLGP,CLTBIL     MOVE BILLING GROUP CODE                      
*                                                                               
         GOTO1 ADDELEM             ADD X'02' ELEM TO CLT RECORD                 
         DROP  R6                                                               
*                                                                               
         LA    R2,CLTCSCH          EDIT CONTRACT STANDARD COMMENT               
         CLI   5(R2),0                                                          
         BE    VR56                NO INPUT, EDIT NEXT FLD                      
         CLI   5(R2),6                                                          
         JH    INVFDERR                                                         
         BRAS  RE,VR_CMKEY         BUILT COMMENT RECORD KEY                     
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         JNE   RECNFERR            RECORD NOT FOUND                             
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING PCLTCSCM,R6                                                      
         MVI   0(R6),X'10'         STANDARD COMMENT NUMBER ELEM CODE            
         MVI   1(R6),X'08'         ELEM LENGTH                                  
         MVC   PCLTCNUM,KEY+4                                                   
         GOTO1 ADDELEM             ADD X'10' ELEM TO CLT RECORD                 
         DROP  R6                                                               
*                                                                               
         MVC   KEY,SVWORK          RESTORE KEY                                  
*                                                                               
VR56     LA    R2,CLTISCH          EDIT I/O STANDARD COMMENT                    
         CLI   5(R2),0                                                          
         BE    VR60                NO INPUT, EDIT NEXT FLD                      
         CLI   5(R2),6                                                          
         JH    INVFDERR                                                         
         BRAS  RE,VR_CMKEY         BUILT COMMENT RECORD KEY                     
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         JNE   RECNFERR            RECORD NOT FOUND                             
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING PCLTISCM,R6                                                      
         MVI   0(R6),X'11'         STANDARD COMMENT NUMBER ELEM CODE            
         MVI   1(R6),X'08'         ELEM LENGTH                                  
         MVC   PCLTINUM,KEY+4                                                   
         GOTO1 ADDELEM             ADD X'11' ELEM TO CLT RECORD                 
         DROP  R6                                                               
*                                                                               
         MVC   KEY,SVWORK          RESTORE KEY                                  
*                                                                               
VR60     LA    R2,CLTISC2H         2ND I/O STND COM                             
         CLI   5(R2),0                                                          
         BE    VR62                NO INPUT, EDIT NEXT FLD                      
         CLI   5(R2),6                                                          
         JH    INVFDERR                                                         
         BRAS  RE,VR_CMKEY         BUILT COMMENT RECORD KEY                     
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         JNE   RECNFERR            RECORD NOT FOUND                             
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING PCLTISCM,R6                                                      
         MVI   0(R6),X'11'         STANDARD COMMENT NUMBER ELEM CODE            
         MVI   1(R6),X'08'         ELEM LENGTH                                  
         MVC   PCLTINUM,KEY+4                                                   
         GOTO1 ADDELEM             ADD X'11' ELEM TO CLT RECORD                 
         DROP  R6                                                               
*                                                                               
         MVC   KEY,SVWORK          RESTORE KEY                                  
*                                                                               
VR62     LA    R2,CLTPSTH          EDIT PST CODE FLD                            
         CLI   WNATION,C'C'        CANADIAN?                                    
         BNE   VR64                                                             
         CLI   5(R2),0                                                          
         BE    VR64                                                             
*                                                                               
         XC    SVWORK,SVWORK       USE FOR INTERFACE BLOCK                      
         LA    R4,SVWORK                                                        
         USING PSTBLKD,R4                                                       
         MVI   PSTACT,PSTVALQ      ACTION = VALIDATE                            
         LA    R1,CLTPSTH                                                       
         ST    R1,PSTADIN          INPUT ADDRESS                                
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM+2           FOR PST OUTPUT                               
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,ACOMFACS                                                 
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A6B'                                           
         GOTO1 CALLOV,DMCB                                                      
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R4)      PST VALIDATION                               
         CLI   PSTERR,0                                                         
         JNE   INVFDERR            PST IS INVALID                               
*                                                                               
         CLI   ELEM+2+0,C' '       Have tax code in British Columbia?           
         JH    INVFDERR                                                         
         CLI   ELEM+2+4,C' '       Have tax code in Ontario?                    
         JH    INVFDERR                                                         
*                                                                               
         LA    R6,ELEM             ELEM+2 HAS VALIDATED PST                     
         USING PCLTPST,R6                                                       
         MVI   0(R6),X'25'         CANADIAN PST CODE ELEM CODE                  
         MVI   1(R6),X'0C'         ELEM LENGTH                                  
         GOTO1 ADDELEM             ADD X'10' ELEM TO CLT RECORD                 
         DROP  R6                                                               
*                                                                               
VR64     LA    R2,CLTMPSH          Edit Main PST field                          
         CLI   WNATION,C'C'        Canadian?                                    
         BNE   VR65                                                             
         CLI   5(R2),0             Any input?                                   
         BE    VR65                                                             
*                                                                               
         XC    SVWORK,SVWORK       USE FOR INTERFACE BLOCK                      
         LA    R4,SVWORK                                                        
         USING PSTBLKD,R4                                                       
         MVI   PSTACT,PSTVALQ      ACTION = VALIDATE                            
         LA    R1,CLTMPSH                                                       
         ST    R1,PSTADIN          INPUT ADDRESS                                
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM+2           FOR PST OUTPUT                               
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,ACOMFACS                                                 
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A6B'                                           
         GOTO1 CALLOV,DMCB                                                      
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R4)      PST VALIDATION                               
         CLI   PSTERR,0                                                         
         JNE   INVFDERR            PST IS INVALID                               
*                                                                               
         LA    R6,ELEM             ELEM+2 HAS VALIDATED PST                     
         USING PCLTMPS,R6                                                       
         MVI   PCLTMPS,PCLTMPEQ                                                 
         MVI   PCLTMPSL,PCLTMPLQ                                                
         GOTO1 ADDELEM             ADD X'10' ELEM TO CLT RECORD                 
         DROP  R6                                                               
*                                                                               
VR65     LA    R2,CLTDRDH          EDIT DRD OVERRIDE CLIENT                     
         CLI   5(R2),0                                                          
         BE    VR66                NO INPUT, EDIT NEXT FLD                      
         MVC   SVWORK(L'KEY),KEY                                                
         LA    R4,KEY                                                           
         MVC   PCLTKCLT-PCLTREC(3,R4),CLTDRD                                    
         OC    PCLTKCLT-PCLTREC(3,R4),SPACES                                    
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         JNE   RECNFERR            RECORD NOT FOUND                             
*                                                                               
* SEARCH DIVISION RECORD                                                        
*                                                                               
         MVI   PDIVKRCD-PDIVREC(R4),X'03'                                       
         MVC   PDIVKDIV-PDIVREC(L'PDIVKDIV,R4),=3X'F0'                          
         GOTO1 HIGH                                                             
         CLC   KEY(PDIVLEN-PDIVKEY),KEYSAVE                                     
         JNE   INVDVERR                                                         
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING PCLTDRD,R6                                                       
         MVI   0(R6),X'30'         DRD CLIENT ELEM CODE                         
         MVI   1(R6),X'08'         ELEM LENGTH                                  
         MVC   PCLTDRDC,KEY+4                                                   
         GOTO1 ADDELEM             ADD X'30' ELEM TO CLT RECORD                 
         MVC   KEY,SVWORK          RESTORE KEY                                  
         DROP  R6                                                               
*                                                                               
VR66     NI    WKCLSTAT,X'FF'-X'20'                                             
         LA    R2,CLTPASDH         PRODUCT ASSIGNED Y/N? FIELD                  
         CLI   8(R2),C'Y'          ALLOW PRD OFFICE?                            
         BNE   VR66H                                                            
         OI    WKCLSTAT,X'20'      TURN ON PRD OFFICE ALLOWED BIT               
         B     VR68                                                             
*                                                                               
VR66H    CLI   8(R2),C'N'                                                       
         BE    VR66K               TEST FOR PASSIVE PRD/OFF POINTERS            
         CLI   5(R2),0                                                          
         JNE   INVFDERR            MUST BE 'Y' OR 'N' OR "BLANK"                
*                                                                               
VR66K    MVC   SVWORK(L'KEY),KEY                                                
         XC    KEY+7(L'KEY-7),KEY+7                                             
         MVI   KEY+3,X'A3'         OFF/PRD PASSIVE POINTER                      
         GOTO1 HIGH                                                             
         CLI   KEY+3,X'A3'         DID WE GET OFF/PRD PASSIVE POINTER?          
         BNE   VR66N               NO                                           
         MVI   KEY+3,X'02'         CHANGE TO CLT REC CODE FOR CLC               
         CLC   KEY(7),SVWORK                                                    
         JE    PTOFDERR            CLTPASD NOT 'Y' AND POINTER FOUND            
*                                                                               
VR66N    DS    0H                                                               
         MVC   KEY,SVWORK          RESTORE KEY                                  
         XC    KEY+7(L'KEY-7),KEY+7                                             
         MVI   KEY+3,X'A4'         TRA/PRD PASSIVE POINTER                      
         GOTO1 HIGH                                                             
         CLI   KEY+3,X'A4'         DID WE GET TRA/PRD PASSIVE POINTER?          
         BNE   VR66X               NO                                           
         MVI   KEY+3,X'02'         CHANGE TO CLT REC CODE FOR CLC               
         CLC   KEY(7),PCLTREC                                                   
         JE    PTOFDERR            CLTPASD NOT 'Y' AND POINTER FOUND            
VR66X    DS    0H                                                               
         MVC   KEY,SVWORK          RESTORE KEY                                  
*                                                                               
VR68     LA    R2,CLTTOFFH         TRAFFIC OFFICE CODE                          
         BRAS  RE,CKTRAOFF         CKING FOR TRAFFIC OFFICE                     
         JNE   INVFDERR                                                         
         MVC   SVCTRAC,BYTE        OLD TRAFF CODE FROM CKTRAOFF                 
*                                                                               
VR70     DS    0H                  FOR FUTURE FLDS                              
*                                                                               
         BRAS  RE,GETPROFV         Get profile values                           
         CLI   F0PROF+09,C'Y'      Autofill COST2 rate option?                  
         JNE   VR74                                                             
         CLI   F0PROF+4,C'$'       Allow COST2 rate option (COS2=Y)?            
         JNE   VR74                                                             
         CLI   ACTNUM,ACTADD       Add?                                         
         JNE   VR74                                                             
         OI    WKCLSTAT,X'04'      Turn on COST2 rate option (COS2=Y)           
*                                                                               
VR74     L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                FIRST ELEM MUST BE THERE!                    
         USING PCLTELEM,R6                                                      
         MVC   PCLTSTAT,WKCLSTAT   PUT BACK UPDATED CLT STATUS                  
         DROP  R6                                                               
*                                                                               
VRX      DS    0H                                                               
         J     DR                  RECORD VALIDATED, REDISPLAY IT               
*                                                                               
* CANADIAN GST CODE TABLE                                                       
*                                                                               
VALGSTC  DC    C'SXZ'                                                           
         DC    X'FF'                                                            
EXCEPTN  LA    RF,AGTAB                                                         
         CLI   0(RF),X'FF'         END OF TABLE?                                
         BE    EXCEPTNX            YES                                          
         CLC   AGENCY,0(RF)        MATCH THAT IN TABLE?                         
         BER   RE                  FOUND (CC IS EQUAL)                          
         LA    RF,3(RF)                                                         
         B     EXCEPTN+4                                                        
*                                                                               
EXCEPTNX LTR   RE,RE               NO EXCEPTIONS (CC IS NOT EQUAL)              
         BR    RE                                                               
*                                                                               
LOADIT   DS    0H                                                               
         SR    RE,RE                                                            
         IC    RE,2(RF)       DISPLACEMENT                                      
         BCTR  RE,0                                                             
         SLL   RE,2           MULTIPLE OF 4                                     
         B     JUMPT(RE)                                                        
*                                                                               
* LIST OF AGENCIES W/ 4 OR 5 DIGIT INTERFACE CODES OR 2 ALPHA-NUMERIC           
* FOR NUMERIC CODES X'FF' FOLLOWED BY 2 PWOS BYTES                              
* FOR 2 ALPHA-NUMERIC CARRY 2 CHARACTERS                                        
*                                                                               
* FOR 5 DIGITS 3 BYTE BINARY WITH HIGH BYTE OI X'80'                            
*                                                                               
AGTAB    DS    0F                                                               
         DC    C'JW',AL1((*+1-AGTAB)/3)                                         
         DC    C'LM',AL1((*+1-AGTAB)/3)                                         
         DC    C'LT',AL1((*+1-AGTAB)/3)                                         
         DC    C'DA',AL1((*+1-AGTAB)/3)                                         
         DC    C'KA',AL1((*+1-AGTAB)/3)                                         
         DC    X'FF'               END OF TABLE                                 
*                                                                               
JUMPT    B     JWTSCHME                                                         
         B     JWTSCHME            LM - SAME AS JW                              
         B     JWTSCHME            LT - SAME AS JW                              
         B     DIGIT5                                                           
         B     DIGIT5                                                           
*                                                                               
JWTSCHME DS    0H                                                               
         LA    R6,ELEM             POINT TO WHERE PCLTNUM LIVES                 
         USING PCLTELEM,R6                                                      
*                                                                               
         MVI   PCLTNUM,255                                                      
         L     RF,DUB+4                                                         
         SRL   RF,4                SHIFT OUT SIGN                               
         STH   RF,DUB                                                           
         MVC   PCLTNUM+1(2),DUB    PACKED UNSIGNED INTERFACE CODE               
         B     VR46                                                             
*                                                                               
DIGIT5   DS    0H                  R0 HAS BINARY VALUE                          
         LA    R6,ELEM             POINT TO WHERE PCLTNUM LIVES                 
         ST    R0,FULL                                                          
         MVC   PCLTNUM(3),FULL+1                                                
         OI    PCLTNUM,X'80'                                                    
         B     VR46                                                             
*                                                                               
         DROP  R6                                                               
*                                                                               
VR_CMKEY MVC   SVWORK(32),KEY                                                   
         MVI   KEY+3,X'40'         STD COMMENT RECORD CODE                      
         MVC   KEY+4(6),SPACES                                                  
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         LCR   RF,R1               NEGATE INPUT LENGTH                          
         AHI   RF,6                                                             
         LA    RF,KEY+4(RF)        RIGHT JUSTIFIED                              
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),8(R2)                                                    
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DR       DS    0H                  DISPLAY RECORD                               
         L     R6,AIO                                                           
         CLI   3(R6),X'02'         CLIENT RECORD CODE?                          
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PCLTELEM,R6                                                      
         MVI   ELCODE,X'02'        FIRST CLIENT ELEM CODE                       
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    CLTCNAM,CLTCNAM                                                  
         MVC   CLTCNAM,PCLTNAME                                                 
         OI    CLTCNAMH+6,X'80'                                                 
*                                                                               
         XC    CLTBRN,CLTBRN                                                    
         MVC   CLTBRN,PCLTBNAM                                                  
         OI    CLTBRNH+6,X'80'                                                  
*                                                                               
         XC    CLTAL1,CLTAL1                                                    
         MVC   CLTAL1,PCLTLIN1                                                  
         OI    CLTAL1H+6,X'80'                                                  
*                                                                               
         XC    CLTAL2,CLTAL2                                                    
         MVC   CLTAL2,PCLTLIN2                                                  
         OI    CLTAL2H+6,X'80'                                                  
*                                                                               
         XC    CLTATTN,CLTATTN                                                  
         MVC   CLTATTN,PCLTATTN                                                 
         OI    CLTATTNH+6,X'80'                                                 
*                                                                               
         XC    CLTOFF,CLTOFF                                                    
         XC    CLTOFN,CLTOFN                                                    
         XC    OFCBLK,OFCBLK       INIT OFFICER BLOCK                           
         MVI   OFCSYS,C'P'         PRINT SYSTEM                                 
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,PCLTOFF                                                   
         GOTO1 OFFICER,DMCB,(C'2',OFFICED),(1,ACOMFACS),(C'S',CLTOFN)           
         TM    OFCINDS,OFCINOLA    USING 2 CHAR OFFICES?                        
         BNZ   DR34M                                                            
         CLI   0(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                IT CAN'T BE INVALID                          
*                                                                               
         MVC   CLTOFF(L'OFCOFC2),OFCOFC2                                        
         OI    CLTOFFH+6,X'80'                                                  
         OI    CLTOFNH+6,X'80'                                                  
         B     DR40                                                             
*                                                                               
DR34M    MVC   CLTOFF(L'PCLTOFF),PCLTOFF                                        
         OI    CLTOFFH+6,X'80'                                                  
         XC    CLTOFN,CLTOFN                                                    
         LA    R2,CLTOFFH          FLD TO BE TRANSLATED TO HEX                  
         LA    R3,CLTOFNH          HEX FLD TO BE DISPLAYED                      
         BRAS  RE,DR_HEX                                                        
*                                                                               
DR40     XC    CLTAOFF,CLTAOFF     ACC OFFICE CODE                              
         MVC   CLTAOFF(L'PCLTAOFC),PCLTAOFC                                     
         OC    PCLTACCA,PCLTACCA   ACC OFFICE AGENCY CODE PRESENT?              
         BZ    DR44U                                                            
         MVC   WORK+0(2),PCLTAOFC                                               
         MVI   WORK+2,C'/'                                                      
         MVC   WORK+3(2),PCLTACCA                                               
         MVC   CLTAOFF,WORK                                                     
DR44U    OI    CLTAOFFH+6,X'80'                                                 
*                                                                               
         XC    CLTPASD,CLTPASD                                                  
         MVI   CLTPASD,C'N'        DEFAULT                                      
         TM    PCLTSTAT,X'20'      PRODUCT ASSIGN ALLOWED?                      
         BNO   *+8                 NO                                           
         MVI   CLTPASD,C'Y'                                                     
         OI    CLTPASDH+6,X'80'                                                 
*                                                                               
         XC    CLTNUM,CLTNUM       INTERFACE NUMBER                             
         BRAS  RE,EXCEPTN          CK FOR AGENCY EXCEPTIONS                     
         BE    *+14                EXCEPTION FOUND                              
         MVC   CLTNUM(L'PCLTNUM),PCLTNUM                                        
         B     DR46U                                                            
*                                                                               
         CLI   PCLTNUM,X'FF'       TWO PWOS BYTES FOLLOW?                       
         BNE   *+20                NO                                           
         UNPK  WORK(5),PCLTNUM+1(3)                                             
         MVC   CLTNUM(4),WORK      NOTE: UNPK'S LAST BYTE IS NOT USED!          
         B     DR46U                                                            
*                                                                               
* CK FOR SPECIAL 3 BYTE BINARY FORMAT (IF X'80' SET ON IN FIRST BYTE)           
*                                                                               
         MVC   BYTE,PCLTNUM                                                     
         NI    BYTE,X'F0'          SET OFF LOWER HALF BYTE BITS                 
         CLI   BYTE,X'80'          ONLY X'80' WILL BE LEFT ON                   
         BNE   DR46U               IN THIS FORMAT                               
*                                  NUMBER IS IN BINARY FORMAT                   
         XC    FULL,FULL                                                        
         MVC   FULL+1(3),PCLTNUM                                                
         NI    FULL+1,X'FF'-X'80'  SET OFF X'80' STATUS BIT                     
         L     R0,FULL                                                          
         CVD   R0,DUB                                                           
         UNPK  WORK(5),DUB                                                      
         OI    WORK+4,X'F0'                                                     
         MVC   CLTNUM(5),WORK                                                   
*                                                                               
DR46U    OI    CLTNUMH+6,X'80'                                                  
*                                                                               
         XC    CLTFIN,CLTFIN                                                    
         BRAS  RE,CKFINCLT         FINANCIAL CLT?                               
         BNE   DR48U                                                            
         MVC   CLTFIN,PCLTFIN                                                   
DR48U    OI    CLTFINH+6,X'80'                                                  
*                                                                               
         XC    CLTBIL,CLTBIL                                                    
         MVC   CLTBIL,PCLTBLGP     BILLING GROUP CODE                           
         OI    CLTBILH+6,X'80'                                                  
*                                                                               
         DROP  R6                  DONE USING MAIN ELEM DSECT FOR CLT           
*                                                                               
         XC    WORK,WORK                                                        
         XC    CLTTOFF,CLTTOFF     CLEAR TRAFFIC OFFICE CODE                    
         OI    CLTTOFFH+6,X'80'                                                 
         XC    CLTTOFX,CLTTOFX     CLEAR TRAFFIC OFFICE "NAME"                  
         OI    CLTTOFXH+6,X'80'                                                 
         L     R6,AIO                                                           
         MVI   ELCODE,X'50'        TRAFFIC OFFICE CODE ELEM                     
         BRAS  RE,GETEL                                                         
         JNE   DR49U                                                            
*                                                                               
         XC    OFCBLK,OFCBLK       INIT OFFICER BLOCK                           
         MVI   OFCSYS,C'P'         PRINT SYSTEM                                 
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,2(R6)                                                     
         GOTOR OFFICER,DMCB,(C'2',OFFICED),(1,ACOMFACS),(C'S',WORK)             
         MVC   CLTTOFF(2),OFCOFC2                                               
*                                                                               
DR49U    DS    0H                                                               
*                                                                               
         LA    R2,CLTTOFFH         FLD TO BE TRANSLATED TO HEX                  
         LA    R3,CLTTOFXH         HEX FLD TO BE DISPLAYED                      
         BRAS  RE,DR_HEX                                                        
*                                                                               
         CLI   WORK,C' '           ANYTHING IN SHORT NAME ?                     
         BNH   *+10                NO                                           
         MVC   CLTTOFX(8),WORK     DISPLAY NAME                                 
*                                                                               
         XC    CLTCSC,CLTCSC                                                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'        LOOK FOR CONTRACT STD COM ELEM               
         BRAS  RE,GETEL                                                         
         BNE   DR50U                                                            
         MVC   CLTCSC(6),2(R6)     CONTRACT STD COM CODE                        
DR50U    OI    CLTCSCH+6,X'80'                                                  
*                                                                               
         XC    CLTISC,CLTISC                                                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'11'        LOOK FOR I/O STD COM ELEM                    
         BRAS  RE,GETEL                                                         
         BNE   DR52U                                                            
         MVC   CLTISC(6),2(R6)     I/O STD COM 1                                
DR52U    OI    CLTISCH+6,X'80'                                                  
*                                                                               
         XC    CLTISC2,CLTISC2     I/O STD COM 2                                
         BRAS  RE,NEXTEL           LOOK FOR ANOTHER                             
         BNE   DR54U                                                            
         MVC   CLTISC2(6),2(R6)    I/O STD COM 2                                
DR54U    OI    CLTISC2H+6,X'80'                                                 
*                                                                               
         XC    CLTDRD,CLTDRD                                                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'30'        LOOK FOR DRD CLT ELEM                        
         BRAS  RE,GETEL                                                         
         BNE   DR56U                                                            
         MVC   CLTDRD(3),2(R6)     I/O STD COM 2                                
DR56U    OI    CLTDRDH+6,X'80'                                                  
*                                                                               
         CLI   WNATION,C'C'        CANADAIAN?                                   
         BNE   *+8                                                              
         BRAS  RE,DISPPGST         DISPLAY PST AND GST CODES                    
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
* CONDITIONALLY SHOW HEX OF CLT OFFICE                                          
* R2 = FLD HDR TO BE TRANSLATED                                                 
* R3 = FLD HDR TO BE USED FOR DISPLAYING HEX VALUE                              
*                                                                               
DR_HEX   ST    RE,FULL                                                          
         XC    8(2,R3),8(R3)       CLR HEXOUT AREA                              
         CLI   8(R2),X'40'         SHOW HEX VALUE?                              
         BNH   DR_HX60             NO                                           
         CLI   8(R2),X'D0'         SHOW HEX VALUE?                              
         BE    DR_HX40             YES                                          
         CLI   8(R2),X'E0'         SHOW HEX VALUE?                              
         BE    DR_HX40             YES                                          
         CLI   8(R2),C'A'          SHOW HEX VALUE?                              
         BL    DR_HX40             YES                                          
         CLI   8(R2),C'9'          SHOW HEX VALUE?                              
         BNH   DR_HX60             NO                                           
*                                                                               
DR_HX40  GOTO1 HEXOUT,DMCB,8(R2),8(R3),1                                        
*                                                                               
DR_HX60  OI    6(R3),X'80'                                                      
         L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DK       DS    0H                  DISPLAY KEY                                  
         L     R6,AIO                                                           
         CLI   3(R6),X'02'         CLIENT RECORD CODE?                          
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PCLTKEY,R6                                                       
         MVC   CLTMED,PCLTKMED                                                  
         MVC   CLTCLT,PCLTKCLT                                                  
         DROP  R6                                                               
*                                                                               
         MVI   USEIONUM,2          USE AIO2 FOR MED/CLT/PRD/EST RECS            
*                                                                               
         OI    CLTMEDH+6,X'80'     TRANSMIT MEDIA CODE                          
         XC    CLTMEDN,CLTMEDN                                                  
         OI    CLTMEDNH+6,X'80'    CLEARED MEDIA NAME                           
         MVI   CLTMEDH+5,1         INPUT LENGTH                                 
         LA    R2,CLTMEDH                                                       
         GOTO1 VALIMED                                                          
         MVC   CLTMEDN,MEDNM                                                    
         OI    CLTMEDNH+6,X'80'    TRANSMIT MEDIA NAME                          
*                                                                               
         OI    CLTCLTH+6,X'80'     TRANSMIT CLIENT CODE                         
         XC    CLTCLTN,CLTCLTN                                                  
         OI    CLTCLTNH+6,X'80'    CLEARED CLIENT NAME                          
         MVI   CLTCLTH+5,3         INPUT LENGTH                                 
         LA    R2,CLTCLTH                                                       
         GOTO1 VALICLT                                                          
         MVC   CLTCLTN,CLTNM                                                    
         OI    CLTCLTNH+6,X'80'    TRANSMIT CLIENT NAME                         
*                                                                               
DKX      DS    0H                                                               
         MVC   AIO,AIO1            AIO1 HAS REC TO BE DISPLAYED                 
         MVI   USEIONUM,1          RESET TO AIO1                                
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
LR       DS    0H                  LIST RECORDS                                 
         GOTOR CLRLSSCR            CLEAR LIST SCREEN                            
*                                                                               
         CLI   KEY+3,X'02'         STILL CLT RECORD?                            
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
*                                                                               
         XC    LISTAR,LISTAR       FILL IN LIST LINE                            
         MVC   SVWORK(L'KEY),KEY   RESTORING SEQUENCE LATER                     
*                                                                               
         XC    KEY+07(18),KEY+07   PREPARE CLIENT REC KEY                       
         MVI   KEY+03,X'02'        CLIENT RECORD CODE                           
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE     CLIENT ON FILE?                              
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY+04(3),SVWORK+04 SAME CLIENT AS IN SAVED KEY?                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* BY PASS RECS WITH LIMIT ACCESS (NOT TO LIST THEM)                             
*                                                                               
         MVI   ERROPT,C'Y'         RETURN TO APPL INSTEAD OF GENCON ERR         
         XC    WKTMPFLD,WKTMPFLD                                                
         MVI   WKTMPFLD+0,8+3      TOTAL LENGTH OF TEMP FLD                     
         MVI   WKTMPFLD+5,3        INPUT LENGTH OF TEMP FLD                     
         MVC   WKTMPFLD+8(3),KEY+4 CLT CODE TO BE VALIDATED FOR SEC             
         LA    R2,WKTMPFLD                                                      
         MVC   WKTMPKEY,KEY                                                     
         MVC   WKTMPAIO,AIO                                                     
         MVC   WKTMPION,USEIONUM                                                
         MVI   USEIONUM,3          VALICLT WILL USE AIO3                        
         XC    CLTNM,CLTNM         IT WILL BE SET IF CLT IS OK                  
         GOTOR CKMAXIO             CK FOR MAX I/O                               
         BNE   LR94_LM                                                          
         GOTO1 VALICLT                                                          
         MVC   KEY,WKTMPKEY                                                     
         MVC   AIO,WKTMPAIO                                                     
         MVC   USEIONUM,WKTMPION                                                
         GOTOR CKMAXIO             CK FOR MAX I/O                               
         BNE   LR94_LM                                                          
         GOTO1 HIGH                RESTORE SEQUENCE                             
         OC    CLTNM,CLTNM         CLT CODE PASS SECURITY?                      
         BZ    LR30_SEQ            NO, DO NOT LIST THIS CLT                     
*                                                                               
* PREPARE REC FOR FILTER VALUE(S) IF PRESENT                                    
*                                                                               
         GOTOR CKMAXIO             CK FOR MAX I/O                               
         BNE   LR94_LM                                                          
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO                                                           
         USING PCLTELEM,R6                                                      
         MVI   ELCODE,X'02'        1ST CLT ELEM                                 
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   LSCLTC(L'LSCLTC),KEY+04                                          
         MVI   LSCLTC+L'LSCLTC,C'/'                                             
         MVC   LSCLT,PCLTNAME                                                   
*                                                                               
* FILTERING ON FLITER FLDS                                                      
*                                                                               
         OC    F_FLDS(F_FLDLNQ),F_FLDS                                          
         BNZ   LR50                                                             
*                                                                               
* NO FITLER VALUE(S) FOUND, DISPLAY DEFAULT LIST DATA                           
*                                                                               
         XC    OFCBLK,OFCBLK       INIT OFFICER BLOCK                           
         MVI   OFCSYS,C'P'         PRINT SYSTEM                                 
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,PCLTOFF                                                   
         GOTO1 OFFICER,DMCB,(C'2',OFFICED),(0,ACOMFACS)                         
*                                                                               
         MVC   LSOFC,OFCOFC2       CLT OFFICE CODE                              
*                                                                               
         MVC   LSAOF,PCLTAOFC      CLT ACC OFFICE CODE                          
         OC    PCLTACCA,PCLTACCA                                                
         BZ    *+14                                                             
         MVI   LSAOFASP,C'/'       SEPARATOR                                    
         MVC   LSAGY,PCLTACCA      ACC OFFICE AGY                               
         MVC   LSCTYPE(1),PCLTPROF+05                                           
         CLI   PCLTPROF+05,C'2'    CLT TYPE IS SUB?                             
         BNE   *+10                                                             
         MVC   LSCTYPE,PCLTPROF+06 MASTER CLT CODE                              
         MVC   LSCONREQ,PCLTPROF+12                                             
         CLI   PCLTPROF+12,C'0'    CONTRACT REQUIRED?                           
         BNE   *+8                                                              
         MVI   LSCONREQ,C'Y'       STORED AS "0", DISPLAY IT AS "Y"             
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         USING PCLTTOEL,R6                                                      
         MVI   ELCODE,X'50'        TRAFFIC OFFICE ELEM                          
         BRAS  RE,GETEL                                                         
         BNE   LR94_LM             DONE WITH LIST LINE                          
*                                                                               
         XC    OFCBLK,OFCBLK       INIT OFFICER BLOCK                           
         MVI   OFCSYS,C'P'         PRINT SYSTEM                                 
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,PCLTTOFC     CLT TRAFFIC OFFICE CODE                      
         GOTO1 OFFICER,DMCB,(C'2',OFFICED),(0,ACOMFACS)                         
         MVC   LSTOF,OFCOFC2       CLT TRAFFIC OFFICE CODE                      
*                                                                               
         B     LR94_LM             DONE WITH LIST LINE                          
         DROP  R6                                                               
*                                                                               
LR50     DS    0H                  CKING FOR INDIVIDUAL FILTER(S)               
         USING PCLTELEM,R6                                                      
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
         OC    PCLTOFF,PCLTOFF     ANYTHING IN OFFICE CODE?                     
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
         B     LR51P                                                            
LR51H    CLC   F_OFC,PCLTOFF       MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
LR51P    SR    RE,RE                                                            
         IC    RE,F_OFC+L'F_OFC    SCANNER ENTRY POSITION NUMBER                
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         ST    RF,FULL                                                          
         XC    OFCBLK,OFCBLK       INIT OFFICER BLOCK                           
         MVI   OFCSYS,C'P'         PRINT SYSTEM                                 
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,PCLTOFF                                                   
         GOTO1 OFFICER,DMCB,(C'2',OFFICED),(0,ACOMFACS)                         
         L     RF,FULL                                                          
         MVI   L_DLNQ/2+00(RF),L'OFCOFC2                                        
         MVC   L_DLNQ/2+01(L'OFCOFC2,RF),OFCOFC2                                
*                                                                               
LR52     CLI   F_AOF+L'F_AOF,0                                                  
         BE    LR53                                                             
         SR    RE,RE                                                            
         IC    RE,F_AOF+L'F_AOF    SCANNER ENTRY POSITION NUMBER                
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),03           LENGTH OF TITLE AND DASHES                   
         MVC   01(03,RF),=C'AOF'                                                
         MVI   L_DLNQ/2+00(RF),03  DEFAULT LENGTH OF FLT DATA                   
         CLI   F_AOF,C' '          FLT KEYWORD ONLY?                            
         BE    LR52P                                                            
         CLI   F_AOF,C'*'          WILDCARD?                                    
         BNE   LR52H                                                            
         OC    PCLTAOFC,PCLTAOFC   ANYTHING IN ACC OFFICE CODE?                 
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
         OC    PCLTAOFC,SPACES     FOR MIXES OF NULL & SPACE (NO CHGS!)         
         CLC   PCLTAOFC,SPACES                                                  
         BE    LR30_SEQ            SPACES, SKIP IT TOO                          
         B     LR52P                                                            
LR52H    CLC   F_AOF,PCLTAOFC      MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
LR52P    SR    RE,RE                                                            
         IC    RE,F_AOF+L'F_AOF    SCANNER ENTRY POSITION NUMBER                
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),L'PCLTAOFC                                       
         MVC   L_DLNQ/2+01(L'PCLTAOFC,RF),PCLTAOFC                              
*                                                                               
LR53     CLI   F_AGY+L'F_AGY,0                                                  
         BE    LR54                                                             
         SR    RE,RE                                                            
         IC    RE,F_AGY+L'F_AGY    SCANNER ENTRY POSITION NUMBER                
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),07           LENGTH OF TITLE AND DASHES                   
         MVC   01(07,RF),=C'AOF/Agy'                                            
         MVI   L_DLNQ/2+00(RF),L'PCLTAOFC+2+L'PCLTACCA                          
         CLI   F_AGY,C' '          FLT KEYWORD ONLY?                            
         BE    LR53P                                                            
         CLI   F_AGY,C'*'          WILDCARD?                                    
         BNE   LR53H                                                            
         OC    PCLTACCA,PCLTACCA   ANYTHING IN ACC OFFICE AGY?                  
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
         B     LR53P                                                            
LR53H    CLC   F_AGY,PCLTACCA      MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
LR53P    SR    RE,RE                                                            
         IC    RE,F_AGY+L'F_AGY    SCANNER ENTRY POSITION NUMBER                
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),L'PCLTAOFC+2+L'PCLTACCA                          
         MVC   L_DLNQ/2+01(L'PCLTAOFC,RF),PCLTAOFC                              
         MVC   L_DLNQ/2+05(L'PCLTACCA,RF),PCLTACCA                              
         DROP  R6                                                               
*                                                                               
LR54     CLI   F_TOF+L'F_TOF,0                                                  
         BE    LR55                                                             
         SR    RE,RE                                                            
         IC    RE,F_TOF+L'F_TOF    SCANNER ENTRY POSITION NUMBER                
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),03           LENGTH OF TITLE AND DASHES                   
         MVC   01(03,RF),=C'TOF'                                                
         MVI   L_DLNQ/2+00(RF),03  DEFAULT LENGTH OF FLT DATA                   
         MVI   ELCODE,X'50'        TRAFFIC OFFICE CODE ELEM CODE                
         USING PCLTTOEL,R6                                                      
         CLI   F_TOF,C' '          FLT KEYWORD ONLY?                            
         BNE   LR54F                                                            
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+8                                                              
         LA    R6,SPACES           SPACES WILL BE DISPLAYED                     
         B     LR54P                                                            
LR54F    L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   LR30_SEQ            DO NOT HAVE IT, NOTHING TO FILTER ON         
         CLI   F_TOF,C'*'          WILDCARD?                                    
         BNE   LR54H                                                            
         OC    PCLTTOFC,PCLTTOFC   ANYTHING IN TRAFFIC OFFICE CODE?             
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
         B     LR54P                                                            
LR54H    CLC   F_TOF,PCLTTOFC      MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
LR54P    SR    RE,RE                                                            
         IC    RE,F_TOF+L'F_TOF    SCANNER ENTRY POSITION NUMBER                
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         ST    RF,FULL                                                          
         XC    OFCBLK,OFCBLK       INIT OFFICER BLOCK                           
         MVI   OFCSYS,C'P'         PRINT SYSTEM                                 
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,PCLTTOFC                                                  
         GOTO1 OFFICER,DMCB,(C'2',OFFICED),(0,ACOMFACS)                         
         L     RF,FULL                                                          
         MVI   L_DLNQ/2+00(RF),L'OFCOFC2         TRAFFIC OFFICE                 
         MVC   L_DLNQ/2+01(L'OFCOFC2,RF),OFCOFC2                                
*                                                                               
         DROP  R6                                                               
*                                                                               
LR55     L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL            POINT TO MAIN CLT ELEM                       
         BE    *+6                                                              
         DC    H'0'                CLT MAIN ELEM MUST BE THERE!                 
         USING PCLTELEM,R6                                                      
         CLI   F_CLTNUM+L'F_CLTNUM,0                                            
         BE    LR56                                                             
         SR    RE,RE                                                            
         IC    RE,F_CLTNUM+L'F_CLTNUM                                           
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),06           LENGTH OF TITLE AND DASHES                   
         MVC   01(06,RF),=C'CltNum'                                             
         MVI   L_DLNQ/2+00(RF),06  DEFAULT LENGTH OF FLT DATA                   
         CLI   F_CLTNUM,C' '       FLT KEYWORD ONLY?                            
         BE    LR55P                                                            
         CLI   F_CLTNUM,C'*'       WILDCARD?                                    
         BNE   LR55H                                                            
         OC    PCLTNUM,PCLTNUM     ANYTHING IN CLT NUMBER?                      
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
         B     LR55P                                                            
LR55H    CLC   F_CLTNUM,PCLTNUM    MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
LR55P    SR    RE,RE                                                            
         IC    RE,F_CLTNUM+L'F_CLTNUM                                           
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),L'PCLTNUM                                        
         MVC   L_DLNQ/2+01(L'PCLTNUM,RF),PCLTNUM                                
         BRAS  RE,EXCEPTN          CK FOR AGENCY EXCEPTIONS                     
         BNE   LR56                NOT EQ -> EXCEPTION NOT FOUND                
         XC    WORK,WORK                                                        
         CLI   PCLTNUM,X'FF'                                                    
         BE    LR55T                                                            
         TM    PCLTNUM,X'80'                                                    
         BZ    LR56                                                             
         MVC   WORK+1(3),PCLTNUM                                                
         NI    WORK+1,X'FF'-X'80'  SET OFF X'80' STATUS BIT                     
         STCM  R0,15,WORK                                                       
         CVD   R0,DUB                                                           
         UNPK  WORK(5),DUB                                                      
         OI    WORK+4,X'F0'                                                     
         MVI   L_DLNQ/2+00(RF),5                                                
         MVC   L_DLNQ/2+01(5,RF),WORK                                           
         B     LR56                                                             
LR55T    MVI   L_DLNQ/2+00(RF),4                                                
         UNPK  WORK(5),PCLTNUM+1(3)                                             
         MVC   L_DLNQ/2+01(4,RF),WORK                                           
         DROP  R6                                                               
*                                                                               
LR56     CLI   F_CSCOM+L'F_CSCOM,0                                              
         BE    LR57                                                             
         SR    RE,RE                                                            
         IC    RE,F_CSCOM+L'F_CSCOM                                             
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),09           LENGTH OF TITLE AND DASHES                   
         MVC   01(09,RF),=C'ConStdCom'                                          
         MVI   L_DLNQ/2+00(RF),09  DEFAULT LENGTH OF FLT DATA                   
         MVI   ELCODE,X'10'        CONTRACT STD COMMENT ELEM CODE               
         USING PCLTCSCM,R6                                                      
         CLI   F_CSCOM+(L'F_CSCOM-1),C' '                                       
         BNE   LR56F                                                            
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+8                                                              
         LA    R6,SPACES           SPACES WILL BE DISPLAYED                     
         B     LR56P                                                            
LR56F    L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   LR30_SEQ            DO NOT HAVE IT, NOTHING TO FILTER ON         
         CLI   F_CSCOM+(L'F_CSCOM-1),C'*'                                       
         BNE   LR56H                                                            
         OC    PCLTCNUM,PCLTCNUM   ANYTHING IN CONTRACT STD COM CODE?           
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
         B     LR56P                                                            
LR56H    CLC   F_CSCOM,PCLTCNUM    MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
LR56P    SR    RE,RE                                                            
         IC    RE,F_CSCOM+L'F_CSCOM                                             
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),L'PCLTCNUM                                       
         MVC   L_DLNQ/2+01(L'PCLTCNUM,RF),PCLTCNUM                              
         DROP  R6                                                               
*                                                                               
LR57     CLI   F_IOCOM1+L'F_IOCOM1,0                                            
         BE    LR58                                                             
         SR    RE,RE                                                            
         IC    RE,F_IOCOM1+L'F_IOCOM1                                           
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),06           LENGTH OF TITLE AND DASHES                   
         MVC   01(06,RF),=C'IOCom1'                                             
         MVI   L_DLNQ/2+00(RF),06  DEFAULT LENGTH OF FLT DATA                   
         MVI   ELCODE,X'11'        I/O STD COMMENT ELEM CODE                    
         USING PCLTISCM,R6                                                      
         CLI   F_IOCOM1+(L'F_IOCOM1-1),C' '                                     
         BNE   LR57F                                                            
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+8                                                              
         LA    R6,SPACES           SPACES WILL BE DISPLAYED                     
         B     LR57P                                                            
LR57F    L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   LR30_SEQ            DO NOT HAVE IT, NOTHING TO FILTER ON         
         CLI   F_IOCOM1+(L'F_IOCOM1-1),C'*'                                     
         BNE   LR57H                                                            
         OC    PCLTINUM,PCLTINUM   ANYTHING IN IO STD COMMENT 1 CODE?           
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
         B     LR57P                                                            
LR57H    CLC   F_IOCOM1,PCLTINUM   MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
LR57P    SR    RE,RE                                                            
         IC    RE,F_IOCOM1+L'F_IOCOM1                                           
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),L'PCLTINUM                                       
         MVC   L_DLNQ/2+01(L'PCLTINUM,RF),PCLTINUM                              
         DROP  R6                                                               
*                                                                               
LR58     CLI   F_IOCOM2+L'F_IOCOM2,0                                            
         BE    LR59                                                             
         SR    RE,RE                                                            
         IC    RE,F_IOCOM2+L'F_IOCOM2                                           
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),06           LENGTH OF TITLE AND DASHES                   
         MVC   01(06,RF),=C'IOCom2'                                             
         MVI   L_DLNQ/2+00(RF),06  DEFAULT LENGTH OF FLT DATA                   
         MVI   ELCODE,X'11'        I/O STD COMMENT ELEM CODE                    
         USING PCLTISCM,R6                                                      
         CLI   F_IOCOM2+(L'F_IOCOM2-1),C' '                                     
         BNE   LR58F                                                            
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   *+12                                                             
         BRAS  RE,NEXTEL                                                        
         BE    *+8                                                              
         LA    R6,SPACES           SPACES WILL BE DISPLAYED                     
         B     LR58P                                                            
LR58F    L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   LR30_SEQ            DO NOT HAVE IT, NOTHING TO FILTER ON         
         BRAS  RE,NEXTEL                                                        
         BNE   LR30_SEQ            DO NOT HAVE IT, NOTHING TO FILTER ON         
         CLI   F_IOCOM2+(L'F_IOCOM2-1),C'*'                                     
         BNE   LR58H                                                            
         OC    PCLTINUM,PCLTINUM   ANYTHING IN IO STD COMMENT 2 CODE?           
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
         B     LR58P                                                            
LR58H    CLC   F_IOCOM2,PCLTINUM   MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
LR58P    SR    RE,RE                                                            
         IC    RE,F_IOCOM2+L'F_IOCOM2                                           
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),L'PCLTINUM                                       
         MVC   L_DLNQ/2+01(L'PCLTINUM,RF),PCLTINUM                              
         DROP  R6                                                               
*                                                                               
LR59     L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL            POINT TO MAIN CLT ELEM                       
         BE    *+6                                                              
         DC    H'0'                CLT MAIN ELEM MUST BE THERE!                 
         USING PCLTELEM,R6                                                      
         CLI   F_BILGRP+L'F_BILGRP,0                                            
         BE    LR60                                                             
         SR    RE,RE                                                            
         IC    RE,F_BILGRP+L'F_BILGRP                                           
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),07           LENGTH OF TITLE AND DASHES                   
         MVC   01(07,RF),=C'BillGrp'                                            
         MVI   L_DLNQ/2+00(RF),07  DEFAULT LENGTH OF FLT DATA                   
         CLI   F_BILGRP,C' '       FLT KEYWORD ONLY?                            
         BE    LR59P                                                            
         CLI   F_BILGRP,C'*'       WILDCARD?                                    
         BNE   LR59H                                                            
         OC    PCLTBLGP,PCLTBLGP   ANYTHING IN CLT BILLING GROUP?               
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
         B     LR59P                                                            
LR59H    CLC   F_BILGRP,PCLTBLGP   MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
LR59P    SR    RE,RE                                                            
         IC    RE,F_BILGRP+L'F_BILGRP                                           
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),L'PCLTBLGP                                       
         MVC   L_DLNQ/2+01(L'PCLTBLGP,RF),PCLTBLGP                              
         DROP  R6                                                               
*                                                                               
LR60     CLI   F_DRDOV+L'F_DRDOV,0                                              
         BE    LR61                                                             
         SR    RE,RE                                                            
         IC    RE,F_DRDOV+L'F_DRDOV                                             
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),05           LENGTH OF TITLE AND DASHES                   
         MVC   01(05,RF),=C'DrdOv'                                              
         MVI   L_DLNQ/2+00(RF),05  DEFAULT LENGTH OF FLT DATA                   
         MVI   ELCODE,X'30'        DRD CLT OVERRIDE ELEM CODE                   
         USING PCLTDRD,R6                                                       
         CLI   F_DRDOV,C' '        FLT KEYWORD ONLY?                            
         BNE   LR60F                                                            
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+8                                                              
         LA    R6,SPACES           SPACES WILL BE DISPLAYED                     
         B     LR60P                                                            
LR60F    L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   LR30_SEQ            DO NOT HAVE IT, NOTHING TO FILTER ON         
         CLI   F_DRDOV,C'*'        WILDCARD?                                    
         BNE   LR60H                                                            
         OC    PCLTDRDC,PCLTDRDC   ANYTHING IN IO DRD CLT CODE?                 
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
         B     LR60P                                                            
LR60H    CLC   F_DRDOV,PCLTDRDC    MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
LR60P    SR    RE,RE                                                            
         IC    RE,F_DRDOV+L'F_DRDOV                                             
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),L'PCLTDRDC                                       
         MVC   L_DLNQ/2+01(L'PCLTDRDC,RF),PCLTDRDC                              
         DROP  R6                                                               
*                                                                               
LR61     L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL            POINT TO MAIN CLT ELEM                       
         BE    *+6                                                              
         DC    H'0'                CLT MAIN ELEM MUST BE THERE!                 
         USING PCLTELEM,R6                                                      
         CLI   F_FIN+L'F_FIN,0                                                  
         BE    LR62                                                             
         SR    RE,RE                                                            
         IC    RE,F_FIN+L'F_FIN                                                 
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),03           LENGTH OF TITLE AND DASHES                   
         MVC   01(03,RF),=C'Fin'                                                
         MVI   L_DLNQ/2+00(RF),03  DEFAULT LENGTH OF FLT DATA                   
         CLI   F_FIN,C' '          FLT KEYWORD ONLY?                            
         BE    LR61P                                                            
         CLI   F_FIN,C'*'          WILDCARD?                                    
         BNE   LR61H                                                            
         OC    PCLTFIN,PCLTFIN     ANYTHING IN CLT FINANCIAL?                   
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
         B     LR61P                                                            
LR61H    CLI   F_FIN,C'N'          FIN=N?                                       
         BNE   LR61K                                                            
         CLI   PCLTFIN,C'Y'                                                     
         BE    LR30_SEQ                                                         
         B     LR61P                                                            
LR61K    CLC   F_FIN,PCLTFIN       MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
LR61P    SR    RE,RE                                                            
         IC    RE,F_FIN+L'F_FIN                                                 
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),L'PCLTFIN                                        
         MVC   L_DLNQ/2+01(L'PCLTFIN,RF),PCLTFIN                                
*                                                                               
LR62     CLI   F_DIV+L'F_DIV,0                                                  
         BE    LR63                                                             
         SR    RE,RE                                                            
         IC    RE,F_DIV+L'F_DIV                                                 
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),03           LENGTH OF TITLE AND DASHES                   
         MVC   01(03,RF),=C'Div'                                                
         MVI   L_DLNQ/2+00(RF),03  DEFAULT LENGTH OF FLT DATA                   
         CLI   F_DIV,C' '          FLT KEYWORD ONLY?                            
         BE    LR62P                                                            
         CLI   F_DIV,C'*'          WILDCARD?                                    
         BNE   LR62H                                                            
         OC    PCLTPROF+00(01),PCLTPROF+00                                      
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
         B     LR62P                                                            
LR62H    CLC   F_DIV,PCLTPROF+00   MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
LR62P    SR    RE,RE                                                            
         IC    RE,F_DIV+L'F_DIV                                                 
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),03                                                        
         MVC   01(03,RF),=C'Div'                                                
         MVI   L_DLNQ/2+00(RF),01                                               
         MVC   L_DLNQ/2+01(01,RF),PCLTPROF+00                                   
         DROP  R6                                                               
*                                                                               
LR63     CLI   F_MEDNM+L'F_MEDNM,0                                              
         BE    LR64                                                             
         SR    RE,RE                                                            
         IC    RE,F_MEDNM+L'F_MEDNM                                             
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),10           LENGTH OF TITLE AND DASHES                   
         MVC   01(10,RF),=C'MedNameOvr'                                         
         MVI   L_DLNQ/2+00(RF),10  DEFAULT LENGTH OF FLT DATA                   
         MVI   ELCODE,X'41'        MEDIA NAME OVERRIDE ELEM CODE                
         USING PCLTMEL,R6                                                       
         CLI   F_MEDNM,C' '        FLT KEYWORD ONLY?                            
         BNE   LR63F                                                            
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+8                                                              
         LA    R6,SPACES           SPACES WILL BE DISPLAYED                     
         B     LR63P                                                            
LR63F    L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   LR30_SEQ            DO NOT HAVE IT, NOTHING TO FILTER ON         
         CLI   F_MEDNM,C'*'        WILDCARD?                                    
         BNE   LR63H                                                            
         OC    PCLTMNAM,PCLTMNAM   ANYTHING IN MEDIA NAME OVERRIDE?             
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
         B     LR63P                                                            
LR63H    CLC   F_MEDNM,PCLTMNAM    MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
LR63P    SR    RE,RE                                                            
         IC    RE,F_MEDNM+L'F_MEDNM                                             
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),L'PCLTMNAM                                       
         MVC   L_DLNQ/2+01(L'PCLTMNAM,RF),PCLTMNAM                              
         DROP  R6                                                               
*                                                                               
LR64     L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL            POINT TO MAIN CLT ELEM                       
         BE    *+6                                                              
         DC    H'0'                CLT MAIN ELEM MUST BE THERE!                 
         USING PCLTELEM,R6                                                      
         CLI   F_SFH+L'F_SFH,0                                                  
         BE    LR65                                                             
         SR    RE,RE                                                            
         IC    RE,F_SFH+L'F_SFH                                                 
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),03           LENGTH OF TITLE AND DASHES                   
         MVC   01(03,RF),=C'SFH'                                                
         MVI   L_DLNQ/2+00(RF),03  DEFAULT LENGTH OF FLT DATA                   
         CLI   F_SFH_W,C'*'        WILDCARD?                                    
         BE    LR64D                                                            
         CLI   F_SFH_W,C'N'        SFH=N?                                       
         BE    LR64F                                                            
         CLI   F_SFH,0             FLT KEYWORD ONLY?                            
         BE    LR64P                                                            
         B     LR64H                                                            
LR64D    TM    PCLTSTAT,X'01'      SFH BIT IS ON?                               
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
         B     LR64P                                                            
LR64F    TM    PCLTSTAT,X'01'      SFH BIT IS ON?                               
         BO    LR30_SEQ            YES, SKIP IT (NEGATIVE FILTERING)            
         B     LR64P                                                            
LR64H    MVC   BYTE,PCLTSTAT                                                    
         NI    BYTE,X'01'          ONLY SFH BIT IS ON                           
         CLC   F_SFH,BYTE          MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
LR64P    SR    RE,RE                                                            
         IC    RE,F_SFH+L'F_SFH                                                 
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),01                                               
         MVI   L_DLNQ/2+01(RF),C'N'                                             
         TM    PCLTSTAT,X'01'      SFH BIT IS ON?                               
         BZ    *+8                                                              
         MVI   L_DLNQ/2+01(RF),C'Y'                                             
         DROP  R6                                                               
*                                                                               
LR65     L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL            POINT TO MAIN CLT ELEM                       
         BE    *+6                                                              
         DC    H'0'                CLT MAIN ELEM MUST BE THERE!                 
         USING PCLTELEM,R6                                                      
         CLI   F_FRZ+L'F_FRZ,0                                                  
         BE    LR66                                                             
         SR    RE,RE                                                            
         IC    RE,F_FRZ+L'F_FRZ                                                 
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),06           LENGTH OF TITLE AND DASHES                   
         MVC   01(06,RF),=C'FROZEN'                                             
         MVI   L_DLNQ/2+00(RF),07  DEFAULT LENGTH OF FLT DATA                   
         CLI   F_FRZ_W,C'*'        WILDCARD?                                    
         BE    LR65D                                                            
         CLI   F_FRZ_W,C'N'        FRZ=N?                                       
         BE    LR65F                                                            
         CLI   F_FRZ,0             FLT KEYWORD ONLY?                            
         BE    LR65P                                                            
         B     LR65H                                                            
LR65D    TM    PCLTSTAT,X'02'      FRZ BIT IS ON?                               
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
         B     LR65P                                                            
LR65F    TM    PCLTSTAT,X'02'      FRZ BIT IS ON?                               
         BO    LR30_SEQ            YES, SKIP IT (NEGATIVE FILTERING)            
         B     LR65P                                                            
LR65H    MVC   BYTE,PCLTSTAT                                                    
         NI    BYTE,X'02'          ONLY FRZ BIT IS ON                           
         CLC   F_FRZ,BYTE          MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
LR65P    SR    RE,RE                                                            
         IC    RE,F_FRZ+L'F_FRZ                                                 
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),01                                               
         MVI   L_DLNQ/2+01(RF),C'N'                                             
         TM    PCLTSTAT,X'02'      FRZ BIT IS ON?                               
         BZ    LR66                                                             
         MVI   L_DLNQ/2+01(RF),C'Y'                                             
         ST    RF,FULL                                                          
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'47'                                                     
         BRAS  RE,GETEL            POINT TO MAIN CLT ELEM                       
         BNE   LR66                                                             
         USING PCLTFEL,R6                                                       
         MVI   BYTE,C' '                                                        
         MVC   WORK(L'PCLTFDTE),PCLTFDTE                                        
         MVI   WORK+2,X'01'        DAY                                          
         TM    PCLTFIND,X'08'      + INDICATED?                                 
         BZ    *+8                 NO                                           
         MVI   BYTE,C'+'                                                        
         TM    PCLTFIND,X'04'      - INDICATED?                                 
         BZ    *+8                 NO                                           
         MVI   BYTE,C'-'                                                        
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK),(6,DUB)                                     
*                                                                               
         CLI   BYTE,C' '           ANYTHING IN BYTE?                            
         BNH   *+10                NO                                           
         MVC   DUB+6(1),BYTE                                                    
*                                                                               
         L     RF,FULL                                                          
         MVI   L_DLNQ/2+00(RF),07                                               
         MVC   L_DLNQ/2+01(7,RF),DUB                                            
         DROP  R6                                                               
*                                                                               
LR66     DS    0H                  FOR FUTURE FILTER FLDS                       
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL            POINT TO MAIN CLT ELEM                       
         BE    *+6                                                              
         DC    H'0'                CLT MAIN ELEM MUST BE THERE!                 
         USING PCLTELEM,R6                                                      
         CLI   F_UBC+L'F_UBC,0                                                  
         BE    LR67                                                             
         SR    RE,RE                                                            
         IC    RE,F_UBC+L'F_UBC                                                 
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),03           LENGTH OF TITLE AND DASHES                   
         MVC   01(03,RF),=C'UBC'                                                
         MVI   L_DLNQ/2+00(RF),03  DEFAULT LENGTH OF FLT DATA                   
         CLI   F_UBC_W,C'*'        WILDCARD?                                    
         BE    LR66D                                                            
         CLI   F_UBC_W,C'N'        UBC=N?                                       
         BE    LR66F                                                            
         CLI   F_UBC,0             FLT KEYWORD ONLY?                            
         BE    LR66P                                                            
         B     LR66H                                                            
LR66D    TM    PCLTSTAT,X'80'      UBC BIT IS ON?                               
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
         B     LR66P                                                            
LR66F    TM    PCLTSTAT,X'80'      UBC BIT IS ON?                               
         BO    LR30_SEQ            YES, SKIP IT (NEGATIVE FILTERING)            
         B     LR66P                                                            
LR66H    MVC   BYTE,PCLTSTAT                                                    
         NI    BYTE,X'80'          ONLY UBC BIT IS ON                           
         CLC   F_UBC,BYTE          MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
LR66P    SR    RE,RE                                                            
         IC    RE,F_UBC+L'F_UBC                                                 
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),01                                               
         MVI   L_DLNQ/2+01(RF),C'N'                                             
         TM    PCLTSTAT,X'80'      UBC BIT IS ON?                               
         BZ    *+8                                                              
         MVI   L_DLNQ/2+01(RF),C'Y'                                             
         DROP  R6                                                               
*                                                                               
*                                                                               
LR67     DS    0H                  FUTURE FILTERS                               
*                                                                               
         LA    R2,LSFSTART         POINT TO LISTING AREA                        
         BRAS  RE,LR_DFLTF         DISPLAY FLT FLDS                             
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
         LA    R4,LSTTL2                                                        
         USING LS_TITLE,R4                                                      
         CLI   LTFSTART,0          TITLES DISPLAYED?                            
         BH    *+12                                                             
         LA    R2,WORK             POINT TO LISTING AREA (DUMMY USE)            
         BRAS  RE,LR_DFLTF         DISPLAY FLT FLDS (ONLY NEED TITLES)          
         OI    LSTTL2H+6,X'80'                                                  
         OI    LSTUL2H+6,X'80'                                                  
         B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
LRX_10   TM    CNTLSW,C_MAXIOQ     MAX I/O REACHED?                             
         BZ    EXIT                                                             
         LA    R2,LSTFLTH          PLACE CURSOR ON FILTER FLD                   
         B     TRAPERR2                                                         
*                                                                               
L_DLNQ   EQU   50                  DATA TITLE AND DATA ITSELF                   
L_DASHES DC    30C'-'                                                           
*                                                                               
* R2 POINTS TO DISPLAYING AREA, AIO2 HAS TABLE OF FLT VALUES                    
*                                                                               
LR_DFLTF ST    RE,FULL             SAVE RETURN ADDRESS                          
         LA    RE,LSTTL2                                                        
         USING LS_TITLE,RE                                                      
         LA    RE,LTFSTART         POINT TO TITLE START                         
         DROP  RE                                                               
         LA    RF,LSTUL2                                                        
         USING LS_TITLE,RF                                                      
         LA    RF,LTFSTART         POINT TO TITLE (UNDERLINE) START             
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
         L     RE,AIO                                                           
         MVC   KEY,0(RE)           KEY MIGHT BE DESTROYED ON ADD                
         GOTO1 HIGH                MAKE SURE KEY HAS DISK ADDRESS               
*                                                                               
         XC    FULL,FULL           INIT PARAMETER FOR CKPPTRS                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'50'        CLT TRAFFIC OFFICE CODE ELEM CODE            
         BRAS  RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   FULL+1(1),2(R6)     CURRENT CLT TRAFFIC OFFICE CODE              
         MVI   FULL+0,PTRCKIDQ     TRAFFIC/CLT PASSIVE PTR DIR CODE             
         MVC   FULL+2(1),SVCTRAC   SAVED TRAFFIC OFFICE CODE                    
         BRAS  RE,CKPPTRS          CK FOR PASSIVE POINTERS                      
*                                                                               
         XC    FULL,FULL           INIT PARAMETER FOR CKPPTRS                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'        1ST CLT ELEM CODE                            
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                1ST CLT ELEM MUST BE THERE!                  
         USING PCLTELEM,R6                                                      
         MVI   FULL+0,POFCKIDQ     OFFICE/CLT PASSIVE PTR DIR CODE              
         MVC   FULL+1(1),PCLTOFF   CURRENT OFFICE CODE                          
         MVC   FULL+2(1),SVCOFFC   SAVED OFFICE CODE                            
         DROP  R6                                                               
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
         MVC   KEY,0(RE)           RESTORE KEY TO REDDISPLAY REC                
         J     DR                                                               
*                                                                               
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
RECACERR MVI   ERROR,012           INVALID RECORD ACTION ERROR                  
         LA    R2,CONACTH          POINT TO ACTION FLD                          
         J     TRAPERR                                                          
*                                                                               
RECNFERR MVI   ERROR,053           RECORD NOT FOUND                             
         J     TRAPERR                                                          
*                                                                               
INVDVERR MVI   ERROR,065           INVALID DIVISION                             
         J     TRAPERR                                                          
*                                                                               
CLTRQERR MVI   ERROR,085           SPECIFIC CLT REQUIRED (SECURITY)             
         J     TRAPERR                                                          
*                                                                               
PFKEYERR MVI   ERROR,088           INVALID PFKEY                                
         LA    R2,CONACTH          POINT TO ACTION FLD                          
         J     TRAPERR                                                          
*                                                                               
CLTACERR MVI   ERROR,089           CLIENT LIMIT ACCESS ERROR                    
         J     TRAPERR                                                          
*                                                                               
PTOFDERR MVI   ERROR,095           PRD ASSIGNED Y/N CANNOT SET TO 'N'           
         J     TRAPERR                                                          
*                                                                               
DUPEDERR MVI   ERROR,179           DUPLICATE ENTRIES ERROR MSG                  
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
CKCTFCLT NTR1  BASE=*,LABEL=*      CK CONTROL FILE CLIENT                       
*                                                                               
         XC    WORK,WORK                                                        
         LA    RE,SVKEY                                                         
         LA    R3,WORK                                                          
         USING ZENRECD,R3                                                       
         USING PCLTKEY,RE                                                       
         MVI   ZENKCODE,ZENKCODQ                                                
         MVI   ZENKTYP,ZENCLTQ                                                  
         MVC   ZENKAGY,SVCTAGY                                                  
         MVC   ZENKCLT,PCLTKCLT                                                 
         L     R4,AIO3                                                          
         GOTOR DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',(R3),(R4)                     
         CLC   0(25,R3),0(R4)                                                   
         BNE   CKCTF_ER                                                         
*                                                                               
         LA    R4,(ZENFIRST-ZENRKEY)(R4)                                        
         USING ZENELEM,R4                                                       
         XC    CLTCLTN,CLTCLTN                                                  
         MVC   CLTCLTN(20),ZENCNAME                                             
         MVI   CLTCLTNH+5,20       FORCE LENGTH                                 
         OI    CLTCLTNH+6,X'80'    TRANSMIT                                     
         XC    CLTCNAM,CLTCNAM                                                  
         MVC   CLTCNAM(20),ZENCNAME                                             
         MVI   CLTCNAMH+5,20       FORCE LENGTH                                 
         OI    CLTCNAMH+6,X'80'    TRANSMIT                                     
         B     CKCTF_X                                                          
*                                                                               
CKCTF_ER LHI   RF,280                                                           
         ST    RF,FULL             SET ERROR MSG NUMBER                         
         J     SETCCNEQ                                                         
*                                                                               
CKCTF_X  J     SETCCEQ                                                          
         LTORG                                                                  
         DROP  RB,R4,R3                                                         
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
DISPPGST NTR1  BASE=*,LABEL=*      DISPLAY PST AND GST CODES                    
*                                                                               
         L     R6,AIO                                                           
         USING PCLTELEM,R6                                                      
         MVI   ELCODE,X'02'        FIRST CLIENT ELEM CODE                       
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   PCLTGST,0                                                        
         BNE   *+8                                                              
         MVI   PCLTGST,C'S'        DISPLAY DEFAULT, WILL NOT WRT TO REC         
         MVC   CLTGST,PCLTGST      GST CODE                                     
         OI    CLTGSTH+6,X'80'                                                  
*                                                                               
         XC    CLTPST,CLTPST                                                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'25'                                                     
         BRAS  RE,GETEL            ANYTHING TO DISPLAY                          
         BNE   DISPPG20                                                         
*                                                                               
         LA    R4,ELEM                                                          
         USING PSTBLKD,R4                                                       
         XC    ELEM,ELEM           CLEAR INTERFACE BLOCK                        
         MVI   PSTACT,PSTFMTQ      ACTION = FORMAT                              
         LA    R1,2(R6)                                                         
         ST    R1,PSTADIN          INPUT ADDRESS                                
         XC    PSTOUT,PSTOUT                                                    
         LA    R1,PSTOUT                                                        
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,ACOMFACS    A(COMFACS)                                   
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A6B'                                           
         GOTO1 CALLOV,DMCB                                                      
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R4)                                                   
         MVC   CLTPST,PSTOUT       OUTPUT                                       
DISPPG20 OI    CLTPSTH+6,X'80'                                                  
*                                                                               
         XC    CLTMPS,CLTMPS                                                    
         L     R6,AIO                                                           
         MVI   ELCODE,PCLTMPEQ                                                  
         BRAS  RE,GETEL            Have Main PST to display?                    
         BNE   DISPPG30                                                         
*                                                                               
         LA    R4,ELEM                                                          
         USING PSTBLKD,R4                                                       
         XC    ELEM,ELEM           CLEAR INTERFACE BLOCK                        
         MVI   PSTACT,PSTFMTQ      ACTION = FORMAT                              
         LA    R1,2(R6)                                                         
         ST    R1,PSTADIN          INPUT ADDRESS                                
         XC    PSTOUT,PSTOUT                                                    
         LA    R1,PSTOUT                                                        
         ST    R1,PSTADOUT         OUTPUT ADDRESS                               
         MVC   PSTACOM,ACOMFACS    A(COMFACS)                                   
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A6B'                                           
         GOTO1 CALLOV,DMCB                                                      
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(R4)                                                   
         MVC   CLTMPS,PSTOUT       OUTPUT                                       
DISPPG30 OI    CLTMPSH+6,X'80'                                                  
*                                                                               
DISPPG_X J     EXIT                                                             
*                                                                               
GET_ITXT ST    RE,SAVERE                                                        
         XC    FULL,FULL                                                        
         MVI   FULL+1,25           SYSTEM                                       
         L     R3,FULL                                                          
         XC    CONHEAD,CONHEAD                                                  
         GOTOR GETTXT,DMCB+12,(R2),0,(C'I',DMCB),0,0,(R3)                       
         OI    CONHEADH+6,X'80'                                                 
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
GET_ETXT ST    RE,SAVERE                                                        
         XC    FULL,FULL                                                        
         MVI   FULL+1,25           SYSTEM                                       
         L     R3,FULL                                                          
         XC    CONHEAD,CONHEAD                                                  
         GOTOR GETTXT,DMCB+12,(R2),0,(C'E',DMCB),0,0,(R3)                       
         OI    CONHEADH+6,X'80'                                                 
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB,R6,R4                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKFINCLT NTR1  BASE=*,LABEL=*      CK FINANCIAL CLT                             
*                                                                               
         LA    RE,AGYXTBL          AGY TO UNPROTECT FINANCIAL FLD               
CKFCL20  CLI   0(RE),X'FF'         END OF TABLE?                                
         JE    SETCCNEQ            YES (SET CC NOT EQUAL)                       
         CLC   AGENCY,0(RE)        AGENCY IN TABLE?                             
         JE    SETCCEQ             YES - UNPROTECT FINANCIAL CLT FLD            
         LA    RE,2(RE)            NEXT ENTRY                                   
         B     CKFCL20                                                          
*                                                                               
* AGENCIES FOR WHICH FINANCIAL FLD IS UNPROTECTED AND NOT CLEARED               
*                                                                               
AGYXTBL  DC    C'DM'                                                            
         DC    C'MK'                                                            
         DC    C'MX'                                                            
         DC    C'SJ'                                                            
         DC    C'WI'                                                            
         DC    C'WJ'                                                            
         DC    C'WT'                                                            
         DC    C'XD'                                                            
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKAOFF   NTR1  BASE=*,LABEL=*      CK ACCOUNTING OFFICE                         
*                                                                               
         LA    R4,ELEM             POINTS TO ELEM TO BE BUILT                   
         USING PCLTELEM,R4                                                      
*                                                                               
         MVI   COMPCD,0                                                         
         MVI   ASW,0               SET TO X'01' IF SWITCHED TO ACC              
         MVI   ACCOFFSW,0          SET TO X'80' IF CLT OFFICE CODE USED         
         XC    POWCODE,POWCODE                                                  
         XC    ACCOFF,ACCOFF                                                    
         MVI   SVACCOFC,C'Y'       SET ACC OFFICE REQUIRED                      
         CLI   SVAGYPF+11,C'A'                                                  
         BE    *+16                                                             
         CLI   SVAGYPF+11,C'B'                                                  
         BE    *+8                                                              
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
         IC    R1,CLTAOFFH+5       INPUT LENGTH                                 
*                                                                               
         LA    R2,CLTAOFF+0                                                     
         CLI   CLTAOFFH+5,0                                                     
         BE    CKAOF50             INPUT LENGTH IS 0                            
         LA    R2,CLTAOFF+1                                                     
         CLI   CLTAOFFH+5,1                                                     
         BE    CKAOF50             INPUT LENGTH IS 1                            
         LA    R2,CLTAOFF+2                                                     
         CLI   CLTAOFFH+5,2                                                     
         BE    CKAOF50             INPUT LENGTH IS 2                            
*                                                                               
         LA    R2,CLTAOFF          POINT TO INPUT FLD                           
         LA    RE,5                MAX OF 5 CHARS INPUT                         
         SR    R1,R1                                                            
CKAOF30  CLI   0(R2),C','                                                       
         BE    CKAOF50                                                          
         CLI   0(R2),C'/'                                                       
         BE    CKAOF50                                                          
         CLI   0(R2),C' '                                                       
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         LA    R2,1(R2)                                                         
         BCT   RE,CKAOF30                                                       
*                                                                               
CKAOF50  DS    0H                  CK FOR LENGTH                                
         STC   R1,OFFLEN                                                        
         MVC   ACCOFF(2),CLTAOFF                                                
         CLI   OFFLEN,1                                                         
         BNE   *+8                                                              
         MVI   ACCOFF+1,C' '                                                    
         CLI   1(R2),C' '          OVERRIDE AGENCY?                             
         BNH   *+10                                                             
         MVC   POWCODE,1(R2)       YES - SAVE POWER CODE                        
*                                                                               
         CLI   OFFLEN,2                                                         
         BH    CKAOF90             ERR: INVALID OFFICE CODE                     
         CLI   OFFLEN,0                                                         
         BNE   CKAOF59                                                          
         CLI   SVACCOFC,C'Y'       ACC OFFICE CODE REQUIRED?                    
         BNE   *+12                                                             
         LA    R2,CLTAOFFH                                                      
         J     MSSNGERR            FIELD IS REQUIRED                            
         XC    PCLTAOFC,PCLTAOFC                                                
         XC    PCLTACCA,PCLTACCA                                                
         CLI   CLTOFFH+5,0         HAVE CLT OFFICE CODE?                        
         JNH   SETCCEQ                                                          
         MVC   PCLTAOFC,SPACES                                                  
         SR    RE,RE                                                            
         IC    RE,CLTOFFH+5                                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PCLTAOFC(0),CLTOFF  DEFAULT TO CLT OFFICE                        
         IC    RE,CLTOFFH+5                                                     
         STC   RE,OFFLEN           VALIDATE PER COMPANY REC                     
         OI    ACCOFFSW,X'80'      SET SWITCH - CLT OFFICE CODE USED            
         B     CKAOF60                                                          
*                                                                               
CKAOF59  OC    POWCODE,POWCODE     OVERRIDE AGY?                                
         BNZ   CKAOF61                                                          
         CLI   SVACCOFC,C'Y'       ACC OFFICE CODE REQUIRED?                    
         BNE   CKAOF88                                                          
*                                                                               
* SWITCH TO ACC SYSTEM                                                          
*                                                                               
CKAOF60  GOTO1 SWITCH,DMCB,=C'ACC',0                                            
         B     CKAOF85                                                          
*                                                                               
CKAOF61  LA    R3,SVACCAGY         VALIDATE AGAINST AGY HDR CODE LIST           
         LA    R1,4                                                             
CKAOF63  CLC   0(2,R3),POWCODE     MATCH?                                       
         BE    CKAOF65                                                          
         CLI   0(R3),C' '          END OF TABLE?                                
         BNH   CKAOF93             ERR: INVALID AGENCY CODE                     
         LA    R3,2(R3)                                                         
         BCT   R1,CKAOF63                                                       
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
         AHI   R6,ACCORFST         FIRST ELEM (IN OLD FILE FORMAT)              
CKAOF86  CLI   0(R6),CPYELQ        X'10' COMPANY ELEM?                          
         BE    CKAOF86H                                                         
         ZIC   R0,0(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   CKAOF86                                                          
         B     CKAOF97             ERR: ACC COMPANY REC NOT FOUND               
         USING CPYELD,R6                                                        
CKAOF86H TM    CPYSTAT4,CPYSOFF2   2 CHAR REQ'D?                                
         BO    CKAOF87             YES = VALIDATE OFFICE                        
         CLI   OFFLEN,1                                                         
         BNE   CKAOF92             ERR: 1 CHAR REQUIRED                         
         B     CKAOF88                                                          
*                                                                               
CKAOF87  CLI   OFFLEN,2                                                         
         BNE   CKAOF91             ERR: 2 CHARS REQUIRED                        
         LA    R6,WKACCKEY         NEW OFFICE - LOOK FOR OFFICE REC             
         USING OFFRECD,R6                                                       
         MVC   WKACCKEY,SPACES                                                  
         MVI   OFFKTYP,OFFKTYPQ    X'01'                                        
         MVC   OFFKCPY,COMPCD                                                   
         MVC   OFFKOFF(2),ACCOFF                                                
*                                                                               
         L     R6,AIO3                                                          
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',WKACCKEY,(R6)                
         CLI   8(R1),0                                                          
         BNE   CKAOF98             ERR: INVALID ACC OFFICE CODE                 
         TM    OFFRSTAT,OFFSLIST   CANNOT BE AN OFFICE LIST                     
         BO    CKAOF99                                                          
*                                                                               
         DROP  R6                                                               
*                                                                               
CKAOF88  DS    0H                  OFFICE CODE IS GOOD                          
         TM    ACCOFFSW,X'80'      CLT OFFICE CODE USED?                        
         BNZ   *+10                                                             
         MVC   PCLTAOFC,ACCOFF     SAVE OFFICE CODE                             
         MVC   PCLTACCA,POWCODE    SAVE AGY CODE                                
         CLI   ASW,0               SWITCHED TO AN ACC SYSTEM?                   
         JE    SETCCEQ                                                          
         BRAS  RE,RETURN                                                        
         J     SETCCEQ                                                          
*                                                                               
CKAOF90  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=C'** ERROR - INVALID OFFICE CODE'                   
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
         B     CKAOF99X                                                         
*                                                                               
CKAOF99  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(39),=C'** ERROR - OFFICE LIST CODE NOT ALLOWED'          
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
         DROP  R4                                                               
*                                                                               
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
ACCOFFSW DS    X                   X'80' - USE CLT OFFICE CODE                  
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* CHECK FOR TRAFFIC OFFICE CODE (ELEM CODE X'50')                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKTRAOFF NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   BYTE,0              WILL RETURN OLD TRAFFIC CODE                 
         MVI   OFCOFC,0                                                         
*                                                                               
         CLI   5(R2),0             ANY INPUTS?                                  
         BE    CKTRA03             NO, NEED TO CK FOR OLD ELEM (IF ANY)         
*                                                                               
         CLI   5(R2),2             MORE THAN 2 CHARACTERS ?                     
         JH    SETCCNEQ            YES - INVALID                                
*                                                                               
         XC    OFCBLK,OFCBLK       INIT OFFICER BLOCK                           
         MVI   OFCSYS,C'P'         PRINT SYSTEM                                 
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC2,8(R2)                                                    
         GOTOR OFFICER,DMCB,(C'2',OFFICED),(0,ACOMFACS)                         
         MVC   WORK,OFCOFC         SAVE 1 BYTE INTERNAL OFFICE CODE             
         TM    OFCINDS,OFCINOLA    USING 2 CHAR OFFICES?                        
         BNZ   CKTRA01                                                          
         CLI   0(R1),0                                                          
         JNE   SETCCNEQ                                                         
         B     CKTRA03                                                          
*                                                                               
CKTRA01  DS    0H                                                               
         CLI   8(R2),C'='          CHECKING FOR INVALID CHARACTERS              
         JE    SETCCNEQ                                                         
         CLI   8(R2),C'-'                                                       
         JE    SETCCNEQ                                                         
         CLI   8(R2),C','                                                       
         JE    SETCCNEQ                                                         
         CLI   8(R2),C'.'                                                       
         JE    SETCCNEQ                                                         
*                                                                               
CKTRA03  L     R6,AIO                                                           
         MVI   ELCODE,X'50'        LOOK FOR TRAFFIC OFFICE CODE ELEM            
         BRAS  RE,GETEL                                                         
         BNE   CKTRA05             NOT FOUND, GO BUILD ELEM AND ADD IT          
*                                                                               
         MVC   BYTE,2(R6)          SAVE CURRENT TRAFFIC OFFICE CODE             
*SMY*    CLC   BYTE,8(R2)          SAME AS THAT OF INPUT?                       
         CLC   BYTE,OFCOFC         SAME AS THAT OF INPUT?                       
         BE    CKTRA50             YES, NO NEED TO REMOVE ELEM                  
         GOTO1 REMELEM             REMOVE TRAFF OFFICE CODE ELEM                
*                                                                               
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   CKTRA05             SHOULD NOT FIND IT!                          
         DC    H'0'                                                             
*                                                                               
CKTRA05  DS    0H                                                               
         CLI   5(R2),0             ANY INPUTS?                                  
         BE    CKTRA50             NO, NO NEED TO BUILD ELEM                    
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM+0,X'50'        ELEM CODE                                    
         MVI   ELEM+1,5            ELEM LENGTH                                  
*SMY*    MVC   ELEM+2(1),8(R2)     NEW TRAFFIC OFFICE CODE                      
         MVC   ELEM+2(1),OFCOFC    NEW TRAFFIC OFFICE CODE                      
         GOTO1 ADDELEM                                                          
*                                                                               
CKTRA50  DS    0H                  FUTURE USES                                  
*                                                                               
         J     SETCCEQ             EQUAL                                        
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* ADD/DELETE OFFICE OR TRAFFIC OFFICE PASSIVE POINTERS                          
*                                                                               
* FULL+0 - PASSIVE POINTER DIR CODE (I.E. X'A1' OR X'A2')                       
* FULL+1 - CURRENT TRAFFIC OFFICE/OFFICE CODE IN CLT REC                        
* FULL+2 - OLD (SAVED) TRAFFIC OFFICE/OFFICE CODE                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKPPTRS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVDMINBT,DMINBTS    SAVE ORIGINAL DM IN/OUT BYTES                
         MVC   SVDMOUTB,DMOUTBTS                                                
         MVI   DMINBTS,X'08'       SET TO PASS DELETES                          
         MVI   DMOUTBTS,0          SUPPRESS DATAMGR ERRORS                      
         MVC   SVWORK(L'KEY),KEY   SAVE KEY                                     
         MVC   SVWORK+L'KEY(4),AIO SAVE ORIGINAL AIO                            
         MVC   DUB+4(4),KEY+27     DISK ADDRESS                                 
*                                                                               
         MVC   AIO,AIO3            USE A DIFFERENT AIO                          
*                                                                               
         CLC   FULL+1(1),FULL+2    OFFICE CODE CHANGED?                         
         BE    CKPPT30             NO, CK IF IT NEEDS TO BE ADDED               
*                                                                               
* DELETE OLD OFFICE/TRAFFIC PASSIVE POINTER (IF ANY)                            
*                                                                               
         CLI   FULL+2,C' '         OLD VALUE PRESENT?                           
         BNH   CKPPT30                                                          
         XC    KEY+3(L'KEY-3),KEY+3                                             
         MVC   KEY+3(1),FULL+0     PAASIVE POINTER DIR CODE                     
         MVC   KEY+4(1),FULL+2     OLD OFFICE/TRAFFIC CODE (SAVED)              
         MVC   KEY+5(3),SVWORK+4   CLT CODE                                     
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE      ON DIR?                                      
         BNE   CKPPT30                                                          
         TM    KEY+25,X'80'        ALREADY DELETED?                             
         BO    CKPPT30                                                          
         OI    KEY+25,X'80'        FLAG FOR DELETION                            
         GOTO1 WRITE                                                            
*                                                                               
CKPPT30  CLI   FULL+1,C' '         NEW OFFICE PRESENT?                          
         BNH   CKPPT70                                                          
         CLI   FULL+0,0            PASSIVE POINTER DIR CODE PRESENT?            
         BNE   *+6                                                              
         DC    H'0'                IT MUST BE THERE!                            
         XC    KEY+3(L'KEY-3),KEY+3                                             
         MVC   KEY+3(1),FULL+0     PASSIVE POINTER DIR CODE                     
         MVC   KEY+4(1),FULL+1     NEW OFFICE/TRAFFIC CODE                      
         MVC   KEY+5(3),SVWORK+4   CLT CODE                                     
         MVC   KEY+27(4),DUB+4     SET DISK ADDRESS                             
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE      ON DIR?                                      
         BNE   CKPPT40                                                          
         TM    KEY+25,X'80'        DELETED?                                     
         BZ    CKPPT70                                                          
         NI    KEY+25,X'FF'-X'80'  UNDELETE                                     
         GOTO1 WRITE                                                            
         B     CKPPT70                                                          
*                                                                               
CKPPT40  MVC   KEY,KEYSAVE         RESTORE KEY FROM HIGH                        
         GOTO1 ADD                                                              
*                                                                               
CKPPT70  MVC   DMINBTS,SVDMINBT    RESTORE DMINBTS AND DMOUTBTS                 
         MVC   DMOUTBTS,SVDMOUTB                                                
         MVC   KEY,SVWORK          RESTORE KEY                                  
         MVC   AIO,SVWORK+L'KEY    RESTORE AIO                                  
*                                                                               
         J     EXIT                                                             
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
         CLI   SC2NDLEN,L'OFCOFC2                                               
         BH    CKFFD_E2                                                         
         CLI   SC2NDLEN,1                                                       
         BNE   CKFFD30K                                                         
         MVC   F_OFC,SC2NDFLD                                                   
         CLI   F_OFC,C'*'          WILDCARD?                                    
         BE    CKFFD30M                                                         
CKFFD30K XC    OFCBLK,OFCBLK       INIT OFFICER BLOCK                           
         MVI   OFCSYS,C'P'         PRINT SYSTEM                                 
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC2,SC2NDFLD                                                 
         GOTO1 OFFICER,DMCB,(C'2',OFFICED),(0,ACOMFACS)                         
         MVC   F_OFC,OFCOFC        1 BYTE INTERNAL OFFICE CODE                  
         CLI   F_OFC,0                                                          
         BNE   *+8                                                              
         MVI   F_OFC,C' '          KEYWORD ONLY                                 
         CLI   0(R1),0                                                          
         BE    *+8                                                              
         MVI   F_OFC,X'FF'         INVALID OFFICE CODE IS ENTERED               
CKFFD30M CLI   F_OFC+L'F_OFC,0                                                  
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_OFC+L'F_OFC                                                 
         B     CKFFD90                                                          
*                                                                               
CKFFD32  CLC   =C'AOF',SC1STFLD                                                 
         BNE   CKFFD34                                                          
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
CKFFD34  CLC   =C'AGY',SC1STFLD                                                 
         BNE   CKFFD36                                                          
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
CKFFD36  CLC   =C'TOF',SC1STFLD                                                 
         BNE   CKFFD38                                                          
         CLI   SC1STLEN,3                                                       
         BNE   CKFFD_E2                                                         
*SMY*    CLI   SC2NDLEN,L'F_TOF                                                 
         CLI   SC2NDLEN,L'OFCOFC2                                               
         BH    CKFFD_E2                                                         
         CLI   SC2NDLEN,1                                                       
         BNE   CKFFD36K                                                         
         MVC   F_TOF,SC2NDFLD      TRAFFIC OFFICE FLT VALUE                     
         CLI   F_TOF,C'*'          WILDCARD?                                    
         BE    CKFFD36M                                                         
CKFFD36K XC    OFCBLK,OFCBLK       INIT OFFICER BLOCK                           
         MVI   OFCSYS,C'P'         PRINT SYSTEM                                 
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC2,SC2NDFLD                                                 
         GOTO1 OFFICER,DMCB,(C'2',OFFICED),(0,ACOMFACS)                         
         MVC   F_TOF,OFCOFC        1 BYTE INTERNAL OFFICE CODE                  
         CLI   F_TOF,0                                                          
         BNE   *+8                                                              
         MVI   F_TOF,C' '          KEYWORD ONLY                                 
         CLI   0(R1),0                                                          
         BE    *+8                                                              
         MVI   F_TOF,X'FF'         INVALID OFFICE CODE IS ENTERED               
CKFFD36M CLI   F_TOF+L'F_TOF,0                                                  
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_TOF+L'F_TOF                                                 
         B     CKFFD90                                                          
*                                                                               
CKFFD38  CLC   =C'INUM',SC1STFLD                                                
         BNE   CKFFD40                                                          
         CLI   SC1STLEN,4                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,3                                                       
         BH    CKFFD38F                                                         
         MVC   F_CLTNUM,SC2NDFLD   CLT NUMBER (INTERFACE NUMBER)                
         B     CKFFD38U                                                         
CKFFD38F TM    SC2NDVAL,SCNUMQ     FILTER VALUE IS NUMERIC?                     
         BZ    CKFFD_E2                                                         
         CLI   SC2NDLEN,5                                                       
         BH    CKFFD_E2            FOR NUMERIC VALUES, MAX IS 5 DIGITS          
         CLI   SC2NDLEN,4                                                       
         BNE   CKFFD38H                                                         
         MVI   F_CLTNUM,X'FF'      4 DIGITS FORMAT IS X'FF' AND PWS             
         ICM   RF,15,SC2NDNUM                                                   
         CVD   RF,DUB                                                           
         L     RF,DUB+4                                                         
         SRL   RF,4                SHIFT OUT SIGN                               
         STH   RF,DUB                                                           
         MVC   F_CLTNUM+1(2),DUB   PACK UNSIGNED INTERFACE CODE                 
         B     CKFFD38U                                                         
CKFFD38H MVC   F_CLTNUM,SC2NDNUM+1                                              
         OI    F_CLTNUM,X'80'      5 DIGITS FORMAT IS X'80' AND BINARY          
CKFFD38U CLI   F_CLTNUM+L'F_CLTNUM,0                                            
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_CLTNUM+L'F_CLTNUM                                           
         B     CKFFD90                                                          
*                                                                               
CKFFD40  CLC   =C'CSCOM',SC1STFLD                                               
         BNE   CKFFD42                                                          
         CLI   SC1STLEN,5                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,L'F_CSCOM                                               
         BH    CKFFD_E2                                                         
         CLI   SC2NDLEN,0                                                       
         BE    CKFFD40H                                                         
         BL    CKFFD_E2            BAD LENGTH                                   
         LA    R2,F_CSCOM                                                       
         BRAS  RE,CKFFD_CM                                                      
         B     *+10                                                             
CKFFD40H MVC   F_CSCOM,SC2NDFLD    CONTRACT STANDARD COMMENT CODE               
         CLI   F_CSCOM+L'F_CSCOM,0                                              
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_CSCOM+L'F_CSCOM                                             
         B     CKFFD90                                                          
*                                                                               
CKFFD42  CLC   =C'IOCOM1',SC1STFLD                                              
         BNE   CKFFD44                                                          
         CLI   SC1STLEN,6                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,L'F_IOCOM1                                              
         BH    CKFFD_E2                                                         
         CLI   SC2NDLEN,0                                                       
         BE    CKFFD42H                                                         
         BL    CKFFD_E2            BAD LENGTH                                   
         LA    R2,F_IOCOM1                                                      
         BRAS  RE,CKFFD_CM                                                      
         B     *+10                                                             
CKFFD42H MVC   F_IOCOM1,SC2NDFLD   I/O COMMENT CODE 1                           
         CLI   F_IOCOM1+L'F_IOCOM1,0                                            
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_IOCOM1+L'F_IOCOM1                                           
         B     CKFFD90                                                          
*                                                                               
CKFFD44  CLC   =C'IOCOM2',SC1STFLD                                              
         BNE   CKFFD46                                                          
         CLI   SC1STLEN,6                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,L'F_IOCOM2                                              
         BH    CKFFD_E2                                                         
         CLI   SC2NDLEN,0                                                       
         BE    CKFFD44H                                                         
         BL    CKFFD_E2            BAD LENGTH                                   
         LA    R2,F_IOCOM2                                                      
         BRAS  RE,CKFFD_CM                                                      
         B     *+10                                                             
CKFFD44H MVC   F_IOCOM2,SC2NDFLD   I/O COMMENT CODE 2                           
         CLI   F_IOCOM2+L'F_IOCOM2,0                                            
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_IOCOM2+L'F_IOCOM2                                           
         B     CKFFD90                                                          
*                                                                               
CKFFD46  CLC   =C'BILGRP',SC1STFLD                                              
         BNE   CKFFD48                                                          
         CLI   SC1STLEN,6                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,L'F_BILGRP                                              
         BH    CKFFD_E2                                                         
         MVC   F_BILGRP,SC2NDFLD   BILLING GROUP CODE                           
         CLI   F_BILGRP+L'F_BILGRP,0                                            
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_BILGRP+L'F_BILGRP                                           
         B     CKFFD90                                                          
*                                                                               
CKFFD48  CLC   =C'DRDOV',SC1STFLD                                               
         BNE   CKFFD50                                                          
         CLI   SC1STLEN,5                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,L'F_DRDOV                                               
         BH    CKFFD_E2                                                         
         MVC   F_DRDOV,SC2NDFLD    DRD OVERRIDE CLT CODE                        
         CLI   F_DRDOV+L'F_DRDOV,0                                              
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_DRDOV+L'F_DRDOV                                             
         B     CKFFD90                                                          
*                                                                               
CKFFD50  CLC   =C'FIN',SC1STFLD                                                 
         BNE   CKFFD52                                                          
         CLI   SC1STLEN,3                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,3          YES OR Y?                                    
         BH    CKFFD_E2                                                         
         MVC   F_FIN,SC2NDFLD      FINANCIAL STATUS                             
         CLI   F_FIN+L'F_FIN,0                                                  
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_FIN+L'F_FIN                                                 
         B     CKFFD90                                                          
*                                                                               
CKFFD52  CLC   =C'DIV',SC1STFLD                                                 
         BNE   CKFFD54                                                          
         CLI   SC1STLEN,3                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,L'F_DIV                                                 
         BH    CKFFD_E2                                                         
         MVC   F_DIV,SC2NDFLD      CLT DIV                                      
         CLI   F_DIV+L'F_DIV,0                                                  
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_DIV+L'F_DIV                                                 
         B     CKFFD90                                                          
*                                                                               
CKFFD54  CLC   =C'MEDNM',SC1STFLD                                               
         BNE   CKFFD56                                                          
         CLI   SECMNOV,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    CKFFD_E3                                                         
         CLI   SC1STLEN,5                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,L'F_MEDNM                                               
         BH    CKFFD_E2                                                         
         MVC   F_MEDNM,SC2NDFLD    MEDIA NAME OVERRIDE                          
         CLI   F_MEDNM+L'F_MEDNM,0                                              
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_MEDNM+L'F_MEDNM                                             
         B     CKFFD90                                                          
*                                                                               
CKFFD56  CLC   =C'SFH',SC1STFLD                                                 
         BNE   CKFFD57                                                          
         CLI   SECSFHG,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    CKFFD_E3                                                         
         CLI   SC1STLEN,3                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,0          JUST FILTER?                                 
         BNH   CKFFD56M            NOTE: NULL(S), MEANS FLT ONLY                
         CLI   SC2NDLEN,3          YES OR Y?                                    
         BH    CKFFD_E2                                                         
         CLI   SC2NDFLD,C'*'       WILDCARD?                                    
         BNE   *+8                                                              
         MVI   F_SFH_W,C'*'        IN THIS CASE, WILL DO SAME AS "Y"            
         CLI   SC2NDFLD,C'N'       SFH=N?                                       
         BNE   *+8                                                              
         MVI   F_SFH_W,C'N'        FOR NEGATIVE FILTERING                       
         CLI   SC2NDFLD,C'Y'       YES OR Y?                                    
         BNE   *+8                                                              
         OI    F_SFH,X'01'         BIT FOR SFH=Y                                
CKFFD56M CLI   F_SFH+L'F_SFH,0                                                  
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_SFH+L'F_SFH                                                 
         B     CKFFD90                                                          
*                                                                               
CKFFD57  CLC   =C'UBC',SC1STFLD                                                 
         BNE   CKFFD58                                                          
         CLI   SECUBCG,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    CKFFD_E3                                                         
         CLI   SC1STLEN,3                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,0          JUST FILTER?                                 
         BNH   CKFFD57M            NOTE: NULL(S), MEANS FLT ONLY                
         CLI   SC2NDLEN,3          YES OR Y?                                    
         BH    CKFFD_E2                                                         
         CLI   SC2NDFLD,C'*'       WILDCARD?                                    
         BNE   *+8                                                              
         MVI   F_UBC_W,C'*'        IN THIS CASE, WILL DO SAME AS "Y"            
         CLI   SC2NDFLD,C'N'       UBC=N?                                       
         BNE   *+8                                                              
         MVI   F_UBC_W,C'N'        FOR NEGATIVE FILTERING                       
         CLI   SC2NDFLD,C'Y'       YES OR Y?                                    
         BNE   *+8                                                              
         OI    F_UBC,X'80'         BIT FOR UBC=Y                                
CKFFD57M CLI   F_UBC+L'F_UBC,0                                                  
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_UBC+L'F_UBC                                                 
         B     CKFFD90                                                          
*                                                                               
CKFFD58  CLC   =C'FRZ',SC1STFLD                                                 
         BNE   CKFFD60                                                          
         CLI   SC1STLEN,3                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,0          JUST FILTER?                                 
         BNH   CKFFD58M            NOTE: NULL(S), MEANS FLT ONLY                
         CLI   SC2NDLEN,3          YES OR Y?                                    
         BH    CKFFD_E2                                                         
         CLI   SC2NDFLD,C'*'       WILDCARD?                                    
         BNE   *+8                                                              
         MVI   F_FRZ_W,C'*'        IN THIS CASE, WILL DO SAME AS "Y"            
         CLI   SC2NDFLD,C'N'       FRZ=N?                                       
         BNE   *+8                                                              
         MVI   F_FRZ_W,C'N'        FOR NEGATIVE FILTERING                       
         CLI   SC2NDFLD,C'Y'       YES OR Y?                                    
         BNE   *+8                                                              
         OI    F_FRZ,X'02'         BIT FOR FRZ=Y                                
CKFFD58M CLI   F_FRZ+L'F_FRZ,0                                                  
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_FRZ+L'F_FRZ                                                 
         B     CKFFD90                                                          
*                                                                               
CKFFD60  DS    0H                  FOR FUTURE FILTER KEYWORDS                   
*                                                                               
         B     CKFFD_E2            KEYWORD IS NOT DEFINED                       
*                                                                               
CKFFD90  LA    R4,SCBLKLQ(R4)      POINT TO NEXT SCANNER BLK                    
         AHI   R5,1                LOOP COUNTER UP BY ONE                       
         B     CKFFD20             CK FOR NEXT SCANNER BLK                      
*                                                                               
         J     SETCCEQ                                                          
*                                                                               
* R2 = OUTPUT AREA FOR RIGHT JUSTIFIED COMMENT CODE                             
*                                                                               
CKFFD_CM MVC   0(6,R2),SPACES      RIGHT JUSTIFY COMMENT CODE                   
         SR    R1,R1                                                            
         IC    R1,SC2NDLEN                                                      
         LCR   RF,R1                                                            
         AHI   RF,6                                                             
         AR    R2,RF               POINT TO END OF LEADING SPACE(S)             
         BCTR  R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),SC2NDFLD                                                 
         BR    RE                                                               
*                                                                               
         DROP  R4                                                               
*                                                                               
CKFFD_E1 XC    F_FLDS(F_FLDLNQ),F_FLDS                                          
         J     DUPEDERR                                                         
*                                                                               
CKFFD_E2 LHI   R2,INVFUSGE                                                      
         B     CKF_GTXT                                                         
*                                                                               
CKFFD_E3 LHI   R2,FLTKWNTA                                                      
         B     CKF_GTXT                                                         
*                                                                               
CKF_GTXT XC    FULL,FULL                                                        
         MVI   FULL+1,25           SYSTEM                                       
         L     R3,FULL                                                          
         GOTOR GETTXT,DMCB+12,(R2),0,(C'E',DMCB),0,0,(R3)                       
         OI    CONHEADH+6,X'80'                                                 
         J     SETCCNEQ                                                         
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INITIALZ NTR1  BASE=*,LABEL=*      INITIALIZE WORKING STORAGES                  
*                                                                               
         ST    R3,RELO                                                          
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A38'                                           
         GOTO1 CALLOV,DMCB         GET OFFICER ADDRESS                          
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   OFFICER,DMCB        SAVE ADDRESS OF OFFICER                      
*                                                                               
         CLI   TRANSSW,C'Y'        TRANSFERRED INTO PROGRAM?                    
         BNE   INITI50                                                          
         CLI   PFAID,0             PF KEY PRESSED?                              
         BE    INITI50             NO                                           
*                                                                               
         OC    KEY(25),KEY         HAVE KEY?                                    
         BZ    INITI50                                                          
         LA    RE,KEY                                                           
         CLI   3(RE),X'02'         CLIENT RECORD CODE?                          
         BNE   INITI50                                                          
         USING PCLTKEY,RE                                                       
         LA    R2,CLTMEDH          MEDIA FLD ON MAINT SCR                       
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   *+8                                                              
         LA    R2,LSTMEDH          POINT TO MEDIA FLD ON LIST SCR               
         MVC   8(1,R2),PCLTKMED                                                 
         MVI   5(R2),1             INPUT LENGTH                                 
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         LA    R2,CLTCLTH          CLIENT FLD ON MAINT SCR                      
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   *+8                                                              
         LA    R2,LSTCLTH          POINT TO CLIENT FLD ON LIST SCR              
         MVC   8(3,R2),PCLTKCLT                                                 
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
         CLI   PHSCREEN,X'B0'      CLT MAINT SCR?                               
         BNE   *+8                                                              
         OI    CLTCNAMH+6,X'81'    CHG TO MODIFIED FLD (GAIN CONTROL)           
*                                                                               
         CLI   PHSCREEN,X'C4'      CLT LIST SCR?                                
         BNE   INITI51                                                          
         XC    LSTTL2+20+4+2(LTFFLDQ),LSTTL2+20+4+2                             
         XC    LSTUL2+20+4+2(LTFFLDQ),LSTUL2+20+4+2                             
         LA    RE,F_FLDLNQ                                                      
         CHI   RE,255                                                           
         BNH   *+6                                                              
         DC    H'0'                TOTAL FILTER FLDS CANNOT EXCEED 255!         
         OC    F_FLDS(F_FLDLNQ),F_FLDS                                          
         BNZ   INITI50U                                                         
T        USING LS_TITLE,LSTTL2                                                  
         MVC   T.LTOFC(03),=C'OFC'                                              
         MVC   T.LTAOF(03),=C'AOF'                                              
         MVI   T.LTAOFASP,C'/'                                                  
         MVC   T.LTAGY(03),=C'Agy'                                              
         MVC   T.LTTOF(03),=C'TOF'                                              
         MVC   T.LTCTYPE(07),=C'CltType'                                        
         MVC   T.LTCONREQ(07),=C'ConReq?'                                       
         DROP  T                                                                
U        USING LS_TITLE,LSTUL2                                                  
         MVC   U.LTOFC(03),=C'---'                                              
         MVC   U.LTAOF(03),=C'---'                                              
         MVI   U.LTAOFASP,C'-'                                                  
         MVC   U.LTAGY(03),=C'---'                                              
         MVC   U.LTTOF(03),=C'---'                                              
         MVC   U.LTCTYPE(07),=C'-------'                                        
         MVC   U.LTCONREQ(07),=C'-------'                                       
         DROP  U                                                                
INITI50U OI    LSTTL2H+6,X'80'                                                  
         OI    LSTUL2H+6,X'80'                                                  
*                                                                               
INITI51  CLI   PHSCREEN,X'B0'      CLT MAINT SCREEN?                            
         BNE   INITI52                                                          
         XC    CLTBOTL+PF12POSQ(L'PF12TXT),CLTBOTL+PF12POSQ                     
         OI    CLTBOTLH+6,X'80'                                                 
*                                                                               
INITI52  CLI   ACTNUM,ACTSEL       SELECT FROM LIST?                            
         BNE   INITI54                                                          
         CLI   PHSCREEN,X'B0'      CLT MAINT SCREEN?                            
         BE    *+6                                                              
         DC    H'0'                WRONG SCREEN!                                
         MVC   CLTBOTL+PF12POSQ(L'PF12TXT),PF12TXT                              
         OI    CLTBOTLH+6,X'80'                                                 
*                                                                               
INITI54  LA    RE,HEDSPECS         SET UP HEADHOOK AND SPECS                    
         ST    RE,SPECS                                                         
         LA    RE,HOOK                                                          
         ST    RE,HEADHOOK                                                      
         MVC   H2+15(10),MEDNM                                                  
         MVC   H2+10(01),QMED                                                   
*                                                                               
         CLI   ACTNUM,ACTLIST      ACTION IS LIST?                              
         BE    INITIX                                                           
         CLI   ACTNUM,ACTREP       ACTION IS REPORT?                            
         BE    INITIX                                                           
         CLI   PHSCREEN,X'B0'      CLIENT MAINT SCREEN?                         
         BE    *+6                                                              
         DC    H'0'                WRONG SCREEN!                                
*                                                                               
         MVC   CLTFINL(09),=C'Financial'                                        
         OI    CLTFINLH+6,X'80'                                                 
*                                   CLEARING OF CLTFIN REMOVED                  
*                                   4/29/08 - IT CAUSED SOME                    
*                                   BAD ERROR MESSAGES                          
*                                                                               
         NI    CLTFINH+1,X'FF'-X'20'                                            
         OI    CLTFINH+6,X'80'                                                  
         BRAS  RE,CKFINCLT         FINANCIAL CLT?                               
         BE    INITI55X                                                         
         XC    CLTFINL,CLTFINL     CLEAR 'FINANCIAL' FLD FROM SCR               
         OI    CLTFINLH+6,X'80'                                                 
         XC    CLTFIN,CLTFIN                                                    
         OI    CLTFINH+1,X'20'     PROTECT INPUT FLD                            
         OI    CLTFINH+6,X'80'                                                  
INITI55X DS    0H                                                               
*                                                                               
         CLI   WNATION,C'C'        CANADIAN?                                    
         BNE   INITI58X                                                         
         CLI   PHSCREEN,X'B0'      CLIENT MAINT SCREEN?                         
         BNE   INITI58X                                                         
*                                                                               
         MVC   CLTGSTA+00(3),=C'GST'                                            
         MVI   CLTGSTA+03,C' '                                                  
         MVC   CLTGSTA+04(4),=C'Code'                                           
         OI    CLTGSTAH+6,X'80'                                                 
         CLI   MODE,NEWSCR         SCR JUST BEING LOADED?                       
         BNE   *+18                                                             
         XC    CLTGST,CLTGST                                                    
         MVI   CLTGST,C'S'         DEFAULT VALUE                                
         MVI   CLTGSTH+5,1         INPUT LENGTH                                 
         NI    CLTGSTH+1,X'FF'-X'20'                                            
         OI    CLTGSTH+6,X'80'                                                  
*                                                                               
         MVC   CLTPSTA+00(3),=C'PST'                                            
         OI    CLTPSTAH+6,X'80'                                                 
         CLI   MODE,NEWSCR         SCR JUST BEING LOADED?                       
         BNE   *+14                                                             
         XC    CLTPST,CLTPST                                                    
         MVI   CLTPSTH+5,0         INPUT LENGTH                                 
         NI    CLTPSTH+1,X'FF'-X'20'                                            
         OI    CLTPSTH+6,X'80'                                                  
*                                                                               
         MVC   CLTMPSA+00(8),=C'Main PST'                                       
         OI    CLTMPSAH+6,X'80'                                                 
         CLI   MODE,NEWSCR         SCR JUST BEING LOADED?                       
         BNE   *+14                                                             
         XC    CLTMPS,CLTMPS                                                    
         MVI   CLTMPSH+5,0         INPUT LENGTH                                 
         NI    CLTMPSH+1,X'FF'-X'20'                                            
         OI    CLTMPSH+6,X'80'                                                  
INITI58X DS    0H                                                               
*                                                                               
INITI70  DS    0H                                                               
*                                                                               
INITIX   J     EXIT                                                             
*                                                                               
HOOK     NTR1                      HEADLINE ROUTINES                            
*                                                                               
HOOKX    J     EXIT                                                             
*                                                                               
HEDSPECS SSPEC H1,01,REQUESTOR                                                  
         SSPEC H2,01,C'Media'                                                   
         SSPEC H1,56,C' Client Report'                                          
         SSPEC H2,56,C'----------------'                                        
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,99,RUN                                                        
         SSPEC H5,99,REPORT                                                     
         SSPEC H6,99,C' '                                                       
         DC    H'0'                                                             
*                                                                               
PF12POSQ EQU   L'CLTBOTL-L'PF12TXT                                              
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
         DC    AL1(SECMNOV-SECVALS,001)                                         
         DC    AL1(SECSFHG-SECVALS,002)                                         
         DC    AL1(SECFROP-SECVALS,003)                                         
         DC    AL1(SECRFPG-SECVALS,004)                                         
         DC    AL1(SECCOS2-SECVALS,005)                                         
         DC    AL1(SECUBCG-SECVALS,019)                                         
*                                                                               
SECFLDSN EQU   (*-SECFLDS)/L'SECFLDS                                            
*                                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETREPLN NTR1  BASE=*,LABEL=*      R6 POINTS TO RECORD                          
*                                                                               
         LA    R5,P1                                                            
         USING REP_LINE,R5                                                      
*                                                                               
         MVC   R_CLT,04(R6)        CLIENT CODE FROM KEY                         
*                                                                               
         USING PCLTELEM,R6                                                      
         MVI   ELCODE,X'02'        CLIENT ELEM ID                               
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                FIRST ELEM MUST BE THERE!                    
*                                                                               
         MVC   R_CLTNM,PCLTNAME    CLIENT NAME                                  
         MVC   R_CLTBNM,PCLTBNAM   BILL RECEIPT NAME                            
         MVC   R_CLTLN1,PCLTLIN1   ADDRESS - LINE 1                             
         MVC   R_CLTLN2,PCLTLIN2   ADDRESS - LINE 2                             
         MVC   R_CLTATN,PCLTATTN   ATTENTION OF                                 
*                                                                               
         J     EXIT                                                             
*                                                                               
         DROP  RB,R6,R5                                                         
         LTORG                                                                  
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
         CLI   PFAID,2             RECORD IS CLIENT2?                           
         BNE   CKPFK15                                                          
         MVC   DUB,=C'CLIENT2 '                                                 
         B     CKPFK25                                                          
*                                                                               
CKPFK15  CLI   PFAID,5             RECORD IS CLIENT?                            
         BNE   CKPFK20                                                          
         MVC   DUB,=C'CLIENT  '                                                 
         B     CKPFK25                                                          
*                                                                               
CKPFK20  CLI   PFAID,6             RECORD IS PRD?                               
         BNE   CKPFK21                                                          
         MVC   DUB,=C'PRODUCT '                                                 
         B     CKPFK25                                                          
*                                                                               
CKPFK21  DS    0H                                                               
         J     SETCCNEQ            NO OTHER PFKEY DEFINED YET                   
*                                                                               
* SET RECORD FLD                                                                
*                                                                               
CKPFK25  GOTO1 VGLOBBER,DMCB,=C'PUTD',DUB,8,GLVXREC                             
*                                                                               
         MVC   DUB,SPACES          PREPARE IT FOR ACTION FLD                    
*                                                                               
         CLI   PFAID,2             CLIENT MAINT?                                
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
CKPFK30  CLI   PFAID,5             CLIENT LIST?                                 
         BNE   CKPFK31                                                          
CKPFK30H MVC   DUB,=C'LIST    '                                                 
         B     CKPFK40                                                          
*                                                                               
CKPFK31  CLI   PFAID,6             PRD LIST?                                    
         BNE   CKPFK35                                                          
         B     CKPFK30H                                                         
*                                                                               
CKPFK35  DS    0H                                                               
         J     SETCCNEQ            NO OTHER PFKEY DEFINED YET                   
*                                                                               
* SET ACTION FLD                                                                
*                                                                               
CKPFK40  GOTO1 VGLOBBER,DMCB,=C'PUTD',DUB,8,GLVXACT                             
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTF',CLTMEDH,,GLVPRKEY   KEY                   
         GOTO1 VGLOBBER,DMCB,=C'PUTF',CLTMEDH,,GLVPRMD    MEDIA                 
         GOTO1 VGLOBBER,DMCB,=C'PUTF',CLTCLTH,,GLVPRCLT   CLIENT                
*                                                                               
         J     SETCCEQ                                                          
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
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETPROFV NTR1  BASE=*,LABEL=*      Get profile values                           
*                                                                               
         L     R6,AIO                                                           
         USING PCLTREC,R6                                                       
         XC    DMCB,DMCB                                                        
         MVC   WORK+00(12),SPACES                                               
         MVC   WORK+00(04),=C'P0F0'                                             
         MVC   WORK+04(02),AGENCY                                               
         MVC   WORK+06(01),PCLTKMED                                             
         MVC   WORK+07(03),PCLTKCLT                                             
         CLI   PCLTOFF,C' '                                                     
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
         GOTO1 GETPROF,DMCB,WORK,F0PROF,DATAMGR                                 
*                                                                               
         J     SETCCEQ                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB,R6                                                            
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
       ++INCLUDE PRSFMB0D          CLIENT MAINT SCREEN                          
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMC4D          CLIENT LIST SCREEN                           
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE PRSFMWORKD                                                     
         ORG   SYSSPARE            WORKING AREA                                 
*                                                                               
RELO     DS    A                   RELOCATION FACTOR                            
OFFICER  DS    A                                                                
*                                                                               
CLTLSFLT DS    CL3                 CLT LIST FILTER                              
SVWORK   DS    XL64                GENERAL WORKING STORAGE                      
*                                                                               
SVDMINBT DS    X                                                                
SVDMOUTB DS    X                                                                
SVCTRAC  DS    XL(L'PCLTTOFC)      SAVED CLT TRAFFIC CODE                       
SVCOFFC  DS    XL(L'PCLTOFF)       SAVED CLT OFFICE CODE                        
*                                                                               
WKTMPFLD DS    XL11                HDR IS 8 AND 3 INPUT CHARS                   
WKTMPKEY DS    XL(L'KEY)                                                        
WKTMPAIO DS    XL(L'AIO)                                                        
WKTMPION DS    XL(L'USEIONUM)                                                   
WKCLTFIN DS    XL(L'PCLTFIN)       CLT FINANCIAL BYTE                           
WKCLSTAT DS    XL(L'PCLTSTAT)      CLT STATUS BYTE                              
WKCLPROF DS    XL(L'PCLTPROF)      CLT PROFILE                                  
*                                                                               
PSTOUT   DS    CL64                                                             
*                                                                               
CNTLSW   DS    XL1                 CONTROL SWTICH                               
C_MAXIOQ EQU   X'80'               MAX I/O IS REACHED                           
C_TITLEQ EQU   X'40'               TITLE LINE IS PRINTED FOR REPORT             
*                                                                               
IO_COUNT DS    H                   I/O COUNTER                                  
MXIOPCT  EQU   30                  % OF MAX IO ALLOWED                          
*                                                                               
SAVERE   DS    F                   FOR SAVING RETURN ADDRESSES                  
QCTL     DS    CL26                                                             
QAREA    DS    CL80                                                             
*                                                                               
* 1ST FLD IS FILTER THEN FOLLOWED BY 1 BYTE POSITION COUNTER                    
* WILDCARD IND USED TO AVOID BIT CONFLICT, ALSO FOR NEGATIVE FILTERING          
*                                                                               
F_FLDS   DS    0X                  FILTER FLDS START                            
*                                                                               
F_OFC    DS    XL(L'PCLTOFF)       OFFICE FILTER                                
         DS    X                                                                
F_AOF    DS    XL(L'PCLTAOFC)      ACC OFFICE CODE FILTER                       
         DS    X                                                                
F_AGY    DS    XL(L'PCLTACCA)      ACC OFFICE AGY FILTER                        
         DS    X                                                                
F_TOF    DS    XL(L'PCLTTOFC)      TRAFFIC OFFICE CODE FILTER                   
         DS    X                                                                
F_CLTNUM DS    XL(L'PCLTNUM)       INTERFACE NUMBER FILTER                      
         DS    X                                                                
F_CSCOM  DS    XL(L'PCLTCNUM)      CONTRACT STD COMMENT CODE FILTER             
         DS    X                                                                
F_IOCOM1 DS    XL(L'PCLTINUM)      I/O STD COMMENT1 CODE FILTER                 
         DS    X                                                                
F_IOCOM2 DS    XL(L'PCLTINUM)      I/O STD COMMENT2 CODE FILTER                 
         DS    X                                                                
F_BILGRP DS    XL(L'PCLTBLGP)      BILLING GROUP FILTER                         
         DS    X                                                                
F_DRDOV  DS    XL(L'PCLTDRDC)      DRD OVERRIDE CLT CODE FILTER                 
         DS    X                                                                
F_FIN    DS    XL(L'PCLTFIN)       FINANCIAL STATUS FILTER                      
         DS    X                                                                
F_DIV    DS    X                   CLT DIV (IN CLT PROFILE) FILTER              
         DS    X                                                                
F_MEDNM  DS    XL(L'PCLTMNAM)      MEDIA NAME OVERRIDE FILTER                   
         DS    X                                                                
*                                                                               
F_SFH    DS    XL(L'PCLTSTAT)      SFH STATUS FILTER                            
         DS    X                                                                
F_SFH_W  DS    C                   WILDCARD CHAR (AVOID BITS CONFLICT)          
*                                                                               
F_FRZ    DS    XL(L'PCLTSTAT)      FROZEN STATUS FILTER                         
         DS    X                                                                
F_FRZ_W  DS    C                   WILDCARD CHAR (AVOID BITS CONFLICT)          
*                                                                               
F_UBC    DS    XL(L'PCLTSTAT)      UCOMM BILL CONTROL FILTER                    
         DS    X                                                                
F_UBC_W  DS    C                   WILDCARD CHAR (AVOID BITS CONFLICT)          
*                                                                               
F_FLDLNQ EQU   *-F_FLDS                                                         
*                                                                               
*                                                                               
* SECURITY VALUES ARE TRANSLATED AS FOLLOW:                                     
* C'Y'   READ ONLY (FIELD WILL BE PROTECTED)                                    
* C'N'   NO ACCESS (FIELD WILL BE HIDDEN AND PROTECTED)                         
* X'00'  WRITE                                                                  
*                                                                               
SECVALS  DS    0X                  ** FIELD SECURITY VALUES **                  
*                                                                               
SECMNOV  DS    C                   MEDIA NAME OVERRIDE                          
SECSFHG  DS    C                   SPECIAL FINANCIAL HANDLING                   
SECFROP  DS    C                   FROZEN OPTIONS                               
SECRFPG  DS    C                   RFP GROUP                                    
SECCOS2  DS    C                   COST 2                                       
SECUBCG  DS    C                   UCOMM BILL CONTROL                           
*                                                                               
SECVALSL EQU   *-SECVALS           MAX IS 255                                   
*                                                                               
OFCBLK   DS    XL(OFCLENQ)         OFFICER BLOCK                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PCLTREC           CLT REC                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE PTRACLTPP         CLT TRAFFIC OFFICE CODE PASSIVE PTR          
         EJECT                                                                  
*                                                                               
       ++INCLUDE POFFCLTPP         CLT OFFICE CODE PASSIVE PTR                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE PDIVREC           DSECT FOR DIVISION RECORD                    
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
       ++INCLUDE DDSCANBLKD        DSECT FOR SCANNER                            
F_SMAXQ  EQU   10                  MAX NUM OF FILTER SCANNER ENTRIES            
         EJECT                                                                  
*                                                                               
       ++INCLUDE FASECRETD                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDOFFICED         TWO CHARS OFFICE CODE CONVERSION             
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSCLTC   DS    CL(L'PCLTKCLT)      CLT CODE                                     
         DS    CL1                                                              
LSCLT    DS    CL(L'PCLTNAME)      ENTIRE CLIENT NAME                           
         DS    CL2                                                              
*                                                                               
LSFSTART DS    0X                  FILTER DATA START HERE                       
*                                                                               
LSOFC    DS    CL(L'OFCOFC2)       CLT OFFICE CODE                              
         DS    CL2                                                              
LSAOF    DS    CL(L'PCLTAOFC)      CLT ACC OFFICE CODE                          
LSAOFASP DS    CL1                 "/"                                          
LSAGY    DS    CL(L'PCLTACCA)      ACC OFFICE AGY                               
         DS    CL3                                                              
LSTOF    DS    CL(L'OFCOFC2)       TRAFFIC OFFICE CODE                          
         DS    CL2                                                              
LSCTYPE  DS    CL3                 CLT TYPE, IF 1=SUB, CLT CODE DISP'D          
         DS    CL5                                                              
LSCONREQ DS    CL1                 CONTRACT REQUIRED                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
REP_LINE DSECT                     REPORT DSECT FOR PRINTING LINES              
*                                                                               
R_CLT    DS    CL(L'PCLTKCLT)      CLIENT CODE                                  
         DS    XL2                                                              
R_CLTNM  DS    CL(L'PCLTNAME)      CLIENT NAME                                  
         DS    XL2                                                              
R_CLTBNM DS    CL(L'PCLTBNAM)      BILL RECEIPT NAME                            
         DS    XL2                                                              
R_CLTLN1 DS    CL(L'PCLTLIN1)      ADDRESS - LINE 1                             
         DS    XL2                                                              
R_CLTLN2 DS    CL(L'PCLTLIN2/2)    ADDRESS - LINE 2                             
         DS    XL2                                                              
R_CLTATN DS    CL(L'PCLTATTN/2)    ATTENTION OF                                 
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
LS_TITLE DSECT                     DSECT FOR DEFAULT LIST TITLE LINES           
*                                                                               
LTCLTC   DS    CL(L'PCLTKCLT)      CLIENT CODE                                  
         DS    CL1                 /                                            
LTCLT    DS    CL(L'PCLTNAME)      NAME                                         
         DS    CL2                                                              
*                                                                               
LTFSTRTQ EQU   *-LS_TITLE+3+2      "SEL  " (5 CHARS)                            
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
LTCTYPE  DS    CL7                 CLTTYPE                                      
         DS    CL1                                                              
LTCONREQ DS    CL7                 CONREQ?                                      
*                                                                               
LTFFLDQ  EQU   80-1-LTFSTRTQ       LENGTH OF FLT DISPLAY AREA                   
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'248PRSFM1C   08/31/17'                                      
         END                                                                    
