*          DATA SET CTSFM0FS   AT LEVEL 006 AS OF 05/01/02                      
*          DATA SET CTSFM0F    AT LEVEL 001 AS OF 04/29/91                      
*PHASE TA0A0FA                                                                  
*INCLUDE DECODE                                                                 
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE: TA0A0F - ESCAPE SEQUENCE RECORD MAINTENANCE/LIST            *         
*                                                                     *         
*  COMMENTS: MAINTAINS ESCAPE SEQUENCE RECORDS.                       *         
*                                                                     *         
*  CALLED FROM: SFM CONTROLLER (TA0A00), WHICH CALLS                  *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO:    DATAMGR                                               *         
*                                                                     *         
*  INPUTS: SCREENS CTSFMDF (TA0ADF) -- MAINTENANCE                    *         
*                  CTSFMCF (TA0ACF) -- LIST                           *         
*                                                                     *         
*  OUTPUTS: UPDATED ESCAPE SEQUENCE RECORDS                           *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - WORK                                                  *         
*          R4 - WORK                                                  *         
*          R5 - WORK                                                  *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R7 - WORK                                                  *         
*          R8 - SPOOLD                                                *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM                                                *         
*          RF - SYSTEM                                                *         
*                                                                     *         
***********************************************************************         
         TITLE 'TA0A0F ESCAPE RECORD MAINTENANCE/LIST'                          
TA0A0F   CSECT                                                                  
**       PRINT NOGEN                                                            
         NMOD1 0,TA0A0F**,RR=R3                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
         XC    ACURSOR,ACURSOR                                                  
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BE    LR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
* VALIDATE KEY ROUTINE      * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
VK       CLI   ACTNUM,ACTREP       REPORT?                                      
         BNE   VK1                                                              
         MVI   SAVEFMT,C'C'        ASSUME CHARACTER FORMAT                      
         LA    R2,SFRFMTH          FORMAT FIELD                                 
         CLI   5(R2),0             GIVEN?                                       
         BE    VKX                                                              
         CLI   SFRFMT,C'C'         CHARACTER                                    
         BE    SAVEF                                                            
CKH      CLI   SFRFMT,C'H'         HEX                                          
         BE    SAVEF                                                            
CKX      CLI   SFRFMT,C'X'         HEX                                          
         BNE   INVRFMT                                                          
SAVEF    MVC   SAVEFMT,SFRFMT      SAVE FORMAT                                  
         B     VKX                                                              
*                                                                               
VK1      MVC   NBRSAV,=H'1'        ASSUME ESCAPE SEQUENCE 1                     
         LA    R2,SFMNBRH          ESCAPE SEQUENCE NUMBER FIELD                 
         CLI   5(R2),0             NUMBER GIVEN?                                
         BNE   VK10                                                             
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BNE   MISSERR                                                          
         B     VK20                                                             
*                                                                               
VK10     TM    4(R2),X'08'         DATA NUMERIC?                                
         BZ    INVNBRN             NO                                           
         ZIC   R1,5(R2)            LENGTH OF INPUT                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,SFMNBR(0)                                                    
         CVB   R1,DUB                                                           
         CH    R1,=H'1'            NUMBER IN RANGE?                             
         BL    INVNBRN                                                          
         CH    R1,=H'255'                                                       
         BH    INVNBRN                                                          
         STC   R1,NBRSAV                                                        
*                                                                               
VK20     XC    KEY,KEY             BUILD KEY                                    
         LA    R4,KEY                                                           
         USING ESCKEY,R4                                                        
         MVI   ESCKSYS,ESCKSYSQ    X'0503'                                      
         MVI   ESCTYPE,ESCTYPEQ    ESCAPE SEQUENCE RECORD                       
         MVC   ESCNBR,NBRSAV                                                    
VKX      B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* VALIDATE RECORD ROUTINE   * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
VR       CLI   PFKEY,3             ERASE LINE?                                  
         BE    *+12                                                             
         CLI   PFKEY,4             ADD LINE?                                    
         BNE   VR100                                                            
*                                                                               
         L     R5,ATIOB            A(TIOB)                                      
         USING TIOBD,R5                                                         
         SR    R1,R1                                                            
         ICM   R1,3,TIOBCURS       ABSOLUTE CURSOR ADDRESS                      
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         MH    R1,=H'80'           ABSOLUTE ADDR OF BEGINNING OF LINE           
         DROP  R5                                                               
*                                                                               
         LA    R2,SFMFILLH         1ST FIELD WHICH COULD CONTAIN CURSOR         
VR10     SR    RF,RF                                                            
         ICM   RF,3,2(R2)          ABSOLUTE SCREEN ADDR OF THIS FIELD           
         SR    RE,RE                                                            
         D     RE,=F'80'                                                        
         MH    RF,=H'80'           ABSOLUTE SCREEN ADDR OF LINE START           
         LA    RE,79(RF)           ABSOLUTE SCREEN ADDR OF LINE END             
*                                                                               
         CR    RF,R1               WAS CURSOR ON THIS LINE?                     
         BH    VR100               NO - IT'S ABOVE THIS FIELD                   
         CR    RE,R1                                                            
         BNL   VR20                YES                                          
*                                                                               
         ZIC   RF,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,RF                                                            
         LA    RF,SFMTXTLH                                                      
         CR    R2,RF               END OF SCREEN?                               
         BH    VR100               YES                                          
         B     VR10                                                             
*                                                                               
VR20     LA    RF,SFMTXTLH         A(LAST TEXT FIELD)                           
         CLI   PFKEY,3             ERASE LINE?                                  
         BNE   VR50                NO, ADD LINE                                 
         LA    R0,SFMFILLH                                                      
         CR    R2,R0               IS CURSOR ABOVE 1ST LINE?                    
         BE    VR100               YES -- ONLY ALLOWED FOR ADD                  
*                                                                               
         ST    R2,ACURSOR          KEEP CURSOR IN PLACE                         
         LR    R3,R2                                                            
VR30     CR    R2,RF               ARE WE UP TO LAST LINE OF TEXT?              
         BE    VR40                YES                                          
         ZIC   R0,0(R2)                                                         
         AR    R3,R0               R3 POINTS TO FOLLOWING LINE                  
         LA    R1,L'SFMTXT                                                      
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),8(R3)       MOVE LINE OF TEXT UP                         
         MVC   4(2,R2),4(R3)       MOVE INPUT INDICATORS AND LENGTH             
         LR    R2,R3                                                            
         B     VR30                                                             
*                                                                               
VR40     XC    4(2,R2),4(R2)       CLEAR INPUT INDICATORS AND LENGTH            
         LA    R1,L'SFMTXT                                                      
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR LAST TEXT FIELD                        
         B     VR100                                                            
*                                                                               
VR50     CR    R2,RF               ARE THEY TRYING TO INSERT AFTER END?         
         BE    VR100               YES                                          
         LR    RF,R2               SAVE A(INSERTION)                            
         LA    R3,SFMTXTLH         LAST LINE OF TEXT                            
         LR    R2,R3                                                            
*                                                                               
VR60     ZIC   R0,0(R2)                                                         
         SR    R2,R0               R3 POINTS TO PREVIOUS LINE                   
         CR    R2,RF               ARE WE UP TO LAST LINE OF TEXT?              
         BE    VR70                YES                                          
         LA    R1,L'SFMTXT                                                      
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R3),8(R2)       MOVE LINE OF TEXT DOWN                       
         MVC   4(2,R3),4(R2)       MOVE INPUT INDICATORS AND LENGTH             
         LR    R3,R2                                                            
         B     VR60                                                             
*                                                                               
VR70     XC    4(2,R3),4(R3)       CLEAR INPUT INDICATORS AND LENGTH            
         LA    R1,L'SFMTXT                                                      
         BCTR  R1,0                FOR EX INSTRUCTIONS:  R1 = L'TEXT            
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R3),8(R3)       CLEAR TEXT FIELD (INSERT BLANK LINE)         
         ST    R3,ACURSOR          KEEP CURSOR IN PLACE                         
*                                                                               
VR100    L     R4,AIO              A(ESCAPE RECORD)                             
         USING ESCKEY,R4                                                        
         MVI   ELCODE,ESCDESEQ     DESCRIPTION ELEMENT                          
         GOTO1 REMELEM                                                          
         LA    R2,SFMDESH          DESCRIPTION FIELD                            
         CLI   5(R2),0             ANY DESCRIPTION?                             
         BE    MISSERR                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING ESCDESEL,R6                                                      
         MVI   ESCDESEL,ESCDESEQ   ELEMENT CODE                                 
         MVI   ESCDESLN,ESCDESLQ   ELEMENT LENGTH                               
         MVC   ESCDESTX,SFMDES     ACTUAL DESCRIPTION                           
         OC    ESCDESTX,SPACES                                                  
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,SFMTXTH          FIRST TEXT FIELD                             
         MVI   TXTFOUND,C'N'       NO TEXT FOUND YET                            
         MVI   SEQNUM,0                                                         
         XC    SAVELN,SAVELN       CLEAR SAVE AREA                              
         XC    SAVETXT,SAVETXT                                                  
         MVI   ELCODE,ESCTXTEQ     ESCAPE TEXT ELEMENT CODE                     
         GOTO1 REMELEM             GET RID OF EXISTING ELEMENTS                 
         MVI   ELCODE,ESCVALEQ     ESCAPE VALUE ELEMENT CODE                    
         GOTO1 REMELEM             GET RID OF EXISTING ELEMENTS                 
         LA    R6,ELEM                                                          
         USING ESCTXTEL,R6                                                      
*                                                                               
VR120    LA    RF,SFMTXTLH                                                      
         CR    R2,RF               END OF SCREEN?                               
         BH    VRX                 YES                                          
         ST    R2,SAVECUR                                                       
         XC    ELEM,ELEM                                                        
         ZIC   RF,SEQNUM           INCREMENT SEQUENCE NUMBER                    
         LA    RF,1(RF)                                                         
         STC   RF,SEQNUM                                                        
         MVI   ESCTXTEL,ESCTXTEQ   ESCAPE TEXT LINE ELEMENT CODE                
         MVC   ESCTXTSQ,SEQNUM     SEQUENCE NUMBER                              
*                                                                               
         ZIC   R1,5(R2)            LENGTH OF INPUT                              
         LTR   R1,R1               NO INPUT ON THIS LINE?                       
         BZ    VR130                                                            
*                                                                               
         LA    R3,8(R2)            SCREEN LINE OF TEXT                          
         LA    R7,ESCTXTTX         TEXT ELEMENT                                 
         MVI   PARENS,C'N'         PARENS INDICATE CHARACTER TEXT               
LOOP     CLI   0(R3),C'('                                                       
         BNE   CKCLS                                                            
         CLI   PARENS,C'Y'         PARENS ALREADY OPEN?                         
         BE    NGTXTERR            ERROR                                        
         MVI   PARENS,C'Y'                                                      
         B     MOVEIT                                                           
CKCLS    CLI   0(R3),C')'                                                       
         BNE   CKPRNS                                                           
         CLI   PARENS,C'N'         PARENS ALREADY CLOSED?                       
         BE    NGTXTERR            ERROR                                        
         MVI   PARENS,C'N'                                                      
         B     MOVEIT                                                           
CKPRNS   CLI   PARENS,C'Y'         CHARACTER MODE?                              
         BE    MOVEIT                                                           
         CLI   0(R3),C' '          NO, IGNORE ANY DATA AFTER SPACE              
         BE    NEWLEN                                                           
MOVEIT   MVC   0(1,R7),0(R3)       SCREEN TEXT TO TEXT ELEMENT                  
         LA    R7,1(R7)                                                         
         LA    R3,1(R3)                                                         
         BCT   R1,LOOP             MORE TEXT ENTERED?                           
         ZIC   R1,5(R2)            SAVE ENTIRE FIELD                            
         B     OLDLEN                                                           
*                                                                               
NEWLEN   LA    R1,8(R2)                                                         
         SR    R3,R1                                                            
         LTR   R1,R3               LENGTH BEFORE SPACE                          
         BZ    VR130                                                            
*                                                                               
OLDLEN   MVI   TXTFOUND,C'Y'       SOME TEXT WAS FOUND                          
         LA    R3,ESCTXTOV         LENGTH OF ELEMENT OVERHEAD                   
         LA    R1,0(R3,R1)         TOTAL LENGTH OF ELEMENT                      
         B     VR150                                                            
*                                                                               
VR130    ST    R2,MYWORK           HANG ON TO CURRENT TWA POINTER               
         LA    RF,SFMTXTLH                                                      
*                                                                               
VR140    ZIC   R0,0(R2)            BUMP TO NEXT TEXT FIELD                      
         AR    R2,R0                                                            
         CR    R2,RF               END OF SCREEN?                               
         BNH   *+12                NO                                           
         L     R2,MYWORK                                                        
         B     VRX                                                              
         CLI   5(R2),0             ANY INPUT THIS FIELD?                        
         BE    VR140               TRY NEXT FIELD                               
*                                                                               
         L     R2,MYWORK                                                        
         MVI   ESCTXTTX,C' '       MUST SAVE A BLANK LINE                       
         LA    R1,ESCTXTOV                                                      
         LA    R1,1(R1)            TOTAL LENGTH OF ELEMENT                      
*                                                                               
VR150    STC   R1,ESCTXTLN                                                      
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   INAREA(L'INAREA),SPACES                                          
         ZIC   R1,ESCTXTLN                                                      
         LA    RF,ESCTXTOV                                                      
         SR    R1,RF               LENGTH OF TEXT                               
         C     R1,=F'1'                                                         
         BH    MVCTXT              CONVERT AND SAVE TEXT                        
         CLI   ESCTXTTX,C' '                                                    
         BE    PASTSV              IGNORE BLANK LINES                           
*                                                                               
MVCTXT   BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   INAREA(0),ESCTXTTX  TEXT TO DECODE INPUT AREA                    
*                                                                               
         LA    R7,SAVETXT          A(SAVE AREA)                                 
         AH    R7,SAVELN                                                        
         GOTO1 =V(DECODE),DMCB,INAREA,(R7),RR=RELO                              
         CLI   8(R1),0                                                          
         BNE   NGTXTERR                                                         
*                                                                               
         SR    RF,RF                                                            
         LH    RF,SAVELN           LENGTH SO FAR                                
         A     RF,8(R1)            DECODE OUTPUT LENGTH                         
         CH    RF,=H'250'          MAX LENGTH                                   
         BH    TOLNGERR                                                         
         STH   RF,SAVELN                                                        
*                                                                               
PASTSV   ZIC   R0,0(R2)            BUMP TO NEXT TEXT FIELD                      
         AR    R2,R0                                                            
         B     VR120                                                            
*                                                                               
VRX      CLI   TXTFOUND,C'Y'         MAKE SURE SOME TEXT WAS FOUND              
         BNE   NOTXTERR                                                         
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING ESCVALEL,R6                                                      
         MVI   ESCVALEL,ESCVALEQ     ELEMENT CODE                               
         MVI   ESCVALSQ,1            SEQUENCE NUMBER                            
         LH    R7,SAVELN             ELEMENT LENGTH                             
         LA    R7,ESCVALOV(R7)       PLUS OVERHEAD                              
         STC   R7,ESCVALLN           TOTAL ELEMENT LENGTH                       
*                                                                               
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   ESCVALTX(0),SAVETXT   SAVE VALUE TO VALUE ELEMENT                
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* DISPLAY RECORD      * * * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
DR       MVC   SAVEKEY,KEY                                                      
         L     R4,AIO                                                           
         LA    R6,ESCFSTEL         A(FIRST ELEMENT)                             
         USING ESCDESEL,R6                                                      
         LA    R2,SFMDESH          DESCRIPTION FIELD                            
         MVC   SFMDES,SPACES                                                    
         MVI   ELCODE,ESCDESEQ     DESCRIPTION ELEMENT CODE                     
         BAS   RE,FIRSTEL          DESCRIPTION ELEMENT IS THERE?                
         BNE   *+10                                                             
         MVC   SFMDES,ESCDESTX     PUT DESCRIPTION IN FIELD                     
         OI    SFMDESH+6,X'80'                                                  
*                                                                               
         LA    R6,ESCFSTEL         A(FIRST ELEMENT)                             
         USING ESCTXTEL,R6                                                      
         LA    R2,SFMTXTH          FIRST ESCAPE TEXT FIELD                      
         MVI   ELCODE,ESCTXTEQ     ESCAPE TEXT ELEMENT CODE                     
         BAS   RE,FIRSTEL          ANY TEXT FIELDS?                             
         BE    *+6                                                              
         DC    H'00'               MUST BE SOME TEXT                            
*                                                                               
DR10     MVC   8(L'SFMTXT,R2),SPACES                                            
         ZIC   R1,ESCTXTLN         LENGTH OF ELEMENT                            
         LA    R3,ESCTXTOV         OVERHEAD LENGTH                              
         SR    R1,R3               LENGTH OF TEXT                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),ESCTXTTX    ESCAPE LINE OF TEXT                          
         OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)            NEXT TEXT FIELD                              
         AR    R2,R0                                                            
*                                                                               
         LA    RF,SFMTXTLH                                                      
         CR    R2,RF               END OF SCREEN?                               
         BH    DRX                 YES                                          
         BAS   RE,NEXTEL           NEXT LINE OF ESCAPE TEXT                     
         BE    DR10                                                             
*                                                                               
DR20     ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'            LENGTH OF DATA + 1 (FOR EX)                  
         TM    1(R2),X'02'         TEXT EXTENDED HEADER                         
         BZ    *+8                                                              
         SH    R1,=H'8'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES      BLANK OUT REMAINING FIELDS                   
         OI    6(R2),X'80'                                                      
         ZIC   R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
*                                                                               
         LA    RF,SFMTXTLH                                                      
         CR    R2,RF               END OF SCREEN?                               
         BNH   DR20                NO                                           
*                                                                               
DRX      MVC   ACURFORC,ACURSOR    PFKEY WAS HIT -- PLACE CURSOR                
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
*                                                                               
* DISPLAY KEY   * * * * * * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
DK       L     R4,AIO              RECORD SELECTED                              
         USING ESCKEY,R4                                                        
         EDIT  (B1,ESCNBR),(5,SFMNBR),ALIGN=LEFT                                
         OI    SFMNBRH+6,X'80'     ESCAPE SEQUENCE                              
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* ONLINE LIST ROUTINE   * * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
LR       LA    R4,KEY                                                           
         USING ESCKEY,R4                                                        
         OC    SAVEKEY,SAVEKEY     DISPLAY FROM SELECT?                         
         BZ    LR05                OVERRIDE GENCON KEY                          
         MVC   KEY,SAVEKEY         USE SELECTED KEY                             
         XC    SAVEKEY,SAVEKEY     CLEAR SAVE AREA                              
         B     LR10                                                             
LR05     OC    KEY(ESCKLENQ),KEY   FIRST TIME THROUGH?                          
         BNZ   LR10                                                             
         MVI   ESCKSYS,ESCKSYSQ    X'0503'                                      
         MVI   ESCTYPE,ESCTYPEQ    ESCAPE SEQUENCE RECORD KEY                   
         MVC   ESCNBR,NBRSAV                                                    
*                                                                               
LR10     GOTO1 HIGH                FIRST RECORD                                 
*                                                                               
LR40     CLI   ESCKSYS,ESCKSYSQ    X'0503'                                      
         BNE   EXIT                                                             
         CLI   ESCTYPE,ESCTYPEQ    ESCAPE SEQUENCE RECORD KEY                   
         BNE   EXIT                                                             
*                                                                               
         CLI   SFMNBRH+5,0         ESCAPE SEQUENCE NUMBER ENTERED?              
         BE    *+14                                                             
         CLC   ESCNBR,NBRSAV       IF SO, TEST KEY MATCH                        
         BL    LR60                                                             
*                                                                               
         MVC   LISTAR,SPACES       CLEAR LIST LINE                              
         EDIT  (B1,ESCNBR),(5,LSTNBR)                                           
         L     R4,AIO              A(ESCAPE RECORD)                             
         LA    R6,ESCFSTEL         A(FIRST ELEMENT)                             
         MVI   ELCODE,ESCDESEQ     DESCRIPTION ELEMENT                          
         USING ESCDESEL,R6                                                      
         BAS   RE,FIRSTEL          LOOK FOR DESCRIPTION ELEMENT                 
         BE    *+6                 LOOK FOR FIRST TEXT LINE ELEMENT             
         DC    H'00'               MUST BE THERE                                
         MVC   LSTDESC,ESCDESTX    PUT DESCRIPTION IN LIST LINE                 
*                                                                               
         GOTO1 LISTMON             SEND LINE TO SCREEN                          
*                                                                               
LR60     GOTO1 SEQ                 NEXT ESCAPE RECORD                           
         LA    R4,KEY              POINT R4 BACK TO KEY                         
         B     LR40                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
*                                                                               
* PRINT REPORT ROUTINE  * * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
PR       LA    R3,HEADING          SET                                          
         ST    R3,SPECS                UP                                       
         LA    R3,HDHK                    REPORT                                
         ST    R3,HEADHOOK                    HEADINGS                          
*                                                                               
PR10     LA    R4,KEY                                                           
         USING ESCKEY,R4                                                        
         MVI   ESCKSYS,ESCKSYSQ    X'0503'                                      
         MVI   ESCTYPE,ESCTYPEQ    ESCAPE SEQUENCE RECORD KEY                   
         MVI   ESCNBR,X'01'                                                     
         GOTO1 HIGH                FIRST RECORD                                 
*                                                                               
PR40     CLI   ESCKSYS,ESCKSYSQ    X'0503'                                      
         BNE   EXIT                                                             
         CLI   ESCTYPE,ESCTYPEQ    ESCAPE SEQUENCE RECORD KEY                   
         BNE   EXIT                                                             
*                                                                               
         MVI   FORCEHED,C'Y'       ONE RECORD PER PAGE                          
*                                                                               
         L     R4,AIO                                                           
         LA    R6,ESCFSTEL         A(FIRST ELEMENT)                             
         MVI   ELCODE,ESCTXTEQ     TEXT LINE ELEMENT                            
         USING ESCTXTEL,R6                                                      
         BAS   RE,FIRSTEL                                                       
         BE    *+6                 LOOK FOR FIRST TEXT LINE ELEMENT             
         DC    H'00'               MUST BE THERE                                
*                                                                               
PR50     ZIC   R1,ESCTXTLN         LENGTH OF ELEMENT                            
         LA    R3,ESCTXTOV         OVERHEAD LENGTH                              
         SR    R1,R3               LENGTH OF TEXT                               
         BCTR  R1,0                                                             
         CLI   SAVEFMT,C'C'        CHARACTER MODE                               
         BNE   HEX                                                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P1+22(0),ESCTXTTX   ESCAPE LINE OF TEXT                          
         B     PRNTIT              PRINT IT                                     
*                                                                               
HEX      MVC   INAREA(L'INAREA),SPACES                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   INAREA(0),ESCTXTTX  ESCAPE LINE OF TEXT                          
*                                                                               
         LA    R7,SAVETXT          A(SAVE AREA)                                 
         GOTO1 =V(DECODE),DMCB,INAREA,(R7),RR=RELO                              
         CLI   8(R1),0                                                          
         BE    TRNS                                                             
         DC    H'0'                                                             
*                                                                               
TRNS     EQU   *                                                                
         L     R1,8(R1)            DECODE OUTPUT LENGTH                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         TR    SAVETXT(0),PRNTCHRS TRANSLATE TO PRINTABLE CHARACTERS            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P1+22(0),SAVETXT    ESCAPE LINE OF TEXT                          
*                                                                               
         MVI   P2+22,C'-'          UNDERLINE                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P2+23(0),P2+22                                                   
*                                                                               
         LA    R7,2(R1)                                                         
         GOTO1 HEXOUT,DMCB,SAVETXT,OUTAREA,(R7),=C'SEP'                         
         OC    16(4,R1),16(R1)                                                  
         BNZ   OK                                                               
         DC    H'0'                                                             
*                                                                               
OK       LR    R1,R7                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P3+22(0),OUTAREA                                                 
*                                                                               
         LA    R7,OUTAREA                                                       
         LA    R7,1(R1,R7)                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P4+22(0),0(R7)                                                   
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   P1,0                SKIP A LINE                                  
PRNTIT   GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   ELCODE,ESCTXTEQ     TEXT LINE ELEMENT                            
         BAS   RE,NEXTEL           NEXT LINE OF ESCAPE TEXT                     
         BE    PR50                                                             
*                                                                               
PR60     GOTO1 SEQ                 NEXT ESCAPE RECORD                           
         LA    R4,KEY              POINT R4 BACK TO KEY                         
         B     PR40                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
*                                                                               
* HEADER ROUTINE    * * * * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
HDHK     NTR1                                                                   
         MVC   H1+53(23),=C'ESCAPE SEQUENCE NUMBER'                             
         LA    R4,KEY                                                           
         USING ESCKEY,R4                                                        
         LA    R7,H1+76                                                         
         EDIT  (B1,ESCNBR),(3,(R7))                                             
         L     R4,AIO              A(ESCAPE RECORD)                             
         LA    R6,ESCFSTEL         A(FIRST ELEMENT)                             
         MVI   ELCODE,ESCDESEQ     DESCRIPTION ELEMENT                          
         USING ESCDESEL,R6                                                      
         BAS   RE,FIRSTEL          LOOK FOR DESCRIPTION ELEMENT                 
         BE    *+6                 LOOK FOR FIRST TEXT LINE ELEMENT             
         DC    H'00'               MUST BE THERE                                
         MVI   H3,C'-'             UNDERLINE                                    
         MVC   H3+1(131),H3                                                     
         MVI   H4,0                SKIP A LINE                                  
         MVC   H5+53(45),ESCDESTX  PUT DESCRIPTION IN REPORT LINE               
         MVI   H6,0                SKIP A LINE                                  
HDHKX    XIT1                                                                   
         SPACE 2                                                                
HEADING  SSPEC H1,95,REPORT                                                     
         SSPEC H1,111,REQUESTOR                                                 
         SSPEC H2,95,RUN                                                        
         SSPEC H2,121,PAGE                                                      
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
RELO     DS    A                   RELOCATION FACTOR                            
*                                                                               
NEEDFLDN XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NEEDFLDM),NEEDFLDM                                     
         LA    R2,SFMNBRH                                                       
         GOTO1 ERREX2                                                           
NEEDFLDM DC    C'* ERROR * ESCAPE SEQUENCE NUMBER REQUIRED *'                   
*                                                                               
INVNBRN  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVNBRM),INVNBRM                                       
         LA    R2,SFMNBRH                                                       
         GOTO1 ERREX2                                                           
INVNBRM  DC    C'* ERROR * INVALID ESCAPE SEQUENCE NUMBER *'                    
*                                                                               
NOTXTERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOTXTMS),NOTXTMS                                       
         LA    R2,SFMTXTH                                                       
         GOTO1 ERREX2                                                           
NOTXTMS  DC    C'* ERROR * AT LEAST ONE LINE OF TEXT REQUIRED *'                
*                                                                               
NGTXTERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NGTXTMS),NGTXTMS                                       
         L     R2,SAVECUR                                                       
         GOTO1 ERREX2                                                           
NGTXTMS  DC    C'* ERROR * INVALID ESCAPE SEQUENCE TEXT FORMAT *'               
*                                                                               
TOLNGERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'TOLNGMS),TOLNGMS                                       
         L     R2,SAVECUR                                                       
         GOTO1 ERREX2                                                           
TOLNGMS  DC    C'* ERROR * ESCAPE SEQUENCE TEXT TOO LONG *'                     
*                                                                               
INVRFMT  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'INVRFMTM),INVRFMTM                                     
         LA    R2,SFRFMTH                                                       
         GOTO1 ERREX2                                                           
INVRFMTM DC    C'* ERROR * INVALID REPORT FORMAT *'                             
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         GOTO1 ERREX                                                            
         B     EXIT                                                             
         SPACE 3                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* PRINTABLE CHARACTERS                                                          
*                   0.1.2.3.4.5.6.7.8.9.A.B.C.D.E.F.                            
PRNTCHRS DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B' 00-0F                     
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B' 10-1F                     
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B' 20-2F                     
         DC    XL16'4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B4B' 30-3F                     
         DC    XL16'404B4B4B4B4B4B4B4B4B4A4B4C4D4E4F' 40-4F                     
         DC    XL16'504B4B4B4B4B4B4B4B4B5A5B5C5D5E5F' 50-5F                     
         DC    XL16'60614B4B4B4B4B4B4B4B6A6B6C6D6E6F' 60-6F                     
         DC    XL16'4B4B4B4B4B4B4B4B4B797A7B7C7D7E7F' 70-7F                     
         DC    XL16'4B8182838485868788894B4B4B4B4B4B' 80-8F                     
         DC    XL16'4B9192939495969798994B4B4B4B4B4B' 90-9F                     
         DC    XL16'4BA1A2A3A4A5A6A7A8A94B4B4B4B4B4B' A0-AF                     
         DC    XL16'4BB1B2B3B4B5B6B7B8B94B4B4B4B4B4B' B0-BF                     
         DC    XL16'C0C1C2C3C4C5C6C7C8C94B4B4B4B4B4B' C0-CF                     
         DC    XL16'D0D1D2D3D4D5D6D7D8D94B4B4B4B4B4B' D0-DF                     
         DC    XL16'E04BE2E3E4E5E6E7E8E94B4B4B4B4B4B' E0-EF                     
         DC    XL16'F0F1F2F3F4F5F6F7F8F94B4B4B4B4B4B' F0-FF                     
*                                                                               
       ++INCLUDE CTGENESC                                                       
***      PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE FATIOB                                                         
       ++INCLUDE CTSFMFFD                                                       
***      PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMDFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMEFD                                                       
         EJECT                                                                  
       ++INCLUDE CTSFMWORKD                                                     
         EJECT                                                                  
*                                                                               
* TA0A0F STORAGE DSECT    * * * * * * * * * * * * * * * * * * * * * *           
*                                                                               
         ORG   SYSSPARE                                                         
ACURSOR  DS    A                   FORCE CURSOR HERE                            
MYWORK   DS    XL96                                                             
SAVEKEY  DS    XL25                ESCAPE SEQUENCE RECORD KEY                   
NBRSAV   DS    XL1                 ESCAPE SEQUENCE NUMBER                       
SEQNUM   DS    XL1                 TEXT LINE NUMBER                             
TXTFOUND DS    CL1                 'Y' IF TEXT LINE WAS FOUND                   
PARENS   DS    CL1                 'Y' IF TEXT IS PARENTHASIZED                 
INAREA   DS    CL77                INPUT AREA FOR DECODE                        
SAVECUR  DS    F                   CURSOR POSITION FOR ERRORS                   
SAVELN   DS    H                   LENGTH OF CONVERTED TEXT                     
SAVETXT  DS    XL255               CONVERTED TEXT SAVE AREA                     
OUTAREA  DS    XL255               CONVERTED TEXT FOR REPORT                    
SAVEFMT  DS    C                   CHARACTER OR HEX FOR REPORT                  
*                                                                               
* ONLINE LIST LINE   * * * * * * * * * * * * * * * * * * * * * * * * *          
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
         DS    CL4                                                              
LSTNBR   DS    CL5                                                              
         DS    CL4                                                              
LSTDESC  DS    CL45                                                             
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006CTSFM0FS  05/01/02'                                      
         END                                                                    
