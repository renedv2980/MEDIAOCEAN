*          DATA SET TAGENA6    AT LEVEL 025 AS OF 11/12/14                      
*PHASE T702A6B,*                                                                
         TITLE 'T702A6 - W2 MAINTENANCE'                                        
T702A6   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702A6,R7                                                      
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         CLI   RECNUM,TN                                                        
         BNE   W2_00                                                            
         GOTO1 SUB_1099                                                         
         J     XIT                                                              
*                                                                               
         SPACE 1                                                                
W2_00    GOTO1 INITIAL,DMCB,PFTABLE                                             
         CLI   MODE,SETFILE                                                     
         BNE   W210                                                             
         BAS   RE,SETCHK           SET CHECK FILE                               
         B     W2X                                                              
*                                                                               
W210     CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   THISLSEL,C'D'       IF DELETING FROM LIST                        
         BE    W215                                                             
         CLI   MODE,DISPREC        IF MODE IS DISPLAY                           
         BE    W230                                                             
*                                                                               
W215     CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,RECDEL         DELETE RECORD                                
         BE    W250                                                             
         CLI   MODE,RECREST        RESTORE RECORD                               
         BE    W250                                                             
         CLI   MODE,XRECDEL        RECORD DELETED                               
         BE    W220                                                             
         CLI   MODE,XRECREST       RECORD RESTORED                              
         BE    W220                                                             
         CLI   MODE,XRECADD        OR NEW RECORD ADDED                          
         BE    W220                                                             
         CLI   MODE,XRECPUT        OR RECORD CHANGED                            
         BNE   W240                                                             
*                                                                               
W220     GOTO1 ADDPTRS,DMCB,PTRBLK HANDLE PASSIVE POINTERS                      
*                                                                               
W230     BAS   RE,DISPLAY          (RE-)DISPLAY THE RECORD                      
*                                                                               
W240     CLI   MODE,VALREC         VALIDATE THE RECORD                          
         BNE   W2X                                                              
         BAS   RE,BLDREC                                                        
         B     W2X                                                              
*                                                                               
W250     XC    PTRBLK,PTRBLK           CLEAR POINTER BLOCK                      
         GOTO1 SAVPTRS,DMCB,PTRBLK     HANDLE PASSIVE POINTERS                  
*                                                                               
W2X      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              VALIDATE KEY                                                     
*                                                                               
VK       GOTO1 FLDVAL,DMCB,(X'40',SW2SSNH),(X'80',SW2CANH)                      
         BE    *+8                                                              
         MVI   PFAID,0                                                          
         TM    TRNSTAT,RACHANG     IF RECORD/ACTION CHANGED                     
         BZ    VK05                                                             
         NI    SW2SSNH+4,X'DF'     FORCE VALIDATION                             
         MVC   SW2EMPN,SPACES      CLEAR NAMES                                  
         OI    SW2EMPNH+6,X'80'                                                 
         MVC   SW2SSNN,SPACES                                                   
         OI    SW2SSNNH+6,X'80'                                                 
*                                                                               
VK05     LA    R2,SW2SSNH                                                       
         BAS   RE,SETTAL           SET TALENT FILE                              
         TM    4(R2),X'20'         WAS FIELD ALREADY VALIDATED                  
         BO    VK10                                                             
         NI    SW2EMPH+4,X'DF'     FORCE VALIDATION                             
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',SW2SSNH),SW2SSNNH                     
*                                                                               
VK10     OI    4(R2),X'20'                                                      
         LA    R2,SW2EMPH                                                       
         TM    4(R2),X'20'         WAS FIELD ALREADY VALIDATED                  
         BO    VK30                                                             
         NI    SW2YEARH+4,X'DF'    FORCE VALIDATION                             
         CLI   5(R2),0             IF NO INPUT                                  
         BNE   VK20                                                             
         MVC   SW2EMP,TGTPEMP      DEFAULT TO TP EMPLOYER                       
         MVI   SW2EMPH+5,3                                                      
         OI    SW2EMPH+6,X'80'                                                  
*                                                                               
VK20     MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'08',SW2EMPH),SW2EMPNH                     
*                                                                               
VK30     OI    4(R2),X'20'                                                      
         LA    R2,SW2YEARH                                                      
         TM    4(R2),X'20'         WAS FIELD ALREADY VALIDATED                  
         BO    VK60                                                             
         NI    SW2CANH+4,X'DF'     FORCE VALIDATION                             
         CLI   5(R2),0             DEFAULT THIS YEAR - 1                        
         BNE   VK40                                                             
         GOTO1 ADDAY,DMCB,(C'Y',TGTODAY0),WORK,F'-1'                            
         GOTO1 DATCON,DMCB,(0,WORK),(20,WORK+6)                                 
         MVC   SW2YEAR,WORK+6      SET CCYY                                     
         OI    SW2YEARH+6,X'80'    TRANSMIT                                     
         B     VK50                                                             
*                                                                               
VK40     MVC   WORK(4),=4X'F0'     INSURE VALID NUMERIC                         
         MVZ   WORK(4),8(R2)                                                    
         CLC   WORK(4),=4X'F0'                                                  
         BNE   INVERR                                                           
*                                                                               
VK50     MVC   TGYEAR,SW2YEAR      SET GLOBAL YEAR FIELD                        
*                                                                               
VK60     OI    4(R2),X'20'                                                      
         LA    R2,SW2CANH          CANADIAN                                     
         TM    4(R2),X'20'         WAS FIELD ALREADY VALIDATED                  
         BO    VK70                                                             
         MVI   TGCUR,C'U'          DEFAULT U.S CURRENCY                         
         CLI   8(R2),C'Y'                                                       
         BNE   VK70                                                             
         MVI   TGCUR,C'C'          SET CANADIAN CURRENCY                        
*                                                                               
VK70     OI    4(R2),X'20'                                                      
         MVI   ADDNEW,C'N'                                                      
         BAS   RE,SETCHK           SET CHECK FILE                               
*                                                                               
         MVC   AIO,AIO1            SET IO AREA FOR GETREC                       
         GOTO1 RECVAL,DMCB,TLW2CDQ,(X'B4',0)   GET REC FOR UPDATE               
         BNE   VK80                                                             
         CLI   ACTNUM,ACTADD       IF ADD  & REC FOUND - ERROR                  
         BE    ADDERR                                                           
         LA    R4,KEY              IF THIS IS NOT TODAY'S RECORD                
         USING TLW2D,R4                                                         
         MVC   XCDATE,TGTODAY1                                                  
         XC    XCDATE,=X'FFFFFF'                                                
         CLC   TLW2CDTE,XCDATE                                                  
         BE    VKX                                                              
         MVI   ADDNEW,C'Y'                                                      
         B     VKX                                                              
*                                                                               
VK80     GOTO1 RECVAL,DMCB,TLW2CDQ,(X'C0',0)   RE-BUILD KEY                     
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              DISPLAY KEY                                                      
*                                                                               
DK       MVC   SVKEY,KEY           SAVE KEY                                     
         L     R4,AIO              LOCATION OF THE RECORD                       
         USING TLW2D,R4                                                         
         MVC   SW2SSN,TLW2SSN      SS NUMBER                                    
         OI    SW2SSNH+6,X'80'                                                  
         MVC   SW2EMP,TLW2EMP      EMPLOYER                                     
         OI    SW2EMPH+6,X'80'                                                  
         MVC   SW2YEAR,TLW2YEAR    YEAR                                         
         OI    SW2YEARH+6,X'80'                                                 
*                                                                               
         MVI   IOOPT,C'N'          CLEAR I/O FLAG                               
         MVI   ADDNEW,C'N'                                                      
         LA    R4,KEY              CHECK IF THIS RECORD WAS CHANGED             
         USING TLW2D,R4                                                         
         MVC   XCDATE,TGTODAY1     TODAY OR IF WE NEED                          
         XC    XCDATE,=X'FFFFFF'                                                
         CLC   TLW2CDTE,XCDATE     TO ADD A NEW ONE                             
         BE    DK10                                                             
         MVI   ADDNEW,C'Y'                                                      
*                                                                               
DK10     MVC   AIO,AIO2                                                         
         BAS   RE,SETTAL           SET TALENT FILE                              
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'0C',SW2SSNH),SW2SSNNH                     
         GOTO1 RECVAL,DMCB,TLEMCDQ,(X'0C',SW2EMPH),SW2EMPNH                     
*                                                                               
         BAS   RE,SETCHK           SET CHECK FILE                               
         MVC   KEY,SVKEY           RESTORE KEY                                  
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*              DISPLAY THE RECORD                                               
*                                                                               
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         TWAXC SW2UNITH            CLEAR UNPROTECTED FIELDS                     
         XC    SW2YTD,SW2YTD       CLEAR YTD FIELD                              
         OI    SW2YTDH+6,X'80'                                                  
         XC    SW2YTDR,SW2YTDR     CLEAR YTD FIELD                              
         OI    SW2YTDRH+6,X'80'                                                 
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TACYELQ      GET YTD WITHHOLDING ELEMENT                  
         USING TACYD,R4                                                         
         BAS   RE,GETEL                                                         
         BNE   DISP2                                                            
         LA    R2,SW2YTD                                                        
         EDIT  TACYEARN,(11,(R2)),2,FLOAT=-,ZERO=BLANK   EARNINGS               
         LA    R2,SW2YTDR                                                       
         EDIT  TACYTXRE,(10,(R2)),2,FLOAT=-,ZERO=BLANK   TAX. REIMB.            
         DROP  R4                                                               
                                                                                
DISP2    L     R4,AIO                                                           
         MVI   ELCODE,TAYEELQ      LOOK FOR YTD EARNINGS EL.                    
         USING TAYED,R4                                                         
         BAS   RE,GETEL                                                         
         BNE   DISP7                                                            
         LA    RF,5                DISPLAY ALL FIVE AMOUNTS                     
         LA    R2,SW2YTD           R2=A(SCREEN LOCATION)                        
         LA    R3,TAYEEARN         R3=A(ACCUM)                                  
         LA    R2,12(R2)           SKIP EARNINGS                                
         LA    R3,4(R3)            BUMP TO NEXT AMOUNT                          
DISP3    EDIT  (4,0(R3)),(11,(R2)),2,FLOAT=-,ZERO=BLANK                         
         LA    R3,4(R3)            BUMP TO NEXT AMOUNT                          
         LA    R2,12(R2)                                                        
         BCT   RF,DISP3                                                         
         DROP  R4                                                               
*                                                                               
DISP7    LA    R2,SW2UNITH         TAX UNITS                                    
         L     R4,AIO                                                           
         MVI   ELCODE,TAW2ELQ                                                   
         USING TAW2D,R4                                                         
         BAS   RE,GETEL                                                         
         BNE   DISP60                                                           
*                                                                               
DISP10   CLC   TAW2UNIT,=C'FD '    SORT FEDERAL UP TO TOP                       
         BNE   DISP30                                                           
         BAS   RE,OUTINFO          OUTPUT INFO TO SCREEN                        
         L     R2,FULL             REST R2 TO NEXT PRINT LINE                   
         B     DISP40                                                           
*                                                                               
DISP30   BAS   RE,NEXTEL                                                        
         BE    DISP10                                                           
*                                                                               
DISP40   L     R4,AIO              START OVER                                   
         MVI   ELCODE,TAW2ELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DISP60                                                           
*                                                                               
DISP45   CLC   TAW2UNIT,=C'FD '    FEDERAL PRINTED ALREADY                      
         BE    DISP50                                                           
         BAS   RE,OUTINFO          OUTPUT INFO TO SCREEN                        
         L     R2,FULL             REST R2 TO NEXT PRINT LINE                   
*                                                                               
DISP50   BAS   RE,NEXTEL           GET NEXT TAX UNIT                            
         BE    DISP45                                                           
*                                                                               
         USING TAWSD,R4                                                         
DISP60   L     R4,AIO              GET W2 SUBSIDARY ELEMENT                     
         MVI   ELCODE,TAWSELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DISP70                                                           
*                                                                               
         LA    R2,SW2LPRTH         LAST DATE PRINTED                            
         OC    TAWSDPRT,TAWSDPRT                                                
         BZ    DISP70                                                           
         GOTO1 DATCON,DMCB,(1,TAWSDPRT),(5,8(R2))                               
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
DISP70   GOTO1 ACTVOUT,DMCB,SW2LCHGH         LAST CHANGED                       
*                                                                               
         CLI   PFAID,13                                                         
         BE    DISP80                                                           
         CLI   PFAID,14                                                         
         BNE   XIT                                                              
*                                                                               
DISP80   BAS   RE,CHPRINT          STOP/REPRINT PRINT OF W2 FORM                
         B     CHPMSG                                                           
         EJECT                                                                  
*                                                                               
*        OUTPUT INFO FROM ELEMENT TO SCREEN (R2)                                
*               R4 - ELEMNT                                                     
*                                                                               
         USING TAW2D,R4                                                         
OUTINFO  NTR1                                                                   
         MVC   8(3,R2),TAW2UNIT    DISPLAY TAX UNIT                             
         LA    R6,TAW2EARN         1ST AMOUNT IN ELEMENT                        
         L     R3,0(R6)                                                         
         LA    R5,2                                                             
*                                                                               
OUT10    ZIC   R1,0(R2)            BUMP TO 1ST AMOUNT ON SCREEN                 
         AR    R2,R1                                                            
         BAS   RE,EDIT11                                                        
         LA    R6,4(R6)            NEXT FIELD IN ELEMENT                        
         L     R3,0(R6)                                                         
         ZIC   R1,0(R2)            BUMP TO NEXT LINE                            
         AR    R2,R1                                                            
         CLI   TAW2LEN,TAW2LNQ2    IF NEW LENGTH,                               
         BL    OUT15                                                            
         L     R3,TAW2TXRE         TAXABLE REIMBURSEMENTS                       
         BAS   RE,EDIT11                                                        
                                                                                
OUT15    ZIC   R1,0(R2)            BUMP TO NEXT LINE                            
         AR    R2,R1                                                            
         L     R3,TAW2TAX          TAX                                          
         BAS   RE,EDIT11                                                        
***      LA    R6,4(R6)            SKIP FLI FIELD IN ELEMENT                    
***      L     R3,0(R6)                                                         
                                                                                
         LA    R5,2                                                             
         LA    R6,TAW2FICA                                                      
         L     R3,0(R6)                                                         
*                                                                               
OUT20    ZIC   R1,0(R2)            BUMP TO 1ST AMOUNT ON SCREEN                 
         AR    R2,R1                                                            
         BAS   RE,EDIT11                                                        
         LA    R6,4(R6)            NEXT FIELD IN ELEMENT                        
         L     R3,0(R6)                                                         
         BCT   R5,OUT20                                                         
*                                                                               
         ZIC   R1,0(R2)            BUMP TO NEXT LINE                            
         AR    R2,R1                                                            
         L     R3,TAW2SFLI         STATE FLI IN ELEMENT                         
         BAS   RE,EDIT11                                                        
*                                                                               
         ZIC   R1,0(R2)            BUMP TO NEXT LINE                            
         AR    R2,R1                                                            
*                                                                               
OUTX     ST    R2,FULL                                                          
         B     XIT                                                              
         SPACE 2                                                                
*                                                                               
*        ROUTINE EDITS AMOUNT IN R3 TO ADDRESS (HEADER) AT R2                   
*                                                                               
         SPACE                                                                  
EDIT11   NTR1                                                                   
         LTR   R3,R3               IF AMOUNT NOT 0                              
         BZ    XIT                                                              
         EDIT  (R3),(11,8(R2)),2,FLOAT=-                                        
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              BUILD THE RECORD                                                 
*                                                                               
BLDREC   NTR1                                                                   
         GOTO1 SAVPTRS,DMCB,PTRBLK                                              
         MVI   ELCODE,TAW2ELQ                                                   
         GOTO1 REMELEM             REMOVE OLD ELEMENTS                          
         MVI   ELCODE,TAWSELQ                                                   
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R2,SW2UNITH         POINT TO 1ST TAX UNIT                        
         MVI   MYBYTE,13           # OF LINES PER PAGE                          
*                                                                               
BLD60    XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING TAW2D,R4                                                         
         MVI   TAW2EL,TAW2ELQ      SET ELCODE                                   
         MVI   TAW2LEN,TAW2LNQ2    & LENGTH                                     
         LA    R6,TAW2EARN         POINT TO 1ST AMOUNT                          
         LA    R5,6                N'AMOUNTS TO VALIDATE                        
*        MVI   MYBYTE2,2           AFTER 2 SKIP 1 FIELD IN ELEMENT              
*                                                                               
BLD70    ZIC   R3,5(R2)                                                         
         LTR   R3,R3                                                            
         BNZ   BLD75                                                            
         BAS   RE,CKAMT            ENSURE UNIT IS INPUT                         
         BNE   MISSERR             IF AMOUNTS ARE INPUT                         
         LA    R5,7                UNIT AND AMOUNTS                             
*                                                                               
BLD72    ZIC   R1,0(R2)            BUMP PAST LINE IF NOTHING IS INPUT           
         AR    R2,R1                                                            
         BCT   R5,BLD72                                                         
         B     BLD100                                                           
*                                                                               
BLD75    GOTO1 TAXVAL,DMCB,((R3),8(R2))                                         
         BNE   INVERR                                                           
         MVC   TAW2UNIT,8(R2)                                                   
         OC    TAW2UNIT,SPACES                                                  
         OC    8(3,R2),SPACES                                                   
         BAS   RE,CKUNIT           CK THAT UNIT ISN'T ALREADY INPUT             
         BE    INVERR                                                           
*                                                                               
BLD80    ZIC   R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
         BAS   RE,GETCASH                                                       
         BE    BLD80AA                                                          
         CHI   R5,5                TAXABLE REIMB ARE SPECIAL                    
         BNE   BLD90                                                            
         B     BLD80AB                                                          
BLD80AA  CHI   R5,5                SECOND AMOUNT FIELD IS TAX REIMB             
         BNE   BLD80B                                                           
BLD80AB  OC    FULL,FULL                                                        
         BZ    BLD95                                                            
         CLC   TGEMP,=C'P+ '       ONLY VALID FOR EMPLOYER P+                   
         BNE   INVERR                                                           
BLD80A   MVC   TAW2TXRE,FULL       TAXABLE REIMBURSEMENTS                       
         B     BLD95                                                            
BLD80B   CH    R5,=H'1'            LAST AMOUNT FIELD IS FLI                     
         BNE   BLD83                                                            
         CLC   TAW2UNIT,=C'NJ '    IF THIS IS NEW JERSEY                        
         BNE   INVERR                                                           
         MVC   TAW2SFLI,FULL                                                    
         B     BLD90                                                            
                                                                                
BLD81    MVC   0(4,R6),FULL                                                     
         B     BLD90                                                            
                                                                                
BLD83    MVC   0(4,R6),FULL                                                     
                                                                                
BLD85    CLI   TAW2UNIT+2,C' '     IF THIS IS A CITY                            
         BNH   BLD90                                                            
         CH    R5,=H'2'            AND THIS IS SUI/SDI                          
         BNH   INVERR              DO NOT ALLOW INPUT                           
*                                                                               
BLD90    LA    R6,4(R6)            BUMP TO NEXT FIELD IN ELEMENT                
*        ZIC   R1,MYBYTE2                                                       
*        BCTR  R1,R0                                                            
*        LTR   R1,R1                                                            
*        BNZ   BLD95                                                            
         CH    R5,=H'4'            SKIP AFTER TAXES (FLI GAP)                   
         BNE   BLD95                                                            
         AHI   R6,4                BUMP TO NEXT FIELD IN ELEMENT                
*        MVI   MYBYTE2,2                                                        
*                                                                               
*LD95    STC   R1,MYBYTE2                                                       
BLD95    DS    0H                                                               
         BCT   R5,BLD80                                                         
*                                                                               
         GOTO1 ADDELEM                                                          
         ZIC   R1,0(R2)            BUMP TO NEXT LINE                            
         AR    R2,R1                                                            
*                                                                               
BLD100   ZIC   R1,MYBYTE                                                        
         BCTR  R1,R0              CHECK ENTIRE PAGE                             
         LTR   R1,R1                                                            
         BZ    BLD120                                                           
         STC   R1,MYBYTE                                                        
         B     BLD60                                                            
*                                                                               
BLD120   XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING TAWSD,R4                                                         
         MVI   TAWSEL,TAWSELQ      SET ELCODE                                   
         MVI   TAWSLEN,TAWSLNQ     & LENGTH                                     
         GOTO1 ADDELEM                                                          
*                                                                               
         GOTO1 ACTVIN,DMCB,SW2LCHGH      LAST CHANGED                           
*                                                                               
         L     R4,AIO                                                           
         USING TLW2D,R4                                                         
         CLI   ADDNEW,C'Y'         IF ADDING (CHANGING) REC - SET KEY           
         BE    BLD130                                                           
         CLI   ACTNUM,ACTADD       ON ACTADD                                    
         BNE   BLD140                                                           
         OI    TLW2STAT,X'40'      SET 'NEED TO PRINT BIT' ON                   
*                                                                               
BLD130   LA    R4,KEY                                                           
         USING TLW2D,R4                                                         
         MVC   TLW2CDTE,TGTODAY1   SET LAST CHANGED DATE TO TODAY               
         XC    TLW2CDTE,=X'FFFFFF' COMPLEMENTED                                 
*                                                                               
BLD140   L     R4,AIO              SET KEY IN TO IO AREA                        
         USING TLW2D,R4                                                         
         MVC   0(L'TLW2KEY,R4),KEY                                              
*                                                                               
         CLI   ADDNEW,C'Y'                                                      
         BNE   BLDX                CHECK IF REALLY 'ADDING' FOR CHANGE          
         GOTO1 ADDREC                                                           
         BAS   RE,DISPLAY                                                       
         MVI   ADDNEW,C'N'                                                      
         MVI   IOOPT,C'Y'          APPLICATION WILL DO I/O                      
*                                                                               
BLDX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
*              CHECK IF KEY FIELD HAVE NEW INPUT                                
*                                                                               
KEYCHG   NTR1                                                                   
         TM    SW2SSNH+4,X'80'     WAS FIELD INPUT THIS TIME                    
         BO    YES                                                              
         TM    SW2EMPH+4,X'80'                                                  
         BO    YES                                                              
         TM    SW2YEARH+4,X'80'                                                 
         BO    YES                                                              
         TM    SW2CANH+4,X'80'                                                  
         BO    YES                                                              
         B     NO                                                               
         EJECT                                                                  
*                                                                               
*        ROUTINE TO CHECK IF ANY AMOUNTS ARE INPUT                              
*              R2 - UNIT FIELD                                                  
*                                                                               
CKAMT    NTR1                                                                   
         LA    R3,6                CHECK ALL 5 FIELDS                           
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               BUMP TO FIRST AMOUNT FIELD                   
*                                                                               
CKA10    CLI   5(R2),0             IF ANY INPUT                                 
         BNE   NO                  RETURN CC NEQ                                
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               BUMP TO NEXT AMOUNT FIELD                    
         BCT   R3,CKA10                                                         
         B     YES                                                              
         SPACE 3                                                                
*                                                                               
*        ROUTINE TO CHECK IF CURRENT UNIT EXISTS ALREADY                        
*              R2 - UNIT FIELD                                                  
*                                                                               
CKUNIT   NTR1                                                                   
         LA    R4,AIO                                                           
         MVI   ELCODE,TAW2ELQ      SEE IF CURRENT UNIT EXISTS                   
         GOTO1 GETL,DMCB,(3,8(R2))                                              
         BNE   NO                                                               
         B     YES                                                              
         EJECT                                                                  
*                                                                               
*        ROUTINE TO TURN OFF PRINT BITS                                         
*                                                                               
CHPRINT  NTR1                                                                   
         BAS   RE,SECURITY         CHECK SECURITY ACCESS OF USER                
         BNE   SECERR                                                           
         MVC   SVEMP,TGEMP         SAVE EMPLOYER                                
         MVC   SVCUR,TGCUR              CURRENCY                                
         MVC   SVKEY,KEY           SAVE W4 KEY                                  
         BAS   RE,BLDEMP           BLD TABLE OF EMPLOYERS                       
         XC    KEY,KEY                                                          
         MVC   AIO,AIO2                                                         
         MVI   TGCUR,C'U'          SET US CURRENCY                              
*                                                                               
CHP10    LA    R3,EMPTAB                                                        
*                                                                               
CHP20    OC    0(3,R3),0(R3)       ANY EMPLOYERS LEFT                           
         BZ    CHP30                                                            
         MVC   TGEMP,0(R3)         SET EMPLOYER                                 
         BAS   RE,UPDW2            UPDATE W2 RECORD                             
         LA    R3,3(R3)                                                         
         B     CHP20               GET NEXT EMPLOYER                            
*                                                                               
CHP30    CLI   TGCUR,C'C'          IF ALREADY PROCESSED CANADIAN $              
         BE    CHP40               EXIT                                         
         MVI   TGCUR,C'C'          ELSE SET CANADIAN CURRENCY                   
         B     CHP10               AND DO ALL EMPLOYERS OVER                    
*                                                                               
CHP40    XC    KEY,KEY             REGET RECORD INTO AIO2 TO PREVENT            
         MVC   KEY(L'SVKEY),SVKEY                                               
         GOTO1 HIGH                GETREC/PUTREC SYNDROME                       
         GOTO1 GETREC                                                           
*                                                                               
CHPX     MVC   AIO,AIO1            RE-SET I/O AREA                              
         MVC   TGEMP,SVEMP         RESTORE EMPLOYER                             
         MVC   TGCUR,SVCUR                 CURRENCY                             
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        ROUTINE TO CHECK IF USER ALLOWED TO STOP A W2 REPRINT                  
*                                                                               
SECURITY NTR1                                                                   
         MVI   BYTE,0                                                           
         CLI   TGSTDSP,0          1ST SECURITY BYTE                             
         BNE   SEC10                                                            
         MVI   BYTE,BP234+BM      SET VALID USERS - PROGRAMMER/ADMIN            
         B     SEC40              SYSTEMS MANAGER/EXECUTIVE/MANAGER             
*                                                                               
SEC10    CLI   TGSTDSP,1          2ND SECURITY BYTE                             
         BNE   SEC20                                                            
         MVI   BYTE,BAB           SET VALID USERS - ACCOUNTING/                 
         B     SEC40              ACCOUNTING MANAGER                            
*                                                                               
SEC20    CLI   TGSTDSP,2          3RD SECURITY BYTE                             
         BNE   SEC30                                                            
         MVI   BYTE,BO            SET VALID USERS - OPERATOR                    
         B     SEC40                                                            
*                                                                               
SEC30    CLI   TGSTDSP,3          4TH SECURITY BYTE                             
         BNE   NO                                                               
*                                                                               
SEC40    MVC   TBYTE,BYTE                                                       
         OC    BYTE,TGSTBIT       IS THIS A VALID USER                          
         CLC   BYTE,TBYTE                                                       
         BNE   NO                                                               
         B     YES                                                              
         EJECT                                                                  
*                                                                               
*        ROUTINE TO BUILD AN EMPLOYER TABLE                                     
*                                                                               
BLDEMP   NTR1                                                                   
         BAS   RE,SETTAL           SET TALENT FILE                              
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING TLEMD,R1                                                         
         XC    EMPTAB(30),EMPTAB                                                
         LA    R2,EMPTAB                                                        
         MVI   KEY,TLEMCDQ         SET EMPLOYER CODE                            
         GOTO1 HIGH                                                             
         B     BE20                                                             
*                                                                               
BE10     GOTO1 SEQ                                                              
*                                                                               
BE20     CLC   KEY(1),KEYSAVE      STILL READING EMPLOYER RECORDS               
         BNE   BEX                                                              
         MVC   0(3,R2),TLEMEMP                                                  
         LA    R2,3(R2)            BUMP TO NEXT PLACE IN TABLE                  
         B     BE10                                                             
*                                                                               
BEX      BAS   RE,SETCHK           SET CHECK FILE                               
         B     XIT                                                              
         SPACE 3                                                                
*                                                                               
*        ROUTINE TO GET & UPDATE A W2 RECORD                                    
*                                                                               
UPDW2    NTR1                                                                   
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLW2CDQ,(X'B4',0)                                    
         BNE   UW2X                                                             
         L     R4,AIO                                                           
         USING TLW2D,R4                                                         
         NI    TLW2STAT,X'BF'       TURN OFF RE-PRINT BIT                       
         CLI   PFAID,13                                                         
         BE    UPD10                                                            
         OI    TLW2STAT,X'40'       TURN ON RE-PRINT BIT                        
*                                                                               
UPD10    GOTO1 PUTREC                                                           
*                                                                               
         LA    R1,KEY                                                           
         USING TLDRD,R1                                                         
         NI    TLDRSTAT,X'BF'       TURN OFF RE-PRINT BIT                       
         CLI   PFAID,13                                                         
         BE    UPD20                                                            
         OI    TLDRSTAT,X'40'       TURN ON RE-PRINT BIT                        
*                                                                               
UPD20    GOTO1 WRITE                                                            
*                                                                               
UW2X     MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        ROUTINE TO SET SYSFIL/DIR TO TALENT FILE                               
*                                                                               
SETTAL   NTR1                                                                   
         XC    FILENAME,FILENAME                                                
         MVC   SYSFIL,=C'TALFIL'                                                
         MVC   SYSDIR,=C'TALDIR'                                                
         B     XIT                                                              
         SPACE 3                                                                
*                                                                               
*        ROUTINE TO SET SYSFIL/DIR TO CHECK FILE                                
*                                                                               
SETCHK   NTR1                                                                   
         XC    FILENAME,FILENAME                                                
         MVC   SYSFIL,=C'CHKFIL'                                                
         MVC   SYSDIR,=C'CHKDIR'                                                
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        ROUTINE GET AMOUNT FROM FIELD (R2) POINTS AT                           
*                                                                               
         SPACE                                                                  
GETCASH  NTR1                                                                   
         XC    FULL,FULL                                                        
         ZIC   R3,5(R2)                                                         
         LTR   R3,R3                                                            
         BZ    NO                                                               
         GOTO1 CASHVAL,DMCB,8(R2),(R3)                                          
         CLI   0(R1),X'FF'                                                      
         BE    INVERR                                                           
         MVC   FULL,4(R1)                                                       
         B     YES                                                              
         EJECT                                                                  
YES      SR    RC,RC                                                            
*                                                                               
NO       LTR   RC,RC                                                            
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         SPACE 2                                                                
MISSERR  MVI   ERROR,MISSING                                                    
         B     ERRXIT                                                           
*                                                                               
ADDERR   MVI   ERROR,RECEXIST                                                   
         LA    R2,SW2SSNH                                                       
         B     ERRXIT                                                           
*                                                                               
RESTERR  MVI   ERROR,RECNTDEL                                                   
         LA    R2,SW2SSNH                                                       
         B     ERRXIT                                                           
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     ERRXIT                                                           
*                                                                               
SECERR   MVI   ERROR,SECLOCK       SECURITY LOCK OUT                            
         B     ERRXIT                                                           
*                                                                               
ADDMSG   MVI   MYMSGNO1,69                                                      
         B     INFXIT                                                           
*                                                                               
CHPMSG   MVI   MYMSGNO1,70                                                      
         CLI   PFAID,13                                                         
         BE    INFXIT                                                           
         MVI   MYMSGNO1,69                                                      
         B     INFXIT                                                           
*                                                                               
INFXIT   LA    R2,CONRECH                                                       
         OI    GENSTAT2,USGETTXT                                                
*                                                                               
ERRXIT   XC    DMCB,DMCB                                                        
         GOTO1 EXIT,DMCB                                                        
*                                                                               
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
PFTABLE  DS    0C                  PF KEYS TABLE                                
         SPACE 1                                                                
         DC    AL1(PF13X-*,13,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
PF13X    EQU   *                                                                
*                                                                               
         DC    AL1(PF14X-*,14,0,0,PFTRETRN)                                     
         DC    CL3' ',CL8'        ',CL8'        '                               
PF14X    EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
SUB_1099 NTR1                                                                   
         BAS   RE,SETTAL           SET TALENT FILE                              
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK99                                                             
         BAS   RE,SETCHK           SET CHECK FILE                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DISP99                                                           
         CLI   MODE,VALREC        DISPLAY RECORD                                
         BE    VREC99                                                           
         XIT1                                                                   
VK99     TM    TRNSTAT,RACHANG     IF RECORD/ACTION CHANGED                     
         BZ    VK99_05                                                          
         MVC   TNNEMPN,SPACES      CLEAR NAMES                                  
         OI    TNNEMPNH+6,X'80'                                                 
VK99_05  DS    0C                                                               
*                                                                               
         MVC   SVSSN,TNNFID                                                     
         CLI   TNNFIDH+5,9                                                      
         BNL   VK99_06                                                          
         MVC   TGSSN,TNNFID                                                     
         GOTO1 SSNUNPK,DMCB,TGSSN,TNNFID                                        
         MVI   TNNFIDH+5,9                                                      
         MVC   SVSSN,TNNFID                                                     
*                                                                               
VK99_06  LA    R2,TNNFIDH                                                       
         CLI   TNNFIDH+5,0                                                      
         JE    MISSERR                                                          
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',TNNFIDH),TNNFIDNH                     
         L     RE,AIO                                                           
         USING TLW4D,RE                                                         
         CLI   TLW4STA2,TAW4TYIN                                                
         BE    *+8                                                              
         CLI   TLW4STA2,TAW4TYCO                                                
         JNE   INVERR                                                           
         DROP  RE                                                               
VK99_10  OI    TNNFIDH+4,X'20'                                                  
*&&DO                                                                           
         LA    R2,TNNSSNH                                                       
         CLI   TNNSSNH+5,0                                                      
         JE    MISSERR                                                          
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',TNNSSNH),TNNSSNNH                     
         L     RE,AIO                                                           
         USING TLW4D,RE                                                         
         CLI   TLW4STA2,TAW4TYCA                                                
         JE    *+8                                                              
         CLI   TLW4STA2,TAW4TYES                                                
         JE    *+8                                                              
         CLI   TLW4STA2,TAW4TYFO                                                
         JE    *+8                                                              
         CLI   TLW4STA2,TAW4TYIN                                                
         JNE   INVERR                                                           
         DROP  RE                                                               
*&&                                                                             
VK99_20  XC    TNNFID,TNNFID                                                    
         GOTO1 SSNPACK,DMCB,SVSSN,TNNFID                                        
         MVI   TNNFIDH+5,6                                                      
         OI    TNNFIDH+6,X'80'                                                  
*                                                                               
         LA    R2,TNNEMPH                                                       
         CLI   TNNEMPH+5,0                                                      
**       JE    MISSERR                                                          
         JNE   VK99_26                                                          
         MVI   TNNEMPH+5,2                                                      
         MVC   TNNEMP(2),=C'TP'                                                 
         OI    TNNEMPH+6,X'80'                                                  
VK99_26  GOTO1 RECVAL,DMCB,TLEMCDQ,(X'08',TNNEMPH),TNNEMPNH                     
VK99_30  DS    0C                                                               
         LA    R2,TNNYEARH                                                      
         CLI   TNNYEARH+5,0                                                     
***      JE    MISSERR                                                          
         JNE   VK99_36                                                          
*                                                                               
*  DEFAULT TO CURRENT YEAR MINUS ONE                                            
         GOTO1 ADDAY,DMCB,(C'Y',TGTODAY0),WORK,F'-1'                            
         GOTO1 DATCON,DMCB,(0,WORK),(20,WORK+6)                                 
         MVC   TNNYEAR,WORK+6      SET CCYY                                     
         MVI   TNNYEARH+5,4                                                     
         OI    TNNYEARH+6,X'80'    TRANSMIT                                     
*                                                                               
VK99_36  DS    0C                                                               
         LA    R2,TNNFILTH                                                      
         CLI   TNNFILTH+5,0                                                     
         JE    MISSERR                                                          
VK99_40  BAS   RE,SETCHK           SET CHECK FILE                               
         LA    R4,KEY                                                           
         USING TL99D,R4                                                         
         XC    KEY,KEY                                                          
         MVI   TL99CD,TL99CDQ                                                   
         MVI   TL99SCD,TL99SCDQ                                                 
         MVC   TL99YEAR,TNNYEAR                                                 
         MVI   TL99CUR,C'U'                                                     
         MVC   TL99EMP,TNNEMP                                                   
         OC    TL99EMP,SPACES                                                   
****     MVC   TL99FID,TNNFID                                                   
         MVC   TL99FID,SVSSN                                                    
         MVC   TL99W4TY,TNNFILT                                                 
****     MVC   TL99SSN,TNNSSN                                                   
         DROP  R4                                                               
         B     XIT                                                              
         SPACE 1                                                                
                                                                                
*                                                                               
DISP99   DS    0H                                                               
*                                                                               
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TA99ELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DISP9910                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TA99D,R4                                                         
DISP9910 EDIT  (B4,TA99EARN),TNNGWAG,2,ALIGN=LEFT                               
         MVI   TNNGWAGH+5,L'TA99EARN                                            
         OI    TNNGWAGH+6,X'80'                                                 
***      EDIT  (B4,TA99REXP),TNNREXP,2,ALIGN=LEFT                               
***      MVI   TNNREXPH+5,L'TA99REXP                                            
***      OI    TNNREXPH+6,X'80'                                                 
**       MVC   TNNTYPE(1),TA99TY                                                
**       MVI   TNNTYPEH+5,1                                                     
**       OI    TNNTYPEH+6,X'80'                                                 
         GOTO1 ACTVOUT,DMCB,TNNLCHGH     LAST CHANGED ACTIVITY                  
*                                                                               
         LA    R2,TNNLPRTH         LAST DATE PRINTED                            
         XC    TNNLPRT,TNNLPRT                                                  
         OI    6(R2),X'80'         TRANSMIT                                     
         USING TAWSD,R4                                                         
         L     R4,AIO              GET W2 SUBSIDARY ELEMENT                     
         MVI   ELCODE,TAWSELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DISP99X                                                          
         OC    TAWSDPRT,TAWSDPRT                                                
         BZ    DISP99X                                                          
         GOTO1 DATCON,DMCB,(1,TAWSDPRT),(5,8(R2))                               
         OI    6(R2),X'80'         TRANSMIT                                     
DISP99X  B     XIT                                                              
*******************************************************************             
* VALIDATE RECORD                                                               
*******************************************************************             
*                                                                               
VREC99   L     R4,AIO                                                           
         MVI   ELCODE,TA99ELQ                                                   
         GOTO1 REMELEM                                                          
         USING TA99D,R4                                                         
*                                                                               
         LA    R4,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   TA99EL,TA99ELQ                                                   
         MVI   TA99LEN,TA99LNQ                                                  
*&&DO                                                                           
         LA    R2,TNNTYPEH                                                      
         CLI   TNNTYPEH+5,0                                                     
         JE    MISSERR                                                          
         CLI   TNNTYPE,TA99TYB                                                  
         JE    VR99_50                                                          
         CLI   TNNTYPE,TA99TYC                                                  
         JE    VR99_50                                                          
         CLI   TNNTYPE,TA99TYE                                                  
         JE    VR99_50                                                          
         CLI   TNNTYPE,TA99TYL                                                  
         JE    VR99_50                                                          
         CLI   TNNTYPE,TA99TYP                                                  
         JNE   INVERR                                                           
VR99_50  MVC   TA99TY,TNNTYPE                                                   
*&&                                                                             
         LA    R2,TNNGWAGH                                                      
         LLC   R0,TNNGWAGH+5                                                    
         GOTO1 CASHVAL,DMCB,(2,TNNGWAG),(R0)                                    
         CLI   DMCB,X'FF'                                                       
         JE    INVERR                                                           
         MVC   TA99EARN,DMCB+4                                                  
         CLC   TA99EARN,=F'1'                                                   
         JL    INVERR                                                           
         CLC   TA99EARN(4),=F'99999900'                                         
         JH    INVERR                                                           
*&&DO                                                                           
**       LA    R2,TNNREXPH                                                      
**       LLC   R0,TNNREXPH+5                                                    
**       GOTO1 CASHVAL,DMCB,(2,TNNREXP),(R0)                                    
         CLI   DMCB,X'FF'                                                       
         JE    INVERR                                                           
         MVC   TA99REXP,DMCB+4                                                  
         CLC   TA99REXP,=F'1'                                                   
         JL    INVERR                                                           
         CLC   TA99REXP(4),=F'9999900'                                          
         JH    INVERR                                                           
*&&                                                                             
         GOTO1 ADDELEM                                                          
*                                                                               
         GOTO1 ACTVIN,DMCB,TNNLCHGH      LAST CHANGED                           
***      B     XIT                                                              
         B     DISP99                                                           
         LTORG                                                                  
         DROP  R4                                                               
**********************************************************************          
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRA6D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR75D          1099 SCREEN                                  
         EJECT                                                                  
*                                                                               
         ORG   SW2WORK                                                          
*                                                                               
SVKEY    DS    CL38                SAVE THE KEY                                 
SVEMP    DS    CL3                 SAVED EMPLOYER                               
SVCUR    DS    CL1                       CURRENCY                               
ADDNEW   DS    CL1                 MUST ADD A NEW RECORD                        
XCDATE   DS    PL3                 COMPLEMENTED PWOS DATE                       
MYBYTE   DS    CL1                                                              
*YBYTE2  DS    CL1                                                              
TBYTE    DS    CL1                                                              
SVSSN    DS    CL9                                                              
*                                                                               
EMPTAB   DS    10CL3               EMPLOYER TABLE                               
         DS    0D                                                               
PTRBLK   DS    CL(L'TLDRREC+1)     1 ACTIVE                                     
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025TAGENA6   11/12/14'                                      
         END                                                                    
