*          DATA SET SRPCT00    AT LEVEL 004 AS OF 05/01/02                      
*PHASE T16400A                                                                  
         PRINT NOGEN                                                            
         TITLE '$PCTRANS - MAINFRAME TO STEREO TWA TANSLATION'                  
PCTRANS  CSECT                                                                  
         NMOD1 PCTWRKLQ,$PCTRANS,CLEAR=YES                                      
         USING PCTWRKD,RC          RC=A(W/S)                                    
         ST    R1,SYSPARAM         STORE SYSTEM PARAMETERS                      
*                                                                               
         L     RA,20(R1)           RA=A(TWA)                                    
         USING T164FFD,RA                                                       
         L     R9,4(R1)            R9=A(TIA)                                    
*                                                                               
         L     RE,12(R1)           SAVE VARIOUS ENTRYS FROM COMFACS             
         USING COMFACSD,RE                                                      
         MVC   DATAMGR,CDATAMGR                                                 
         DROP  RE                                                               
*                                                                               
         L     RE,28(R1)           SAVE THE CURSOR INFO                         
         USING TIOBD,RE                                                         
         MVC   CURADDR,TIOBCURS                                                 
         DROP  RE                                                               
*                                                                               
         MVC   UTLADDR,8(R1)       SAVE THE UTL ADDRESS                         
         EJECT                                                                  
ROWS     EQU   23                                                               
COLS     EQU   L'PCTLINE                                                        
*                                                                               
*  STEREO  EQUATES                                                              
*                                                                               
STMSGQ   EQU   1                                                                
STCEXQ   EQU   19                                                               
ROW1Q    EQU   21                                                               
*                                                                               
PFZINT   EQU   10                  PROTECTED FIELD ZERO INTENSITY               
PFNINT   EQU   11                  PROTECTED FIELD NORMAL INTENSITY             
PFHINT   EQU   12                  PROTECTED FIELD HIGH INTENSITY               
*                                                                               
UFZINT   EQU   13                  UNPROTECTED ZERO INT                         
UFNINT   EQU   14                  UNPROTECTED NORMAL INT                       
UFHINT   EQU   15                  UNPROTECTED HIGH INT                         
UFZINTC  EQU   16                  UNPROTECTED ZERO INT   & CURSOR              
UFNINTC  EQU   17                  UNPROTECTED NORMAL INT & CURSOR              
UFHINTC  EQU   18                  UNPROTECTED HIGH INT   & CURSOR              
         EJECT                                                                  
*********************************************************************           
*   MAIN                                                                        
*********************************************************************           
         BAS   RE,READTWA                                                       
         BAS   RE,TRANTWA                                                       
EXIT     XMOD1 1                                                                
         EJECT                                                                  
*********************************************************************           
*   READTWA - MODIFIED FROM CTMAD00                                             
*       READS TWA0 OF THE CALLER AND COPYS IT INTO TIA WHERE                    
*           RA -> TWA                                                           
*           R9 -> TIA                                                           
*      UTLADDR -> UTL                                                           
*********************************************************************           
READTWA  NTR1                                                                   
         SR    R3,R3                                                            
         ICM   R3,3,2(RA)          LOW ORDER TWO BYTES TO TERM #                
*                                                                               
         MVC   CMDMCB+20(2),=C'L=' SET DATAMGR PARM 6 TO READ THE NEW           
         MVC   CMDMCB+22(2),LENTWA     LARGE TEMPSTR RECORD SIZE                
*                                                                               
*                                  CALL DATAMGR TO READ TEMPSTR RECORD          
         GOTO1 DATAMGR,CMDMCB,(0,=C'DMREAD'),=C'TEMPSTR',(0,(R3)),(R9)          
         CLI   8(R1),0                                                          
         BE    EXIT                                                             
         DC    H'0'                SERIOUS BUG - NEED CORE DUMP                 
         EJECT                                                                  
*********************************************************************           
*   TRANTWA - TRANSLATES THE TWA TO STEREO FORMAT                               
*           RA -> TWA                                                           
*           R9 -> TIA(CONTAING INPUT TWA)                                       
*      UTLADDR -> UTL                                                           
*********************************************************************           
TRANTWA  NTR1                                                                   
         LA    R8,64(R9)           POINT TO BEGINING OF INPUT SCREEN            
         LA    R7,TRANWORK         POINT TO TWA BUFFER AREA                     
         L     R0,=F'-1'           THE ROW NUMBER                               
*                                                                               
*   DO THE HEADER                                                               
*                                                                               
         MVI   0(R7),STMSGQ        SET TO BE A STEREO MESSAGE                   
         TR    0(1,R7),DECTRAN     TRANSLATE TO STERE0                          
         L     R2,UTLADDR          POINT TO UTL                                 
         USING UTLD,R2                                                          
         ZIC   R1,TSESSION         GET THE SESSION NUMBER                       
         DROP  R2                                                               
         LA    R1,C'A'(R1)         MAKE IT THE LETTER                           
         STC   R1,1(R7)                                                         
         LA    R7,2(R7)                                                         
*                                                                               
*   READ THROUGH THE FIELDS                                                     
*                                                                               
TWALOOP  DS    0H                                                               
         CLI   0(R8),0             END OF SCREEN?                               
         BE    TWAOUT              YES                                          
*                                                                               
         LA    R1,2(R8)            POINT TO SCREEN ADDRESS                      
         BAS   RE,GETROW                                                        
         CR    R1,R0                                                            
         BE    SAMEROW             SAME ROW                                     
         BL    TWAERR1             FIELDS OUT OF ORDER                          
         LR    R0,R1               SET NEW CURRENT ROW                          
         LA    R1,ROW1Q(R1)        SET R1 FOR TRANSLATION                       
         STC   R1,0(R7)            SET THE NEW ROW                              
         TR    0(1,R7),DECTRAN     TRANSLATE TO STERE0                          
         LA    R7,1(R7)            NEXT BYTE                                    
*                                                                               
SAMEROW  DS    0H                                                               
         TM    1(R8),X'20'         PROTECTED FIELD?                             
         BZ    *+12                NO                                           
         BAS   RE,TRANPFLD                                                      
         B     *+8                                                              
         BAS   RE,TRANUFLD                                                      
*                                                                               
TWABUMP  ZIC   R1,0(R8)            FIELD LENGTH                                 
         LA    R8,0(R1,R8)         NEXT FIELD                                   
         B     TWALOOP                                                          
*                                                                               
TWAOUT   DS    0H                                                               
         LA    R8,PCTLINEH         POINT TO TWA LINE                            
         LA    R7,TRANWORK         POINT TO TRANSLATED SCREEN                   
         LA    R0,ROWS             # OF ROWS ON SCREEN                          
*                                                                               
TWACOPY  DS    0H                                                               
         ZIC   R1,0(R8)            GET THE FIELD LENGTH(LAST ROW 78)            
         LR    R2,R1                                                            
         TM    1(R8),X'02'         EXTENDED FIELD HEADER?                       
         BNO   *+8                 NO                                           
         SH    R1,=H'8'                                                         
         SH    R1,=H'8'                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8              MOVE LINE OF DATA                            
         B     *+10                                                             
         MVC   8(0,R8),0(R7)                                                    
         OI    6(R8),X'80'         TRANSMIT THE LINE                            
*                                                                               
         LA    R8,0(R2,R8)         NEXT FIELD                                   
         LA    R7,1(R1,R7)         NEXT DATA LINE                               
         BCT   R0,TWACOPY                                                       
         B     *+10                                                             
*                                                                               
TWAERR1  MVC   PCTHEAD,ERR1MSG     COPY ERROR MESSAGE                           
         OI    PCTHEADH+6,X'80'                                                 
         B     EXIT                                                             
         EJECT                                                                  
*********************************************************************           
*    TRANPFLD - TRANSLATE PROTECTED FIELD                                       
*                                                                               
*      INPUT:                                                                   
*        R7 -> CURRENT LOACTION IN THE TWA WORKAREA                             
*        R8 -> HEADER OF FIELD TO BE TRANSLATED                                 
*                                                                               
*      OUTPUT:                                                                  
*        R7 -> NEW LOCATION IN THE TWA WORKAREA                                 
*                                                                               
*********************************************************************           
TRANPFLD NTR1                                                                   
*                                                                               
*     START BY DOING THE INTENSITY                                              
*                                                                               
         MVI   0(R7),PFNINT        ASSUME NORMAL INTENSITY                      
         TM    1(R8),X'0C'         CHECK INTENSITY                              
         BZ    *+24                NORMAL                                       
         BO    *+8                 ZERO                                         
         BM    *+12                HIGH                                         
         MVI   0(R7),PFZINT        ZERO                                         
         B     *+8                                                              
         MVI   0(R7),PFHINT                                                     
         TR    0(1,R7),DECTRAN     TRANSLATE TO STEREO                          
         LA    R7,1(R7)                                                         
*                                                                               
*     NOW GET THE COLUMN NUMBER                                                 
*                                                                               
         LA    R1,2(R8)            POINT TO SCREEN ADDRESS                      
         BAS   RE,GETCOL           GET THE COULUMN NUMBER                       
         STC   R1,0(R7)            STORE THE COLUMN NUMBER                      
         TR    0(1,R7),DECTRAN     TRANSLATE TO STEREO                          
         LA    R7,1(R7)                                                         
*                                                                               
*    NOW DO THE DATA LENGTH                                                     
*                                                                               
         ZIC   R1,0(R8)            FIELD LENGTH                                 
         TM    1(R8),X'02'         EXTENDED FIELD HEADER?                       
         BZ    *+8                 NO                                           
         SH    R1,=H'8'                                                         
         SH    R1,=H'8'            DATA LENGTH IN R1                            
         LA    R2,7(R1,R8)                                                      
         CLI   0(R2),C' '                                                       
         BH    *+8                 END OF DATA                                  
         BCT   R1,*-12                                                          
*                                                                               
         LR    R2,R1               COMPRESS THE DATA                            
         GOTO1 COMPRESS,CMDMCB,8(R8),(R2),COMBUF,(R2)                           
*                                                                               
         MVC   0(1,R7),12+3(R1)    STORE THE DATA LENGTH                        
         TR    0(1,R7),DECTRAN     TRANSLATE TO STEREO                          
         LA    R7,1(0,R7)                                                       
*                                                                               
         OC    12(4,R1),12(R1)     ANY DATA?                                    
         BZ    PFOUT               NO                                           
*                                                                               
         L     R2,12(R1)           COPY THE COMPRESSED DATA                     
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),COMBUF                                                   
         LA    R7,1(R2,R7)                                                      
*                                                                               
PFOUT    DS    0H                                                               
         XIT1  REGS=(R7)                                                        
         EJECT                                                                  
*********************************************************************           
*    TRANUFLD - TRANSLATE UNPROTECTED FIELD                                     
*                                                                               
*      INPUT:                                                                   
*        R7 -> CURRENT LOACTION IN THE TWA WORKAREA                             
*        R8 -> HEADER OF FIELD TO BE TRANSLATED                                 
*                                                                               
*      OUTPUT:                                                                  
*        R7 -> NEW LOCATION IN THE TWA WORKAREA                                 
*                                                                               
*********************************************************************           
TRANUFLD NTR1                                                                   
*                                                                               
*     START BY DOING THE INTENSITY                                              
*                                                                               
         LA    R4,UFNINT           ASSUME NORMAL INTENSITY                      
         TM    1(R8),X'0C'         CHECK INTENSITY                              
         BZ    *+24                NORMAL                                       
         BO    *+8                 ZERO                                         
         BM    *+12                HIGH                                         
         LA    R4,UFZINT                                                        
         B     *+8                                                              
         LA    R4,UFHINT                                                        
*                                                                               
         ZIC   R1,0(R8)            FIELD LENGTH                                 
         TM    1(R8),X'02'         EXTENDED FIELD HEADER?                       
         BZ    *+8                 NO                                           
         SH    R1,=H'8'                                                         
         SH    R1,=H'8'            DATA LENGTH IN R1                            
         LR    R0,R1               FIELD LENGTH IN R0                           
         LA    R2,7(R1,R8)                                                      
         CLI   0(R2),C' '                                                       
         BH    *+8                 END OF DATA                                  
         BCT   R1,*-12                                                          
*                                                                               
         SR    R6,R6               NO CURSOR EXCEPTION                          
         LH    R2,2(R8)            SCREEN ADDRESS OF FIELD                      
         CH    R2,CURADDR          IS THE CURSOR HERE?                          
         BE    UFCUR               YES                                          
         BH    UFNOCUR             ALREADY FOUND THE CURSOR                     
*                                                                               
         AR    R2,R0               SCREEN ADDRESS + FIELD LENGTH                
         CH    R2,CURADDR          IN THE MIDDLE OF THE FIELD?                  
         BNH   UFNOCUR             NO                                           
*                                                                               
UFCUR    LA    R4,UFZINTC-UFZINT(R4)                                            
         LH    R6,CURADDR                                                       
         SH    R6,2(R8)                                                         
*                                                                               
UFNOCUR  STC   R4,0(R7)                                                         
         TR    0(1,R7),DECTRAN     TRANSLATE TO STEREO                          
         LA    R7,1(R7)                                                         
*                                                                               
         LR    R2,R1               SAVE THE DATA LENGTH                         
*                                                                               
         LA    R1,2(R8)            GET THE COLUMN NUMBER                        
         BAS   RE,GETCOL                                                        
         STC   R1,0(R7)            STORE THE COLUMN NUMBER                      
         TR    0(1,R7),DECTRAN     TRANSLATE TO STEREO                          
         LA    R7,1(R7)                                                         
*                                                                               
         GOTO1 COMPRESS,CMDMCB,8(R8),(R2),COMBUF,(R2)                           
*                                                                               
         MVC   0(1,R7),12+3(R1)    STORE THE DATA LENGTH                        
         TR    0(1,R7),DECTRAN     TRANSLATE TO STEREO                          
         LA    R7,1(0,R7)                                                       
*                                                                               
         STC   R0,0(R7)            STORE THE FIELD LENGTH                       
         TR    0(1,R7),DECTRAN     TRANSLATE TO STEREO                          
         LA    R7,1(0,R7)                                                       
*                                                                               
         OC    12(4,R1),12(R1)     ANY DATA?                                    
         BZ    UFOUT               NO                                           
*                                                                               
         L     R2,12(R1)           COPY THE COMPRESSED DATA                     
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),COMBUF                                                   
         LA    R7,1(R2,R7)                                                      
*                                                                               
UFOUT    DS    0H                                                               
         LTR   R6,R6               CURSOR EXEPTION?                             
         BZ    UFXIT               NO                                           
*                                                                               
         MVI   0(R7),STCEXQ        CURSOR EXCEPTION                             
         STC   R6,1(R7)            OFFSET OF CURSOR INTO FIELD                  
         TR    0(2,R7),DECTRAN     TRANSLATE TO STEREO                          
         LA    R7,2(R7)                                                         
UFXIT    XIT1  REGS=(R7)                                                        
         EJECT                                                                  
*********************************************************************           
*     GETROW - RETURN ROW NUMBER                                                
*        INPUT:                                                                 
*          R1 = START OF DATA SCREEN ADDRESS = (ROW-1)*80+(COL-1)               
*                                                                               
*        OUTPUT:                                                                
*          R1 = ROW NUMBER                                                      
*********************************************************************           
GETROW   NTR1                                                                   
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         LH    R3,0(R1)                                                         
         LA    R5,80                                                            
         DR    R2,R5                                                            
         LR    R1,R3                                                            
         XIT1  REGS=(R1)                                                        
*********************************************************************           
*     GETCOL - RETURN COLUMN NUMBER                                             
*        INPUT:                                                                 
*          R1 = START OF DATA SCREEN ADDRESS = (ROW-1)*80+(COL-1)               
*                                                                               
*        OUTPUT:                                                                
*          R1 = COLUMN NUMBER                                                   
*********************************************************************           
GETCOL   NTR1                                                                   
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         LH    R3,0(R1)                                                         
         LA    R5,80                                                            
         DR    R2,R5                                                            
         LR    R1,R2                                                            
         LA    R1,1(R1)                                                         
         XIT1  REGS=(R1)                                                        
*                                                                               
* THE FOLLOWING ROUTINES WRE MODIFIED FROM:                                     
*          DATA SET CTMAD00    AT LEVEL 148 AS OF 06/16/95                      
*                                                                               
***********************************************************************         
* THIS ROUTINE COMPRESSES OUTGOING DATA OF THE ITEM ABOUT TO BE PUT.            
*                                                                               
* 'DINK' WILL BE REPLACED BY A '?0A'  REMOVED THIS                              
* '+' WILL BE REPLACED BY A '?0B'      AND THIS                                 
* '?' WILL BE REPLACED BY A '??'                                                
*                                                                               
* DMCB                                                                          
*        PARAM1 A(DATA)            ADDRESS OF DATA TO BE COMPRESSED             
*        PARAM2 L'DATA                                                          
*        PARAM3 A(DEST)            ADRESS OF DESTINATION                        
*        PARAM4 L'COMDATA          RETURNED LENGTH OF COMPRESSED DATA           
*                                                                               
* COMPRESSIONS WILL HAVE THE FOLLOWING FORMAT: ?NX.                             
* ? IS THE ESCAPE CHARACTER                                                     
* N IS THE # OF CHRACTER REPEATS TRANSLATED TO STEREO                           
* X IS THE CHARACTER TO BE REPEATED                                             
***********************************************************************         
COMPRESS NTR1                                                                   
         OC    4(4,R1),4(R1)       IF NO LENGTH                                 
         BZ    CMX                 THEN NOTHING TO COMPRESS                     
*                                                                               
         L     R5,0(R1)            R5 = A(FIRST CHAR IN SOURCE DATA)            
         L     R4,4(R1)            R4 = LENGTH(SOURCE DATA)                     
         L     R3,8(R1)            R3 = A(FIRST CHAR IN DEST DATA)              
         SR    R2,R2               R2 = REPEATING CHARACTER                     
         SR    R6,R6               R6 = NUMBER OF REPETITIONS                   
*                                                                               
CM10     CLM   R2,1,0(R5)          IF NEW CHARACTER                             
         BE    CM100                                                            
*                                                                               
         BAS   RE,COMPREV          THEN COMPRESS PREV REPEATED CHARS            
*                                                                               
*        CLI   0(R5),DINKCHR       IF NEW CHARACTER IS A 'DINK'                 
*        BNE   CM20                                                             
*        MVI   CMBYTE,C'A'         THEN EXPAND IT TO '?0A'                      
*        B     CM200                                                            
*                                                                               
*M20     CLI   0(R5),C'+'          ELSE IF NEW CHARACTER IS A '+'               
*        BNE   CM30                                                             
*        MVI   CMBYTE,C'B'         THEN EXPAND IT TO '?0B'                      
*        B     CM200                                                            
*                                                                               
CM30     CLI   0(R5),C'?'          ELSE IF NEW CHARACTER IS A '?'               
         BNE   CM40                                                             
         MVI   CMBYTE,C'?'         THEN EXPAND IT TO '?0C'                      
         B     CM200                                                            
*                                                                               
CM40     IC    R2,0(R5)            ELSE R2 = NEW CHAR                           
         LA    R6,1                     R6 = NUM OF REPS = 1                    
         B     CM300                                                            
*                                                                               
CM100    LA    R6,1(R6)            ELSE (NOT NEW CHAR) BUMP REP COUNT           
*                                                                               
         CH    R6,=H'61'           IF REP COUNT >= 61                           
         BL    CM300                                                            
         BAS   RE,COMPREV          THEN COMPRESS PREV REPEATED CHARS            
         B     CM300                                                            
*                                                                               
CM200    MVC   0(1,R3),=C'?0'      STORE ESCAPE SEQUENCE ?0* WHERE              
         MVC   1(1,R3),CMBYTE          * IS STORED IN CMBYTE                    
         LA    R3,2(R3)                                                         
*                                                                               
CM300    LA    R5,1(R5)            BUMP R5 TO NEXT BYTE OF SOURCE DATA          
         BCT   R4,CM10             REPEAT UNTIL NO MORE SOURCE DATA             
*                                                                               
         LTR   R6,R6               IF CHARS LEFT OVER TO BE PUT                 
         BZ    *+8                                                              
         BAS   RE,COMPREV          THEN COMPRESS LEFT OVER DATA                 
*                                                                               
*        MVC   ADATA,ACOMP         SET A(DATA) TO A(COMPRESSED BUFFER)          
*                                                                               
         L     RF,8(R1)            SET LEN OF COMPRESSED DATA                   
         SR    R3,RF                                                            
         ST    R3,12(R1)                                                        
*                                                                               
CMX      B     EXIT                                                             
***********************************************************************         
* THIS ROUTINE STORES THE APPROPRIATE COMPRESSION SEQUENCE FOR THE              
* PREVIOUSLY REPEATED CHARACTERS IN THE DESTINATION DATA.                       
***********************************************************************         
COMPREV  DS    0H                                                               
         LTR   R6,R6               IF NO PREVIOUS CHAR                          
         BZ    CPX                 THEN RETURN                                  
*                                                                               
         CH    R6,=H'4'            ELSE IF NUMBER OF REPS < 4                   
         BNL   CP100                                                            
*                                                                               
CP10     STC   R2,0(R3)            THEN LOOP AND SAVE REPEATED CHAR             
         LA    R3,1(R3)                TO DEST DATA                             
         BCT   R6,CP10                                                          
         B     CPX                 AND RETURN                                   
*                                                                               
CP100    MVI   0(R3),C'?'          STORE COMP SEQUENCE IN DEST DATA             
         STC   R6,1(R3)                                                         
         TR    1(1,R3),DECTRAN     CONVERT TO STEREO                            
         STC   R2,2(R3)                                                         
*                                                                               
         LA    R3,3(R3)            BUMP R3 TO NEXT POS IN DEST DATA             
*                                                                               
         SR    R6,R6               CLEAR REPEAT CHAR AND COUNT                  
         SR    R2,R2                                                            
*                                                                               
CPX      BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CONVERTS A DECIMAL CHARACTER STRING TO BINARY.  IF ANY           
*********************************************************************           
* CONSTANTS                                                                     
*********************************************************************           
ERR1MSG  DC    CL(60)'FIELDS NOT IN SCREEN ORDER'                               
LENTWA   DC    H'18432'            LENGTH TIA                                   
DECTRAN  DS    0CL256                                                           
         DC    X'A1'                          00                                
         DC    X'C1C2C3C4C5C6C7C8C9D1'        01-10                             
         DC    X'D2D3D4D5D6D7D8D9E2E3'        11-20                             
         DC    X'E4E5E6E7E8E9F0F1F2F3'        21-30                             
         DC    X'F4F5F6F7F8F981828384'        31-40                             
         DC    X'85868788899192939495'        41-50                             
         DC    X'96979899A2A3A4A5A6A7'        51-60                             
         DC    X'A8A97D4B4C4D4E7E505A'        61-70                             
         DC    X'5B5C5D5E7F60616A6B6C'        71-80                             
         DC    X'6D6E6F797A7B7C7D7E7F'        81-90                             
DEFDECQ  EQU   *-DECTRAN                                                        
         DC    XL(L'DECTRAN-DEFDECQ)'00'                                        
*                                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* WORKING STORAGE                                                               
*********************************************************************           
PCTWRKD  DSECT                                                                  
*                                                                               
CMDMCB   DS    6F                                                               
DATAMGR  DS    V                                                                
UTLADDR  DS    A                   ADDRESS OF UTL                               
SYSPARAM DS    F                   SYSTEM PARAMETERS                            
TWAKEY   DS    F                   KEY TO READ TWA RECORD                       
TWANUM   DS    H                   ALWAYS 0                                     
TWATERM  DS    H                                                                
CURADDR  DS    XL2                 SCREEN ADRRESS OF FIELD W/CURSOR             
CMBYTE   DS    X                                                                
TRANWORK DS    30CL(L'PCTLINE)     AREA TO CREATE TWA                           
COMBUF   DS    CL256               COMPRESS DATA BUFFER                         
*                                                                               
PCTWRKLQ EQU   *-PCTWRKD                                                        
         EJECT                                                                  
* SRPCTFFD                                                                      
         PRINT OFF                                                              
       ++INCLUDE SRPCTFFD                                                       
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* FADSECTS                                                                      
         PRINT OFF                                                              
       ++INCLUDE FADSECTS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SRPCT00   05/01/02'                                      
         END                                                                    
