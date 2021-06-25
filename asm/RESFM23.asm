*          DATA SET RESFM23    AT LEVEL 030 AS OF 05/02/12                      
*PHASE T81823A,*                                                                
         TITLE 'T81823 - RESFM23 - STANDARD COMMENT RECORD'                     
*********************************************************************           
*                                                                   *           
*  RESFM23 (T81823) --- STANDARD COMMENT REC ADD/CHA/LIST/REPORT    *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 19AUG91  (EFJ) --- INITIAL ENTRY                                  *           
*                                                                   *           
* 05SEP91  (SKU) --- LIST DISPLAY FIRST NON-BLANK LINE IF NO COMMT  *           
*                                                                   *           
* 27JAN92  (SKU) --- ENHANCED DISPLAY SCREEN                        *           
*                                                                   *           
* 19FEB92  (SKU) --- CHANGE KEY PART OF NUMBER TO ALPHNUMERIC       *           
*                                                                   *           
* 19MAR92  (SKU) --- LIMIT OFFICE ACCESS                            *           
*                                                                   *           
* 05MAY11  (SKU) --- DDS ONLY '*' COMMENT ACCESS                    *           
*                                                                   *           
* 02MAY12  (SKU) --- DESCRIPTION IS MANDATORY                       *           
*********************************************************************           
*                                                                               
T81823   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1823**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ST    R3,RELO                                                          
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    *+8                                                              
         BAS   RE,CHECKACC         CHECK OFFICE ACCESS                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PREP                                                             
         CLI   MODE,XRECPUT                                                     
         BE    DREC                                                             
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE KEY ROUTINE                            *                
****************************************************************                
****************************************************************                
         SPACE 3                                                                
VKEY     DS    0H                                                               
         MVC   AIO,AIO1                                                         
         L     R4,AIO                                                           
         USING ROCMREC,R4                                                       
         MVI   ROCMKTYP,X'34'                                                   
         MVC   ROCMKREP,AGENCY                                                  
*                                                                               
* PICK UP OFFICE CODE                                                           
OFFED    DS    0H                                                               
         LA    R2,CMTOFFH                                                       
         MVI   ERROR,MISSING                                                    
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BNZ   OE20                                                             
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   OE10                                                             
         CLI   ACTNUM,ACTLIST                                                   
         BE    NUMED                                                            
         CLI   ACTNUM,ACTREP                                                    
         BE    NUMED                                                            
         B     ERREND                                                           
* VALIDATE OFFICE IS VALID FOR REP                                              
OE10     DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         XC    KEY,KEY                                                          
         MVI   KEY,X'04'                                                        
         MVC   KEY+23(2),AGENCY                                                 
         MVC   KEY+25(2),8(R2)                                                  
         GOTO1 READ                                                             
OE20     MVC   ROCMKOFC,8(R2)                                                   
         OI    4(R2),X'20'                                                      
*                                                                               
* PICK UP COMMENT NUMBER                                                        
NUMED    DS    0H                                                               
         LA    R2,CMTNUMH                                                       
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   NE10                                                             
         CLI   ACTNUM,ACTLIST      NOT REQUIRED FOR LIST                        
         BE    PAGED                                                            
         CLI   ACTNUM,ACTREP       OR REPORT                                    
         BE    PAGED                                                            
         B     ERREND                                                           
NE10     DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),2                                                          
         BNE   ERREND                                                           
*                                                                               
         CLI   8(R2),C'A'          MUST BE ALPHANUMERIC                         
         BL    ERREND                                                           
         CLI   8(R2),C'9'                                                       
         BH    ERREND                                                           
         CLI   8(R2),C'}'                                                       
         BE    ERREND                                                           
         CLI   8(R2),C'\'                                                       
         BE    ERREND                                                           
         CLI   9(R2),C'A'          MUST BE ALPHANUMERIC                         
         BL    ERREND                                                           
         CLI   9(R2),C'9'                                                       
         BH    ERREND                                                           
         CLI   9(R2),C'}'                                                       
         BE    ERREND                                                           
         CLI   9(R2),C'\'                                                       
         BE    ERREND                                                           
         MVC   ROCMKNUM,8(R2)                                                   
*                                                                               
         OI    4(R2),X'20'         MARK FIELD PRE-VALID                         
* PICK UP PAGE                                                                  
PAGED    DS    0H                                                               
         MVI   ROCMKPAG,C'0'                                                    
*                                                                               
VKXIT    DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY,ROCMKEY                                                      
VKXX     B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              VALIDATE RECORD ROUTINE                         *                
****************************************************************                
****************************************************************                
         SPACE 3                                                                
VREC     DS    0H                                                               
         L     R4,AIO                                                           
         USING OCMRECD,R4                                                       
         XC    LIN,LIN                                                          
         XC    INSADR,INSADR                                                    
         XC    NUMINS,NUMINS                                                    
         LA    R5,INSADR                                                        
* COPY OF REC                                                                   
         L     R0,AIO1             'FROM' ADDRESS                               
         LA    R1,1000             'FROM' LENGTH                                
         L     RE,AIO2             'TO' ADDRESS                                 
         LR    RF,R1               'TO' LENGTH = 'FROM' LENGTH                  
         MVCL  (RE),(R0)                                                        
*                                                                               
* VALIDATE DESC FIELD                                                           
         LA    R2,CMTDSCH                                                       
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),0                                                          
         BE    ERREND                                                           
         TM    4(R2),X'20'         PRE-VALID?                                   
         BNZ   VR10                                                             
         MVI   ELCODE,X'01'                                                     
         GOTO1 REMELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING ROCMDSEL,R6                                                      
         MVI   ROCMDSCD,X'01'                                                   
         ZIC   R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZ    VR10                NO LABEL                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ROCMDSDS(0),8(R2)                                                
         LA    R1,3(R1)                                                         
         STC   R1,ROCMDSLN                                                      
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
* VALIDATE COMMENT LINES                                                        
VR10     DS    0H                                                               
         MVI   ELCODE,X'02'                                                     
         GOTO1 REMELEM             DELETE OLD X'02' ELEMS                       
         LA    R2,CMTCMTH                                                       
*                                                                               
* START OF ELEM                                                                 
VR20     XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING ROCMCMEL,R6                                                      
         MVI   ROCMCMCD,X'02'                                                   
*                                                                               
* VALIDATE CMT (MANDATORY)                                                      
         CLI   5(R2),0                                                          
         BNE   VR30                                                             
         SR    R0,R0                                                            
         MVC   RERROR,=AL2(IAFTBLK)                                             
*                                                                               
* IF TEXT IS BLANK, CAN BE NO INPUT FOLLOWING                                   
VR25     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    VRXIT                                                            
         CLI   5(R2),0                                                          
         BNE   ERREND2                                                          
         B     VR25                                                             
*                                                                               
VR30     DS    0H                  COUNT LINES ON SCREEN                        
         ZIC   R1,LIN                                                           
         LA    R1,1(R1)                                                         
         STC   R1,LIN                                                           
*                                                                               
         CLC   =C'@D',8(R2)        DELETE THIS LINE?                            
         BNE   VR40                                                             
*                                                                               
* DELETE THIS LINE BY NOT ADDING IT (POSN R2 TO NEXT LINE)                      
         ZIC   R0,0(R2)            R2 TO START OF NEXT LINE                     
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     VR20                                                             
*                                                                               
VR40     DS    0H                                                               
         CLC   =C'@I',8(R2)        INSERT A BLANK LINE?                         
         BNE   VR50                                                             
*                                                                               
* KEEP COUNT OF NUMBER OF LINES INSERTED (SO WE KNOW WHERE TO                   
* POSN BLANK LINE)                                                              
         MVC   RERROR,=AL2(MANYLINE)                                            
         ZIC   R1,NUMINS                                                        
         LA    R1,1(R1)                                                         
         STC   R1,NUMINS                                                        
*                                                                               
         CLI   NUMINS,6                                                         
         BNH   VR45                                                             
         MVI   RTXTLEN,2                                                        
         LA    RE,=C'12'                                                        
         STCM  RE,7,RTXTADR                                                     
         B     ERREND2                                                          
*                                                                               
VR45     LR    R3,R2                                                            
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BZ    *+12                                                             
*                                                                               
         LA    R3,CMTCMT2H-CMTCMTH(R3)                                          
         BCT   R1,*-4                                                           
*                                                                               
         ST    R3,0(R5)            STORE ADDRESS WHERE TO INS ON SCREEN         
         LA    R5,4(R5)                                                         
         BAS   RE,REDISP           REDISPLAY CURRENT LINE FROM REC              
*                                                                               
VR50     DS    0H                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ROCMCMCM(0),8(R2)                                                
         LA    R1,3(R1)                                                         
         STC   R1,ROCMCMLN                                                      
*                                                                               
VR110    GOTO1 HELLO,DMCB,(C'P',=C'REPFILE'),AIO,ELEM,=C'ADD=CODE'              
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     VR20                                                             
*                                                                               
* MAKE SURE AT LEAST 1 DAYPART ELEM                                             
VRXIT    DS    0H                                                               
VRXX     B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              DISPLAY KEY ROUTINE                             *                
****************************************************************                
****************************************************************                
         SPACE 3                                                                
DKEY     DS    0H                                                               
         L     R4,AIO                                                           
         USING OCMRECD,R4                                                       
*                                                                               
         LA    R2,CMTOFFH                                                       
         MVC   8(L'ROCMKOFC,R2),ROCMKOFC                                        
         OI    6(R2),X'80'         XMIT FLD                                     
*                                                                               
         LA    R2,CMTNUMH                                                       
         MVC   8(L'ROCMKNUM,R2),ROCMKNUM                                        
         OI    6(R2),X'80'         XMIT FLD                                     
*                                                                               
DKXX     B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              DISPLAY RECORD ROUTINE                          *                
****************************************************************                
****************************************************************                
         SPACE 3                                                                
DREC     DS    0H                                                               
         XC    INSERR,INSERR       INSERT ERROR FLAG                            
         LA    R5,INSADR                                                        
*                                                                               
* DISPLAY NAME                                                                  
         XC    CMTDSC,CMTDSC       CLEAR FIELD                                  
         L     R6,AIO                                                           
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BNE   DR5                                                              
         USING ROCMDSEL,R6                                                      
         ZIC   R1,ROCMDSLN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CMTDSC(0),ROCMDSDS                                               
         DROP  R6                                                               
*                                                                               
DR5      OI    CMTDSCH+6,X'80'     SET XMIT BIT                                 
         LA    R2,CMTCMTH                                                       
         SR    R1,R1                                                            
DR10     DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    DR20                END OF SCREEN                                
         TM    1(R2),X'20'                                                      
         BNZ   DR20                NO PROTECTED FLDS IN DATA AREA               
         IC    R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         TM    1(R2),X'02'         EXTENDED FLD HDR?                            
         BZ    *+8                                                              
         SH    R1,=H'8'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES                                                   
         OI    6(R2),X'80'         XMIT FLD                                     
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         B     DR10                                                             
*                                                                               
* DISPLAY COMMENTS                                                              
DR20     LA    R2,CMTCMTH                                                       
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   DRXIT                                                            
         USING ROCMCMEL,R6                                                      
DR30     DS    0H                                                               
         CLI   INSERR,1            WAS AN ERROR FOUND?                          
         BE    DR35                                                             
*                                                                               
         TM    1(R2),X'20'         PROTECTED?                                   
         BZ    *+12                                                             
         MVI   INSERR,1                                                         
         B     DR5                                                              
*                                                                               
         CLI   MODE,XRECPUT                                                     
         BNE   DR35                                                             
         CLM   R2,7,1(R5)          IS THIS ADDR IN TABLE?                       
         BNE   DR35                                                             
*                                                                               
* LEAVE BLANK LINE FOR INSERT                                                   
         LA    R5,4(R5)            NEXT INSADR                                  
         ZIC   R0,0(R2)            R2 TO START OF NEXT LINE                     
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     DR30                                                             
*                                                                               
DR35     DS    0H                                                               
         ZIC   R1,ROCMCMLN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),ROCMCMCM                                                 
*                                                                               
DR70     DS    0H                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)            R2 TO NEXT 'TEXT' FIELD                      
         AR    R2,R0                                                            
         BAS   RE,NEXTEL                                                        
         BE    DR30                                                             
*                                                                               
DRXIT    DS    0H                                                               
         CLI   INSERR,1            WAS AN ERROR FOUND?                          
         BNE   DRXX                                                             
*                                                                               
         LA    R2,CMTCMTH                                                       
         MVC   RERROR,=AL2(MANYLINE)                                            
         MVI   RTXTLEN,2                                                        
         LA    RE,=C'12'                                                        
         STCM  RE,7,RTXTADR                                                     
         B     ERREND2                                                          
DRXX     B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
****************************************************************                
****************************************************************                
*                        LIST ROUTINE                          *                
****************************************************************                
****************************************************************                
         SPACE 3                                                                
LIST     DS    0H                                                               
         LA    R4,KEY                                                           
         USING ROCMKEY,R4                                                       
         OC    KEY(27),KEY         TEST FIRST TIME THRU                         
         BNZ   LR7                                                              
         MVI   ROCMKTYP,X'34'                                                   
         MVC   ROCMKREP,AGENCY                                                  
         MVC   ROCMKOFC,CMTOFF                                                  
*                                                                               
         CLI   TWAOFFC,C'*'        TEST IF DDS TERMINAL                         
         BE    LR5                                                              
         OC    ROCMKOFC,ROCMKOFC                                                
         BNZ   LR5                                                              
         MVI   ROCMKOFC,C'A'       IF NOT, SKIP OVER SPECIAL * CODE             
*                                                                               
LR5      DS    0H                                                               
         MVC   ROCMKNUM,CMTNUM                                                  
         MVI   ROCMKPAG,C'0'                                                    
         MVC   SAVEKEY,KEY                                                      
*                                                                               
LR7      GOTO1 HIGH                                                             
         B     LR15                                                             
         DROP  R4                                                               
*                                                                               
LR10     GOTO1 SEQ                                                              
LR15     CLC   KEY(22),SAVEKEY      CORRECT REP                                 
         BNE   LRXX                                                             
         OC    CMTOFF,CMTOFF       FILTER ON OFFICE                             
         BZ    LR20                                                             
         CLC   KEY+22(2),CMTOFF                                                 
         BNE   LRXX                                                             
*                                                                               
LR20     GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING OCMRECD,R4                                                       
*                                                                               
         MVC   LISTAR,SPACES                                                    
         LA    R2,LISTAR                                                        
         USING LISTD,R2                                                         
*                                                                               
         MVC   LOFF,ROCMKOFC                                                    
         MVC   LNUM,ROCMKNUM                                                    
         L     R6,AIO                                                           
         USING ROCMDSEL,R6                                                      
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         BNE   LR50                                                             
         ZIC   R1,ROCMDSLN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LDESC(0),ROCMDSDS                                                
         B     LR100                                                            
         DROP  R6                                                               
*                                                                               
LR50     L     R6,AIO                                                           
         USING ROCMCMEL,R6                                                      
         MVI   ELCODE,2            COMMENT ELEM                                 
         BAS   RE,GETEL                                                         
         BNE   LR100                                                            
         ZIC   R1,ROCMCMLN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LDESC(0),ROCMCMCM                                                
         DROP  R6                                                               
*                                                                               
LR100    GOTO1 LISTMON             FOR LIST                                     
         B     LR10                NEXT RECORD                                  
         SPACE 1                                                                
LRXX     B     XIT                                                              
         EJECT                                                                  
****************************************************************                
****************************************************************                
*                        PRINT ROUTINE                         *                
****************************************************************                
****************************************************************                
         SPACE 3                                                                
PREP     DS    0H                                                               
         LA    R1,HEDSPECS                                                      
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         SPACE 1                                                                
         LA    R4,KEY                                                           
         USING ROCMKEY,R4                                                       
         MVI   ROCMKTYP,X'34'                                                   
         MVC   ROCMKREP,AGENCY                                                  
         MVC   ROCMKOFC,CMTOFF                                                  
*                                                                               
         CLI   TWAOFFC,C'*'        TEST IF DDS TERMINAL                         
         BE    PR05                                                             
         OC    ROCMKOFC,ROCMKOFC                                                
         BNZ   PR05                                                             
         MVI   ROCMKOFC,C'A'       IF NOT, SKIP OVER SPECIAL * CODE             
*                                                                               
PR05     DS    0H                                                               
         MVC   ROCMKNUM,CMTNUM                                                  
         MVI   ROCMKPAG,C'0'                                                    
         MVC   SAVEKEY,KEY                                                      
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
         B     PR20                SKIP FIRST SEQ                               
PR10     GOTO1 SEQ                                                              
PR20     CLC   KEY(22),SAVEKEY     CORRECT REP                                  
         BNE   PRXX                                                             
         OC    CMTOFF,CMTOFF       FILTER ON OFFICE                             
         BZ    PR25                                                             
         CLC   KEY+22(2),CMTOFF                                                 
         BNE   PRXX                                                             
*                                                                               
PR25     GOTO1 GETREC                                                           
*                                                                               
PR30     DS    0H                  DISPLAY COMMENT LINES                        
         L     R6,AIO                                                           
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL                                                         
         BNE   PR50                                                             
         USING ROCMCMEL,R6                                                      
PR40     ZIC   R1,ROCMCMLN         GET LINE LENGTH                              
         SH    R1,=H'3'            2 FOR ELCODE & LEN, 1 FOR EX                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P(0),ROCMCMCM                                                    
         GOTO1 SPOOL,DMCB,(R8)     PRINT LINE                                   
         BAS   RE,NEXTEL                                                        
         BE    PR40                                                             
*                                                                               
PR50     DS    0H                                                               
         MVI   LINE,99             NEXT PAGE                                    
         B     PR10                NEXT RECORD                                  
         SPACE 1                                                                
PRXX     B     XIT                                                              
         EJECT                                                                  
****************************************************************                
* REDISPLAY ELEM CHANGED BY @I                                 *                
****************************************************************                
         SPACE 3                                                                
REDISP   NTR1                                                                   
         L     R6,AIO2             COPY OF OLD REC                              
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   LIN,1                                                            
         BE    RD10                                                             
         ZIC   R3,LIN                                                           
         BCTR  R3,0                                                             
*                                                                               
         MVI   ERROR,INVALID                                                    
         BAS   RE,NEXTEL                                                        
         BNE   ERREND                                                           
         BCT   R3,*-8                                                           
*                                                                               
         USING ROCMCMEL,R6                                                      
RD10     DS    0H                                                               
         MVC   8(L'CMTCMT,R2),SPACES                                            
         ZIC   R1,ROCMCMLN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),ROCMCMCM                                                 
RDX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
****************************************************************                
*  CHECK TERMINAL OFFICE LIMIT ACCESS                          *                
*  LIMIT IF O=OFFICE ELSE NO LIMIT                             *                
****************************************************************                
CHECKACC NTR1                                                                   
         CLI   TWAOFFC,C'*'        TEST IF DDS TERMINAL                         
         BE    CHECKAX                                                          
         CLC   =C'O=',TWAACCS      TEST FOR OFFICE RESTRICTION                  
         BNE   CHECKAX                                                          
*        TM    TWAAUTH,X'80'       TEST IF TERMINAL ALLOWED ACCESS              
*        BO    CHECKAX             TO ALL OFFICES                               
*                                                                               
         CLC   =C'REP',CONACT                                                   
         BE    CHECKAX                                                          
         CLC   =C'DIS',CONACT                                                   
         BE    CHECKAX                                                          
         CLC   =C'LIS',CONACT                                                   
         BE    CHECKAX                                                          
         CLC   =C'SEL',CONACT                                                   
         BNE   CHECKA10                                                         
         CLI   MODE,RECDEL                                                      
         BE    CHECKA10                                                         
         CLI   MODE,VALREC         CANNOT CHANGE OR DELETE FROM LIST            
         BNE   CHECKAX                                                          
*                                                                               
CHECKA10 DS    0H                  FOR ADD CHECK IF SCREEN OFFICE               
         CLC   TWAACCS+2(2),CMTOFF                                              
         BNE   CHECKERR                                                         
         B     CHECKAX                                                          
*                                                                               
CHECKA20 DS    0H                  CHANGE, DELETE, ADD OR RESTORE               
         L     R4,AIO              ALLOWED IF MATCHING OFFICE                   
         USING OCMRECD,R4                                                       
         CLC   ROCMKOFC,TWAACCS+2  ELSE,COMPARE OFFICES                         
         BE    CHECKAX                                                          
         DROP  R4                                                               
*                                                                               
CHECKERR DS    0H                                                               
         MVC   RERROR,=AL2(282)                                                 
         LA    R2,CONACTH                                                       
         B     ERREND2                                                          
*                                                                               
CHECKAX  B     XIT                                                              
         EJECT                                                                  
****************************************************************                
*  HEDSPECS                                                    *                
****************************************************************                
         SPACE 2                                                                
HEDSPECS DS    0H                                                               
         SSPEC H1,001,REQUESTOR                                                 
         SSPEC H2,001,AGYNAME                                                   
         SSPEC H1,038,C'C O M M E N T   R E C O R D S'                          
         SSPEC H2,038,C'-----------------------------'                          
         SSPEC H1,093,RUN                                                       
         SSPEC H2,093,REPORT                                                    
         SSPEC H2,109,PAGE                                                      
         SSPEC H4,001,C'Office'                                                 
         SSPEC H4,020,C'Number'                                                 
         SSPEC H5,001,C'Description'                                            
         DC    X'00'                                                            
         SPACE 4                                                                
HOOK     NTR1                                                                   
         L     R4,AIO                                                           
         USING ROCMREC,R4                                                       
         MVC   H4+10(02),ROCMKOFC                                               
         MVC   H4+30(02),ROCMKNUM                                               
         CLI   ROCMDSCD,X'01'      X'01' ELEM PRESENT?                          
         BNE   HKEXIT                                                           
         ZIC   R1,ROCMDSLN                                                      
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   H5+15(00),ROCMDSDS                                               
HKEXIT   B     XIT                                                              
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
ERREND   GOTO1 ERREX               ERROR NUM IN 'ERROR'  (1 BYTE)               
ERREND2  GOTO1 MYERROR             ERROR NUM IN 'RERROR' (2 BYTE)               
         SPACE 3                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
LISTD    DSECT                                                                  
LOFF     DS    CL2                                                              
         DS    CL3                                                              
LNUM     DS    CL2                                                              
         DS    CL3                                                              
LDESC    DS    CL20                                                             
         DS    CL51                                                             
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RESFMFFD                                                                      
* DDGENTWA                                                                      
* RESFMD9D                                                                      
* RESFMWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE RESFMFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMD9D                                                       
         EJECT                                                                  
       ++INCLUDE RESFMWORKD                                                     
         EJECT                                                                  
         ORG   SYSSPARE                                                         
*                                                                               
*               WORK AREA                                                       
         DS    0F                                                               
MYWORK   DS    0CL512                                                           
SAVEKEY  DS    CL27                                                             
INSADR   DS    7F                  SCRN ADDR WHICH TO INSERT BLNK LN            
NUMINS   DS    X                   NUMBER OF LINES INSERTED                     
LIN      DS    X                   DISPLAY LINE LOOKING AT                      
INSERR   DS    X                   FLAG FOR INSERT ERROR                        
*                                                                               
OCMRECD  DSECT                                                                  
       ++INCLUDE REGENOCM                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030RESFM23   05/02/12'                                      
         END                                                                    
