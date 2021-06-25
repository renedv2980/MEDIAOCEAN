*          DATA SET MPRDR02    AT LEVEL 008 AS OF 05/01/02                      
*PHASE T51002,*                                                                 
         TITLE 'T51002 - SURVEY FILE DEFINITION RECORD'                         
T51002   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**T51002                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         GOTO1 SETADDR             RESET 'FLOATING' ADDRESSES                   
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*        VALIDATE KEY                                                           
         SPACE 3                                                                
         USING QSDKEY,R4                                                        
VKEY     LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         LA    R2,SURSURH                                                       
         GOTO1 VALSURV                                                          
         B     XIT                                                              
         SPACE 2                                                                
*        DISPLAY KEY                                                            
         SPACE 1                                                                
DKEY     LA    R4,KEY              DISPLAY KEY (FOR SELECT)                     
         LA    R2,SURSURH                                                       
         MVC   WORK(8),SPACES                                                   
         MVC   WORK(6),QSDKFCD                                                  
         BAS   RE,GENDISP                                                       
         B     XIT                                                              
         EJECT                                                                  
*        VALIDATE RECORD                                                        
*        ---------------                                                        
         SPACE 1                                                                
VREC     DS    0H                                                               
         L     R6,AIO                                                           
         CLI   ACTNUM,ACTADD       IF ADDING                                    
         BNE   VR4                                                              
         L     R7,VNODBLK          MAKE RECORD A NODIO MASTER                   
         USING NODBLKD,R7                                                       
         LR    RF,R6                                                            
         AH    RF,LKEY                                                          
         MVC   0(2,RF),DATADISP                                                 
*                                                                               
         MVI   NDWRITE,C'N'    DONT LET NODIO ACTUALLY ADD RECORD               
*                              NOTE- MOST NODIO FIELDS ARE SET BY BASE          
*                                    OR BY DEFAULT                              
         MVI   NDNDPOS,QSPKNOD-QSPKEY   NODE POSITION                           
*                                                                               
         MVC   NDIOA,AIO           USE NORMAL IOA                               
         GOTO1 NODIO,DMCB,VNODBLK,=C'MAST'                                      
         MVC   NDIOA,AIO2          NODIO NORMALLY USES 2 (AND 3)                
         MVI   NDWRITE,C'Y'                                                     
         DROP  R7                                                               
*                                                                               
*                                  DEFINITION ELEMENT                           
*                                  ------------------                           
*                                                                               
VR4      DS    0H                                                               
         LA    R6,ELEM                                                          
         USING QSDEFEL,R6                                                       
*                                                                               
         MVI   ELCODE,X'10'                                                     
         GOTO1 REMELEM             GET RID OF EXISTING                          
*                                                                               
         XC    ELEM(50),ELEM                                                    
         MVI   0(R6),X'10'         ELEM CODE                                    
         MVI   1(R6),50            LENGTH                                       
*                                                                               
         LA    R2,SURCPRH          CARDS/RESP                                   
         BAS   RE,VRNUM            REQUIRED                                     
         MVI   ERROR,INVALID                                                    
         OC    QSDNCRDS,FULL+2                                                  
         BZ    TRAPERR                                                          
*                                                                               
         LA    R2,SURCPCH          COLS/CARD                                    
         LA    RF,80               DEFAULT IS 80                                
         MVI   ERROPT,C'Y'         OPTIONAL                                     
         BAS   RE,VRNUM                                                         
         MVC   QSDNCOLS,FULL+2                                                  
*                                                                               
         LA    R2,SURCNCH          CARD NO. COLUMN                              
         SR    RF,RF               DEFAULT IS 0                                 
         MVI   ERROPT,C'Y'         OPTIONAL                                     
         BAS   RE,VRNUM                                                         
         MVC   QSDCDCOL,FULL+2                                                  
*                                                                               
         LA    R2,SURCNLH          CARD NO. FIELD LENGTH                        
         SR    RF,RF               DEFAULT IS 0                                 
         MVI   ERROPT,C'Y'         OPTIONAL                                     
         BAS   RE,VRNUM                                                         
         CH    R0,=H'255'          MUST GO IN ONE BYTE                          
         BNH   *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         MVC   QSDCDLEN,FULL+3                                                  
*                                 CHECK CARD NO FIELD CONSISTENCY               
*                                 MUST HAVE BOTH COL AND LEN OR NEITHER         
         MVI   ERROR,INCON                                                      
         MVI   ERROPT,0                                                         
         MVI   BYTE,0                                                           
         OC    QSDCDCOL,QSDCDCOL                                                
         BZ    *+8                                                              
         OI    BYTE,X'80'                                                       
         OC    QSDCDLEN,QSDCDLEN                                                
         BZ    *+8                                                              
         OI    BYTE,X'40'                                                       
         TM    BYTE,X'C0'          MUST BE BOTH ON OR BOTH OFF                  
         BM    TRAPERR                                                          
*                                  ALSO CHECK FITS IN CARD                      
         MVC   FULL,QSDCDCOL       COL AND LEN                                  
         LH    RF,FULL                                                          
         ZIC   RE,FULL+2                                                        
         AR    RF,RE                                                            
         STH   RF,FULL                                                          
         CLC   FULL(2),QSDNCOLS                                                 
         BH    TRAPERR                                                          
         MVI   ERROR,0                                                          
*                                                                               
         LA    R2,SURRNCH          RESP NO. COLUMN                              
         SR    RF,RF               DEFAULT IS 0                                 
         MVI   ERROPT,C'Y'         OPTIONAL                                     
         BAS   RE,VRNUM                                                         
         MVC   QSDRSCOL,FULL+2                                                  
*                                                                               
         LA    R2,SURRNLH          RESP NO. FIELD LENGTH                        
         SR    RF,RF               DEFAULT IS 0                                 
         MVI   ERROPT,C'Y'         OPTIONAL                                     
         BAS   RE,VRNUM                                                         
         CH    R0,=H'255'          MUST GO IN ONE BYTE                          
         BNH   *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         MVC   QSDRSLEN,FULL+3                                                  
*                                 CHECK RESP NO FIELD CONSISTENCY               
*                                 MUST HAVE BOTH COL AND LEN OR NEITHER         
         MVI   ERROR,INCON                                                      
         MVI   ERROPT,0                                                         
         MVI   BYTE,0                                                           
         OC    QSDRSCOL,QSDRSCOL                                                
         BZ    *+8                                                              
         OI    BYTE,X'80'                                                       
         OC    QSDRSLEN,QSDRSLEN                                                
         BZ    *+8                                                              
         OI    BYTE,X'40'                                                       
         TM    BYTE,X'C0'          MUST BE BOTH ON OR BOTH OFF                  
         BM    TRAPERR                                                          
*                                  ALSO CHECK FITS IN CARD                      
         MVC   FULL,QSDRSCOL       COL AND LEN                                  
         LH    RF,FULL                                                          
         ZIC   RE,FULL+2                                                        
         AR    RF,RE                                                            
         STH   RF,FULL                                                          
         CLC   FULL(2),QSDNCOLS                                                 
         BH    TRAPERR                                                          
         MVI   ERROR,0                                                          
*                                                                               
         LA    R2,SURWMGH          WAVES MERGED CONTROL                         
         CLI   5(R2),0             IF NO INPUT                                  
         BNE   *+12                                                             
         MVI   QSDWMRGE,C'N'       DEFAULT TO NO                                
         B     VR8B                                                             
*                                                                               
         CLI   8(R2),C'Y'          MUST BE Y                                    
         BE    VR8                                                              
         CLI   8(R2),C'N'          OR N                                         
         BNE   TRAPERR                                                          
*                                                                               
VR8      DS    0H                                                               
         MVC   QSDWMRGE,8(R2)                                                   
*                                                                               
VR8B     DS    0H                                                               
         LA    R2,SURRDMH          READERSHIP METHOD                            
         GOTO1 ANY                                                              
         CLI   WORK,C'1'           MUST BE 1                                    
         BE    VR9                                                              
         CLI   WORK,C'2'           OR 2                                         
         BE    VR9                                                              
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
VR9      DS    0H                                                               
         MVC   QSDRMETH,WORK                                                    
*                                                                               
VR10     DS    0H                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         CLI   ACTNUM,ACTADD        UNLESS ADDING                               
         BE    VR39B                                                            
         GOTO1 SURVXT              DO A SURVEY DATA EXTRACT                     
*                                                                               
VR39B    DS    0H                                                               
         B     VDREC               EDIT 'FREE' FIELDS                           
         SPACE 3                                                                
VRNUM    DS    0H                  HANDLE NUMERIC VALIDATION                    
         ST    RE,SAVRE                                                         
         ST    RF,FULL             SAVE DEFAULT                                 
         MVI   ERROR,0                                                          
         GOTO1 ANY                                                              
         MVI   ERROPT,0                                                         
         CLI   ERROR,0             ERROPT MAY HAVE BEEN SET                     
         BNE   VRNUM4              NO DATA MEANS USE DEFAULT                    
         MVC   WK,WORK             SET FOR VALNUM                               
         MVC   LWK,5(R2)                                                        
         GOTO1 VALNUM                                                           
*                                                                               
VRNUM4   DS    0H                                                               
         MVI   ERROR,0                                                          
         L     R0,FULL             RETURN VALUE IN FULL AND R0                  
         L     RE,SAVRE                                                         
         BR    RE                                                               
         EJECT                                                                  
*        DISPLAY RECORD                                                         
*        --------------                                                         
         SPACE 1                                                                
DREC     DS    0H                                                               
*                                  DEFINITION ELEMENT                           
*                                  ------------------                           
         MVI   ELCODE,X'10'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   DR30                                                             
*                                                                               
         USING QSDEFEL,R6                                                       
*                                                                               
         LA    R2,SURCNLH          CARD NO. FIELD LENGTH                        
         XC    FULL,FULL                                                        
         MVC   FULL+3(1),QSDCDLEN                                               
         BAS   RE,NUMDISP                                                       
*                                                                               
         LA    R2,SURRNLH          RESP NO. FIELD LENGTH                        
         MVC   FULL+3(1),QSDRSLEN                                               
         BAS   RE,NUMDISP                                                       
*                                                                               
         LA    R2,SURCPRH          CARDS/RESP                                   
         MVC   FULL+2(2),QSDNCRDS                                               
         BAS   RE,NUMDISP                                                       
*                                                                               
         LA    R2,SURCPCH          COLUMNS/CARD                                 
         MVC   FULL+2(2),QSDNCOLS                                               
         BAS   RE,NUMDISP                                                       
*                                                                               
         LA    R2,SURCNCH          CARD NO. COLUMN                              
         MVC   FULL+2(2),QSDCDCOL                                               
         BAS   RE,NUMDISP                                                       
*                                                                               
         LA    R2,SURRNCH          RESP NO. COLUMN                              
         MVC   FULL+2(2),QSDRSCOL                                               
         BAS   RE,NUMDISP                                                       
*                                                                               
         LA    R2,SURWMGH          WAVE MERGE CONTROL                           
         MVC   WORK(L'QSDWMRGE),QSDWMRGE                                        
         BAS   RE,GENDISP                                                       
*                                                                               
         LA    R2,SURRDMH          READERHSIP METHOD                            
         MVC   WORK(L'QSDRMETH),QSDRMETH                                        
         BAS   RE,GENDISP                                                       
*                                                                               
DR30     DS    0H                                                               
         B     VDREC               DISPLY 'FREE' FIELDS                         
         EJECT                                                                  
*        EDIT/DISPLAY 'FREE' FIELDS                                             
*        --------------------------                                             
         SPACE 2                                                                
VDREC    DS    0H                                                               
         LA    R2,SURTTLH          TITLE                                        
         MVI   ELCODE,X'C1'                                                     
         MVI   FREEMAX,1           1 LINE                                       
         MVI   FREEKEY,0           NO 'KEY'                                     
         MVI   FREEKLN,1           KEY LEN                                      
         MVI   FREECTL,X'80'       REQUIRED                                     
         GOTO1 VALIFREE                                                         
*                                                                               
         LA    R2,SURFT1H          FOOTNOTES                                    
         MVI   ELCODE,X'C4'                                                     
         MVI   FREEMAX,2           2 LINE                                       
         MVI   FREECTL,0           NOT REQUIRED                                 
         GOTO1 VALIFREE                                                         
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*        LIST RECORDS                                                           
*        ------------                                                           
         SPACE 3                                                                
LIST     LA    R4,KEY                                                           
         OC    KEY,KEY             WAS A KEY PROVIDED?                          
         BNZ   LIST4               YES- CONTINUE WITH THIS KEY                  
         MVC   KEY,SVQSDKEY        NO- SET KEY                                  
         SPACE 1                                                                
LIST1    GOTO1 HIGH                                                             
         B     LIST4                                                            
         SPACE 1                                                                
LIST2    GOTO1 SEQ                                                              
         SPACE 1                                                                
LIST4    CLC   KEY(4),KEYSAVE      CHECKMAIN C/B                                
         BNE   XIT                                                              
         LA    RF,KEY+QSPKNOD-QSPREC  NODE MUST BE ZERO                         
         OC    0(4,RF),0(RF)                                                    
         BZ    LIST5                                                            
         MVC   0(4,RF),=4X'FF'     BUMP TO NEXT SURVEY                          
         B     LIST1                                                            
*                                                                               
LIST5    DS    0H                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVC   SULHED(L'LISTHD),LISTHD   SET HEADLINE                           
         OI    SULHEDH+6,X'80'                                                  
*                                                                               
         MVC   LISTAR,SPACES                                                    
         LA    R5,LISTAR                                                        
         USING LSTLIND,R5                                                       
*                                                                               
         MVC   LSURV,QSDKFCD                                                    
*                                                                               
LIST6    DS    0H                  TITLE                                        
         MVI   FREECTL,X'40'       RETURN FIELD IN WORK                         
         MVI   FREEMAX,1                                                        
         MVI   FREEKEY,0                                                        
         MVI   FREEKLN,1                                                        
         MVI   FREEFRST,C'Y'                                                    
         MVI   ELCODE,X'C1'                                                     
         GOTO1 DISPFREE                                                         
         MVC   LTITL,WORK                                                       
*                                                                               
         GOTO1 LISTMON                                                          
         B     LIST2                                                            
         SPACE 2                                                                
LISTHD   DC    C'SURVEY    TITLE'                                               
         EJECT                                                                  
*        VARIOUS SHORT ROUTINES                                                 
*        ----------------------                                                 
*                                                                               
GENDISP  ZIC   R1,0(R2)            GENERAL DISPLAY                              
         SH    R1,=H'9'                                                         
         EX    R1,GDCLC            TEST ALREADY ON SCREEN                       
         BER   RE                  YES- RETURN (CC=)                            
         EX    R1,GDMVC            NO- PUT IT THERE                             
         OI    6(R2),X'80'         TRANSMIT                                     
         BR    RE                  RETURN (CC NOT=)                             
*                                                                               
GDCLC    CLC   8(0,R2),WORK                                                     
GDMVC    MVC   8(0,R2),WORK                                                     
*                                                                               
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT SCREEN FIELD                    
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETURN =                                
         BR    RE                                                               
         SPACE 2                                                                
BUMPU    ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED FIELD               
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETRUN =                                
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BNZ   BUMPU                                                            
         LTR   RE,RE               NOT EOS- RETURN NOT =                        
         BR    RE                                                               
         SPACE 1                                                                
*                                                                               
NUMDISP  DS    0H                  DISPLAY A SIMPLE NUMERIC FIELD               
         EDIT  (B4,FULL),(10,WORK),ALIGN=LEFT                                   
         B     GENDISP                                                          
*                                                                               
BADX     LTR   RB,RB                                                            
         B     XIT                 CC OF NEQ = ERROR EXIT                       
*                                                                               
GOODX    CR    RB,RB                                                            
         B     XIT                 CC OF EQ = GOOD EXIT                         
*                                                                               
XIT      XIT1                                                                   
         SPACE 2                                                                
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         SPACE 1                                                                
TRAPERR  GOTO1 ERREX                                                            
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE ROUTINES                                                
         SPACE 3                                                                
HOOK     NTR1                                                                   
         B     XIT                                                              
         SPACE 2                                                                
HEDSPECS DS    0H                                                               
         DC    X'00'               END MARKER FOR SPECS                         
         SPACE 1                                                                
RELO     DS    A                                                                
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDNODBLKD                                                      
       ++INCLUDE MPGENQS                                                        
       ++INCLUDE MPRDRFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE MPRDRF2D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE MPRDRE2D                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE MPRDRWORKD                                                     
         SPACE 3                                                                
LSTLIND  DSECT                     DSECT FOR LIST DISPLAY LINE                  
LSTLIN   DS    0CL(L'LISTAR)                                                    
LSURV    DS    CL6                                                              
         DS    CL3                                                              
LTITL    DS    CL40                                                             
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008MPRDR02   05/01/02'                                      
         END                                                                    
