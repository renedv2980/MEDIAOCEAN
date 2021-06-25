*          DATA SET MPRDR0D    AT LEVEL 062 AS OF 05/01/02                      
*PHASE T5100D,*                                                                 
         TITLE 'T5100D - QSPEC LIBRARY ATTACHMENT'                              
T5100D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T5100D                                                         
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R7,VNODBLK          R7 USED THROUGHOUT FOR NODBLK                
         USING NODBLKD,R7                                                       
*                                                                               
         GOTO1 SETADDR             SET 'FLOATING' ADDRESSES                     
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         B     XIT                                                              
         EJECT                                                                  
*        VALIDATE KEY                                                           
         SPACE 2                                                                
         USING QSPKEY,R4                                                        
         SPACE 2                                                                
VKEY     DS    0H                                                               
         MVI   VDSW,C'V'           SET VAL/DISP SW TO VALIDATE                  
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         LA    R2,QSASURH                                                       
         TM    4(R2),X'20'         TEST VALIDATED                               
         BNZ   *+12                                                             
         NI    QSAQSPH+4,X'DF'     UNVALIDATE QSPEC                             
         NI    QSALIBH+4,X'DF'     AND LIBRARY                                  
         GOTO1 VALSURV                                                          
*                                                                               
         LA    R2,QSAQSPH          VALIDATE QSPEC                               
         TM    4(R2),X'20'         TEST VALIDATED                               
         BNZ   *+8                                                              
         MVI   VDSW,C'D'           SET TO DISPLAY                               
         GOTO1 ANY                                                              
*                                                                               
         CLI   WORK,C'$'           CALLER CANNOT ITSELF BE A LIBRARY            
         BNE   *+12                                                             
         MVI   ERROR,LIBUSERR                                                   
         B     TRAPERR                                                          
*                                                                               
         GOTO1 VALQSP                                                           
         MVC   SVQSPEC,WORK                                                     
         XC    SVATTCOD,SVATTCOD   SAVE ATTACH CODE                             
         CLI   NDATTLEV,0          IF ANY                                       
         BE    *+10                                                             
         MVC   SVATTCOD,NDATTCOD                                                
         MVC   SVSQTYP,SQTYP       SAVE QSPEC TYPE                              
*                                                                               
         L     R6,NDLEVPTR         LEVEL POINTER                                
         USING NDLVTABD,R6                                                      
         MVC   SVQSPDA,NDLVDA      SAVE DISKADDR                                
         MVC   SVQSPKEY,NDLVKEY    SAVE KEY                                     
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*        VALIDATE 'RECORD'                                                      
         SPACE 2                                                                
VREC     DS    0H                                                               
         LA    R2,QSALIBH          LIBRARY HEADER                               
         TM    4(R2),X'20'         TEST VALIDATED                               
         BZ    VR1M                NO                                           
         MVI   NEEDLIB,C'N'        YES-SET NOT TO RE-READ WHOLE LIB             
         CLI   VDSW,C'D'           BUT STILL MAY HAVE TO DISPLAY                
         BE    VR7                 DISLAY SUBS INPUT                            
         B     VR8                 VALIDATE SUBS INPUT                          
*                                                                               
VR1M     DS    0H                  ***VALIDATE LIB HEADER**                     
         MVI   VDSW,C'D'                                                        
         CLI   5(R2),0             IF NO INPUT                                  
         BNE   VR2                                                              
         CLI   SVATTCOD,C' '       TEST HAVE CURRENT ATTACH                     
         BNH   VR2                 NO                                           
         MVC   WORK(L'QSALIB),SVATTCOD    SET CURRENT ATTACHMENT                
         BAS   RE,GENDISP                                                       
         MVI   5(R2),9                                                          
*                                                                               
VR2      DS    0H                                                               
         GOTO1 ANY                                                              
         LA    RF,WORK+12          BLANKS TO NULLS                              
         CLI   0(RF),C' '                                                       
         BH    *+12                                                             
         MVI   0(RF),0                                                          
         BCT   RF,*-12                                                          
*                                                                               
         MVC   SVLIBCOD,WORK       SAVE LIBRARY CODE                            
         CLC   =C'DELETE',WORK      DELETE MEANS REMOVE ATTACH                  
         BNE   VR4                                                              
         MVI   ELCODE,X'BC'        REMOVE EXCLUSION ELEMS                       
         GOTO1 REMELEM                                                          
         LA    R2,QSAEXCFH         CLEAR EXCLUSION FIELDS                       
         GOTO1 CLRSCR,DMCB,(R2),6                                               
         LA    R2,QSAPARFH                                                      
         BAS   RE,SPDELEL          REMOVE PARM ELEMS                            
         BAS   RE,CLEAR            AND CLEAR SCREEN                             
         MVI   SVSETACD,X'FF'                                                   
         B     VR12                                                             
*                                                                               
VR4      DS    0H                                                               
         CLI   WORK,C'$'           MUST BE LIBRARY                              
         BNE   VR92                                                             
*                                                                               
         MVC   NDIOA,AIO2          READ LIB INTO AIO2                           
         GOTO1 VALQSP              NB- VALQSP SET FIELD VALIDATED               
         CLI   NDLEV,1             MUST BE 1 LEVEL                              
         BNE   VR92                                                             
         MVI   NEEDLIB,C'Y'        HAVE SETPARL READ WHOLE LIB                  
         CLC   SQTYP,SVSQTYP       QSPEC AND LIB HEADER MUST AGREE              
         BNE   VR93                                                             
*                                                                               
         L     R6,NDLEVPTR         LEVEL POINTER                                
         USING NDLVTABD,R6                                                      
         MVC   SVSETAND,NDLVNOD    SET ATTACH NODE                              
         MVC   SVSETACD,NDLVCOD    AND CODE                                     
*                                                                               
VR7      DS    0H                  **DISPLAY SUBS AND EXCLUSIONS**              
         CLI   SVATTCOD,C' '       IF CURRENT ATTACHMENT                        
         BNH   VR7D                                                             
         CLC   SVATTCOD,SVLIBCOD   MUST BE EQUAL                                
         BNE   VR91                                                             
*                                                                               
VR7D     DS    0H                                                               
         MVI   MODE,DISPREC                                                     
         BAS   RE,SETPARL          SET LIST OF PARAMS                           
         BAS   RE,VDEXCL           DISPLAY EXCLUSIONS                           
         MVC   NDIOA,AIO1          RESTORE TO AIO1                              
*                                                                               
         MVI   MSGNUM,CADMSG       CURRENT ATTACHMENT DISPLAYED                 
         GOTO1 MSGSET                                                           
         LA    R2,QSAVALFH         SET CURSOR                                   
         B     VR40                                                             
*                                                                               
VR8      DS    0H                  ***VALIDATE EXCLUSIONS AND SUBS***           
         BAS   RE,VDEXCL                                                        
         BAS   RE,SETPELM                                                       
*                                                                               
VR12     DS    0H                  RE-WRITE QSPEC RECORD                        
         XC    NDHOOK,NDHOOK       NO HOOK                                      
         MVC   NDSETACD,SVSETACD                                                
         MVC   NDSETAND,SVSETAND                                                
         CLI   SVSETACD,C' '                                                    
         BH    *+6                                                              
         DC    H'0'                STTACH CODE MUST BE THERE                    
         GOTO1 NODIO,DMCB,VNODBLK,=C'PUT',SVQSPEC,0,0                           
*                                                                               
         XC    NDSETAND,NDSETAND   CLEAR ATTACH NODE                            
         XC    NDSETACD,NDSETACD   AND CODE                                     
         LA    R2,QSAQSPH          CURSOR TO QSPEC FIELD                        
         CLI   NDERR,0                                                          
         BE    VR40                                                             
         GOTO1 NODERRP             HANDLE NODIO ERROR                           
*                                                                               
VR40     DS    0H                                                               
         MVI   VDSW,C'V'                                                        
         ST    R2,CURSFLD                                                       
         B     XIT                                                              
*                                                                               
VR91     MVI   ERROR,ATCHGERR      CANNOT CHANGE ATTACH CODE                    
         B     TRAPERR                                                          
VR92     MVI   ERROR,LIBNGERR      BAD LIBRARY CODE                             
         NI    QSALIBH+4,X'DF'     UNVALIDATE LIBRARY                           
         B     TRAPERR                                                          
VR93     MVI   ERROR,QSTYPERR      INVALID QSPEC TYPE                           
         NI    QSALIBH+4,X'DF'     UNVALIDATE LIBRARY                           
         B     TRAPERR                                                          
         EJECT                                                                  
*        SETPARL - SET LIST OF PARAMATERS/VALUES                                
         SPACE 2                                                                
SETPARL  NTR1                                                                   
         MVI   SVERR,0                                                          
         LA    R3,PARLST                                                        
         USING PARLSTD,R3                                                       
*                                                                               
         CLI   NEEDLIB,C'Y'        TEST TO READ WHOLE LIB                       
         BNE   SPR8                                                             
*                                                                               
         MVI   0(R3),X'FF'         SET EOL                                      
         SR    R4,R4               FOR PARAM COUNT                              
*                                  NB- USING AIO2                               
         LA    RF,LIBHOOK                                                       
         ST    RF,NDHOOK                                                        
         GOTO1 NODIO,DMCB,VNODBLK,=C'SEQ',SQSPEC                                
*                                                                               
         CLI   NDERR,0                                                          
         BNE   NODERRP                                                          
*                                                                               
SPR8     DS    0H              SET VALUES FROM QSPEC REC IN PARLST              
         LA    R3,PARLST           FIRST CLEAR EXISTING VALS                    
SPR8B    DS    0H                                                               
         CLI   0(R3),X'FF'         EOL                                          
         BE    SPR8D                                                            
         MVC   PARVAL,SPACES                                                    
         MVI   PARERR,0                                                         
         LA    R3,PARLSTL(R3)                                                   
         B     SPR8B                                                            
*                                                                               
SPR8D    DS    0H                                                               
         L     R6,AIO1             REC IS IN AIO1                               
         CLC   0(L'QSDKEY,R6),SVQSPKEY   TEST HAVE QSPEC RECORD                 
         BE    SPR9                                                             
         LA    RF,KEY              NO- RE-READ                                  
         AH    RF,LKEY                                                          
         AH    RF,LSTATUS                                                       
         MVC   0(4,RF),SVQSPDA                                                  
         GOTO1 GETREC                                                           
*                                                                               
SPR9     DS    0H                                                               
         AH    R6,DATADISP                                                      
         MVI   ELCODE,X'BA'        PARAM ID ELEM                                
*                                                                               
         BAS   RE,FIRSTEL                                                       
         B     *+8                                                              
*                                                                               
SPR10    DS    0H                                                               
         BAS   RE,NEXTEL                                                        
         BNE   SPR16                                                            
         MVC   WORK,SPACES                                                      
         ZIC   RE,1(R6)                                                         
         SH    RE,=H'4'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),3(R6)                                                    
*                                  FIND PARAM IN PARLST                         
         LA    R3,PARLST                                                        
*                                                                               
SPR11    DS    0H                                                               
         CLI   0(R3),X'FF'         EOL                                          
         BE    SPR10               IF NOT IN LIBRARY, IGNORE                    
         CLC   PARPAR,WORK                                                      
         BE    SPR12                                                            
         LA    R3,PARLSTL(R3)                                                   
         B     SPR11                                                            
*                                                                               
SPR12    DS    0H                                                               
         LA    R4,4(RE,R6)         POINT TO NEXT ELEM                           
         CLI   0(R4),X'BB'         MUST BE A REPLACEMENT ELEM                   
         BE    *+6                                                              
         DC    H'0'                MISSING REPLACEMENT ELEM                     
*                                                                               
         MVC   PARVAL,SPACES                                                    
         ZIC   RE,1(R4)                                                         
         SH    RE,=H'4'                                                         
         BM    SPR10               NO REPLACEMENT, LEAVE BLANK                  
         CH    RE,=Y(L'PARVAL-1)   CHECK VS MAX LENGTH                          
         BNH   *+8                                                              
         LA    RE,L'PARVAL-1                                                    
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PARVAL(0),3(R4)                                                  
*                                  SET LIST ON SCREEN                           
         B     SPR10                                                            
*                                  SET PARLST TO SCREEN                         
SPR16    DS    0H                                                               
         LA    R2,QSAPARFH         FIRST PARAMETER                              
         GOTO1 CLRSCR,DMCB,(R2),0  CLEAR UNPROT FLDS                            
         LA    R3,PARLST                                                        
*                                                                               
SPR21    DS    0H                                                               
         CLI   0(R3),X'FF'                                                      
         BE    SPR22                                                            
*                                                                               
         MVC   8+4(L'PARPAR,R2),PARPAR                                          
         MVI   8+4+L'PARPAR(R2),C'='                                            
         OI    6(R2),X'80'                                                      
*                                                                               
         BAS   RE,BUMP             SUBSTITUTION FIELD                           
         MVC   WORK(L'PARVAL),PARVAL                                            
         BAS   RE,GENDISP                                                       
*                                                                               
         CLI   PARERR,0            IF ANY ERROR                                 
         BE    *+8                                                              
         ST    R2,CURSFLD          SET CURSOR AT VALUE FIELD                    
*                                                                               
         BAS   RE,BUMP                                                          
         LA    R3,PARLSTL(R3)                                                   
         B     SPR21                                                            
*                                                                               
SPR22    DS    0H                  CLEAR REST OF SCREEN                         
         BAS   RE,CLEAR                                                         
*                                                                               
SPR23    DS    0H                                                               
         CLI   SVERR,0                                                          
         BE    *+14                                                             
         MVC   ERROR,SVERR         SET ANY SAVED ERROR                          
         B     TRAPERR                                                          
*                                                                               
         B     XIT                                                              
         SPACE 2                                                                
LIBHOOK  NTR1                                                                   
         CLI   NDERR,0                                                          
         BNE   NODERRP                                                          
         CLI   NDMODE,NDPROC                                                    
         BNE   LIBHX                                                            
*                                                                               
         L     R6,NDIOA                                                         
         AH    R6,DATADISP                                                      
         B     LIBH3B                                                           
*                                                                               
LIBH3    DS    0H                                                               
         ZIC   R0,1(R6)            NEXT ELEM                                    
         AR    R6,R0                                                            
*                                                                               
LIBH3B   DS    0H                                                               
         CLI   0(R6),0             EOR                                          
         BE    LIBH10                                                           
*                                                                               
         CLI   0(R6),X'B0'         SKIP NODIO ELEMS                             
         BL    LIBH4                                                            
         CLI   0(R6),X'BF'                                                      
         BNH   LIBH3                                                            
*                                                                               
LIBH4    DS    0H                                                               
         ZIC   R0,1(R6)                                                         
         AR    R0,R6                                                            
         ST    R0,EOELEM           SET END OF ELEM                              
         SH    R0,=H'2'                                                         
         ST    R0,EOELM2           END OF ELEM MINUS 2                          
*                                                                               
         LA    RF,2(R6)                                                         
*                                                                               
LIBH4B   DS    0H                                                               
         C     RF,EOELM2                                                        
         BNL   LIBH3               NEXT ELEM                                    
         CLC   0(2,RF),=C'&&&&'                                                 
         BE    LIBH5                                                            
         LA    RF,1(RF)                                                         
         B     LIBH4B              NEXT ELEM                                    
*                                                                               
LIBH5    DS    0H                                                               
         LA    RE,2(RF)            START OF PARAMETER                           
         LA    R0,3(RF)                                                         
*                                                                               
LIBH5D   DS    0H                                                               
         C     RE,EOELEM           STOP AT END OF ELEM                          
         BNL   LIBH5F                                                           
         CLC   0(2,RE),=C'&&&&'     OR MORE AMPERSANDS                          
         BE    LIBH5F                                                           
         CLI   0(RE),C' '          OR NULL OR SPACE                             
         BNH   LIBH5F                                                           
         LA    RE,1(RE)                                                         
         B     LIBH5D                                                           
*                                                                               
LIBH5F   DS    0H                                                               
         MVC   WORK(12),SPACES                                                  
         SR    RE,R0               SET LENGTH -1 OF PARAM                       
         BM    LIBH7                                                            
         CH    RE,=Y(L'PARPAR-1)   TEST VS MAX PARAM SIZE                       
         BNH   *+8                                                              
         MVI   SVERR,PARLERR       PARAMETER LENGTH ERROR                       
         EX    RE,*+8              SET PARAM IN WORK                            
         B     *+10                                                             
         MVC   WORK(0),2(RF)                                                    
*                                  SET IN PARAM LIST                            
         LA    R3,PARLST                                                        
         USING PARLSTD,R3                                                       
*                                                                               
LIBH6    DS    0H                                                               
         CLI   0(R3),X'FF'         EOLIST                                       
         BE    LIBH6F                                                           
         CLC   PARPAR,WORK                                                      
         BE    LIBH6D                                                           
         LA    R3,PARLSTL(R3)                                                   
         B     LIBH6                                                            
*                                                                               
LIBH6D   DS    0H                                                               
         B     LIBH7                                                            
*                                                                               
LIBH6F   DS    0H                                                               
         LA    R0,PARLST                                                        
         AH    R0,=Y(PARLSTX-PARLST)                                            
         CR    R3,R0                                                            
         BL    *+12                                                             
         MVI   SVERR,PARXERR       TOO MANY PARAMS                              
         B     LIBHX                                                            
         MVC   PARERR,SVERR                                                     
         MVC   PARPAR,WORK                                                      
         MVC   PARVAL,SPACES                                                    
         MVI   PARLSTL(R3),X'FF'   SET NEW END                                  
*                                                                               
LIBH7    DS    0H                                                               
         LA    RF,2(RF,RE)                                                      
         B     LIBH4B              TRY FOR ANOTHER WITHIN THIS ELEM             
*                                                                               
LIBH10   DS    0H                                                               
LIBHX    DS    0H                                                               
         B     XIT                 RETURN TO NODIO                              
         SPACE 2                                                                
CLEAR    NTR1                      CLEAR PARAMS                                 
         GOTO1 CLRSCR,DMCB,(R2),0  USE SCRCLR TO CLEAR UNPROTECTED              
         LA    R3,QSAPARLH         LAST LINE                                    
*                                                                               
CLR4     DS    0H                                                               
         CR    R2,R3                                                            
         BH    CLR5                                                             
         MVC   WORK(L'QSAPARF),8(R2)                                            
         MVC   WORK+4(14),SPACES   LEAVE NUMBER                                 
         BAS   RE,GENDISP                                                       
*                                                                               
         BAS   RE,BUMP                                                          
         BAS   RE,BUMP                                                          
         B     CLR4                                                             
*                                                                               
CLR5     DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*        SETPELM - SET PARAM ID AND SUBSTITUTION ELEMS IN CALLING REC           
         SPACE 2                                                                
SETPELM  NTR1                                                                   
         BAS   RE,SPDELEL       FIRST DELETE ANY EXISTING PARM ELEMS            
*                                                                               
SPE4     DS    0H                  GET VALUES FROM SCREEN                       
         LA    R3,PARLST           NB- ONE T0 ONE RELATION BETWEEN              
         LA    R2,QSAVALFH         SCREEN AND PARLST                            
*                                                                               
SPE4D    DS    0H                                                               
         CLI   0(R3),X'FF'         EOL                                          
         BE    SPE5                                                             
*                                                                               
         MVC   PARVAL,SPACES                                                    
         CLI   5(R2),0             ANY INPUT                                    
         BE    SPE4F                                                            
         ZIC   RE,5(R2)                                                         
         BCTR  RE,R0                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PARVAL(0),8(R2)                                                  
*                                                                               
SPE4F    DS    0H                                                               
         BAS   RE,BUMPU                                                         
         LA    R3,PARLSTL(R3)                                                   
         B     SPE4D                                                            
*                                                                               
SPE5     DS    0H                  SET NEW ELEMS                                
         LA    R3,PARLST                                                        
         USING PARLSTD,R3                                                       
         LA    R4,1                SEQ NO.                                      
*                                                                               
SPE6     DS    0H                                                               
         CLI   0(R3),X'FF'         END OF LIST                                  
         BE    SPE10                                                            
*                                                                               
         LA    RF,PARPAR+L'PARPAR-1   CALC LENGTH OF PARAM ID                   
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    R0,PARPAR                                                        
         SR    RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK+3(0),PARPAR                                                 
         MVI   WORK,X'BA'          PARAM ID ELEM                                
         LA    RF,4(RF)                                                         
         STC   RF,WORK+1           ELEM LENGTH                                  
         STC   R4,WORK+2           SEQ NO.                                      
         BAS   RE,SPADDEL                                                       
*                                                                               
         LA    RF,3                                                             
         CLI   PARVAL,C' '         NO REPLACEMENT                               
         BNH   SPE7                                                             
         LA    RF,PARVAL+L'PARVAL-1   CALC LENGTH OF REPL VALUE                 
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    R0,PARVAL                                                        
         SR    RF,R0                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK+3(0),PARVAL                                                 
         LA    RF,4(RF)                                                         
*                                                                               
SPE7     DS    0H                                                               
         MVI   WORK,X'BB'          PARAM SUB ELEM                               
         STC   RF,WORK+1           ELEM LENGTH                                  
         STC   R4,WORK+2           SEQ NO.                                      
         BAS   RE,SPADDEL                                                       
*                                                                               
         LA    R3,PARLSTL(R3)                                                   
         LA    R4,1(R4)            SEQ NO.                                      
         B     SPE6                                                             
*                                                                               
SPE10    DS    0H                                                               
         B     XIT                                                              
         SPACE 2                                                                
SPADDEL  DS    0H                                                               
         LR    R0,RE                                                            
         L     R6,AIO1             ADD AT END OF REC                            
         AH    R6,LKEY                                                          
         LH    R6,0(R6)                                                         
         A     R6,AIO1                                                          
         GOTO1 RECUP,DMCB,(C'L',AIO1),WORK,(C'R',(R6))                          
         CLI   DMCB+8,C'R'         TEST TOO BIG                                 
         BE    *+12                                                             
         MVI   ERROR,TOOLONG                                                    
         B     TRAPERR                                                          
*                                                                               
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
SPDELEL  DS    0H                                                               
         LR    R0,RE                                                            
         L     R6,AIO1          FIRST DELETE ANY EXISTING PARM ELEMS            
         AH    R6,DATADISP                                                      
*                                                                               
SPD2     DS    0H                                                               
         CLI   0(R6),0             EOR                                          
         BE    SPD4                                                             
         CLI   0(R6),X'BA'         BA                                           
         BL    SPD3                                                             
         CLI   0(R6),X'BB'         THRU BC                                      
         BNH   SPD3D                                                            
*                                                                               
SPD3     DS    0H                                                               
         ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     SPD2                                                             
*                                                                               
SPD3D    DS    0H                                                               
         GOTO1 RECUP,DMCB,(C'L',AIO1),(R6)                                      
         B     SPD2                NEXT ELEM                                    
*                                                                               
SPD4     DS    0H                                                               
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*        VALIDATE/DISPLAY EXCLUSIONS                                            
         SPACE 2                                                                
VDEXCL   DS    0H                                                               
         LR    R0,RE                                                            
         LA    R2,QSAEXCFH         FIRST EXCLUSION                              
         MVI   ELCODE,X'BC'                                                     
         MVI   FREEMAX,6           6 MAX                                        
         MVI   FREEKEY,0           NO 'KEY'                                     
         MVI   FREEKLN,0           KEY LEN                                      
         MVI   FREECTL,0           NOT REQUIRED                                 
         GOTO1 VALIFREE                                                         
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
         EJECT                                                                  
*              VARIOUS SHORT ROUTINES                                           
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
         SPACE 2                                                                
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             EOS- RETURN =                                
         BR    RE                                                               
*                                                                               
BUMPU    ZIC   R0,0(R2)            BUMP TO NEXT UNPROTECTED FIELD               
         AR    R2,R0                                                            
         CLI   0(R2),0             EOS- RETURN =                                
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BNZ   BUMPU                                                            
         LTR   RE,RE               NOT EOS- RETURN NOT =                        
         BR    RE                                                               
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
TRAPERR  GOTO1 ERREX                                                            
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE MPGENQS                                                        
       ++INCLUDE MPRDRFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE MPRDREDD                                                       
         PRINT OFF                                                              
       ++INCLUDE DDNODBLKD                                                      
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE MPRDRWORKD                                                     
         PRINT ON                                                               
         SPACE 2                                                                
PARLEN   EQU   L'QSAPARF+L'QSAVALF+16       LENGTH OF ONE 'LINE'                
PARMAX   EQU   1+(QSAPARL-QSAPARF)/PARLEN   MAX LINES                           
         SPACE 2                                                                
PARLSTD  DSECT                     DSECT FOR PARAM/VALUE LIST                   
PARPAR   DS    CL12                                                             
PARVAL   DS    CL30                                                             
PARERR   DS    XL1                                                              
PARLSTL  EQU   *-PARLSTD                                                        
         SPACE 2                                                                
SYSD     DSECT                     RETURN TO SYSD DSECT                         
         ORG   SYSUSAV             USER SAVE AREA                               
VDSW     DS    C                                                                
SVSQTYP  DS    C                                                                
NEEDLIB  DS    C                                                                
SVATTCOD DS    CL12                                                             
SVLIBCOD DS    CL12                                                             
SVSETACD DS    CL12                                                             
SVSETAND DS    XL4                                                              
SVQSPDA  DS    XL4                                                              
SVQSPEC  DS    CL30                                                             
PARLST   DS    XL(PARMAX*PARLSTL)                                               
PARLSTX  EQU   *                                                                
         DS    XL1                                                              
         SPACE 3                                                                
         ORG   SYSUNON             NON-SAVED AREA                               
EOELEM   DS    A                                                                
EOELM2   DS    A                                                                
SVERR    DS    XL1                                                              
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'062MPRDR0D   05/01/02'                                      
         END                                                                    
