*          DATA SET SPSFM0C    AT LEVEL 017 AS OF 05/01/02                      
*PHASE T2170CA                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE        T2170C - SIR PERIOD RECORD MAINTENANCE                *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T21700 (SFM CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, DISPLAY, DELETE, RESTORE, CHANGE        *         
*                                                                     *         
*  INPUTS       SCREEN T217BC (MAINTENANCE)                           *         
*                                                                     *         
*  OUTPUTS      UPDATED PERIOD DEFINITION RECORDS                     *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- WORK                                            *         
*               R4 -- WORK                                            *         
*               R5 -- WORK                                            *         
*               R6 -- WORK                                            *         
*               R7 -- WORK                                            *         
*               R8 -- FREE                                            *         
*               R9 -- SYSD                                            *         
*               RA -- TWA                                             *         
*               RB -- FIRST BASE                                      *         
*               RC -- GEND                                            *         
*               RD -- SYSTEM                                          *         
*               RE -- SYSTEM                                          *         
*               RF -- SYSTEM                                          *         
*                                                                     *         
*  I/O AREAS    IO1 - PERIOD RECORD                                   *         
*                                                                     *         
***********************************************************************         
         TITLE 'T2170C - SIR PERIOD DEFINITION RECORDS'                         
T2170C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T2170C                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,XRECPUT                                                     
         BE    DR                                                               
         CLI   MODE,XRECADD                                                     
         BE    DR                                                               
*                                                                               
         CLI   MODE,RECDEL         DELETE NOT ALLOWED                           
         BNE   *+16                                                             
         LA    R2,CONACTH                                                       
         MVI   ERROR,INVACT                                                     
         B     TRAPERR                                                          
*                                                                               
         CLI   MODE,RECREST        RESTORE NOT ALLOWED EITHER                   
         BNE   *+16                                                             
         LA    R2,CONACTH                                                       
         MVI   ERROR,INVACT                                                     
         B     TRAPERR                                                          
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       LA    R5,SVKEY                                                         
         USING SIRKEY,R5                                                        
         XC    SVKEY,SVKEY                                                      
         MVI   SIRKTYPE,SIRKTYPQ   SIR RECORD TYPE                              
*                                                                               
         LA    R2,=X'0900000000010000E3'                                        
         GOTO1 VALIMED             FAKE THE MEDIA FIELD TO 'T'                  
         MVC   SIRKAM,BAGYMD                                                    
*                                                                               
         LA    R2,SIRYRH           YEAR                                         
         XC    WORK,WORK                                                        
         GOTO1 PERVAL,DMCB,(SIRYRH+5,SIRYR),('PVINSGLO',WORK)                   
*                                                                               
         CLI   DMCB+4,PVRCOK       YEAR WAS OK?                                 
         BE    *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     TRAPERR             NO                                           
*                                                                               
         LA    RF,WORK             PERVAL OUTPUT AREA                           
         MVC   SIRKYEAR,PVALBSTA-PERVALD(RF)  BINARY YEAR                       
         MVC   YEAR,SIRKYEAR                                                    
         XI    SIRKYEAR,X'FF'      YEAR IS IN ONE'S COMPLEMENT                  
*                                                                               
         LA    R2,SIRSCHH          SCHEME                                       
         CLI   SIRSCHH+5,0                                                      
         BNE   VK10                SCHEME WAS GIVEN                             
*                                                                               
         XC    WORK,WORK           READ SID PROFILE FOR AGENCY/MEDIA            
         MVC   WORK+16(4),=C'S0SI'                                              
         MVC   WORK+20(2),AGENCY                                                
         MVC   WORK+22(1),QMED                                                  
         GOTO1 GETPROF,DMCB,WORK+16,WORK,DATAMGR                                
         OC    WORK(16),WORK       TEST PROFILE FOUND                           
         BZ    VK5                 NO -- NO SCHEME IS OK                        
         CLI   WORK+2,C'N'         DEFAULT ALLOWED?                             
         BNE   VK5                 YES                                          
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VK5      MVC   SIRSCH(3),=C'ALL'                                                
         OI    SIRSCHH+6,X'80'     XMIT                                         
         B     VK20                                                             
*                                                                               
VK10     CLC   =C'ALL',SIRSCH      TEST SCHEME FOR ENTIRE AGENCY                
         BE    VK20                                                             
         OC    SIRSCH,=C'   '                                                   
         GOTO1 CLPACK,DMCB,SIRSCH,SIRKCODE                                      
         CLI   DMCB,0              TEST VALID SCHEME CODE                       
         BE    VK20                YES                                          
         MVI   ERROR,INVSCH                                                     
         B     TRAPERR                                                          
*                                                                               
VK20     LA    R5,KEY              CREATE THE SCHEME KEY                        
         MVC   KEY,SVKEY                                                        
         MVI   SIRKYEAR,0          *** CAN NEVER HAVE YEAR 2000 ***             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST SCHEME EXISTS                           
         BE    *+12                                                             
         MVI   ERROR,NOSCHM                                                     
         B     TRAPERR                                                          
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,EPNCODEQ     PERIOD NAMES ELEMENT                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0                                                            
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'2'                                                         
         D     R0,=F'5'            R1 = NO. OF PERIODS                          
         STH   R1,NUMPERS                                                       
         LA    R2,SIRDPERH         FIRST PERIOD NAME FIELD                      
         LA    R6,2(R6)            FIRST PERIOD NUMBER                          
*                                                                               
VK30     MVC   8(4,R2),1(R6)       PUT PERIOD NAME ON SCREEN                    
         OI    6(R2),X'80'         XMIT                                         
         ZIC   R0,0(R2)            BUMP TO DATES FIELD                          
         AR    R2,R0                                                            
         NI    1(R2),X'DF'         UNPROTECT                                    
         OI    6(R2),X'80'         XMIT                                         
         ZIC   R0,0(R2)            BUMP TO UPGRADE FIELD                        
         AR    R2,R0                                                            
         NI    1(R2),X'DF'         UNPROTECT                                    
         OI    6(R2),X'80'         XMIT                                         
         ZIC   R0,0(R2)            BUMP TO OTHER BOOKS FIELD                    
         AR    R2,R0                                                            
         NI    1(R2),X'DF'         UNPROTECT                                    
         OI    6(R2),X'80'         XMIT                                         
         ZIC   R0,0(R2)            BUMP TO PERIOD NAME FIELD                    
         AR    R2,R0                                                            
         LA    R6,5(R6)                                                         
         BCT   R1,VK30             DO NEXT PERIOD                               
*                                                                               
         CLI   0(R2),0             TEST END OF SCREEN                           
         BE    VKX                                                              
*                                                                               
VK40     ZIC   R1,0(R2)            CLEAR / PROTECT REMAINDER OF SCREEN          
         SH    R1,=H'9'                                                         
         TM    1(R2),X'02'         TEST EXTENDED HEADER                         
         BZ    *+8                                                              
         SH    R1,=H'8'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR THE FIELD                              
         OI    1(R2),X'20'         PROTECT                                      
         NI    1(R2),X'FE'         UNMODIFY                                     
         OI    6(R2),X'80'         XMIT                                         
         ZIC   R0,0(R2)            BUMP                                         
         AR    R2,R0                                                            
*                                                                               
         CLI   0(R2),0             TEST END OF SCREEN                           
         BNE   VK40                                                             
*                                                                               
VKX      MVC   KEY,SVKEY           RESTORE THE PERIOD KEY                       
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VR       MVI   ELCODE,EPDCODEQ     PERIOD                                       
         GOTO1 REMELEM                                                          
         LA    R2,SIRPERH          DO ALL THE PERIODS FIRST                     
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
         LH    R4,NUMPERS          NUMBER OF PERIODS                            
         LA    R5,DATES            STORAGE AREA FOR DATES                       
         XC    DATES,DATES                                                      
*                                                                               
VR10     XC    WORK,WORK           VALIDATE PERIOD                              
         MVC   BYTE,5(R2)          INPUT LENGTH                                 
         OI    BYTE,PVINDDMM       VALIDATE FOR MONTH/DAY ONLY                  
         GOTO1 PERVAL,DMCB,(BYTE,8(R2)),WORK                                    
*                                                                               
         CLI   DMCB+4,PVRCOK       BOTH DATES OK?                               
         BE    *+12                                                             
         MVI   ERROR,INVDATE                                                    
         B     TRAPERR             NO                                           
*                                                                               
         LA    R3,WORK             PERVAL OUTPUT AREA                           
         USING PERVALD,R3                                                       
         MVC   1(2,R5),PVALBSTA+1  BINARY MONTH/DAY (START OF PERIOD)           
         MVC   4(2,R5),PVALBEND+1  BINARY MONTH/DAY (END OF PERIOD)             
         DROP  R3                                                               
         MVC   0(1,R5),YEAR        PUT THE YEAR IN THE TABLE                    
         MVC   3(1,R5),YEAR                                                     
*                                                                               
         CLC   0(3,R5),3(R5)       COMPARE START DATE / END DATE                
         BL    VR20                                                             
         CH    R4,NUMPERS          TEST SPECIAL CASE - PERIOD 1                 
         BE    *+12                                                             
         MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
*                                                                               
         ZIC   R1,0(R5)                                                         
         BCTR  R1,0                                                             
         STC   R1,0(R5)            DECREMENT START YEAR                         
*                                                                               
VR20     ZIC   R0,0(R2)            BUMP TO NEXT PERIOD FIELD                    
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         LA    R5,6(R5)            INCREMENT DATE TABLE                         
         BCT   R4,VR10                                                          
*                                                                               
         LA    R4,1                R4 = PERIOD COUNTER                          
         LA    R5,DATES            RESET R5 TO BEG OF DATES AREA                
         LA    R2,SIRUPGH                                                       
*                                                                               
VR50     LA    R6,MYWORK           BUILD ELEMENT IN MYWORK                      
         USING EPDELEM,R6                                                       
         XC    MYWORK,MYWORK                                                    
         MVI   EPDCODE,EPDCODEQ                                                 
         MVI   EPDLEN,EPDLENXQ     NEW VERSION OF ELEMENT                       
         STC   R4,EPDNUM                                                        
         MVC   EPDSTART,0(R5)                                                   
         MVC   EPDEND,3(R5)                                                     
         LA    R5,6(R5)                                                         
*                                                                               
         CLI   5(R2),0             UPGRADE IS REQUIRED                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
         GOTO1 VALIUPG,DMCB,BLOCK+256                                           
         LA    R7,BLOCK+256                                                     
         USING SPDEMUPD,R7                                                      
         MVI   ERROR,INVALID                                                    
         OC    SPUPSTA,SPUPSTA     STATION OVERRIDES NOT ALLOWED HERE           
         BNZ   TRAPERR                                                          
         OC    SPUPUDAY,SPUPUDAY   DAY OVERRIDES NOT ALLOWED HERE               
         BNZ   TRAPERR                                                          
         OC    SPUPUTIM,SPUPUTIM   TIME OVERRIDES NOT ALLOWED HERE              
         BNZ   TRAPERR                                                          
*                                                                               
         MVC   EPDUPFIL,SPUPFIL                                                 
         MVC   EPDUPGRD,SPUPTYPE                                                
         MVC   EPDUPFBK,SPUPFBK                                                 
         MVC   EPDUPINP,SPUPPRG                                                 
         MVC   EPDUPMBK(6),SPUPFBKL   ADDITIONAL BOOKS FOR CANADA               
         DROP  R7                                                               
*                                                                               
         ZIC   R0,0(R2)            BUMP TO OTHER BOOKS FIELD                    
         AR    R2,R0                                                            
         CLI   5(R2),0                                                          
         BE    VR70                NO BOOKS                                     
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 BOOKVAL,DMCB,(C'N',(R2)),(4,WORK),(0,SCANNER)                    
         CLI   4(R1),0             TEST FIELDS VALID                            
         BNE   *+12                                                             
         MVI   ERROR,BOOKERR                                                    
         B     TRAPERR                                                          
*                                                                               
         ZIC   R0,4(R1)            NUMBER OF OTHER BOOKS                        
         STC   R0,EPDNUMBK                                                      
         LA    R3,WORK             BOOK CODE LIST                               
         LA    R7,EPDBOOKS         FIRST BOOK CODE                              
         DROP  R6                                                               
*                                                                               
VR60     TM    0(R3),X'BF'         TEST ANY GARBAGE WAS SPECIFIED               
         BZ    *+12                                                             
         MVI   ERROR,BOOKERR                                                    
         B     TRAPERR                                                          
         MVC   0(2,R7),1(R3)       SAVE BOOK YEAR/MONTH                         
         LA    R7,2(R7)            NEXT BOOK                                    
         LA    R3,3(R3)                                                         
         BCT   R0,VR60                                                          
*                                                                               
VR70     ZIC   R0,0(R2)            BUMP TO NEXT UPGRADE FIELD                   
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               R2 - NXT UPG HEADER                          
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM(EPDLENXQ),MYWORK                                            
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R4,1(R4)            INCREMENT PERIOD COUNTER                     
         CH    R4,NUMPERS          TEST ANY MORE PERIODS                        
         BNH   VR50                                                             
*                                                                               
VRX      B     XIT                                                              
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       LA    R2,SIRPERH          CLEAR SCREEN                                 
*                                                                               
DR10     TM    1(R2),X'20'         PROTECTED?                                   
         BO    DR20                YES -- DON'T CLEAR                           
         ZIC   RF,0(R2)            RF HAS LENGTH OF FIELD                       
         SH    RF,=H'9'            SUBTRACT HEADER LENGTH +1                    
         TM    1(R2),X'02'         TEST EXTENDED HEADER                         
         BZ    *+8                                                              
         SH    RF,=H'8'                                                         
         EX    RF,*+8              CLEAR FIELD                                  
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
*                                                                               
DR20     OI    6(R2),X'80'         XMIT                                         
         ZIC   R0,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   DR10                                                             
*                                                                               
         L     R6,AIO              PERIOD DEFINITION ELEMENT                    
         MVI   ELCODE,EPDCODEQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    WORK,WORK                                                        
         USING EPDELEM,R6                                                       
         LA    R2,SIRPERH                                                       
*                                                                               
DR30     GOTO1 DATCON,DMCB,(3,EPDSTART),(5,WORK)                                
         GOTO1 DATCON,DMCB,(3,EPDEND),(5,WORK+8)                                
         MVC   8(5,R2),WORK        MONTH/DAY                                    
         MVC   14(5,R2),WORK+8                                                  
         MVI   13(R2),C'-'                                                      
*                                                                               
         ZIC   R0,0(R2)            BUMP TO UPGRADE                              
         AR    R2,R0                                                            
         CLI   EPDUPFIL,0          TEST ANY UPGRADE IN ELEMENT                  
         BE    DR45                NO                                           
*                                                                               
         MVI   WORK,C' '           BUILD UPGRADE EXPRESSION                     
         MVC   WORK+1(L'WORK-1),WORK                                            
         MVC   WORK(4),=C'UPX='                                                 
         MVC   WORK+2(1),EPDUPFIL                                               
         MVC   WORK+4(16),EPDUPINP                                              
*                                                                               
         OC    EPDUPFBK,EPDUPFBK   TEST ANY SHARE BOOK                          
         BZ    DR40                                                             
         OC    EPDUPINP,EPDUPINP   CANADIAN UPGRADE EXPRESSION?                 
         BNZ   *+18                                                             
         LA    R5,WORK+4           YES                                          
         MVC   0(3,R5),=C'BK/'                                                  
         B     DR35                                                             
*                                                                               
         LA    R5,WORK+21                                                       
         CLI   0(R5),C' '          BACK UP TO NON-BLANK                         
         BNE   *+8                                                              
         BCT   R5,*-8                                                           
         MVI   1(R5),C','          EDIT IN COMMA                                
         LA    R5,2(R5)                                                         
         MVC   0(3,R5),=C'BK='                                                  
*                                                                               
DR35     GOTO1 DATCON,DMCB,(3,EPDUPFBK),(6,3(R5))                               
         CLI   EPDLEN,EPDLENXQ     NEW VERSION OF ELEMENT?                      
         BL    DR40                NO                                           
*                                                                               
         LA    R5,9(R5)            BUMP PAST THIS BOOK                          
         LA    R0,3                MAXIMUM OF 3 MORE BOOKS                      
         LA    R3,EPDUPMBK         EXTRA BOOKS FOR CANADA                       
DR37     OC    0(2,R3),0(R3)       ANY MORE?                                    
         BZ    DR40                                                             
         MVI   0(R5),C'/'          BOOK DELIMITER                               
         GOTO1 DATCON,DMCB,(3,0(R3)),(6,1(R5))                                  
         LA    R5,7(R5)                                                         
         LA    R3,2(R3)                                                         
         BCT   R0,DR37                                                          
*                                                                               
DR40     ZIC   RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         TM    1(R2),X'02'         TEST EXTENDED HEADER                         
         BZ    *+8                                                              
         SH    RE,=H'8'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),WORK        EBCDIC UPGRADE EXPRESSION                    
*                                                                               
DR45     ZIC   R0,0(R2)            BUMP TO OTHER BOOKS FIELD                    
         AR    R2,R0                                                            
         CLI   EPDNUMBK,0          TEST ANY BOOKS TO DISPLAY                    
         BE    DR60                                                             
*                                                                               
         LA    R4,8(R2)            BEGINNING OF DATA                            
         ZIC   R5,EPDNUMBK         NUMBER OF BOOKS                              
         LA    R3,EPDBOOKS                                                      
         XC    WORK,WORK                                                        
         DROP  R6                                                               
*                                                                               
DR50     MVC   WORK(2),0(R3)       YEAR/MONTH                                   
         GOTO1 DATCON,DMCB,(3,WORK),(6,(R4))                                    
         BCTR  R5,0                DECREMENT BOOK COUNTER                       
         CH    R5,=H'0'            TEST FINISHED                                
         BE    DR60                YES                                          
         MVI   6(R4),C','          EDIT IN COMMA                                
         LA    R4,7(R4)            NEXT FIELD                                   
         LA    R3,2(R3)            NEXT YEAR/MONTH                              
         B     DR50                                                             
*                                                                               
DR60     BAS   RE,NEXTEL           NEXT PERIOD                                  
         BNE   DRX                                                              
*                                                                               
         ZIC   R0,0(R2)            BUMP TO NEXT PERIOD                          
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     DR30                                                             
*                                                                               
DRX      B     XIT                                                              
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 3                                                                
TRAPERR  GOTO1 ERREX                                                            
         SPACE 3                                                                
         LTORG                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE SPSFMFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMBCD                                                       
         PRINT OFF                                                              
       ++INCLUDE SPSFMWORKD                                                     
         PRINT ON                                                               
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE                                                         
NUMPERS  DS    H                   NUMBER OF PERIODS                            
YEAR     DS    X                   YEAR WE ARE USING                            
MYWORK   DS    CL64                                                             
DATES    DS    XL48                                                             
         SPACE 4                                                                
       ++INCLUDE SPDEMUPD                                                       
         EJECT                                                                  
SIRRECD  DSECT                                                                  
       ++INCLUDE SPGENSIR                                                       
         SPACE 4                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017SPSFM0C   05/01/02'                                      
         END                                                                    
