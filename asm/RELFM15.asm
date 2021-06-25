*          DATA SET RELFM15    AT LEVEL 022 AS OF 04/23/06                      
*PHASE T80415A,*                                                                
         TITLE 'T80413 - RELFM15 - ALT REC'                                     
*                                                                               
*********************************************************************           
*                                                                   *           
*        RELFM15 - T80415 - REP FILE PROGRAM                        *           
*                                                                   *           
*********************************************************************           
*                                                                   *           
*  MOD LOG                                                          *           
*  -------                                                          *           
*  08/15/97  JRD  INITIAL ENTRY                                     *           
*  04/08/98  JRD  FIX END DATE VS. PRECEEDING START CHECK           *           
*                                                                   *           
*********************************************************************           
T80415   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T80415                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T804FFD,RA                                                       
*                                                                               
         MVC   KEY,BKEY                                                         
         MVI   KEY+27,0                                                         
         MVC   KEY+28(4),BSVDA                                                  
*                                                                               
         CLI   BFMTSW,0            FORMAT OR EDIT?                              
         BNE   ACLEDT              EDIT                                         
*                                                                               
         BAS   RE,GETREC           GET RECORD FOR DISPLAY                       
         B     ACLFMT              FORMAT                                       
         EJECT                                                                  
**********************                                                          
*    EDIT ROUTINE    *                                                          
**********************                                                          
ACLEDT   DS    0H                                                               
         CLI   OVRDFLGS,C'Y'       RECORD LOCKED?                               
         BE    FLERR4              YES - CAN'T BE CHANGED                       
*                                                                               
         MVC   REC+27(2),=Y(34)    LENGTH OF RECORD                             
         XC    ELEM,ELEM           SETUP DATES ELEMENT                          
         MVC   ELEM,=X'101C'                                                    
         XC    ELEM2,ELEM2         SETUP WEEKS ELEMENT                          
         MVC   ELEM2,=X'201A'                                                   
         XC    ELEM3,ELEM3         SETUP DAYS ELEMENT                           
         MVC   ELEM3,=X'301A'                                                   
*                                                                               
         LA    R2,ACLSJANH                                                      
         CLI   5(R2),0             MUST INPUT EVERY FIELD                       
         BE    FLERR1                                                           
*                                                                               
         GOTO1 VDATVAL,DMCB,(0,8(R2)),WORK    VALIDATE START DATE               
         OC    0(4,R1),0(R1)                                                    
         BZ    FLERR2                                                           
*                                                                               
         GOTO1 VDATCON,DMCB,(0,WORK),(8,WORK3)                                  
         MVI   WORK3+8,C'-'                   FOR WEEK/DAYS ELEMENTS            
*                                                                               
         GOTO1 VGETDAY,DMCB,(0,WORK),FULL     MUST START ON MONDAY              
         CLI   FULL,C' '                                                        
         BNH   FLERR2                                                           
         CLI   DMCB,1              MONDAY?                                      
         BNE   FLERR2              NO                                           
*                                                                               
*   CONVERT START DATE OF JAN TO END DATE OF DEC                                
*                                                                               
         GOTO1 VADDAY,DMCB,WORK,WORK+3,-1                                       
         GOTO1 VDATCON,DMCB,(0,WORK+3),(2,WORK+6)    COMPRESSED                 
*                                                                               
RC2      USING RACLREC,REC2                                                     
         LA    R0,REC              MOVE REC TO REC2, IN CASE REC IS             
         LA    R1,1000              NEEDED FOR LAST OR NEXT YEAR'S              
         LA    R4,REC2              RECORD                                      
         LA    R5,1000                                                          
         MVCL  R4,R0                                                            
*                                                                               
*   MAKE SURE END OF DEC MATCHES PREVIOUS YEAR RECORD                           
*                                                                               
         CLI   1(RA),C'*'          DDS TERMINAL?                                
         BE    ACLEDT50            IF SO, DON'T CHECK LAST YEAR                 
*                                                                               
         XC    KEY,KEY                                                          
K        USING RACLKEY,KEY                                                      
         MVI   KEY,X'20'                                                        
         MVC   K.RACLKREP,REPALPHA                                              
         MVC   K.RACLKNAM,RC2.RACLKNAM                                          
         ZIC   R4,RC2.RACLKYR      YEAR                                         
         BCTR  R4,0                LAST YEAR                                    
         STC   R4,K.RACLKYR                                                     
         DROP  K                                                                
*                                                                               
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   ACLEDT50            NO RECORD FOR LAST YEAR                      
*                                                                               
         BAS   RE,GETREC                                                        
         LA    R5,REC                                                           
         MVI   ELCODE,X'10'        SURROGATE ELCODE                             
         BAS   RE,GETEL                                                         
         BE    *+6                 REQUIRED                                     
         DC    H'0'                                                             
*                                                                               
         USING RACLELEM,R5                                                      
         CLC   WORK+6(2),RACLDATE+24                                            
         BE    *+12                                                             
         LA    R3,142              ERROR, START DATE MUST FOLLOW                
         B     ERROR               LAST YEAR'S END DATE                         
         DROP  R5                                                               
*                                                                               
ACLEDT50 DS    0H                                                               
         LA    R6,ELEM+(RACLDATE-RACLELEM)                                      
         LA    R3,ELEM2+(RACLWKPM-RACLWKS)                                      
         LA    R4,ELEM3+(RACLDYPM-RACLDAYS)                                     
         MVC   0(2,R6),WORK+6                                                   
*                                                                               
ACLEDT60 BAS   RE,NEXTUF                                                        
         CLI   5(R2),0                                                          
         BE    FLERR1                                                           
*                                                                               
*   IF INPUT IS MONTH/DAY, USE YEAR FROM PREVIOUS MONTH                         
*                                                                               
         CLI   5(R2),5                                                          
         BH    ACLEDT68            THEY SUPPLIED YEAR                           
         GOTO1 VDATVAL,DMCB,(1,8(R2)),WORK+12   VALIDATE M/D                    
         OC    0(4,R1),0(R1)                                                    
         BZ    FLERR2                                                           
         MVC   WORK+2(4),WORK+14                                                
         B     ACLEDT72                                                         
*                                                                               
ACLEDT68 GOTO1 VDATVAL,DMCB,(0,8(R2)),WORK      VALIDATE M/D/Y                  
         OC    0(4,R1),0(R1)                                                    
         BZ    FLERR2                                                           
*                                                                               
ACLEDT72 DS    0H                                                               
         GOTO1 VGETDAY,DMCB,(0,WORK),FULL     MUST END ON SUNDAY                
         CLI   FULL,C' '                                                        
         BNH   FLERR2                                                           
         CLI   DMCB,7              SUNDAY?                                      
         BNE   FLERR2              NO                                           
*                                                     COMPRESS DATE             
         GOTO1 VDATCON,DMCB,(0,WORK),(2,WORK+6)                                 
*                                                                               
*   THIS MONTH MUST BE LATER THAN LAST MONTH                                    
*&&DO                                                                           
*   TEST                                                                        
         CLC   =X'FAF5F0F8F0F7',WORK                                            
***      CLC   =X'FAF5F0F7F0F3',WORK                                            
         BNE   TEST0020                                                         
         MVC   DIE(2),=X'0000'                                                  
TEST0020 EQU   *                                                                
*&&                                                                             
         CLC   0(2,R6),WORK+6                                                   
         BL    *+12                                                             
         LA    R3,65               ERROR, OVERLAPPING DATES                     
         B     ERROR                                                            
*                                                                               
         LA    R6,2(R6)                                                         
         MVC   0(2,R6),WORK+6                                                   
*                                  BUILD WEEKS & DAYS ELEMENTS                  
         GOTO1 VDATCON,DMCB,(2,0(R6)),(8,WORK3+9)                               
         L     RF,ACOMFACS                                                      
         LA    RF,CPERVAL-COMFACSD(RF)                                          
         L     RF,0(RF)                                                         
         GOTO1 (RF),DMCB,(17,WORK3),(0,PERVBLK),0,0,0,0                         
         MVC   0(2,R3),PERVBLK+2   WEEKS                                        
         LA    R3,2(R3)                                                         
         MVC   0(2,R4),PERVBLK     DAYS                                         
         LA    R4,2(R4)                                                         
*                                                                               
         GOTO1 VADDAY,DMCB,WORK,WORK+3,1                START OF NXT            
         GOTO1 VDATCON,DMCB,(0,WORK+3),(8,WORK3)          MONTH                 
         MVI   WORK3+8,C'-'                                                     
*                                                                               
         LA    RE,ACLDECH          CHECK FOR LAST MONTH OF YEAR                 
         CR    R2,RE                                                            
         BL    ACLEDT60                                                         
*                                                                               
*   MAKE SURE END DATE OF DEC MATCHES END DATE OF DEC FOR NEXT YEAR             
*   (IF RECORD EXISTS)                                                          
*                                                                               
         CLI   1(RA),C'*'          DDS TERMINAL?                                
         BE    ACLEDT80            IF SO, DON'T CHECK NEXT YEAR                 
*                                                                               
         GOTO1 VADDAY,DMCB,WORK+3,WORK+3,-1          RESTORE END DATE           
         GOTO1 VDATCON,DMCB,(0,WORK+3),(2,WORK+6)    COMPRESSED                 
*                                                                               
         XC    KEY,KEY                                                          
K        USING RACLKEY,KEY                                                      
         MVI   KEY,X'20'                                                        
         MVC   K.RACLKREP,REPALPHA                                              
         MVC   K.RACLKNAM,RC2.RACLKNAM                                          
         ZIC   RE,RC2.RACLKYR      YEAR                                         
         LA    RE,1(RE)                                                         
         STC   RE,K.RACLKYR                                                     
         DROP  K                                                                
*                                                                               
         BAS   RE,HIGH                                                          
         CLC   KEYSAVE(27),KEY                                                  
         BNE   ACLEDT80            NEXT YEAR'S RECORD DOESN'T EXIST             
*                                                                               
         BAS   RE,GETREC                                                        
*                                                                               
         LA    R5,REC                                                           
         MVI   ELCODE,X'10'        SURROGATE ELCODE                             
         BAS   RE,GETEL                                                         
         BE    *+6                 REQUIRED                                     
         DC    H'0'                                                             
*                                                                               
         CLC   WORK+6(2),RACLDATE-RACLELEM(R5)                                  
         BE    *+12                                                             
         LA    R3,143              ERROR, END DATE MUST IMMEDIATELY             
         B     ERROR               PRECEDE NEXT YEAR'S START DATE               
*                                                                               
ACLEDT80 DS    0H                                                               
         DROP  RC2                                                              
         LA    R0,REC              MOVE REC2 BACK TO REC                        
         LA    R1,1000                                                          
         LA    R4,REC2                                                          
         LA    R5,1000                                                          
         MVCL  R0,R4                                                            
*                                                                               
         LA    R4,REC              A(NEW RECORD)                                
         LA    R4,RACLDESC-RACLREC(R4)                                          
         GOTO1 VRECUP,DMCB,(C'R',REC),(0,ELEM3),(0,(R4))                        
         GOTO1 VRECUP,DMCB,(C'R',REC),(0,ELEM2),(0,(R4))                        
         GOTO1 VRECUP,DMCB,(C'R',REC),(0,ELEM),(0,(R4))                         
*                                                                               
         CLI   BACT,C'A'           TEST ADD                                     
         BE    FLADD                                                            
***********************************************************************         
*        CHANGE - READ REC, THEN WRITE NEW                                      
***********************************************************************         
         LA    R0,REC              MOVE REC TO REC2                             
         LA    R1,1000                                                          
         LA    R4,REC2                                                          
         LA    R5,1000                                                          
         MVCL  R4,R0                                                            
*                                                                               
         MVC   KEY(28),REC                                                      
         MVC   KEY+28(4),BSVDA                                                  
         BAS   RE,GETREC                                                        
*                                                                               
* UPDATE DESCRIPTION ELEMENT                                                    
*                                                                               
         LA    R5,REC              A(OLD RECORD)                                
         MVI   ELCODE,X'01'        SURROGATE ELCODE                             
         BAS   RE,GETEL                                                         
         BE    *+6                 REQUIRED                                     
         DC    H'0'                                                             
         USING RACLDESC,R5                                                      
         GOTO1 VDATCON,DMCB,(5,0),(19,RACLDCHG)                                 
         DROP  R5                                                               
*                                                                               
         L     RF,ACOMFACS         LUID OF CHANGE                               
         LA    RF,CGETFACT-COMFACSD(RF)                                         
         L     RF,0(RF)                                                         
         GOTO1 (RF),DMCB,0                                                      
         L     RF,0(R1)                                                         
         USING FACTSD,RF                                                        
         USING RACLDESC,R5                                                      
         MVC   RACLDLU,FASYM                                                    
         DROP  R5,RF                                                            
*                                                                               
         MVI   RACLDLOK,C'N'                                                    
         CLI   ACLLOCK,C'Y'        REQUEST TO LOCK ACL?                         
         BNE   ACLEDT90                                                         
         MVI   RACLDLOK,C'Y'       YES                                          
ACLEDT90 EQU   *                                                                
*                                                                               
         LA    R4,REC2             A(NEW RECORD)                                
         LA    R4,RACLDESC-RACLREC(R4)                                          
         GOTO1 VRECUP,DMCB,(C'R',REC2),(0,(R5)),(0,(R4))                        
*                                                                               
         LA    R0,REC              MOVE REC2 TO REC                             
         LA    R1,1000                                                          
         LA    R4,REC2                                                          
         LA    R5,1000                                                          
         MVCL  R0,R4                                                            
*                                                                               
         BAS   RE,PUTREC                                                        
*                                                                               
         B     ACLFMT                                                           
***********************************************************************         
* ADD RECORD                                                                    
***********************************************************************         
FLADD    DS    0H                                                               
*                                                                               
* BUILD DESCRIPTION ELEMENT                                                     
*                                                                               
         XC    WORK,WORK                                                        
         LA    R5,WORK                                                          
         USING RACLDESC,R5                    SET DATE LAST CHANGED             
         MVI   RACLDELC,X'01'                                                   
         MVI   RACLDELN,RACLDLNQ                                                
         GOTO1 VDATCON,DMCB,(5,0),(19,RACLDCHG)                                 
         GOTO1 VDATCON,DMCB,(5,0),(19,RACLDADD)                                 
         DROP  R5                                                               
DIE      EQU   *                                                                
*                                                                               
         L     RF,ACOMFACS         LUID OF CHANGE                               
         LA    RF,CGETFACT-COMFACSD(RF)                                         
         L     RF,0(RF)                                                         
         GOTO1 (RF),DMCB,0                                                      
         L     RF,0(R1)                                                         
         USING FACTSD,RF                                                        
*                                                                               
*   R5 STILL POINTING TO ELEMENT BEING BUILT IN WORK                            
*                                                                               
         USING RACLDESC,R5                                                      
         MVC   RACLDLU,FASYM                                                    
*                                                                               
         MVI   RACLDLOK,C'N'                                                    
         CLI   ACLLOCK,C'Y'        REQUEST TO LOCK ACL?                         
         BNE   FLADD020                                                         
         MVI   RACLDLOK,C'Y'       YES                                          
         DROP  R5,RF                                                            
*                                                                               
FLADD020 EQU   *                                                                
*                                                                               
         LA    R4,REC                                                           
         LA    R4,RACLDESC-RACLREC(R4)                                          
         GOTO1 VRECUP,DMCB,(C'R',REC),(0,(R5)),(0,(R4))                         
*                                                                               
         BAS   RE,ADDREC                                                        
         MVC   BSVDA,KEY     SAVE DISK ADDRESS                                  
         EJECT                                                                  
*************************                                                       
*    FORMAT ROUTINE     *                                                       
*************************                                                       
ACLFMT   DS    0H                                                               
*                                                                               
*   FIRST POSITION OF 'OVRDFLGS' WILL BE USED AS AN INDICATOR.                  
*        THESE FIELDS WERE USED WHEN 'FILE' WAS USED FOR                        
*        RESEARCH.  THAT FUNCTION IS NOW IN RMP.                                
*                                                                               
         MVI   OVRDFLGS,C'N'                                                    
         CLI   RACLDLOK-RACLREC+REC,C'Y'                                        
         BNE   ACLFMT05                                                         
         MVI   OVRDFLGS,C'Y'                                                    
ACLFMT05 EQU   *                                                                
         LA    R5,REC                                                           
         MVI   ELCODE,X'10'        SURROGATE ELCODE                             
         BAS   RE,GETEL                                                         
         BE    *+6                 REQUIRED                                     
         DC    H'0'                                                             
         LA    R6,RACLDATE-RACLELEM(R5)                                         
*                                                                               
         LA    R5,REC                                                           
         MVI   ELCODE,X'20'        SURROGATE ELCODE                             
         BAS   RE,GETEL                                                         
         BE    *+6                 REQUIRED                                     
         DC    H'0'                                                             
*                                                                               
         LA    R3,RACLWKPM-RACLWKS(R5)                                          
*                                                                               
         LA    R5,REC                                                           
         MVI   ELCODE,X'30'        SURROGATE ELCODE                             
         BAS   RE,GETEL                                                         
         BE    *+6                 REQUIRED                                     
         DC    H'0'                                                             
*                                                                               
         LA    R4,RACLDYPM-RACLDAYS(R5)                                         
*                                                                               
         LA    R2,LFMLAST                                                       
         BAS   RE,NEXTUF           POINT TO START DATE                          
         B     ACLFMT20            DISPLAY FIRST MONTH                          
*                                                                               
ACLFMT10 DS    0H                                                               
         ZIC   RE,0(R2)                                                         
         AR    R2,RE               POINT TO NEXT MONTH NAME                     
         IC    RE,0(R2)                                                         
         AR    R2,RE               POINT TO NEXT MONTH START DATE               
*                                                                               
ACLFMT20 DS    0H                                                               
         GOTO1 VDATCON,DMCB,(2,0(R6)),(0,WORK)      TO YYMMDD                   
         GOTO1 VADDAY,DMCB,WORK,WORK+3,1        ADD 1 TO GET START DAY          
         GOTO1 VDATCON,DMCB,(0,WORK+3),(8,8(R2))    TO MMMDD/YY                 
         FOUT  (R2)                                                             
*                                                                               
         BAS   RE,NEXTUF           TO END DATE                                  
         LA    R6,2(R6)                                                         
         GOTO1 VDATCON,DMCB,(2,0(R6)),(8,8(R2))     TO MMMDD/YY                 
         FOUT  (R2)                                                             
*                                                                               
         ZIC   RE,0(R2)            CLEAR & SHOW WEEKS(DAYS)                     
         AR    R2,RE                                                            
         ZIC   RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         TM    1(R2),X'02'                                                      
         BZ    *+8                                                              
         SH    RE,=H'8'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
*                                                                               
         EDIT  (B2,0(R3)),(2,8(R2)),ZERO=NOBLANK,ALIGN=LEFT                     
         LA    R3,2(R3)                                                         
         LA    RE,8(R2)                                                         
         AR    RE,R0                                                            
         MVI   0(RE),C'('                                                       
         EDIT  (B2,0(R4)),(3,1(RE)),ZERO=NOBLANK,ALIGN=LEFT                     
         LA    R4,2(R4)                                                         
         AR    RE,R0                                                            
         MVI   1(RE),C')'                                                       
         FOUT  (R2)                                                             
*                                                                               
         LA    RE,ACLDECH          CHECK FOR LAST MONTH OF YEAR                 
         CR    R2,RE                                                            
         BL    ACLFMT10                                                         
*                                                                               
         MVC   ACLLOCK(1),RACLDLOK DISPLAY LOCK CODE                            
         FOUT  ACLLOCKH                                                         
*                                                                               
         CLI   OVRDFLGS,C'Y'       IS ALT CALENDAR LOCKED?                      
         BE    FLERR4              YES - SHOW THAT MESSAGE                      
         B     EXXMOD                                                           
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO POINT R2 TO NEXT UNPROTECTED FIELD                              
***********************************************************************         
NEXTUF   SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BCR   8,RE                                                             
         CLI   0(R2),9                                                          
         BE    NEXTUF2                                                          
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    NEXTUF                                                           
         BR    RE                                                               
NEXTUF2  CLI   9(R2),0             CHECK FOR LAST                               
         BNE   NEXTUF4                                                          
         CR    R2,R2               IF LAST, SET CC=                             
         BR    RE                                                               
NEXTUF4  LTR   R2,R2               NOT LAST, SET CC NOT=                        
         BR    RE                                                               
***********************************************************************         
FLERR1   LA    R3,MSSNGERR                                                      
         B     ERROR                                                            
FLERR2   LA    R3,INVERR                                                        
         B     ERROR                                                            
FLERR3   LA    R3,NUMERR                                                        
         B     ERROR                                                            
FLERR4   SR    R3,R3                                                            
         MVC   LFMMSG(L'ACLLOCD),ACLLOCD                                        
         MVI   ERRAREA,X'FF'                                                    
         B     EXIT                                                             
ACLLOCD  DC    C'ALT CALENDAR IS LOCKED, AND CANNOT BE CHANGED'                 
***********************************************************************         
         GETEL R5,=Y(RACLDESC-RACLREC),ELCODE                                   
         EJECT                                                                  
***********************************************************************         
MONTHS   DC    X'01',CL3'JAN'                                                   
         DC    X'02',CL3'FEB'                                                   
         DC    X'03',CL3'MAR'                                                   
         DC    X'04',CL3'APR'                                                   
         DC    X'05',CL3'MAY'                                                   
         DC    X'06',CL3'JUN'                                                   
         DC    X'07',CL3'JUL'                                                   
         DC    X'08',CL3'AUG'                                                   
         DC    X'09',CL3'SEP'                                                   
         DC    X'0A',CL3'OCT'                                                   
         DC    X'0B',CL3'NOV'                                                   
         DC    X'0C',CL3'DEC'                                                   
         DC    X'01',CL3'JAN'                                                   
         DC    X'02',CL3'FEB'                                                   
         DC    X'03',CL3'MAR'                                                   
         DC    X'04',CL3'APR'                                                   
         DC    X'05',CL3'MAY'                                                   
         DC    X'06',CL3'JUN'                                                   
         DC    X'07',CL3'JUL'                                                   
         DC    X'08',CL3'AUG'                                                   
         DC    X'09',CL3'SEP'                                                   
         DC    X'0A',CL3'OCT'                                                   
         DC    X'0B',CL3'NOV'                                                   
         DC    X'0C',CL3'DEC'                                                   
         DC    X'FF'                                                            
         EJECT                                                                  
* RELFMGEN                                                                      
* REGENACL                                                                      
* RELFMTWA                                                                      
* RELFMD6D                                                                      
* DDCOMFACS                                                                     
* FAFACTS                                                                       
       ++INCLUDE RELFMGEN                                                       
         EJECT                                                                  
         ORG   REC                                                              
       ++INCLUDE REGENACL                                                       
         EJECT                                                                  
       ++INCLUDE RELFMTWA                                                       
         EJECT                                                                  
         ORG   LFMLAST                                                          
       ++INCLUDE RELFMD6D                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
GENOLD   DSECT                                                                  
         ORG   WORK2                                                            
ELCODE   DS    X                                                                
ELEM     DS    XL256                                                            
ELEM2    DS    XL256                                                            
ELEM3    DS    XL256                                                            
PERVBLK  DS    XL56                PERVAL BLOCK AREA                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022RELFM15   04/23/06'                                      
         END                                                                    
