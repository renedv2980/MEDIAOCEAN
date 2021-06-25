*          DATA SET SPTRA99    AT LEVEL 020 AS OF 07/22/09                      
*PHASE T21699A                                                                  
*                                                                               
***********************************************************************         
* ADD COMML TABLE -----------------                                             
* CK COPY SPLIT PRODS ? CK ALL USED?                                            
***********************************************************************         
*                                                                     *         
*        THIS PROGRAM READS ALL UNITS FOR A NET, AND MARKS UNITS      *         
*        WITH ANY INVALID COMMERCIALS.                                *         
*                                                                     *         
*   WILL ONLY RUN OFFLINE, AS IT READS AND WRITES TOO MANY RECORDS    *         
*   FOR ONLINE.                                                       *         
*                                                                     *         
* AIO USAGE - AIO1 - READ UNIT RECORDS                                *         
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER      *         
*                    (DDGENCON-T00A30)                                *         
*             AIO2 - READ COMMLS                                      *         
*             AIO3 -                                                  *         
*                                                                     *         
* REGISTER USAGE -                                                    *         
*        R0 - WORK REG                                                *         
*        R1 - WORK REG                                                *         
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR       *         
*        R3 - POINTER TO STATION RECORD                               *         
*        R4 - WORK REG & KEY DSECT POINTER                            *         
*        R5 - WORK REG - IN                                           *         
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM   *         
*              FOR DSECT IN VALREC                                    *         
*        R7 - SECOND BASE                                             *         
*        R8 - POINTER TO SPOOLD                                       *         
*        R9 - POINTER TO SYSD                                         *         
*        RA - POINTER TO ATWA                                         *         
*        RB - FIRST BASE                                              *         
*        RC - POINTER TO GEND                                         *         
*        RD - SAVE AREA POINTER                                       *         
*        RE - GOTO1 REG                                               *         
*        RF - GOTO1 REG                                               *         
*                                                                     *         
*        PROGRAM LABELS MEANING:                                      *         
*        V PREFIX = VALIDATE                                          *         
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                    *         
*        F PREFIX = FIND                                              *         
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST          *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21699 READ UITS AND MARK INVALID COMMLS'                       
***********************************************************************         
*                                                                     *         
*  LEV  8-9  NOV02/87 ONLY ALLOW DDS TERMINALS ACCESS                 *         
*  LEV  14   JUL16/03 FIX UNIT START DATE AND USE ROTATION            *         
*  LEV  15   SEP10/03 CODE FOR 2 CML LENGTHS FOR 1 PRD SCENARIO       *         
*  LEV  16   NOV05/04 CHANGE ST DATE TO 10 MONTHS AGO                 *         
*                     ADD CMML TABLE FOR FASTER PROCESSING            *         
* LEV  17 SM APR14/05 ADD CODE FOR AD-ID,FIX BAD BRANCH IN PRD COMPARE*         
* LEV  18 SM MAY03/07 MORE BRANDS                                     *         
* LEV  19 SM JUN24/09 ALL ADID SUPPORT                                *         
*                                                                     *         
***********************************************************************         
*                                                                               
T21699   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21699*,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,SPTR99RR                                                      
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,PRINTREP       NOW PURGE RECORDS                            
         BE    LR                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       DS   0H                                                                
         B     EXIT                BYPASS VALIDATE KEY FOR NOW                  
*                                                                               
         LA    R2,TRAMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
*                                                                               
         LA    R2,TRACLTH          CLIENT                                       
         XC    BCLT(6),BCLT        CLEAR BCLT, BPRD, BSLN, BPRD2, BSLN2         
         XC    QPRD,QPRD                                                        
         XC    SVBCLT,SVBCLT                                                    
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK20                                                             
*                                                                               
         GOTO1 VALICLT                                                          
         MVC   SVBCLT,BCLT                                                      
*                                                                               
VK20     LA    R2,TRAPRDH          PRODUCT                                      
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK30                NO                                           
         OC    BCLT,BCLT                                                        
         BZ    MISSCLT                                                          
*                                                                               
         GOTO1 VALIPRD                                                          
         MVC   QPRD,WORK                                                        
*NOP     MVC   BPRD,WORK+3         GET BIN PROD                                 
*                                                                               
VK30     DS   0H                                                                
         B     EXIT                                                             
         EJECT                                                                  
* INITIALIZE *                                                                  
*                                                                               
LR       TM    WHEN,X'18'          OV OR DDS                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LM    R0,R1,=A(HEADING,HDHK)   HDG FOR REPORT, HEAD HOOK               
         A     R0,SPTR99RR                                                      
         ST    R0,SPECS                 STORE FOR CONTROLLER                    
*                                                                               
*TEMP                                                                           
         XC    CMLKS(40),CMLKS                                                  
*TEMP                                                                           
         A     R1,SPTR99RR                                                      
         ST    R1,HEADHOOK         STORE FOR CONTROLLER                         
*                                                                               
         XC    COUNTERS(ENDCTRS-COUNTERS),COUNTERS                              
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(X'20',TODAY)                                  
         MVC   TODAY+4(2),=C'01'            START ON DAY 1                      
         GOTO1 ADDAY,(R1),(C'M',TODAY),(X'20',WORK),F'-10'                      
         GOTO1 ADDAY,(R1),(C'D',WORK),(X'20',WORK+6),F'-1'                      
         GOTO1 DATCON,(R1),(0,WORK+6),(2,TODAYP)                                
*                                                                               
         L     R3,VADUMMY                                                       
         LA    R3,16(,R3)                                                       
         SRL   R3,8                                                             
         SLL   R3,8                                                             
         MVC   8(8,R3),=CL8'*CMLTAB*'                                           
         LA    R3,16(,R3)                                                       
         ST    R3,CMLTADR                                                       
         A     R3,=F'100000'                                                    
         ST    R3,CMLTMAX                                                       
*                                                                               
         L     RE,CMLTADR                                                       
         L     RF,CMLTMAX                                                       
         LR    RF,RE                                                            
         XCEFL                                                                  
*                                                                               
* START GETTING UNITS *                                                         
*                                                                               
         BRAS  RE,INITNET                                                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NURECD,R4                                                        
         MVI   NUKTYPE,04                                                       
         CLC   AGENCY,=C'SJ'                                                    
         BNE   *+8                                                              
         MVI   NUKAM,X'F3'         FORCE SJR                                    
         CLC   AGENCY,=C'H9'                                                    
         BNE   *+8                                                              
         MVI   NUKAM,X'F3'         FORCE H9                                     
         CLC   AGENCY,=C'DU'                                                    
         BNE   *+8                                                              
         MVI   NUKAM,X'13'         FORCE DU                                     
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
LR20     DS   0H                                                                
         MVI   UNITSW,C'Y'          SET TO UNIT OKAY                            
*                                                                               
         LA    R4,KEY                                                           
         USING NURECD,R4                                                        
         CLI   NUKTYPE,04          STILL IN UNITS?                              
         BE    LR22                 YES, GO ON                                  
*                                                                               
         BAS   RE,CLTOT                                                         
*                                                                               
         BRAS  RE,PAGYT            PRINT AGENCY TOTALS                          
*                                                                               
         BAS   RE,EOJ               NO, DONE (UNLESS CHECKING PATTERNS          
         B     EXIT                                                             
*                                                                               
LR22     DS   0H                                                                
         CLC   AGENCY,=C'SJ'       THIS STARCOM                                 
         BE    *+14                                                             
         CLC   AGENCY,=C'H9'       THIS STARCOM                                 
         BNE   LR23                                                             
         CLI   NUKAM,X'F3'         IS THIS H9 - STAR                            
         BNE   LR40                                                             
         B     LR24                                                             
*                                                                               
LR23     DS   0H                                                                
         CLC   AGENCY,=C'DU'       THIS MEDIAVEST                               
         BNE   LR40                                                             
*                                                                               
         CLI   NUKAM,X'13'         IS THIS DU - MVNYD                           
         BNE   LR40                                                             
*                                                                               
LR24     DS    0H                                                               
         CLC   NUKAM,SVBAGYMD      SAME AGENCY                                  
         BNE   LR26                 NO                                          
*                                                                               
         CLC   NUKCLT,SVBCLT        SAME CLIENT                                 
         BE    LR30                                                             
         B     LR28                                                             
*                                                                               
LR26     DS   0H                                                                
         MVC   SVBAGYMD,NUKAM                                                   
         MVC   BAGYMD,SVBAGYMD                                                  
*                                                                               
         BRAS  RE,PAGYT            PRINT AGENCY TOTALS                          
*                                                                               
LR28     DS   0H                                                                
         MVC   SVBCLT,NUKCLT                                                    
*                                                                               
         BRAS  RE,FCLT                                                          
         BNE   LR20                IF NO CLIENT, SEE IF NEW AGENCY              
*                                                                               
LR30     DS    0H                                                               
         L     R1,UNTCTS           UNITS READ                                   
         LA    R1,1(,R1)                                                        
         ST    R1,UNTCTS                                                        
*                                                                               
*        CLC   SVSTDTE,NUKDATE                                                  
         CLC   TODAYP,NUKDATE                                                   
         BH    LR40                                                             
*                                                                               
         L     R1,UNTCTC           UNITS CHECKED                                
         LA    R1,1(,R1)                                                        
         ST    R1,UNTCTC                                                        
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         BAS   RE,CKCML            GO VALIDATE COMMLS                           
*                                                                               
LR40     DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
         B     LR20                                                             
SVSTDTE  DC    X'CE21'                                                          
         EJECT                                                                  
CKCML    NTR1                                                                   
         MVI   SVCOPY,C'N'         SET AS NOT COPY SPLIT                        
         XC    SVCSPROD,SVCSPROD                                                
         XC    SVPROD1,SVPROD1                                                  
         XC    SVPROD2,SVPROD2                                                  
         XC    SVCSUSED,SVCSUSED                                                
*                                                                               
         MVC   SVUNTKEY,KEY                                                     
         LA    R4,KEY                                                           
         USING NUKEY,R4                                                         
         MVC   SVAIRDAT,NUKDATE                                                 
         GOTO1 DATCON,DMCB,(2,SVAIRDAT),(3,SVAIRDTP)                            
         MVC   SVNET,NUKNET                                                     
         MVC   SVPROG,NUKPROG                                                   
         MVC   SVEST,NUKEST                                                     
         MVC   SVSUB,NUKSUB                                                     
         MVC   SVDP,NUKDP                                                       
*                                                                               
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NUMAINEL,R6                                                      
         MVC   SVTIME,NUTIME                                                    
         MVC   SVPACK,NUPACK                                                    
*NOP     MVC   SVDAY,NUDAY                                                      
         MVC   SVLENHLD,NULEN                                                   
         MVC   SVLEN,NULEN                                                      
*                                                                               
*NOP     MVC   SVBPRD1,NUPRD                                                    
****     MVC   SVBPRD2,NUPRD2                                                   
*                                                                               
         CLI   NUPRD,0                                                          
         BE    CKCML04                                                          
*                                                                               
         MVC   BYTE,NUPRD                                                       
         BAS   RE,GPRD             GET 3 CHAR PROD                              
         MVC   SVPROD1,QPRD                                                     
*                                                                               
         CLI   NUPRD2,0                                                         
         BE    CKCML04                                                          
         MVC   BYTE,NUPRD2                                                      
         BAS   RE,GPRD             GET 3 CHAR PROD                              
         MVC   SVPROD2,QPRD                                                     
*                                                                               
CKCML04  MVI   SVLEN2,0                                                         
*                                                                               
         MVI   SVDAY,0                                                          
         MVI   ELCODE,X'02'                                                     
         L     R6,AIO1                                                          
         BAS   RE,GETEL                                                         
         BNE   CKCML20                                                          
         USING NUSDRD,R6                                                        
*                                                                               
         MVC   SVDAY,NUSDROT                                                    
*                                                                               
         TM    NUSDST3,X'40'       THIS A COPY SPLIT?                           
         BZ    *+8                                                              
         MVI   SVCOPY,C'Y'         SET AS COPY SPLIT                            
*                                                                               
         L     R6,AIO1                                                          
         LR    R5,R6                                                            
         MVI   ELCODE,X'19'        NEW 3 CHAR PROD                              
         BAS   RE,GETEL                                                         
         BE    *+16                                                             
         CLI   SVCOPY,C'Y'         THIS A COPY SPLIT?                           
         BNE   CKCML20                                                          
         B     CKCML10             YES, MUST BE OLD RECORD                      
*                                                                               
         USING NUPDED,R6                                                        
         SR    R0,R0                                                            
         ZIC   R1,1(R6)                                                         
         D     R0,=F'7'                                                         
         CHI   R0,3                MUST BE A REMAINDER OF 3                     
         BE    *+8                                                              
         B     CKCML17             BAD C/S                                      
         LR    R0,R1                                                            
         CHI   R0,0                MUST BE AT LEAST 1 PRODUCT                   
         BH    *+8                                                              
         B     CKCML17             BAD C/S                                      
*                                                                               
         LA    R1,NUPDEPR                                                       
         MVC   SVPROD1,0(R1)       SAVE 3 CHAR PROD                             
         CHI   R0,2                P/B PROD                                     
         BL    CKCML08                                                          
         LA    R1,7(R1)            POINT TO NEXT PROD                           
         MVC   SVPROD2,0(R1)                                                    
*                                                                               
         TM    NUPDEIND,X'40'      IS IT C/S                                    
         BZ    CKCML10              NO                                          
*                                                                               
CKCML08  DS    0H                                                               
         LA    R1,NUPDEPR          PT TO 3 CHAR PROD                            
         LA    RF,SVCSPROD         MOVE IN C/S PRODS                            
         B     BLPRG21C                                                         
*                                                                               
CKCML10  L     R6,AIO1                                                          
         MVI   ELCODE,X'14'                                                     
         BAS   RE,GETEL                                                         
         BNE   CKCML16                                                          
*                                                                               
         USING NUPRDD,R6                                                        
         SR    R0,R0                                                            
         ZIC   R1,1(R6)                                                         
         D     R0,=F'6'                                                         
         CHI   R0,3                MUST BE A REMAINDER OF 3                     
         BE    *+8                                                              
         B     CKCML17             BAD C/S                                      
         LR    R0,R1                                                            
         CHI   R0,1                MUST BE MORE THAN 1 PRODUCT                  
         BH    *+8                                                              
         B     CKCML17             BAD C/S                                      
*                                                                               
         LA    R1,NUPRDPR                                                       
         LA    RF,SVCSPROD         MOVE IN C/S PRODS                            
*                                                                               
BLPRD00  LA    R2,NCLSTSIZ         FIND PROD                                    
         L     RE,ASVNCLST                                                      
*                                                                               
BLPRD10  CLC   0(1,R1),3(RE)                                                    
         BE    BLPRD20                                                          
         LA    RE,4(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNH   *+8                                                              
         BCT   R2,BLPRD10                                                       
*                                                                               
         MVC   SVREASON,=CL8'NO PROD '                                          
         B     CKCML17F                                                         
*                                                                               
BLPRD20  MVC   0(3,RF),0(RE)                                                    
         B     *+10                                                             
BLPRG21C MVC   0(3,RF),0(R1)                                                    
         LA    RF,3(,RF)           BUMP IN SVCSPROD                             
         LA    R1,6(,R1)           TO NEXT PROD IN ELEM                         
         CLI   ELCODE,X'19'        NEW 3 CHAR PROD ELEM                         
         BNE   BLPRD30                                                          
         LA    R1,1(R1)            YES, BUMP ONE MORE IN ELEM                   
         BCT   R0,BLPRG21C                                                      
         B     CKCML20                                                          
BLPRD30  BCT   R0,BLPRD00                                                       
         B     CKCML20                                                          
*                                                                               
CKCML16  DS    0H                                                               
         OC    SVPROD1(6),SVPROD1  PROD1 AND PROD2                              
         BNZ   CKCML20                                                          
*NOP     CLI   SVBPRD1,0                                                        
*        BE    *+12                                                             
*        CLI   SVBPRD2,0                                                        
******   BNE   CKCML20                                                          
*                                                                               
CKCML17  MVC   SVREASON,=CL8'BAD C/S'                                           
*                                                                               
CKCML17F BRAS  RE,PERR             PRINT ERROR COMML                            
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVI   SVCOPY,C'N'         SET AS NOT COPY SPLIT                        
*                                                                               
CKCML20  DS    0H                                                               
         MVI   ELCODE,X'21'                                                     
         L     R6,AIO1                                                          
         BAS   RE,GETEL                                                         
         BNE   CKCMLX                                                           
*                                                                               
         USING NUCMLEL,R6                                                       
         TM    NUCMLFLG,X'E0'      ANY FLAGS ON ALREADY?                        
         BNZ   CKCMLX                                                           
*                                                                               
         OC    NUCML1,NUCML1       ANY CML                                      
         BZ    CKCML40                                                          
*                                                                               
         MVI   SVCMLPOS,C'1'       SET FOR PROD 1                               
         MVC   SVSRC,=CL8'PROD 1'                                               
*                                                                               
         MVI   CMLADIDF,C'N'       PRESET AD-ID=NO                              
         TM    NUCMADFL,NUCMADF1 IS THIS AN AD-ID                               
         BZ    *+8                                                              
         MVI   CMLADIDF,C'Y'       SET AD-ID=YES                                
*                                                                               
         MVC   CKCMLCOD,NUCML1                                                  
         MVC   CKCMLPRO,SVPROD1                                                 
*                                                                               
         OC    NUCMPROD,NUCMPROD   ANY 3 CHAR PROD                              
         BZ    *+14                                                             
         MVC   CKCMLPRO,NUCMPROD                                                
         B     CKCML30                                                          
*                                                                               
         CLI   NUCMLPRD,0          ANY COPY SPLIT PROD                          
         BE    CKCML30                                                          
         MVC   BYTE,NUCMLPRD                                                    
         BAS   RE,GPRD                                                          
         MVC   CKCMLPRO,QPRD                                                    
*                                                                               
CKCML30  BAS   RE,VCML                                                          
         BE    CKCML40                                                          
*                                                                               
         BRAS  RE,PERR             PRINT ERROR COMML                            
*                                                                               
CKCML40  DS    0H                                                               
         OC    NUCML2,NUCML2       ANY CML                                      
         BZ    CKCML50                                                          
*                                                                               
         MVI   SVCMLPOS,C'2'       SET FOR PROD 2                               
         MVC   SVSRC,=CL8'PROD 2'                                               
*                                                                               
         MVI   CMLADIDF,C'N'       PRESET AD-ID=NO                              
         TM    NUCMADFL,NUCMADF2 IS THIS AN AD-ID                               
         BZ    *+8                                                              
         MVI   CMLADIDF,C'Y'       SET AD-ID=YES                                
*                                                                               
         MVC   CKCMLCOD,NUCML2                                                  
         MVC   CKCMLPRO,SVPROD2                                                 
*                                                                               
         BAS   RE,VCML                                                          
         BE    CKCML50                                                          
*                                                                               
         BRAS  RE,PERR             PRINT ERROR COMML                            
*                                                                               
CKCML50  DS    0H                                                               
         OC    NUCMLBSN,NUCMLBSN       ANY CML                                  
         BZ    CKCML60                                                          
*                                                                               
         CLC   NUCMLBSN,=C'VIGNETTE'                                            
         BE    CKCML60                                                          
*                                                                               
         MVI   SVCMLPOS,C'B'       SET FOR BILLBOARD (NO PRODUCT CK             
         MVC   SVSRC,=CL8'BB SLIDE'                                             
*                                                                               
         MVI   CMLADIDF,C'N'       PRESET AD-ID=NO                              
         TM    NUCMADFL,NUCMADF3 IS THIS AN AD-ID                               
         BZ    *+8                                                              
         MVI   CMLADIDF,C'Y'       SET AD-ID=YES                                
*                                                                               
         MVC   CKCMLCOD,NUCMLBSN                                                
         BAS   RE,VCML                                                          
         BE    CKCML60                                                          
*                                                                               
         BRAS  RE,PERR             PRINT ERROR COMML                            
*                                                                               
CKCML60  DS    0H                                                               
         OC    NUCMLBCN,NUCMLBCN   ANY CML                                      
         BZ    CKCML70                                                          
*                                                                               
         MVI   SVCMLPOS,C'B'       SET FOR BILLBOARD (NO PRODUCT CK             
         MVC   SVSRC,=CL8'BB COPY'                                              
*                                                                               
         MVI   CMLADIDF,C'N'       PRESET AD-ID=NO                              
         TM    NUCMADFL,NUCMADF4 IS THIS AN AD-ID                               
         BZ    *+8                                                              
         MVI   CMLADIDF,C'Y'       SET AD-ID=YES                                
*                                                                               
         MVC   CKCMLCOD,NUCMLBCN                                                
         BAS   RE,VCML                                                          
         BE    CKCML70                                                          
*                                                                               
         BRAS  RE,PERR             PRINT ERROR COMML                            
*                                                                               
CKCML70  DS    0H                                                               
         MVI   ELCODE,X'23'                                                     
         L     R6,AIO1                                                          
         BAS   RE,GETEL                                                         
         BNE   CKCMLX                                                           
         B     CKCML74                                                          
*                                                                               
CKCML72  DS    0H                                                               
         USING NUFDCEL,R6                                                       
         BAS   RE,NEXTEL                                                        
         BNE   CKCMLX                                                           
*                                                                               
CKCML74  DS    0H                                                               
         MVC   SVLEN,SVLENHLD                                                   
         MVI   SVLEN2,0                                                         
         OC    NUFDCML1,NUFDCML1  ANY CML                                       
         BZ    CKCML80                                                          
*                                                                               
         MVI   SVCMLPOS,C'1'       SET FOR PROD 1                               
         MVC   SVSRC,=CL8'FD PRD 1'                                             
*                                                                               
         MVI   CMLADIDF,C'N'       PRESET AD-ID=NO                              
         TM    NUFDADFL,NUFDADF1 IS THIS AN AD-ID                               
         BZ    *+8                                                              
         MVI   CMLADIDF,C'Y'       SET AD-ID=YES                                
*                                                                               
         MVC   CKCMLCOD,NUFDCML1                                                
         MVC   CKCMLPRO,SVPROD1                                                 
*                                                                               
         OC    NUFDPROD,NUFDPROD   ANY 3 CHAR PROD                              
         BZ    *+14                                                             
         MVC   CKCMLPRO,NUFDPROD                                                
         B     CKCML78                                                          
*                                                                               
         CLI   NUFDCPRD,0          ANY COPY SPLIT PROD                          
         BE    CKCML78                                                          
         MVC   BYTE,NUFDCPRD                                                    
         BAS   RE,GPRD                                                          
         MVC   CKCMLPRO,QPRD                                                    
*                                                                               
CKCML78  BAS   RE,VCML                                                          
         BE    CKCML80                                                          
*                                                                               
         BRAS  RE,PERR             PRINT ERROR COMML                            
*                                                                               
CKCML80  DS    0H                                                               
         OC    NUFDCML2,NUFDCML2       ANY CML                                  
         BZ    CKCML90                                                          
*                                                                               
         MVI   SVCMLPOS,C'2'       SET FOR PROD 2                               
         MVC   SVSRC,=CL8'FD PRD 2'                                             
*                                                                               
         MVI   CMLADIDF,C'N'       PRESET AD-ID=NO                              
         TM    NUFDADFL,NUFDADF2 IS THIS AN AD-ID                               
         BZ    *+8                                                              
         MVI   CMLADIDF,C'Y'       SET AD-ID=YES                                
*                                                                               
         MVC   CKCMLCOD,NUFDCML2                                                
         MVC   CKCMLPRO,SVPROD2                                                 
*                                                                               
         BAS   RE,VCML                                                          
         BE    CKCML90                                                          
*                                                                               
         BRAS  RE,PERR             PRINT ERROR COMML                            
*                                                                               
CKCML90  DS    0H                                                               
         OC    NUFDCBSN,NUFDCBSN       ANY CML                                  
         BZ    CKCML94                                                          
*                                                                               
         CLC   NUFDCBSN,=C'VIGNETTE'                                            
         BE    CKCML94                                                          
*                                                                               
         MVI   SVCMLPOS,C'B'       SET FOR BILLBOARD (NO PRODUCT CK             
         MVC   SVSRC,=CL8'FD BB SLD'                                            
*                                                                               
         MVI   CMLADIDF,C'N'       PRESET AD-ID=NO                              
         TM    NUFDADFL,NUFDADF3 IS THIS AN AD-ID                               
         BZ    *+8                                                              
         MVI   CMLADIDF,C'Y'       SET AD-ID=YES                                
*                                                                               
         MVC   CKCMLCOD,NUFDCBSN                                                
         BAS   RE,VCML                                                          
         BE    CKCML94                                                          
*                                                                               
         BRAS  RE,PERR             PRINT ERROR COMML                            
*                                                                               
CKCML94  DS    0H                                                               
         OC    NUFDCBCN,NUFDCBCN   ANY CML                                      
         BZ    CKCML72                                                          
*                                                                               
         MVI   SVCMLPOS,C'B'       SET FOR BILLBOARD (NO PRODUCT CK             
         MVC   SVSRC,=CL8'FD BB CP'                                             
*                                                                               
         MVI   CMLADIDF,C'N'       PRESET AD-ID=NO                              
         TM    NUFDADFL,NUFDADF4 IS THIS AN AD-ID                               
         BZ    *+8                                                              
         MVI   CMLADIDF,C'Y'       SET AD-ID=YES                                
*                                                                               
         MVC   CKCMLCOD,NUFDCBCN                                                
         BAS   RE,VCML                                                          
         BE    CKCML72                                                          
*                                                                               
         BRAS  RE,PERR             PRINT ERROR COMML                            
         B     CKCML72                                                          
*                                                                               
CKCMLX   DS    0H                                                               
         MVC   KEY(L'SVUNTKEY),SVUNTKEY RESTORE KEY AND DISK ADDR               
*                                                                               
         BAS   RE,INITNET          SET TO UNIT FILE                             
*                                                                               
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1                                                         
*                                                                               
         CLI   UNITSW,C'Y'         WAS THIS UNIT OKAY                           
         BE    EXIT                                                             
*                                                                               
         L     R1,UNTCTCU          UNITS UPDATED                                
         LA    R1,1(,R1)                                                        
         ST    R1,UNTCTCU                                                       
*                                                                               
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'21'                                                     
         L     R6,AIO1                                                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING NUCMLEL,R6                                                       
         OI    NUCMLFLG,X'E0'      SET ALL FLAGS ON                             
*                                                                               
         GOTO1 PUTREC                                                           
*                                                                               
         B     EXIT                                                             
         DROP  R6                                                               
*                                                                               
*  GET 3 CHAR PRODUCT                                                           
*                                                                               
GPRD     NTR1                                                                   
*                                                                               
         XC    QPRD,QPRD                                                        
*                                                                               
GPRD20   LA    R0,NCLSTSIZ         MAX COUNT BUG CATCHER                        
         L     R1,ASVNCLST         TABLE OF CLIENT PROD CODES                   
GPRD20B  CLC   BYTE,3(R1)          THIS A VALID PROD CODE                       
         BE    GPRD20D                                                          
         LA    R1,4(,R1)           BUMP PROD PTR                                
         CLI   0(R1),C' '          AT END OF TABLE?                             
         BNH   *+8                 YES, DEATH                                   
         BCT   R0,GPRD20B                                                       
         DC    H'0'                                                             
GPRD20D  MVC   QPRD,0(R1)                                                       
         XIT1                                                                   
*                                                                               
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
*                                                                               
ERREXIT  A     R1,SPTR99RR                                                      
         MVC   CONHEAD,0(R1)                                                    
         GOTO1 ERREX2                                                           
*                                                                               
MISSCLT  LA    R2,TRACLTH                                                       
MISSERR  MVI   ERROR,MISSING                                                    
TRAPERR  GOTO1 ERREX                                                            
INITNET  DS    0H                                                               
         MVI   LKEY+1,20                                                        
         MVI   DATADISP+1,27                                                    
         MVI   SYSDIR,C'U'                                                      
         MVI   SYSDIR+1,C'N'                                                    
         MVI   SYSFIL,C'U'                                                      
         MVI   SYSFIL+1,C'N'                                                    
         BR    RE                                                               
INITSPT  DS    0H                                                               
         MVI   LKEY+1,13                                                        
         MVI   DATADISP+1,24                                                    
         MVI   SYSDIR,C'S'                                                      
         MVI   SYSDIR+1,C'P'                                                    
         MVI   SYSFIL,C'S'                                                      
         MVI   SYSFIL+1,C'P'                                                    
         BR    RE                                                               
         DROP  RB,RC                                                            
         EJECT                                                                  
* FIND COMMERCIAL INFO *                                                        
*                                                                               
VCML     NMOD1 0,**VCML**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
*TEMP                                                                           
         L     R0,CMLK                                                          
         AHI   R0,1                                                             
         ST    R0,CMLK                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(3,SVAIRDTP),(0,WORK)                                
         GOTO1 GETDAY,(R1),WORK,WORK+6                                          
         ZIC   RF,0(R1)            GET DAY OF WEEK                              
*                                                                               
         MVC   SVAIRDTS,SVAIRDTP                                                
         MVC   SVAIRDTE,SVAIRDTP                                                
*                                                                               
         XC    WORK,WORK                                                        
         ZIC   R0,SVDAY                                                         
         SLL   R0,25                                                            
         SR    R1,R1               CLEAR COUNTER                                
*                                                                               
         LTR   RF,RF                                                            
         BZ    VCML10                                                           
         SLL   R0,1                ONLY COUNT DAYS AFTER NBACTDAT               
         BCT   RF,*-4                                                           
*                                                                               
VCML10   LTR   R0,R0               COUNT UNTIL NO DAYS LEFT                     
         BZ    VCML12                                                           
*                                                                               
         LTR   R0,R0               COUNT UNTIL NO DAYS LEFT                     
         BZ    *+12                                                             
         SLL   R0,1                                                             
         BCT   R1,*-10                                                          
*                                                                               
         LPR   R0,R1               GET # OF DAYS IN ROT AFTER NBACTDAT          
*                                                                               
         GOTO1 DATCON,DMCB,(3,SVAIRDTP),(0,WORK)                                
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,(R1),(0,WORK+6),(3,SVAIRDTE)                              
         GOTO1 (RF),(R1),(0,WORK),(3,SVAIRDTS)                                  
*                                                                               
VCML12   DS    0H                                                               
         MVI   CMLOKSW,C'Y'                                                     
*                                                                               
         L     R3,CMLTADR                                                       
         USING CMLTBL,R3                                                        
VCML16   DS    0H                                                               
         OC    CMLTENT,CMLTENT     END OF EXISTING TABLE                        
         BZ    VCML18               YES                                         
         CLC   CKCMLCOD,CMLTCML    THIS THE CODE                                
         BE    VCML17               TEMP                                        
         BE    VCML40               YES, CK IT OUT                              
         LA    R3,CMLTNEXT                                                      
         C     R3,CMLTMAX                                                       
         BL    VCML16                                                           
*TEMP                                                                           
         B     VCML40               YES, CK IT OUT                              
VCML17   DS    0H                                                               
         L     R0,CMLTF                                                         
         AHI   R0,1                                                             
         ST    R0,CMLTF                                                         
         B     VCML40                                                           
*                                                                               
VCML18   DS    0H                                                               
*TEMP                                                                           
         L     R0,CMLR                                                          
         AHI   R0,1                                                             
         ST    R0,CMLR                                                          
         CLC   CMLTMX,CMLR                                                      
         BNL   *+10                                                             
         MVC   CMLTMX,CMLR                                                      
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
*                                                                               
         CLI   CMLADIDF,C'Y'       AD-ID CML                                    
         BNE   *+14                                                             
         MVC   CMLKID,=X'0AC1'                                                  
         B     *+10                                                             
         MVC   CMLKID,=X'0A21'                                                  
*                                                                               
         MVC   CMLKAM(3),SVBAGYMD AND SVBCLT                                    
         MVC   CMLKCML,CKCMLCOD                                                 
*                                                                               
         MVC   CMLTCML,CKCMLCOD                                                 
*                                                                               
         BRAS  RE,INITSPT           CHANGE TO SPOT                              
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    VCML20                                                           
*                                                                               
         MVI   CMLOKSW,C'N'                                                     
         MVC   SVREASON,=CL8'MISSING'                                           
         OI    CMLTSTA,CMLTMIS      SET TABLE TO MISSING CODE                   
         B     VCMLX                                                            
*                                                                               
VCML20   DS    0H                                                               
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         CLI   CMLADIDF,C'Y'       AD-ID CML                                    
         BNE   VCML23                                                           
*                                                                               
         MVI   ELCODE,X'A0'        ADID ELEMENT                                 
         BRAS  RE,GETEL                                                         
         BE    VCML22                                                           
*                                                                               
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVC   CKCMLCOD(8),5(R6)   SAVE 8 CHAR ISCII                            
         XC    CKCMLCOD+8(4),CKCMLCOD+8 AND CLEAR THE REST                      
         B     *+10                                                             
VCML22   MVC   CKCMLCOD(12),2(R6)   SAVE PRINTABLE ADID                         
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
*                                                                               
VCML23   MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
*                                                                               
         MVC   CKCMLLEN,CMLTSLN                                                 
         MVC   CKCMLOVR(2),CMLTOVR  SAVE PRN OVRD 1 & 2                         
*                                                                               
         MVC   CMLTSLN,CMLSLN                                                   
         MVC   CMLTOVR(2),CMLOVRD1  SAVE PRN OVRD 1 & 2                         
*                                                                               
         TM    CMLSTAT,X'80'       TEST DELETED COMMERCIAL                      
         BZ    VCML24                                                           
*                                                                               
         MVC   SVREASON,=CL8'DELETED'                                           
         OI    CMLTSTA,CMLTDEL                                                  
         MVI   CMLOKSW,C'N'                                                     
         B     VCMLX                                                            
*                                                                               
VCML24   DS    0H                                                               
         MVC   CMLTRLS,CMLRLSE     SAVE CMML RELAESE DATEDS                     
         MVC   CMLTRCL,CMLRCL                                                   
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,X'29'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   2(R6),X'FF'         ALL PRODUCTS OKAY?                           
         BNE   VCML31                                                           
*                                                                               
         MVI   CMLTPROD,X'FF'                                                   
         B     VCML40              USED TO BE VCMLX                             
*                                                                               
VCML31   DS    0H                                                               
         ZIC   RE,1(R6)                                                         
         SHI   RE,3                MINUS CODE,LEN,EX MVC                        
         LR    R0,RE                                                            
         LA    R1,2(,R6)                                                        
         LR    RF,R1                                                            
         CHI   RE,14               MORE THAN 5 PRODS                            
         BH    VCML32                                                           
         EX    RE,SAVPRDS                                                       
         MVI   BYTE,0              INIT BUFFER SWITCH                           
*TEMP                                                                           
         L     R0,CMLT                                                          
         AHI   R0,1                                                             
         ST    R0,CMLT                                                          
         B     VCML40                                                           
SAVPRDS  MVC   CMLTPROD(0),2(R6)                                                
*                                                                               
VCML32   MVI   BYTE,X'FF'          SET CAN'T BUFFER THIS CML                    
*                                                                               
         ZIC   R0,1(R6)            GET LEN                                      
         SHI   R0,2                                                             
*                                                                               
         CLI   SVCMLPOS,C'B'       IF BILLBOARD, NO PROD/LEN CHECK              
         BE    VCML38                                                           
*                                                                               
VCML34   CLC   CKCMLPRO,0(R1)      PROD OKAY                                    
         BE    VCML38                                                           
         LA    R1,3(,R1)                                                        
         SHI   R0,2                                                             
         BCT   R0,VCML34                                                        
*                                                                               
         XC    CMLTENT,CMLTENT     CAN'T BUFFER THIS COMML                      
*                                                                               
         MVC   SVREASON,=CL8'BAD PROD'                                          
         MVI   CMLOKSW,C'N'                                                     
         B     VCMLX                                                            
*                                                                               
VCML38   CLC   CMLTRLS,SVAIRDTE   SEE IF AIR DATES FALL IN CML PERIOD           
         BH    VCML44                                                           
*                                                                               
         CLC   CMLTRCL,SVAIRDTS                                                 
         BL    VCML44                                                           
*                                                                               
         XC    CMLTENT,CMLTENT     CAN'T BUFFER THIS COMML                      
         B     VCMLX                                                            
*                                                                               
VCML40   DS    0H                                                               
         CLC   CMLTRLS,SVAIRDTE   SEE IF AIR DATES FALL IN CML PERIOD           
         BH    VCML44                                                           
         CLC   CMLTRCL,SVAIRDTS                                                 
         BNL   VCML50                                                           
*                                                                               
VCML44   DS    0H                                                               
         MVC   P+5(7),=C'CMLRLS='                                               
         GOTO1 HEXOUT,DMCB,CMLTRLS,P+12,3,0                                     
         MVC   P+20(7),=C'CMLRCL='                                              
         GOTO1 HEXOUT,DMCB,CMLTRCL,P+27,3,0                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P+5(7),=C'UNT ST='                                               
         GOTO1 HEXOUT,DMCB,SVAIRDTE,P+12,3,0                                    
         MVC   P+20(8),=C'UNTEND='                                              
         GOTO1 HEXOUT,DMCB,SVAIRDTS,P+27,3,0                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   SVREASON,=CL8'DATES'                                             
         MVI   CMLOKSW,C'N'                                                     
         CLI   BYTE,X'FF'          CAN'T BUFF CML?                              
         BNE   VCMLX                                                            
         XC    CMLTENT,CMLTENT     CAN'T BUFFER THIS COMML                      
         MVI   BYTE,0                                                           
         B     VCMLX                                                            
*                                                                               
VCML50   DS   0H                                                                
         CLI   SVCMLPOS,C'B'       IF BILLBOARD, NO PROD/LEN CHECK              
         BE    VCMLX                                                            
*                                                                               
         CLI   CMLTPROD,X'FF'      ALL PRODUCTS OKAY?                           
         BE    VCMLX                                                            
*                                                                               
         LA    R0,5                5 PRODS                                      
         LA    R1,CMLTPROD                                                      
VCML52   DS    0H                                                               
         CLC   CKCMLPRO,0(R1)      PROD OKAY                                    
         BE    VCMLX                                                            
*                                                                               
         LA    R1,3(,R1)                                                        
         CLI   0(R1),0                                                          
         BE    *+8                 PRD DO NOT MATCH                             
         BCT   R0,VCML52                                                        
*                                                                               
         MVC   SVREASON,=CL8'BAD PROD'                                          
         MVI   CMLOKSW,C'N'                                                     
         B     VCMLX                                                            
*                                                                               
* NEVER GETS HERE - WILL LEAVE IT ALONE FOR NOW.                                
*                                                                               
VCML54   DS    0H                                                               
         CLC   SVLEN,CMLTSLN       IS LENGTH OKAY                               
         BE    VCMLX                                                            
         BL    VCML70                                                           
*                                                                               
         ZIC   R0,SVLEN            GET UNIT LEN                                 
         ZIC   R1,CKCMLLEN         GET COMML LEN                                
         SR    R0,R1               GET REMAINDER                                
         STC   R0,SVLEN            SAVE LEEFT-OVER                              
*                                                                               
         ZIC   RF,SVLEN2           GET LEN USED TO DATE                         
         AR    RF,R1                                                            
         STC   RF,SVLEN2                                                        
         B     VCMLX                                                            
*                                                                               
VCML70   DS    0H                                                               
         MVC   SVREASON,=CL8'BAD LEN'                                           
         MVI   CMLOKSW,C'N'                                                     
*                                                                               
VCMLX    CLI   CMLOKSW,C'Y'                                                     
         XIT1                                                                   
         LTORG                                                                  
         DROP  R4                                                               
         EJECT                                                                  
* FIND CLIENT HEADER AND SAVE CLIST *                                           
*                                                                               
FCLT     NMOD1 0,**FCLT**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         OC    UNTCTC,UNTCTC       IF NO UNITS READ, BYPASS                     
         BZ    FCLT20                                                           
         OC    UNTCTCU,UNTCTCU     IF NO UNITS UPDATED, BYPASS                  
*        BZ    FCLT20                                                           
*                                                                               
         BRAS  RE,CLTOT                                                         
*                                                                               
FCLT20   DS    0H                                                               
         BRAS  RE,INITSPT          SET TO SPOT FILE                             
*                                                                               
         MVC   SVUNTKEY,KEY                                                     
*                                                                               
*NOP     LA    R4,KEY                                                           
*        USING CLTHDR,R4                                                        
*        XC    KEY,KEY                                                          
*        MVC   CKEYAM,SVBAGYMD                                                  
*        MVC   CKEYCLT,SVBCLT                                                   
*        SPACE                                                                  
*        GOTO1 CLUNPK,DMCB,((R0),SVBCLT),QCLT                                   
*        SPACE                                                                  
*        GOTO1 HIGH                                                             
*        CLC   KEY(L'CKEY),KEYSAVE                                              
*NOP     BE    FCLT30                                                           
*                                                                               
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,3            DATA LEN                                     
         GOTO1 CLUNPK,DMCB,((R0),SVBCLT),FLD                                    
         LA    R2,FLDH                                                          
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALICLT                                                          
         MVI   ERROPT,0                                                         
*                                                                               
         CLI   ERROR,0                                                          
         BE    FCLT30                                                           
*                                                                               
         MVC   P+2(6),=C'AGENCY'                                                
*                                                                               
         ZIC   R0,SVUNTKEY+1                                                    
         SRL   R0,4                                                             
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+9(2),DUB                                                       
*                                                                               
         MVC   P+12(24),=C'MISSING CLIENT FOR UNITS'                            
         MVC   P+37(L'QCLT),QCLT                                                
         MVC   P+41(28),=C'UNITS FOR CLIENT NOT CHECKED'                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         BRAS  RE,INITNET          SET TO UNIT FILE                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(4),SVUNTKEY MOVE IN 04, AGYMD, CLT                           
         MVI   KEY+4,X'FF'         FORCE TO NEXT CLIENT                         
*                                                                               
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1                                                         
         LTR   RB,RB               SET CC NE                                    
*                                                                               
         B     FCLTX                                                            
*                                                                               
FCLT30   DS    0H                                                               
* DONE IN VALICLT                                                               
*NOP     L     R6,AIO2                                                          
*        ST    R6,AIO                                                           
*        DROP  R4                                                               
*        USING CLTHDR,R6                                                        
*        SPACE                                                                  
*        GOTO1 GETREC                                                           
*        SPACE                                                                  
*        MVC   CLTNM,CNAME                                                      
*        IC    R0,CPROF+6                                                       
*        LA    R2,CLIST                                                         
*        LA    R3,880                                                           
*        LA    RE,SVCLIST                                                       
*        LR    RF,R3                                                            
*NOP     MVCL  RE,R2                                                            
*                                                                               
         MVC   KEY(L'SVUNTKEY),SVUNTKEY RESTORE KEY AND DISK ADDR               
*                                                                               
FCLT40   DS    0H                                                               
         BRAS  RE,INITNET          SET TO UNIT FILE                             
*                                                                               
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1                                                         
         CR    R0,R0               SET CC EQ                                    
*                                                                               
FCLTX    XIT1                                                                   
         DROP  R6,RB,RC                                                         
         EJECT                                                                  
* PRINT CLIENT TOTALS *                                                         
*                                                                               
PERR     NMOD1 0,**PERR**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         MVC   PCLT,QCLT                                                        
         MVC   PNET,SVNET                                                       
*                                                                               
         GOTO1 DATCON,DMCB,(2,SVAIRDAT),(5,PDATE)                               
         MVC   PPROG,SVPROG                                                     
*                                                                               
         ZIC   R0,SVEST                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PEST,DUB                                                         
*                                                                               
         ZIC   R0,SVSUB                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PSUB,DUB                                                         
*                                                                               
         ZIC   R0,SVPACK                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PPACK,DUB                                                        
*                                                                               
         MVC   PDAYPT,SVDP                                                      
*                                                                               
         CLI   SVDAY,0             BAD DAY                                      
         BNE   *+14                                                             
         MVC   PDAY(3),=C'VAR'                                                  
         B     PERR30                                                           
         GOTO1 UNDAY,DMCB,SVDAY,PDAY                                            
                                                                                
PERR30   DS    0H                                                               
         GOTO1 UNTIME,DMCB,SVTIME,PTIME                                         
                                                                                
         ZIC   R0,SVLEN                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PLEN,DUB                                                         
                                                                                
         MVC   PCOMML,CKCMLCOD                                                  
         CLI   CMLADIDF,C'Y'       AD-ID=YES                                    
         BNE   *+10                                                             
         MVC   PCOMML(12),CKCMLCOD PRINT AD-IDI                                 
                                                                                
         CLI   CKCMLCOD,C'A'                                                    
         BNL   PERR35                                                           
                                                                                
         MVC   DMCB+4(4),=X'D9000AFE' GET ADDRESS OF TRPACK                     
         GOTO1 CALLOV,DMCB                                                      
         L     RF,0(R1)                                                         
                                                                                
         GOTO1 (RF),DMCB,(C'U',CKCMLCOD),PCOMML                                 
         BE    PERR35                                                           
                                                                                
         MVC   PCOMML(11),=C'**UNKNOWN**'                                       
                                                                                
PERR35   MVC   PSRC,SVSRC                                                       
         MVC   PREASON,SVREASON                                                 
*                                                                               
         GOTO1 HEXOUT,DMCB,SVUNTKEY+21,PREASON+10,4,0,0                         
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         CLI   CMLOKSW,C'Y'        ONLY COUNT MORE THAN 1 ERR ONCE              
         BNE   PERR40                                                           
         CLI   UNITSW,C'Y'        ONLY COUNT MORE THAN 1 ERR ONCE               
         BNE   PERRX                                                            
*                                                                               
PERR40   DS    0H                                                               
         MVI   UNITSW,C'N'                                                      
*                                                                               
PERRX    DS    0H                                                               
         XIT1                                                                   
         DROP  RB,RC                                                            
         EJECT                                                                  
* PRINT CLIENT TOTALS *                                                         
*                                                                               
CLTOT    NMOD1 0,**CLTO**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         OC    UNTCTS,UNTCTS       ANY UNITS FOUND?                             
         BNZ   CLTOT20             YES                                          
         OC    UNTCTC,UNTCTC      ANY UNITS CHECKED?                            
         BZ    CLTOTX              ALL IS WELL                                  
         DC    H'0'                HOW CAN THERE BE CHECKED                     
*                                                                               
CLTOT20  DS    0H                                                               
         MVC   PCLT,QCLT                                                        
         MVC   P+6(18),=C'UNIT RECS FOR CLT='                                   
         EDIT  (4,UNTCTS),(9,P+26),COMMAS=YES                                   
         MVC   P+40(22),=C'UNITS CHECKED FOR CLT='                              
         EDIT  (4,UNTCTC),(9,P+64),COMMAS=YES                                   
         MVC   P+75(22),=C'UNITS UPDATED FOR CLT='                              
         EDIT  (4,UNTCTCU),(9,P+98),COMMAS=YES                                  
         MVI   SPACING,2                                                        
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LM    R4,R6,UNTCTS        GET UNIT & UPDATED CTS                       
*                                                                               
         L     RE,UNTCTA                                                        
         AR    RE,R4                                                            
         ST    RE,UNTCTA                                                        
         L     RE,UNTCTAC                                                       
         AR    RE,R5                                                            
         ST    RE,UNTCTAC                                                       
         L     RE,UNTCTAU                                                       
         AR    RE,R6                                                            
         ST    RE,UNTCTAU                                                       
*                                                                               
         L     RE,UNTCTF                                                        
         AR    RE,R4                                                            
         ST    RE,UNTCTF                                                        
         L     RE,UNTCTFC                                                       
         AR    RE,R5                                                            
         ST    RE,UNTCTFC                                                       
         L     RE,UNTCTFU                                                       
         AR    RE,R6                                                            
         ST    RE,UNTCTFU                                                       
*                                                                               
         XC    UNTCTS,UNTCTS       UNITS READ                                   
         XC    UNTCTC,UNTCTC       UNITS CHECKED                                
         XC    UNTCTCU,UNTCTCU     UNITS UPDTED                                 
         L     RE,CMLTADR                                                       
         L     RF,CMLTMAX                                                       
         SR    RF,RE                                                            
         XCEFL                                                                  
*                                                                               
*TEMP                                                                           
         MVC   P+1(9),=C'TOT VCML='                                             
         EDIT  (B4,CMLK),(10,P+10),0,COMMAS=YES                                 
         MVC   P+22(9),=C'CMLT ADD='                                            
         EDIT  (B4,CMLT),(10,P+31),0,COMMAS=YES                                 
         MVC   P+43(9),=C'CMLT FND='                                            
         EDIT  (B4,CMLTF),(10,P+53),0,COMMAS=YES                                
         MVC   P+65(8),=C'CML I/O='                                             
         EDIT  (B4,CMLR),(10,P+73),0,COMMAS=YES                                 
         MVC   P+85(8),=C'CML MAX='                                             
         EDIT  (B4,CMLTMX),(10,P+93),0,COMMAS=YES                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R0,4                                                             
         LA    RE,CMLK                                                          
         LA    RF,TCMLK                                                         
TEMP10   L     R1,0(,RE)                                                        
         A     R1,0(,RF)                                                        
         ST    R1,0(,RF)                                                        
         AHI   RE,4                                                             
         AHI   RF,4                                                             
         BCT   R0,TEMP10                                                        
         CLC   0(4,RF),0(RE)                                                    
         BNL   *+10                                                             
         MVC   0(4,RF),0(RE)                                                    
         XC    CMLKS,CMLKS                                                      
*TEMP                                                                           
CLTOTX   XIT1                                                                   
         DROP  RB,RC                                                            
         EJECT                                                                  
* PRINT AGENCY TOTALS *                                                         
*                                                                               
PAGYT    NMOD1 0,**PAGY**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         OC    UNTCTA,UNTCTA       ANY UNITS FOUND?                             
         BNZ   PAGYT20             YES                                          
         OC    UNTCTAC,UNTCTAC     ANY UNITS CHECKED?                           
         BZ    PAGYTX              ALL IS WELL                                  
         DC    H'0'                HOW CAN THERE BE CHECKED                     
*                                                                               
PAGYT20  DS    0H                                                               
*                                                                               
         MVC   P+6(18),=C'UNIT RECS FOR AGY='                                   
         EDIT  (4,UNTCTA),(9,P+26),COMMAS=YES                                   
         MVC   P+40(22),=C'UNITS CHECKED FOR AGY='                              
         EDIT  (4,UNTCTAC),(9,P+64),COMMAS=YES                                  
         MVC   P+75(22),=C'UNITS UPDATED FOR AGY='                              
         EDIT  (4,UNTCTAU),(9,P+98),COMMAS=YES                                  
         MVI   SPACING,2                                                        
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
*TMP                                                                            
         MVC   P+1(9),=C'TOT VCML='                                             
         EDIT  (B4,TCMLK),(10,P+10),0,COMMAS=YES                                
         MVC   P+22(9),=C'CMLT ADD='                                            
         EDIT  (B4,TCMLT),(10,P+31),0,COMMAS=YES                                
         MVC   P+43(9),=C'CMLT FND='                                            
         EDIT  (B4,TCMLTF),(10,P+53),0,COMMAS=YES                               
         MVC   P+65(8),=C'CML I/O='                                             
         EDIT  (B4,TCMLR),(10,P+73),0,COMMAS=YES                                
         MVC   P+85(8),=C'CML MAX='                                             
         EDIT  (B4,TCMLTMX),(10,P+93),0,COMMAS=YES                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   FORCEHED,C'Y'                                                    
         XC    UNTCTA,UNTCTA                                                    
         XC    UNTCTAC,UNTCTAC                                                  
         XC    UNTCTAU,UNTCTAU                                                  
*                                                                               
PAGYTX   XIT1                                                                   
         DROP  RB,RC                                                            
         EJECT                                                                  
* PRINT OVERALL TOTALS FOR FILE *                                               
*                                                                               
EOJ      NMOD1 0,*EOJTOT*                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         MVI   FORCEHED,C'Y'                                                    
         MVI   SPACING,2                                                        
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+7(19),=C'UNIT RECS FOR FILE='                                  
         EDIT  (4,UNTCTF),(9,P+26),COMMAS=YES                                   
         MVC   P+40(23),=C'UNITS CHECKED FOR FILE='                             
         EDIT  (4,UNTCTFC),(9,P+64),COMMAS=YES                                  
         MVC   P+75(23),=C'UNITS UPDATED FOR FILE='                             
         EDIT  (4,UNTCTFU),(9,P+98),COMMAS=YES                                  
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         DROP  RB,RC                                                            
         EJECT                                                                  
* HEAD HOOK ROUTINE FOR OFF-LINE REPORTS                                        
*                                                                               
HDHK     NMOD1 0,**HDHK**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         TM    FTRSW,TESTSW       TEST RUN                                      
         BZ    *+10                                                             
         MVC   H4+43(8),=CL8'**TEST**'                                          
*                                                                               
         GOTO1 DATCON,DMCB,(2,TODAYP),(11,H3+50)                                
*                                                                               
         ZIC   R0,SVBAGYMD                                                      
         SRL   R0,4                                                             
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  H5+10(2),DUB                                                     
*                                                                               
HDHKX    XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB,RC                                                            
         EJECT                                                                  
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,34,C'COMMERCIAL VALIDATION LIST'                              
         SSPEC H2,34,C'--------------------------'                              
         SSPEC H3,34,C'CKG UNITS AFTER'                                         
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,73,AGYADD                                                     
         SSPEC H5,3,C'AGENCY'                                                   
         SSPEC H5,85,RUN                                                        
         SSPEC H5,73,REPORT                                                     
         SSPEC H6,73,REQUESTOR                                                  
         SSPEC H6,103,PAGE                                                      
         SSPEC H9,3,C'CLT'                                                      
         SSPEC H10,3,C'---'                                                     
         SSPEC H9,7,C'NET'                                                      
         SSPEC H10,7,C'----'                                                    
         SSPEC H9,14,C'DATE'                                                    
         SSPEC H10,12,C'--------'                                               
         SSPEC H9,24,C'PROG'                                                    
         SSPEC H10,23,C'------'                                                 
         SSPEC H9,31,C'EST'                                                     
         SSPEC H10,31,C'---'                                                    
         SSPEC H9,35,C'SUB'                                                     
         SSPEC H10,35,C'---'                                                    
         SSPEC H9,39,C'PKG'                                                     
         SSPEC H10,39,C'---'                                                    
         SSPEC H9,43,C'D'                                                       
         SSPEC H10,43,C'P'                                                      
         SSPEC H9,45,C'DAY'                                                     
         SSPEC H10,45,C'--------'                                               
         SSPEC H9,54,C'TIME'                                                    
         SSPEC H10,54,C'-----------'                                            
         SSPEC H9,66,C'LEN'                                                     
         SSPEC H10,66,C'---'                                                    
         SSPEC H9,70,C'COMMERCIAL'                                              
         SSPEC H10,70,C'----------'                                             
         SSPEC H9,82,C'SOURCE'                                                  
         SSPEC H10,82,C'--------'                                               
         SSPEC H9,90,C'REASON'                                                  
         SSPEC H10,90,C'----------'                                             
*                                                                               
         DC    X'00'               END MARKER FOR SSPEC                         
*                                                                               
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE SPTRKEYS                                                       
         EJECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
         EJECT                                                                  
*        PRINT OFF                                                              
       ++INCLUDE DDREPMASTD                                                     
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPTRAFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAE7D                                                       
*        PRINT OFF                                                              
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
SPTR99RR DS    F                                                                
*                                                                               
COUNTERS DS    0F                                                               
UNTCTS   DS    F                   UNITS READ FOR CLIENT                        
UNTCTC   DS    F                   UNITS CHECKED FOR CLIENT                     
UNTCTCU  DS    F                   UNITS UPDATED CLIENT                         
*                                                                               
UNTCTA   DS    F                   UNITS READ FOR AGENCY                        
UNTCTAC  DS    F                   UNITS CHECKED FOR AGENCY                     
UNTCTAU  DS    F                   UNITS UPDATED AGENCY                         
*                                                                               
UNTCTF   DS    F                   UNITS READ FOR FILE                          
UNTCTFC  DS    F                   UNIT CHECKED FOR FILE                        
UNTCTFU  DS    F                   UNITS UPDATED FILE                           
ENDCTRS  EQU   *                                                                
CTRSIZ   EQU   (*-COUNTERS)/4                                                   
*                                                                               
CMLTADR  DS    F                                                                
CMLTMAX  DS    F                                                                
*                                                                               
SVUNTKEY DS    XL25                                                             
*                                                                               
WRTRECSW DS    XL1                 WRITE RECORD SWITCH                          
*                                                                               
TODAY    DS    CL6                                                              
TODAYP   DS    XL2                                                              
*                                                                               
FLDH     DS    CL8                                                              
FLD      DS    CL40                                                             
ENDATE   DS    XL3                                                              
ENDATEP  DS    XL2                                                              
PRTDATE  DS    CL8                                                              
SVBAGYMD DS    XL1                                                              
SVBCLT   DS    XL2                                                              
SVPROD1  DS    XL3 ----|  KEEP TOGETHER AND IN ORDER                            
SVPROD2  DS    XL3 ----|                                                        
SVCOPY   DS    CL1                 SET TO C IF C/SP FLAG ON AT NUSDST3          
SVCSPROD DS    XL6                                                              
SVCSUSED DS    XL6                                                              
SVCMLPOS DS    CL1                 1=CML1, 2=CML2, 3=CML3 B=SLIDE/COPY,         
*                                  C=COPY SPLIT                                 
SVAIRDAT DS    XL2                                                              
SVAIRDTP DS    XL3                                                              
SVAIRDTS DS    XL3                                                              
SVAIRDTE DS    XL3                                                              
SVTIME   DS    XL4                                                              
SVNET    DS    CL4                                                              
SVPROG   DS    CL6                                                              
SVEST    DS    XL1                                                              
SVSUB    DS    XL1                                                              
SVDP     DS    CL1                                                              
SVLENHLD DS    CL1                 LEN FROM UNIT                                
SVLEN    DS    CL1                 LEN FROM UNIT                                
SVLEN2   DS    CL1                 REMAINDER OF LEN IF 1ST COMML DOES           
*                                  NOT USE ALL LEN                              
SVPACK   DS    XL1                                                              
SVDAY    DS    XL1                                                              
SVSRC    DS    CL8                                                              
SVREASON DS    CL8                                                              
*                                                                               
CMLADIDF DS    CL1                 CML IS AD-ID Y/N                             
*                                                                               
*                 ---|                                                          
CKCMLCOD DS    CL8   | KEEP TOGETHER   8 CHAR ISCII                             
         DS    CL4   | AND IN ORDER    + 4 FOR AD-ID                            
*                 ---|                                                          
CKCMLPRO DS    XL3                                                              
CKCMLLEN DS    XL1                                                              
CKCMLOVR DS    XL2                                                              
CMLOKSW  DS    CL1                                                              
UNITSW   DS    CL1        IF N, MUST MARK UNIT TO FLAG FOR TRAF                 
*                                                                               
FILTERS  DS    0CL7                                                             
DATE     DS    CL6                                                              
FTRSW    DS    XL1                                                              
TESTSW   EQU   X'80'                                                            
*TEMP                                                                           
         DS    0F                                                               
CMLKS    DS   0XL20                                                             
CMLK     DS    F                   CML LOOK - CALL TO VCML                      
CMLT     DS    F                   CML TABLE ADD                                
CMLTF    DS    F                   CML TABLE FIND                               
CMLR     DS    F                   CML - DO I/O                                 
CMLTMX   DS    F                   LARGEST CML TABLE                            
TCMLK    DS    F                   CML LOOK - CALL TO VCML                      
TCMLT    DS    F                   CML TABLE ADD                                
TCMLTF   DS    F                   CML TABLE FIND                               
TCMLR    DS    F                   CML - DO I/O                                 
TCMLTMX DS     F                   LARGEST CML TABLE                            
*                                                                               
CMLTBL   DSECT                                                                  
CMLTENT  DS   0XL(CMLTNEXT-CMLTCML)                                             
CMLTCML  DS    CL8                                                              
CMLTSTA  DS    XL1                                                              
CMLTDEL  EQU   X'80'               DELETED                                      
CMLTMIS  EQU   X'40'               NO SUCH CODE ON FILE                         
CMLTSLN  DS    XL1                                                              
CMLTOVR  DS    XL2                 PRT OVERRIDE                                 
CMLTRLS  DS    XL3                 RELEASE DATE                                 
CMLTRCL  DS    XL3                 RECALL                                       
CMLTPROD DS    XL15                SAVED PROD LIST (5 PRODS)                    
         DS    XL3                                                              
CMLTNEXT EQU   *                                                                
*                                                                               
* OFFLINE REPORT                                                                
*                                                                               
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL1                                                              
PNET     DS    CL4                                                              
         DS    CL1                                                              
PDATE    DS    CL8                                                              
         DS    CL3                                                              
PPROG    DS    CL6                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PSUB     DS    CL3                                                              
         DS    CL1                                                              
PPACK    DS    CL3                                                              
         DS    CL1                                                              
PDAYPT   DS    CL1                                                              
         DS    CL1                                                              
PDAY     DS    CL8                                                              
         DS    CL1                                                              
PTIME    DS    CL11                                                             
         DS    CL1                                                              
PLEN     DS    CL3                                                              
         DS    CL1                                                              
PCOMML   DS    CL8                                                              
         DS    CL5                                                              
PSRC     DS    CL8                                                              
         DS    CL1                                                              
PREASON  DS    CL8                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020SPTRA99   07/22/09'                                      
         END                                                                    
