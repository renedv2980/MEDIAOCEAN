*          DATA SET SRWRK00    AT LEVEL 014 AS OF 10/30/13                      
*PHASE T15100A                                                                  
*INCLUDE SQUASHER                                                               
         TITLE '$WK - ROOT CONTROLLER'                                          
         PRINT NOGEN                                                            
WKCNTL   CSECT                                                                  
         NMOD1 WRKWKX-WRKWKD,**$WK0**,RR=R4                                     
         USING WRKWKD,RC                                                        
         LR    R2,R1                                                            
         USING SRPARMD,R2          R2=A(S/R PARAM LIST)                         
         L     R3,SRPARM6                                                       
         USING SRWRKFFD,R3         R3=A(TWA)                                    
         LR    R8,R3                                                            
         USING WRKSVD,R8           R8=A(TWA SAVE DATA)                          
         L     R9,SRPARM4                                                       
         USING COMFACSD,R9         R9=A(COMMON FACILITY LIST)                   
         NI    SRVP1H+6,X'BF'                                                   
         XC    MSG,MSG                                                          
         SPACE 1                                                                
INIT0    L     RF,SRPARM3          SET CURSOR LOCK                              
         USING UTLD,RF                                                          
         TM    TSVCREQ,X'01'                                                    
         BNO   *+8                                                              
         OI    TSVCREQ,X'02'                                                    
         DROP  RF                                                               
*                                                                               
INIT1    MVC   WKFILE,=CL8'WKFILE' SET FILE NAME FOR $WK                        
         CLC   SRVID+1(5),=C'FACWK'                                             
         BNE   *+10                                                             
         MVC   WKFILE,=CL8'FACWRK' SET FILE NAME FOR $FACWK                     
*&&US*&& CLC   SRVID+1(6),=C'EASIWK'                                            
*&&US*&& BNE   *+10                                                             
*&&US*&& MVC   WKFILE,=CL8'EASIWK' SET FILE NAME FOR $EASIWK                    
         EJECT                                                                  
**********************************************************************          
* Initialize                                                                    
**********************************************************************          
INIT2    ST    RB,ABASE            SAVE REGS AND INIT ADCONS                    
         ST    RC,AWORK                                                         
         ST    R2,APARM                                                         
         LA    R5,FIDXPND                                                       
         ST    R5,AFIDXPND                                                      
         L     R5,=V(SQUASHER)                                                  
         AR    R5,R4                                                            
         ST    R5,CSQUASH                                                       
         LA    R5,L'WKINDEX                                                     
         STH   R5,CINDXLN                                                       
         L     RE,SRPARM1                                                       
         USING SYSFACD,RE                                                       
         L     R5,VSSB                                                          
         MVC   SYSCH,SSBSYSCH-SSBD(R5)                                          
         L     R5,VENQDEQ                                                       
         ST    R5,CIENQDEQ                                                      
         L     R5,SRPARM2          USE TIA FOR CIREC BUFFER                     
         ST    R5,ACIREC                                                        
         ST    R5,AFILTAB                                                       
*                                                                               
         LHI   R5,WKFREC-WRKWKD    SET RECORD/BUFFER DATA                       
         AR    R5,RC                                                            
         ST    R5,AWKFREC                                                       
         LHI   R5,CXREC-WRKWKD                                                  
         AR    R5,RC                                                            
         ST    R5,ACXREC                                                        
         GOTO1 CDATAMGR,DMCB,=C'BUF',WKFILE,WKFNDX,AWKFREC,(R5)                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AWKTAB(52),0(R5)    INITIALISE AND SAVE WKTAB DATA               
*                                                                               
         MVI   DDS,0               SET DDS TERMINAL FLAG                        
         GOTO1 CGETFACT,DMCB,0                                                  
         L     RE,0(R1)                                                         
         USING FACTSD,RE                                                        
         TM    FATSTAT,X'60'                                                    
         BZ    *+8                                                              
         MVI   DDS,1                                                            
         MVC   DUB,FADATE          STORE TODAYS DATE                            
         GOTO1 CDATCON,DMCB,(4,DUB),(0,DATE)                                    
         GOTO1 (RF),(R1),,(1,DATE1)                                             
         SPACE 2                                                                
         L     RE,SRPARM3                                                       
         USING UTLD,RE                                                          
         MVC   USERID,TUSER                                                     
         MVC   TRM,TNUM                                                         
         DROP  RE                                                               
         EJECT                                                                  
**********************************************************************          
* PROCESS PARAMETER 1 (ACTION)                                                  
**********************************************************************          
P1VAL    LA    R4,SRVP1H           P1=ACTION (,RECNUM,RECSTR,RECEND)            
         USING FLDHDRD,R4                                                       
         CLI   FLDILEN,0                                                        
         BE    ERR1                MISSING ACTION                               
         XC    IFREC(8),IFREC                                                   
         MVI   IFREC+3,1           DEFAULT RECNUM TO ONE                        
         L     R6,ACIREC                                                        
         GOTO1 CSCANNER,DMCB,(R4),(5,(R6))                                      
         MVC   FLAG,4(R1)                                                       
         CLI   FLAG,0                                                           
         BE    ERR0                NUMBER/SYNTAX OF INPUT FIELDS                
*                                                                               
P1V1     CLI   1(R6),0             PART1 IS ACTION NAME                         
         BNE   ERR1A                                                            
         CLI   0(R6),3             ACTION IS 3 THRU 8 CHRS                      
         BL    ERR1A                                                            
         CLI   0(R6),8                                                          
         BH    ERR1A                                                            
         ZIC   R1,0(R6)                                                         
         BCTR  R1,0                                                             
         LA    R7,ACTNTBL                                                       
P1V1A    CLI   0(R7),0             SEARCH ACTION TABLE                          
         BE    ERR1A                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R6),0(R7)                                                   
         BE    P1V1B                                                            
         LA    R7,L'ACTNTBL(R7)                                                 
         B     P1V1A                                                            
P1V1B    MVC   ACTN,8(R7)          SAVE ACTION VALUE                            
         CLI   ACTN,ACTFDX                                                      
         BNH   *+12                                                             
         TM    DDS,X'01'           NON DISPLAY ACTIONS FOR DDS ONLY             
         BZ    ERR1A                                                            
         CLI   FLAG,1                                                           
         BE    P1VX                                                             
         TM    DDS,X'01'           MULTI FIELD INPUT FOR DDS ONLY               
         BZ    ERR1A                                                            
*                                                                               
P1V2     CLI   ACTN,ACTRD          PART2 IS REC NUM FOR REC DISP ACTION         
         BL    ERR1B                                                            
         CLI   ACTN,ACTRDX                                                      
         BH    ERR1B                                                            
         LA    R6,32(R6)                                                        
         CLI   1(R6),0                                                          
         BNE   ERR1B                                                            
         CLI   0(R6),0                                                          
         BE    ERR1B                                                            
         TM    2(R6),X'80'                                                      
         BZ    ERR1B                                                            
         CLC   4(4,R6),=F'1'                                                    
         BL    ERR1B                                                            
         CLI   4(R6),0                                                          
         BNE   ERR1B                                                            
         MVC   IFREC,4(R6)         SAVE REC NUM                                 
         CLI   FLAG,2                                                           
         BE    P1VX                                                             
*                                                                               
P1V3     LA    R6,32(R6)           PART3 IS REC START BYTE                      
         CLI   1(R6),0                                                          
         BNE   ERR1C                                                            
         CLI   0(R6),0                                                          
         BE    ERR1C                                                            
         TM    2(R6),X'80'                                                      
         BZ    ERR1C                                                            
         CLC   4(4,R6),MAXSTR                                                   
         BH    ERR1C                                                            
         MVC   IFSTR,6(R6)         SAVE REC START BYTE                          
         CLI   FLAG,3                                                           
         BE    P1VX                                                             
*                                                                               
P1V4     LA    R6,32(R6)           PART4 IS REC END BYTE                        
         CLI   1(R6),0                                                          
         BNE   ERR1D                                                            
         CLI   0(R6),0                                                          
         BE    ERR1D                                                            
         TM    2(R6),X'80'                                                      
         BZ    ERR1D                                                            
         CLC   4(4,R6),MAXEND                                                   
         BH    ERR1D                                                            
         MVC   IFEND,6(R6)         SAVE REC END BYTE                            
         CLC   IFEND,IFSTR                                                      
         BL    ERR1D                                                            
         CLI   FLAG,4                                                           
         BE    P1VX                                                             
*                                                                               
P1V5     B     ERR0                                                             
*                                                                               
P1VX     DC    0H'0'                                                            
         EJECT                                                                  
**********************************************************************          
* PROCESS PARAMETER 2 (KEY)                                                     
**********************************************************************          
P2VAL    LA    R4,SRVP2H           P2=(USERID,) FILEID (,SEQL,SEQH)             
         XC    IFDEFN,IFDEFN                                                    
         MVC   IFUSRID,USERID      DEFAULT USERID TO LOG ON ID                  
         MVC   IFFILNOX,=X'FFFF'                                                
         CLI   FLDILEN,0                                                        
         BNE   P2VALA                                                           
         OC    IFUSRID,IFUSRID                                                  
         BNZ   P2VX                                                             
         CLI   ACTN,ACTSIZ         NO INPUT REQUIRED FOR SIZE ACTION            
         BE    P2VX                                                             
         B     ERR2                NO INPUT AND NOT LOGGED ON                   
*                                                                               
P2VALA   L     R6,ACIREC                                                        
         GOTO1 CSCANNER,DMCB,(R4),(5,(R6))                                      
         MVC   FLAG,4(R1)                                                       
         CLI   FLAG,0                                                           
         BE    ERR0                NUMBER/SYNTAX OF INPUT FIELDS                
*                                                                               
P2V0     CLI   1(R6),0             PART0 IS U=XX.. FOR USER ID                  
         BNE   *+18                                                             
         OC    IFUSRID,IFUSRID     MUST BE LOGGED ON IF NO USER ID              
         BZ    ERR2                                                             
         B     P2V1                                                             
*                                                                               
         TM    DDS,X'01'           ONLY FOR DDS TERMINALS                       
         BZ    ERR2A                                                            
         OI    DDS,X'80'           SET USER ID INPUT FLAG                       
         CLI   12(R6),C'U'                                                      
         BE    *+24                                                             
         CLI   12(R6),C'T'                                                      
         BNE   ERR2A                                                            
         OI    DDS,X'40'           SET TOTALS FLAG                              
         CLI   ACTN,ACTDIS                                                      
         BNE   ERR2A               TOTALS ONLY FOR DISPLAY                      
         XC    IFUSRID,IFUSRID                                                  
         CLI   1(R6),3                                                          
         BL    ERR2A                                                            
         BH    *+14                                                             
         CLC   22(3,R6),=C'ALL'    U=ALL FOR ALL USERS                          
         BE    P2V0C                                                            
         TM    3(R6),X'80'         U=NNNN IS VALID                              
         BZ    *+14                                                             
         MVC   IFUSRID,10(R6)                                                   
         B     P2V0C                                                            
         CLI   22(R6),C'+'                                                      
         BNE   P2V0A                                                            
         OI    DDS,X'20'           SET PLUS ID FLAG                             
         MVC   22(9,R6),23(R6)                                                  
         MVI   31(R6),C' '                                                      
*                                                                               
P2V0A    L     R5,ACXREC           READ USER ID REC                             
         USING CTIREC,R5                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,22(R6)                                                    
         GOTO1 CDATAMGR,DMCB,DMREAD,CTFILE,(R5),(R5)                            
         CLI   8(R1),0                                                          
         BNE   ERR2A                                                            
         LA    R5,CTIDATA                                                       
         SR    RE,RE                                                            
P2V0B    CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   0(R5),2                                                          
         BE    *+14                                                             
         IC    RE,1(R5)                                                         
         AR    R5,RE                                                            
         B     P2V0B                                                            
         MVC   IFUSRID,2(R5)                                                    
*                                                                               
P2V0C    SR    RF,RF               DROP ZEROTH FIELD                            
         IC    RF,FLAG                                                          
         SH    RF,=H'1'                                                         
         BZ    P2VX                                                             
         STC   RF,FLAG                                                          
         LA    R6,32(R6)                                                        
*                                                                               
P2V1     CLI   1(R6),0             PART1 IS REPORT ID SPPXDDCX                  
         BNE   ERR2B                                                            
         CLI   0(R6),0                                                          
         BE    ERR2B                                                            
         CLI   0(R6),8                                                          
         BH    ERR2B                                                            
         LA    R5,12(R6)                                                        
         LA    R0,8                                                             
         CLI   0(R5),C'*'          REPLACE *'S BY NULL'S                        
         BNE   *+8                                                              
         MVI   0(R5),0                                                          
         LA    R5,1(R5)                                                         
         BCT   R0,*-16                                                          
*                                                                               
         LA    R5,12(R6)                                                        
P2V1A    CLI   0(R6),3             FILEID=ALL IS OK                             
         BNE   P2V1B                                                            
         CLC   0(3,R5),=C'ALL'                                                  
         BE    P2V1X                                                            
P2V1B    MVC   IFSYSPRG(1),0(R5)                                                
         CLI   0(R6),2                                                          
         BL    P2V1X               FILEID=S IS OK                               
         BE    ERR2B                                                            
         MVC   IFSYSPRG+1(2),1(R5)                                              
         OC    IFSYSPRG+1(2),IFSYSPRG+1                                         
         BZ    P2V1C                                                            
         CLI   IFSYSPRG+1,0                                                     
         BE    ERR2B                                                            
         CLI   IFSYSPRG+2,0                                                     
         BE    ERR2B                                                            
P2V1C    CLI   0(R6),4             FILEID=SPP IS OK (ALSO S**)                  
         BL    P2V1X                                                            
         MVC   IFSUBPRG,3(R5)                                                   
         BE    P2V1X               FILEID=SPPX IS OK                            
         CLI   0(R6),6                                                          
         BL    ERR2B                                                            
P2V1D    OC    4(2,R5),4(R5)                                                    
         BZ    P2V1E                                                            
         MVC   DUB(2),=C'00'                                                    
         MVZ   DUB(2),4(R5)                                                     
         CLC   DUB(2),=C'00'                                                    
         BNE   ERR2B                                                            
         PACK  DUB(2),4(3,R5)                                                   
         MVC   IFDAY,DUB                                                        
P2V1E    CLI   0(R6),7             FILEID=SPPXDD IS OK                          
         BL    P2V1X                                                            
         MVC   IFCLASS,6(R5)       FILEID=SPPXDDC IS OK                         
         CLI   0(R6),8                                                          
         BL    P2V1X                                                            
         MVC   IFEXTRA,7(R5)       FILEID=SPPXDDCX IS OK                        
*                                                                               
P2V1X    CLI   FLAG,1                                                           
         BE    P2VX                                                             
*                                                                               
P2V2     LA    R6,32(R6)           PART2 IS FILE SEQUENCE NUMBER                
         CLI   1(R6),0                                                          
         BNE   ERR2C                                                            
         CLI   0(R6),0                                                          
         BE    ERR2C                                                            
         TM    2(R6),X'80'                                                      
         BZ    ERR2C                                                            
         CLC   4(4,R6),=F'1'                                                    
         BL    ERR2C                                                            
         CLC   4(4,R6),MAXSEQ                                                   
         BH    ERR2C                                                            
         MVC   IFFILNO,6(R6)                                                    
         CLI   FLAG,2                                                           
         BH    P2V3                                                             
         OC    IFUSRID,IFUSRID     CANT BE VAGUE FOR SPECIFIC SEQ NUM           
         BZ    ERR2C                                                            
         OC    IFSYSPRG,IFSYSPRG                                                
         BZ    ERR2C                                                            
         MVC   IFFILNOX,IFFILNO    SET SPECIFIC FILE SEQUENCE NUMBER            
         B     P2VX                                                             
*                                                                               
P2V3     LA    R6,32(R6)           PART3 IS FILE HIGH SEQ NUM                   
         CLI   1(R6),0                                                          
         BNE   ERR2C                                                            
         CLI   0(R6),0                                                          
         BE    ERR2C                                                            
         TM    2(R6),X'80'                                                      
         BO    P2V3A                                                            
         CLI   0(R6),3                                                          
         BNE   ERR2C                                                            
         CLC   12(3,R6),=C'END'                                                 
         BE    P2V3B                                                            
         CLC   12(3,R6),=C'MAX'                                                 
         BNE   ERR2C                                                            
         MVC   IFFILNOX,MAXSEQ+2                                                
         B     P2V3B                                                            
P2V3A    CLC   4(4,R6),=F'1'                                                    
         BL    ERR2C                                                            
         CLC   4(4,R6),MAXSEQ                                                   
         BH    ERR2C                                                            
         MVC   IFFILNOX,6(R6)                                                   
         CLC   IFFILNOX,IFFILNO                                                 
         BL    ERR2C                                                            
P2V3B    CLI   FLAG,3                                                           
         BE    P2VX                                                             
*                                                                               
P2V4     B     ERR0                                                             
*                                                                               
P2VX     CLI   ACTN,ACTCHA         INDIV FILE FOR CHANGE ACTION                 
         BE    P2VX1                                                            
         CLI   ACTN,ACTRD          INDIV FILE FOR RECORD ACTIONS                
         BL    *+12                                                             
         CLI   ACTN,ACTRDX                                                      
         BNH   P2VX1                                                            
         B     P2VXX                                                            
*                                                                               
P2VX1    OC    IFUSRID,IFUSRID     CHECK FOR INDIVIDUAL FILE                    
         BZ    ERR2B                                                            
         CLC   IFFILNO,IFFILNOX                                                 
         BNE   ERR2B                                                            
*                                                                               
P2VXX    DC    0H'0'                                                            
         EJECT                                                                  
**********************************************************************          
* PROCESS PARAMETER 3 (FILTERS)                                                 
**********************************************************************          
P3VAL    LA    R4,SRVP3H           P3=FILTERS NAMED BY KEYWORDS                 
         XC    IFFILTS(IFFILTL),IFFILTS                                         
         MVI   IFFMTV,2            SET DEFAULT FORMAT                           
         CLI   FLDILEN,0                                                        
         BE    P3VXX                                                            
         L     R6,ACIREC                                                        
         GOTO1 CSCANNER,DMCB,(R4),(8,(R6))                                      
         MVC   FLAG,4(R1)                                                       
         CLI   FLAG,0                                                           
         BE    ERR0                NUMBER/SYNTAX OF INPUT FIELDS                
         ZIC   R0,FLAG                                                          
*                                                                               
P3V0     CLI   1(R6),0             FORMAT MUST BE KEYWORD=VALUE                 
         BE    ERR0                                                             
         ZIC   R1,0(R6)                                                         
         LTR   R1,R1                                                            
         BZ    ERR0                                                             
         MVI   FLAG1,X'80'         SET DEFAULT EQ SIGN                          
         LA    RE,11(R6,R1)                                                     
         CLI   0(RE),X'4C'         CHECK AND SET LT SIGN                        
         BNE   *+8                                                              
         MVI   FLAG1,X'D0'                                                      
         CLI   0(RE),X'6E'         CHECK AND SET GT SIGN                        
         BNE   *+8                                                              
         MVI   FLAG1,X'B0'                                                      
         CLI   0(RE),X'61'         CHECK AND SET NE SIGN                        
         BNE   *+8                                                              
         MVI   FLAG1,X'70'                                                      
         CLI   FLAG1,X'80'         ADJUST LEN IF SIGN VALUE LAST CHR            
         BE    *+14                                                             
         MVI   0(RE),C' '                                                       
         BCTR  R1,0                                                             
         STC   R1,0(R6)                                                         
         CLI   0(R6),3             KEYWORD MUST BE 3 THRU 8 CHRS LONG           
         BL    ERR3                                                             
         CLI   0(R6),8                                                          
         BH    ERR3                                                             
         BCTR  R1,0                                                             
         LA    R7,FILTTBL                                                       
*                                                                               
P3V1     CLI   0(R7),0             SEARCH FILTER NAME TABLE                     
         BE    ERR3                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R6),0(R7)                                                   
         BE    P3V2                                                             
         LA    R7,L'FILTTBL(R7)                                                 
         B     P3V1                                                             
*                                                                               
P3V2     CLI   FLAG1,X'80'         TEST IF SIGN VALUE ALLOWED                   
         BE    P3V3                                                             
         CLI   FLAG1,X'70'         TEST NE ALLOWED                              
         BNE   *+12                                                             
         TM    9(R7),X'80'                                                      
         BZ    ERR3A                                                            
         CLI   FLAG1,X'D0'         TEST LT ALLOWED                              
         BNE   *+12                                                             
         TM    9(R7),X'40'                                                      
         BZ    ERR3A                                                            
         CLI   FLAG1,X'B0'         TEST GT ALLOWED                              
         BNE   *+12                                                             
         TM    9(R7),X'20'                                                      
         BZ    ERR3A                                                            
*                                                                               
P3V3     ZIC   RF,8(R7)            GOTO ROUTINE FOR VALUE                       
         SLL   RF,2                                                             
         B     P3ROUTS(RF)         R6=A(SCANNER TABLE ENTRY)                    
*                                                                               
P3VX     LA    R6,32(R6)           BACK FOR NEXT KEYWORD                        
         BCT   R0,P3V0                                                          
P3VXX    B     LOADSCR                                                          
*                                                                               
P3ROUTS  B     ERR3                VALIDATION ROUTINES                          
         B     CLAVALR                                                          
         B     DATVALR                                                          
         B     KEYVALR                                                          
         B     NCIVALR                                                          
         B     SORVALR                                                          
         B     STAVALR                                                          
         B     FMTVALR                                                          
         B     ERR3                                                             
         B     ERR3                                                             
         B     ERR3                                                             
         B     ERR3                                                             
         B     ERR3                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE CLASS                                                                
***********************************************************************         
CLAVALR  MVC   IFCLAF,FLAG1        SET CLASS FILTER INPUT FLAG                  
         LA    R7,IFCLAV                                                        
         CLI   1(R6),8             MAX OF 8 CLASSES                             
         BH    CLAVW                                                            
         ZIC   RF,1(R6)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     CLAVX                                                            
         MVC   0(0,R7),22(R6)      SET LIST OF CLASSES                          
CLAVW    B     ERR3B                                                            
CLAVX    B     P3VX                                                             
                                                                                
***********************************************************************         
* VALIDATE DATE                                                                 
***********************************************************************         
DATVALR  MVC   IFDATF,FLAG1        SET DATE FILTER INPUT FLAG                   
         LA    R7,IFDATV                                                        
         CLI   1(R6),5                                                          
         BNE   DATV1                                                            
         CLC   22(5,R6),=C'TODAY'  CHECK FOR TODAYS DATE                        
         BNE   DATV1                                                            
         MVC   0(3,R7),DATE1                                                    
         B     DATVX                                                            
*                                                                               
DATV1    GOTO1 CDATVAL,DMCB,(0,22(R6)),DUB                                      
         OC    0(4,R1),0(R1)                                                    
         BNZ   DATV3                                                            
DATV2    GOTO1 (RF),(R1),(1,22(R6))                                             
         OC    0(4,R1),0(R1)                                                    
         BZ    DATVW                                                            
         MVC   DUB(2),DATE                                                      
DATV3    GOTO1 CDATCON,DMCB,(0,DUB),(1,(R7))                                    
         B     DATVX                                                            
DATVW    B     ERR3B                                                            
DATVX    B     P3VX                                                             
                                                                                
***********************************************************************         
* VALIDATE FORMAT FILTER                                                        
***********************************************************************         
FMTVALR  MVC   IFFMTF,FLAG1        SET FORMAT FILTER INPUT FLAG                 
         LA    R7,IFFMTV                                                        
         TM    3(R6),X'80'         MUST BE INTEGER 1 THRU 2                     
         BZ    FMTVW                                                            
         L     RE,8(R6)                                                         
         LTR   RE,RE                                                            
         BZ    FMTVW                                                            
         CH    RE,=H'2'                                                         
         BH    FMTVW                                                            
         STC   RE,0(R7)                                                         
         B     FMTVX                                                            
FMTVW    B     ERR3B                                                            
FMTVX    B     P3VX                                                             
                                                                                
***********************************************************************         
* VALIDATE KEY                                                                  
***********************************************************************         
KEYVALR  MVC   IFKEYF,FLAG1        SET KEY VALUE INPUT FLAG                     
         LA    R7,IFKEYV                                                        
         CLI   1(R6),7             MUST BE FULL SPPXDDC(X) KEY                  
         BL    KEYVW                                                            
         LA    R5,22(R6)                                                        
         LA    R1,8                                                             
         CLI   0(R5),C'*'          REPLACE *'S BY NULL'S                        
         BNE   *+8                                                              
         MVI   0(R5),0                                                          
         LA    R5,1(R5)                                                         
         BCT   R1,*-16                                                          
*                                                                               
         LA    R5,22(R6)                                                        
KEYV1    MVC   0(2,R7),IFUSRID                                                  
         OC    0(2,R7),0(R7)                                                    
         BZ    KEYVW               MUST BE SPECIFIC USER ID                     
         MVC   2(4,R7),0(R5)                                                    
         CLI   2(R7),0                                                          
         BE    KEYVW               MUST BE SPECIFIC SYSTEM                      
KEYV2    MVI   6(R7),0             DAY MUST BE NUMERIC                          
         OC    4(2,R5),4(R5)                                                    
         BZ    KEYV3                                                            
         MVC   DUB(2),=C'00'                                                    
         MVZ   DUB(2),4(R5)                                                     
         CLC   DUB(2),=C'00'                                                    
         BNE   KEYVW                                                            
         PACK  DUB(2),4(3,R5)                                                   
         MVC   6(1,R7),DUB         DATE                                         
KEYV3    MVC   7(1,R7),6(R5)       CLASS                                        
         CLI   1(R6),7             MUST BE FULL SPPXDDC(X) KEY                  
         BE    *+10                                                             
         MVC   8(1,R7),7(R5)                                                    
         CLC   IFKEYV,IFUSRID      NEW KEY CANT BE SAME AS OLD KEY              
         BNE   KEYVX                                                            
KEYVW    B     ERR3B                                                            
KEYVX    B     P3VX                                                             
                                                                                
***********************************************************************         
* Validate Number of CIs                                                        
***********************************************************************         
NCIVALR  MVC   IFNCIF,FLAG1        SET NCI'S FILTER INPUT FLAG                  
         LA    R7,IFNCIV                                                        
         TM    3(R6),X'80'         MUST BE INTEGER 0 THRU 255                   
         BZ    NCIVW                                                            
         L     RE,8(R6)                                                         
         LTR   RE,RE                                                            
         BM    NCIVW                                                            
         CH    RE,=H'255'                                                       
         BH    NCIVW                                                            
         STC   RE,0(R7)                                                         
         B     NCIVX                                                            
NCIVW    B     ERR3B                                                            
NCIVX    B     P3VX                                                             
                                                                                
***********************************************************************         
* Validate Sort option                                                          
***********************************************************************         
SORVALR  MVC   IFSORF,FLAG1        SET SORT FILTER INPUT FLAG                   
         LA    R7,IFSORV                                                        
         CLI   1(R6),1                                                          
         BNE   SORVW                                                            
         CLI   22(R6),C'A'         ALPHA ON KEY                                 
         BE    SORV1                                                            
         CLI   22(R6),C'D'         NUMERIC DESCENDING                           
         BE    SORV1                                                            
         CLI   22(R6),C'N'         NUMERIC ASCENDING                            
         BE    SORV1                                                            
         B     SORVW                                                            
SORV1    MVC   0(1,R7),22(R6)                                                   
         B     SORVX                                                            
SORVW    B     ERR3B                                                            
SORVX    B     P3VX                                                             
                                                                                
***********************************************************************         
* Validate Status values                                                        
***********************************************************************         
STAVALR  MVC   IFSTAF,FLAG1        SET STATUS FILTER INPUT FLAG                 
         LA    R7,IFSTAV                                                        
         CLI   1(R6),4                                                          
         BH    STAVW                                                            
         ZIC   RF,1(R6)                                                         
         LA    RE,22(R6)                                                        
STAV1    CLI   0(RE),C'A'                                                       
         BNE   *+12                                                             
         OI    0(R7),WKSTAC                                                     
         B     STAV2                                                            
         CLI   0(RE),C'H'                                                       
         BNE   *+12                                                             
         OI    0(R7),WKSTHO                                                     
         B     STAV2                                                            
         CLI   0(RE),C'L'                                                       
         BNE   *+12                                                             
         OI    0(R7),WKSTAC+WKSTHO                                              
         B     STAV2                                                            
         CLI   0(RE),C'K'                                                       
         BNE   *+12                                                             
         OI    0(R7),WKSTKE                                                     
         B     STAV2                                                            
         CLI   0(RE),C'D'                                                       
         BNE   *+12                                                             
         OI    0(R7),WKSTUS                                                     
         B     STAV2                                                            
         B     STAVW                                                            
STAV2    LA    RE,1(RE)                                                         
         BCT   RF,STAV1                                                         
         B     STAVX                                                            
STAVW    B     ERR3B                                                            
STAVX    B     P3VX                                                             
         EJECT                                                                  
***********************************************************************         
* LOAD OVERLAY SCREEN FOR ACTION                                                
***********************************************************************         
LOADSCR  XC    DMCB(12),DMCB       LOAD OVERLAY SCREEN FOR ACTION               
         LA    R6,SRVFCAH                                                       
         ST    R6,DMCB                                                          
         MVC   DMCB+4(4),WKOLAY                                                 
         CLI   ACTN,ACTRD                                                       
         BNL   LOADPGM                                                          
         GOTO1 CCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
***********************************************************************         
* LOAD OVERLAY PROGRAM FOR ACTION                                               
***********************************************************************         
LOADPGM  IC    R0,ACTN             LOAD OVERLAY PHASE                           
         SRL   R0,4                                                             
         SLL   R0,28                                                            
         SRL   R0,4                                                             
         GOTO1 CCALLOV,DMCB,(R0),(R3)                                           
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,0(R1)            PASS CONTROL TO OVERLAY                      
         LR    R1,RC               R1=A(ROOT WORKING STORAGE)                   
         BASR  RE,RF                                                            
EXIT     XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
* Error messages                                                                
***********************************************************************         
ERR0     MVC   MSG(26),=C'Invalid input field syntax'                           
         B     ERRX                                                             
ERR1     MVC   MSG(12),=C'Enter action'                                         
         MVC   SRVMSG,MSG                                                       
         B     ERRX1                                                            
ERR1A    MVC   MSG(14),=C'Invalid action'                                       
         B     ERRX                                                             
ERR1B    MVC   MSG(21),=C'Invalid record number'                                
         B     ERRX                                                             
ERR1C    MVC   MSG(25),=C'Invalid record start byte'                            
         B     ERRX                                                             
ERR1D    MVC   MSG(23),=C'Invalid record end byte'                              
         B     ERRX                                                             
ERR2     MVC   MSG(33),=C'Not logged on or missing U=userid'                    
         B     ERRX                                                             
ERR2A    MVC   MSG(15),=C'Invalid user id'                                      
         B     ERRX                                                             
ERR2B    MVC   MSG(15),=C'Invalid file id'                                      
         B     ERRX                                                             
ERR2C    MVC   MSG(28),=C'Invalid file sequence number'                         
         B     ERRX                                                             
ERR3     MVC   MSG(23),=C'Invalid filter keyword='                              
         MVC   MSG+23(10),12(R6)                                                
         B     ERRX                                                             
ERR3A    MVC   MSG(28),=C'Invalid filter sign keyword='                         
         MVC   MSG+28(10),12(R6)                                                
         B     ERRX                                                             
ERR3B    MVC   MSG(33),=C'Invalid value for filter keyword='                    
         MVC   MSG+33(10),12(R6)                                                
         B     ERRX                                                             
*                                                                               
ERRX     XC    SRVMSG,SRVMSG                                                    
         MVC   SRVMSG(8),=C'ED/9999 '                                           
         MVC   SRVMSG+2(1),SYSCH                                                
         MVC   SRVMSG+8(48),MSG                                                 
ERRX1    OI    SRVMSGH+6,X'80'                                                  
         OI    6(R4),X'40'         POSN CURSOR TO ERROR FIELD                   
         LA    R4,SRVFCAH                                                       
         LH    RE,2(R4)            TRUNCATE SCREEN WITH TAB FIELD               
         LA    RE,9(RE)                                                         
         MVC   0(12,R4),=X'090000000000800100000100'                            
         STH   RE,2(R4)                                                         
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* ROUTINE TO EXPAND                                                             
* 1) USER ID                                                                    
* 2) KEY OF WORKER FILE                                                         
**********************************************************************          
FIDXPND  NTR1  BASE=ABASE          EXPAND FILE ID AT FIDEFN TO FILIDA           
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         LA    R4,FILIDA           CLEAR OUTPUT AREA                            
         MVC   FILIDA,SPACES                                                    
*                                                                               
FIDX2    OC    FIUSRID,FIUSRID     EXPAND USERID                                
         BZ    FIDX6                                                            
         L     R5,AWKFREC                                                       
         USING CTIREC,R5                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),FIUSRID                                              
         GOTO1 CDATAMGR,DMCB,DMREAD,CTFILE,(R5),(R5)                            
         CLI   8(R1),0                                                          
         BE    FIDX4                                                            
FIDX3    L     R5,AWKFREC          OUTPUT BAD USERID AS NNNN                    
         MVC   0(12,R5),SPACES                                                  
         LH    RF,FIUSRID                                                       
         CVD   RF,DUB                                                           
         UNPK  2(4,R5),DUB                                                      
         OI    5(R5),X'F0'                                                      
         B     FIDX4B                                                           
FIDX4    LA    R5,CTIDATA                                                       
         SR    R6,R6                                                            
FIDX4A   CLI   0(R5),0                                                          
         BE    FIDX3                                                            
         CLI   0(R5),X'02'                                                      
         BE    *+14                                                             
         IC    R6,1(R5)                                                         
         AR    R5,R6                                                            
         B     FIDX4A                                                           
FIDX4B   MVC   0(10,R4),2(R5)                                                   
         MVC   LUSRID,FIUSRID                                                   
         MVC   LUSRIDA,2(R5)                                                    
         LA    R4,11(R4)                                                        
**********************************************************************          
* Expand worker key                                                             
**********************************************************************          
FIDX6    OC    FISYSPRG(6),FISYSPRG                                             
         BZ    FIDX8                                                            
         MVC   0(4,R4),FISYSPRG                                                 
         OC    0(4,R4),SPACES                                                   
         LR    RE,R4                                                            
         LA    RF,4                                                             
         CLI   0(RE),C' '                                                       
         BNE   *+8                                                              
         MVI   0(RE),C'*'                                                       
         LA    RE,1(RE)                                                         
         BCT   RF,*-16                                                          
         MVI   4(R4),C'0'                                                       
         MVI   5(R4),C'0'                                                       
         CLI   FIDAY,X'01'                                                      
         BL    *+18                                                             
         CLI   FIDAY,X'99'                                                      
         BH    *+10                                                             
         UNPK  4(3,R4),FIDAY(2)                                                 
         MVI   6(R4),C'*'                                                       
         CLI   FICLASS,C' '                                                     
         BNE   *+8                                                              
         MVI   6(R4),C' '                                                       
         CLI   FICLASS,C'A'                                                     
         BL    *+10                                                             
         MVC   6(1,R4),FICLASS                                                  
         CLI   FIEXTRA,C' '                                                     
         BNH   *+10                                                             
         MVC   7(1,R4),FIEXTRA                                                  
         LA    R4,9(R4)                                                         
*                                                                               
FIDX8    OC    FIFILNO,FIFILNO                                                  
         BZ    FIDXA                                                            
         EDIT  (B2,FIFILNO),(4,(R4))                                            
         LA    R4,5(R4)                                                         
*                                                                               
FIDXA    OC    FILIDA,SPACES                                                    
         CLC   FILIDA,SPACES                                                    
         BE    FIDXX                                                            
         LA    R0,L'FILIDA                                                      
         GOTO1 CSQUASH,DMCB,FILIDA,(C',',(R0))                                  
*                                                                               
FIDXX    XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* Constants                                                                     
**********************************************************************          
MAXSEQ   DC    F'9999'                                                          
MAXSTR   DC    F'4122'             2+24+4096                                    
MAXEND   DC    F'4122'                                                          
DMREAD   DC    CL8'DMREAD'                                                      
CTFILE   DC    CL8'CTFILE'                                                      
WKOLAY   DC    XL4'D90151FE'                                                    
SPACES   DC    CL40' '                                                          
                                                                                
**********************************************************************          
* Action table                                                                  
**********************************************************************          
ACTNTBL  DS    0CL9                ACTION NAMES AND VALUES                      
*                                                                               
         DC    C'ACTIVATE',X'11'                                                
         DC    C'ACTV    ',X'11'                                                
         DC    C'CHANGE  ',X'12'                                                
         DC    C'DELETE  ',X'13'                                                
         DC    C'DISPLAY ',X'14'                                                
         DC    C'HOLD    ',X'15'                                                
         DC    C'KEEP    ',X'16'                                                
         DC    C'PURGE   ',X'17'                                                
         DC    C'SIZE    ',X'18'                                                
         DC    C'UNKEEP  ',X'19'                                                
*                                                                               
         DC    C'RDISPLAY',X'21'                                                
         DC    C'RCHANGE ',X'22'                                                
*                                                                               
         DC    X'00'                                                            
**********************************************************************          
* Filter table                                                                  
**********************************************************************          
FILTTBL  DS    0CL10               FILTER NAMES AND VALUES                      
*                                  X'80'=NE,X'40'=LT,X'20'=GT                   
         DC    C'CLASS   ',X'0180'                                              
         DC    C'DATE    ',X'02E0'                                              
         DC    C'KEY     ',X'0300'                                              
         DC    C'NCIS    ',X'04E0'                                              
         DC    C'SORT    ',X'0500'                                              
         DC    C'STATUS  ',X'0680'                                              
         DC    C'FORMAT  ',X'0700'                                              
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*SRWRKWK                                                                        
       ++INCLUDE SRWRKWK                                                        
         EJECT                                                                  
SRWRKFFD DSECT                                                                  
         DS    CL64                                                             
*SRWRKFFD                                                                       
       ++INCLUDE SRWRKFFD                                                       
         EJECT                                                                  
*DMWRKRD                                                                        
       ++INCLUDE DMWRKRD                                                        
         EJECT                                                                  
*FAFACTS                                                                        
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
*DDCOMFACS                                                                      
       ++INCLUDE DDCOMFACS                                                      
                                                                                
*FATWA                                                                          
       ++INCLUDE FATWA                                                          
         EJECT                                                                  
*FASYSFAC                                                                       
       ++INCLUDE FASYSFAC                                                       
                                                                                
*FASRPARM                                                                       
       ++INCLUDE FASRPARM                                                       
         EJECT                                                                  
*FASSB                                                                          
       ++INCLUDE FASSB                                                          
         EJECT                                                                  
*FAUTL                                                                          
       ++INCLUDE FAUTL                                                          
         EJECT                                                                  
*DDFLDHDR                                                                       
       ++INCLUDE DDFLDHDR                                                       
                                                                                
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014SRWRK00   10/30/13'                                      
         END                                                                    
