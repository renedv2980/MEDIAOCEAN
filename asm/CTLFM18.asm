*          DATA SET CTLFM18    AT LEVEL 003 AS OF 05/01/02                      
*PHASE TA0218A                                                                  
         TITLE 'CTLFM18 - CONTROL FILE MAINT - SCREEN MAP RECORDS'              
CTLFM18  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,**LFMO**,RR=RE                                       
         USING WORKD,RC            RC=A(TEMP W/S)                               
         ST    RE,MYRELO                                                        
         LR    R9,R1                                                            
         USING LFMTEMP,R9          R9=A(GLOBAL W/S)                             
         L     R3,ATWA                                                          
         USING LFMSAVE,R3          R3=A(SAVE W/S)                               
         LR    R2,R3                                                            
         USING CTLFMFFD,R2         R2=A(TWA)                                    
         L     R4,AREC                                                          
         USING CTMREC,R4           R4=A(RECORD)                                 
         EJECT                                                                  
* VALIDATE KEY FIELDS & READ RECORD                                             
*                                                                               
KEYVAL   XC    CTMKEY,CTMKEY                                                    
         MVI   CTMKTYP,C'M'                                                     
*                                  VALIDATE SYSTEM                              
         GOTO1 AFVAL,MAPSYSH                                                    
         BZ    EXIT                                                             
         L     R5,ASYSTBL          R5=A(SYSTEM TABLE)                           
         USING SYSLSTD,R5                                                       
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
KEYV2    CLI   SYSLNUM,0           END OF TABLE                                 
         BE    EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),SYSLNAME                                                  
         BE    *+12                                                             
         LA    R5,SYSLLEN(R5)                                                   
         B     KEYV2                                                            
         CLC   FLD(L'SYSLNAME),SYSLNAME                                         
         BE    *+14                                                             
         MVC   MAPSYS(L'SYSLNAME),SYSLNAME                                      
         OI    MAPSYSH+6,X'80'                                                  
         MVC   CTMKSYS,SYSLNUM     SET SYSTEM NUMBER FROM TABLE                 
*                                  FIND AN SELIST ENTRY FOR SYSTEM              
         L     RE,AFACLIST                                                      
         USING SYSFACD,RE                                                       
         L     R5,VSELIST                                                       
         DROP  RE                                                               
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING SELISTD,R5                                                       
         CLC   CTMKSYS,SEOVSYS                                                  
         BE    *+12                                                             
         BXLE  R5,R6,*-10                                                       
         B     EIIF                                                             
*                                  VALIDATE PROGRAM                             
         L     R5,SEPGMS           A(PGMLST) FROM SELIST                        
         LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         USING PGMLSTD,R5                                                       
         GOTO1 AFVAL,MAPPROGH                                                   
         BZ    EXIT                                                             
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
KEYV4    EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   PGMNAME(0),FLD                                                   
         BE    *+12                                                             
         BXLE  R5,R6,KEYV4                                                      
         B     EIIF                                                             
         CLC   PGMNAME,FLD                                                      
         BE    *+14                                                             
         MVC   MAPPROG,PGMNAME                                                  
         OI    MAPPROGH+6,X'80'                                                 
         MVC   CTMKPROG,PGMNUM     SET PROGRAM FROM PGMLST                      
         DROP  R5                                                               
*                                  VALIDATE SCREEN NUMBER                       
         GOTO1 AFVAL,MAPSCRNH                                                   
         BZ    EXIT                                                             
         CLI   FLDH+5,2            MUST BE 2 HEX CHRS BETWEEN X'AA-FF'          
         BL    EFTS                                                             
         TM    FLDH+4,X'02'                                                     
         BZ    EFNH                                                             
         GOTO1 VHEXIN,DMCB,FLD,CTMKSCRN,2                                       
         CLI   CTMKSCRN,X'AA'                                                   
         BL    EIIF                                                             
*                                  VALIDATE USER-ID                             
         GOTO1 AFVAL,MAPUSERH                                                   
         BZ    KEYV6                                                            
         CLI   FLDH+5,2                                                         
         BL    EFTS                                                             
         MVC   CTMKUSER,FLD                                                     
         BE    KEYV6                                                            
         CLI   FLDH+5,4                                                         
         BL    EIIF                                                             
         CLI   FLD+2,C','                                                       
         BNE   EIIF                                                             
         MVC   CTMKOTHR,FLD+3                                                   
*                                                                               
KEYV6    MVC   KEY,CTMKEY                                                       
         MVC   KEYNEXT,KEY                                                      
         LA    R1,MAPSYSH                                                       
         ST    R1,FADR                                                          
*                                                                               
         CLI   ACTN,CHANGE         IF ACTN=CHANGE AND KEY NEQ LKEY              
         BNE   *+18                SET ACTION TO DISPLAY                        
         CLC   KEY,LKEY                                                         
         BE    *+8                                                              
         MVI   ACTN,DISPLAY                                                     
         CLI   ACTN,DISPLAY                                                     
         BE    *+8                                                              
         MVI   UPDATE,C'Y'         SET RFU FOR UPDATABLE ACTIONS                
         GOTO1 AREAD                                                            
         BZ    EXIT                                                             
*                                                                               
         TM    DMCB+8,X'10'        CHECK FOR NOT FOUND                          
         BZ    *+16                                                             
         CLI   ACTN,ADD            ONLY VALID FOR ADD                           
         BE    DATAVAL                                                          
         B     ERNF                                                             
         CLI   ACTN,ADD            FOUND NOT VALID FOR ADD                      
         BE    ERAE                                                             
         TM    DMCB+8,X'02'        DELETED RECORDS MAY ONLY BE DSPLYD           
         BZ    *+12                                                             
         CLI   ACTN,DISPLAY                                                     
         BNE   ERNF                                                             
         CLI   ACTN,CHANGE                                                      
         BE    DATAVAL                                                          
         B     DISPREC                                                          
         EJECT                                                                  
* DISPLAY SCREEN MAP RECORD                                                     
*                                                                               
DISPREC  TWAXC MAPFNUMH                                                         
         LA    R5,CTMDATA          R5=A(ELEMENT)                                
         LA    R7,MAPFNUMH                                                      
         USING LINED,R7            R7=A(TWA LINE)                               
         MVI   LINFLDN,0                                                        
         BAS   RE,SETAKEY          GET A(SYSTEM KEY TABLE)                      
*                                                                               
DISP2    CLI   0(R5),0                                                          
         BE    DISP16                                                           
         CLI   0(R5),X'B1'                                                      
         BE    DISP6                                                            
*                                                                               
DISP4    ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     DISP2                                                            
*                                  DISPLAY MAP ELEMENT                          
         USING CTMAPD,R5                                                        
DISP6    CLC   CTMAPFLD,LINFLDN    IS THIS FIELD EQ LAST FIELD NUMBER           
         BE    DISP8                                                            
         CLI   LINFLDN,0                                                        
         BE    *+8                                                              
         LA    R7,LINLEN(R7)       BUMP TO NEXT TWA LINE                        
         EDIT  (B1,CTMAPFLD),(3,LINFNUM),ALIGN=LEFT                             
         MVC   LINFLDN,CTMAPFLD                                                 
         LA    RE,LINVAL           SET DISP TO FIRST                            
         ST    RE,LINDISP                                                       
*                                                                               
DISP8    L     R6,LINDISP                                                       
         CLI   CTMAPNUM,0                                                       
         BNE   DISP10                                                           
         MVI   0(R6),C''''         HANDLE LITERALS                              
         ZIC   R1,CTMAPLEN                                                      
         SH    R1,=H'6'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R6),CTMAPLIT    MOVE LITERAL TO LINE                         
         LA    R6,2(R6,R1)                                                      
         MVI   0(R6),C''''                                                      
         LA    R6,1(R6)                                                         
         ST    R6,LINDISP          SET NEXT DISP                                
         B     DISP4                                                            
*                                                                               
DISP10   L     RE,AKEYTAB          HANDLE KEYWORDS                              
         MVI   0(R6),C'&&'                                                      
DISP12   CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   CTMAPNUM,0(RE)      LOOK UP KEYWORD NUMBER IN TABLE              
         BE    *+12                                                             
         LA    RE,9(RE)                                                         
         B     DISP12                                                           
         MVC   1(8,R6),1(RE)       MOVE KEYWORD TO LINE                         
         LA    R6,8(R6)                                                         
         CLI   0(R6),C' '                                                       
         BNE   *+8                                                              
         BCT   R6,*-8                                                           
*                                                                               
         ZIC   R1,CTMAPLEN                                                      
         SH    R1,=H'6'                                                         
         BM    DISP14                                                           
         MVI   1(R6),C'='          HANDLE KEYWORD=VALUE                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R6),CTMAPLIT    MOVE VALUE TO LINE                           
         LA    R6,2(R1,R6)                                                      
DISP14   LA    R6,1(R6)                                                         
         ST    R6,LINDISP          SET DISP TO NEXT                             
         B     DISP4                                                            
*                                  SET NEXT ACTION & EXIT                       
DISP16   MVI   FERN,X'FF'                                                       
         MVI   FNDX,0                                                           
         TM    CTMSTAT,X'80'                                                    
         BZ    *+12                                                             
         MVI   NACTN,OKRES                                                      
         B     EXIT                                                             
         LA    R1,MAPFNUMH                                                      
         ST    R1,FADR                                                          
         MVI   NACTN,OKDEL+OKCHA                                                
         B     EXIT                                                             
         DROP  R5,R7                                                            
         EJECT                                                                  
* ADD/CHANGE SCREEN MAP RECORD                                                  
*                                                                               
DATAVAL  MVI   TEMP,0              BUILD VIRGIN RECORD                          
         GOTO1 ABLDREC                                                          
         GOTO1 ABLDACT                                                          
         GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
*                                                                               
         BAS   RE,SETAKEY          GET A(SYSTEM KEY TABLE)                      
         XC    FLDLIST,FLDLIST                                                  
         LA    R7,MAPFNUMH                                                      
         USING LINED,R7            R7=A(TWA LINE)                               
*                                                                               
DATAV2   CLI   LINFNUMH,11         END OF TWA                                   
         BL    DATAV24                                                          
         GOTO1 AFVAL,LINFNUMH      VALIDATE FIELD NUMBER                        
         BNZ   DATAV4                                                           
         CLI   LINVALH+5,0         ENSURE NO VALUE INPUT                        
         BNE   EMIF                                                             
         B     DATAV22                                                          
*                                                                               
DATAV4   TM    FLDH+4,X'08'                                                     
         BZ    EFNN                                                             
         OC    FLDH(4),FLDH                                                     
         BZ    EIIF                                                             
         CLC   FLDH(4),=F'20'                                                   
         BH    EIIF                                                             
         ZIC   R1,FLDH+3                                                        
         LA    R1,FLDLIST-1(R1)                                                 
         CLI   0(R1),0             CHECK FIELD NOT PREVIOUSLY DEFINED           
         BNE   EDIF                                                             
         OI    0(R1),X'80'                                                      
         MVC   LINFLDN,FLDH+3      SAVE FIELD NUMBER                            
*                                  VALIDATE FIELD VALUE                         
         GOTO1 AFVAL,LINVALH                                                    
         BZ    EXIT                                                             
*                                  BUILD SCUNKEY OUTPUT TABLE                   
         LA    R1,SCUNTAB                                                       
         LA    RF,20                                                            
         MVC   0(L'SCUNTAB,R1),SCUNFLD                                          
         LA    R1,L'SCUNTAB(R1)                                                 
         BCT   RF,*-10                                                          
         MVI   0(R1),0                                                          
         GOTO1 VSCUNKEY,DMCB,(C'''',FLDH),(20,SCUNTAB)                          
*                                  PROCESS SCUNKEY TABLE                        
         LA    R6,SCUNTAB          R6=A(TABLE ENTRY)                            
         MVI   FNDX,1                                                           
         MVI   LINSUBF,0           PRESET VALUES                                
         MVI   SCUNFLAG,0                                                       
         MVI   LASTFLD,0                                                        
DATAV6   CLI   0(R6),0                                                          
         BE    DATAV22                                                          
         CLI   5(R6),0             ANY DATA IN THIS SUB-FIELD                   
         BNE   DATAV10                                                          
         CLI   SCUNFLAG,0          NO - SPECIAL CODE FOR FIRST FIELD            
         BE    DATAV8                                                           
         MVI   SCUNFLAG,2          SET END OF DATA FLAG                         
         B     DATAV20                                                          
*                                                                               
DATAV8   LA    R6,L'SCUNTAB(R6)    FIRST FIELD MAY BE EMPTY ONLY IF             
         CLI   5(R6),0             DATA IS A LITERAL                            
         BE    EIIF                                                             
         CLI   8(R6),C'&&'                                                      
         BE    EIIF                                                             
*                                                                               
DATAV10  CLI   SCUNFLAG,2          CHECK FOR MULTIPLE DELIMITERS                
         BE    EIIF                                                             
         MVI   SCUNFLAG,1          SET DATA FLAG                                
*                                                                               
         LA    R5,TEMP                                                          
         USING CTMAPD,R5           R5=A(MAP ELEMENT)                            
         XC    CTMAPEL(100),CTMAPEL                                             
         MVC   CTMAPEL(2),=X'B105'                                              
         MVC   CTMAPFLD,LINFLDN                                                 
         ZIC   R1,LINSUBF                                                       
         LA    R1,1(R1)                                                         
         STC   R1,LINSUBF                                                       
         MVC   CTMAPSUB,LINSUBF                                                 
*                                  HANDLE KEYWORDS                              
DATAV12  CLI   8(R6),C'&&'                                                      
         BNE   DATAV16                                                          
         CLI   LASTFLD,C'K'        KEYWORD MUST BE PRECEEDED BY LITERAL         
         BE    EIIF                                                             
         MVI   LASTFLD,C'K'                                                     
         GOTO1 VSCANNER,DMCB,(R6),(2,SCANBLK)                                   
         CLI   4(R1),1                                                          
         BNE   EIIF                                                             
         ZIC   R1,SCANBLK                                                       
         SH    R1,=H'2'                                                         
         BM    EIIF                                                             
         ICM   RF,15,AKEYTAB                                                    
         BZ    EIIF                                                             
DATAV14  CLI   0(RF),0             LOOK UP KEYWORD IN TABLE                     
         BE    EIIF                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   1(0,RF),SCANBLK+13                                               
         BE    *+12                                                             
         LA    RF,9(RF)                                                         
         B     DATAV14                                                          
         MVC   CTMAPNUM,0(RF)      SET KEYWORD NUMBER                           
         CLI   SCANBLK+1,0                                                      
         BE    DATAV18                                                          
         ZIC   R1,SCANBLK+1        HANDLE KEYWORD VALUES                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CTMAPLIT(0),SCANBLK+22                                           
         ZIC   RF,CTMAPLEN                                                      
         LA    RF,1(R1,RF)                                                      
         STC   RF,CTMAPLEN                                                      
         B     DATAV18                                                          
*                                  HANDLE LITERALS                              
DATAV16  CLI   LASTFLD,C'L'        LITERAL MUST BE PRECEEDED BY KEYWORD         
         BE    EIIF                                                             
         MVI   LASTFLD,C'L'                                                     
         ZIC   R1,5(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CTMAPLIT(0),8(R6)   MOVE LITERAL TO ELEMENT                      
         ZIC   RF,CTMAPLEN                                                      
         LA    RF,1(RF,R1)                                                      
         STC   RF,CTMAPLEN                                                      
*                                                                               
DATAV18  GOTO1 APUTEL                                                           
         BZ    EXIT                                                             
         ZIC   R1,FNDX             BUMP FIELD INDEX                             
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
DATAV20  LA    R6,L'SCUNTAB(R6)    BUMP TO NEXT SUB-FIELD                       
         B     DATAV6                                                           
DATAV22  LA    R7,LINLEN(R7)       BUMP TO NEXT TWA LINE                        
         B     DATAV2                                                           
*                                                                               
DATAV24  LA    R1,MAPSYSH                                                       
         ST    R1,FADR                                                          
         MVI   FERN,X'FF'                                                       
         L     RF,AADD                                                          
         CLI   ACTN,ADD                                                         
         BE    *+8                                                              
         L     RF,AWRITE                                                        
         MVC   KEY,KEYSAVE         RESTORE KEY/A(RECORD)                        
         ST    R4,AREC                                                          
         BASR  RE,RF                                                            
         CLI   DMCB+8,0                                                         
         BNE   EIIO                                                             
         MVI   NACTN,OKDEL+OKCHA                                                
         LA    R1,BASACTNH         SET CURSOR & EXIT                            
         ST    R1,FADR                                                          
         B     EXIT                                                             
         EJECT                                                                  
* SET A(SYSTEM KEYWORD TABLE)                                                   
*                                                                               
SETAKEY  XC    AKEYTAB,AKEYTAB                                                  
         LA    RF,SYSTAB                                                        
SETAKEY2 CLI   0(RF),0                                                          
         BER   RE                                                               
         CLC   CTMKSYS,0(RF)                                                    
         BE    *+12                                                             
         LA    RF,L'SYSTAB(RF)                                                  
         B     SETAKEY2                                                         
         L     RF,0(RF)                                                         
         LA    RF,0(RF)                                                         
         A     RF,MYRELO                                                        
         ST    RF,AKEYTAB                                                       
         BR    RE                                                               
         EJECT                                                                  
* CTLFMERRS                                                                     
       ++INCLUDE CTLFMERRS                                                      
         EJECT                                                                  
* LITERALS ETC.                                                                 
*                                                                               
         LTORG                                                                  
SCUNFLD  DC    AL1(68),67X'00'                                                  
*                                                                               
SYSTAB   DS    0F                                                               
*&&US*&& DC    X'02',AL3(SPTKEYS)                                               
*&&US*&& DC    X'04',AL3(PRTKEYS)                                               
*&&UK*&& DC    X'04',AL3(MEDKEYS)                                               
         DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
MEDKEYS  DS    0CL9                                                             
         DC    AL1(001),CL8'MED'                                                
         DC    AL1(002),CL8'CLI'                                                
         DC    AL1(003),CL8'PRO'                                                
         DC    AL1(004),CL8'CAM'                                                
         DC    AL1(005),CL8'STD'                                                
         DC    AL1(006),CL8'END'                                                
         DC    AL1(007),CL8'SUP'                                                
         DC    AL1(008),CL8'BRAND'                                              
         DC    AL1(009),CL8'BUYER'                                              
         DC    AL1(010),CL8'BUYACT'                                             
         DC    AL1(011),CL8'SERIAL'                                             
         DC    AL1(012),CL8'REQSTR'                                             
         DC    AL1(013),CL8'REQID'                                              
         DC    AL1(014),CL8'SCHTYP'                                             
         DC    AL1(015),CL8'ENQTYP'                                             
         DC    AL1(016),CL8'DENTYP'                                             
         DC    AL1(017),CL8'RATACTN'                                            
         DC    AL1(018),CL8'LFMACTN'                                            
         DC    AL1(019),CL8'PERIOD'                                             
         DC    AL1(255),CL8'ANY'                                                
         DC    X'00'                                                            
*&&US                                                                           
SPTKEYS  DS    0CL9                                                             
         DC    AL1(001),CL8'MED'                                                
         DC    AL1(002),CL8'CLT'                                                
         DC    AL1(003),CL8'PRD'                                                
         DC    AL1(004),CL8'EST'                                                
         DC    AL1(005),CL8'MKT'                                                
         DC    AL1(006),CL8'STA'                                                
         DC    AL1(007),CL8'STDT'                                               
         DC    AL1(008),CL8'ENDT'                                               
         DC    AL1(009),CL8'PERIOD'                                             
         DC    AL1(010),CL8'REQSTR'                                             
         DC    AL1(011),CL8'SPTLN'                                              
         DC    AL1(255),CL8'ANY'                                                
         DC    X'00'                                                            
*                                                                               
PRTKEYS  DS    0CL9                                                             
         DC    AL1(001),CL8'MED'                                                
         DC    AL1(002),CL8'CLT'                                                
         DC    AL1(003),CL8'PRD'                                                
         DC    AL1(004),CL8'EST'                                                
         DC    AL1(005),CL8'PUB'                                                
         DC    AL1(006),CL8'CON'                                                
         DC    AL1(007),CL8'STDT'                                               
         DC    AL1(008),CL8'ENDT'                                               
         DC    AL1(009),CL8'PERIOD'                                             
         DC    AL1(010),CL8'REQSTR'                                             
         DC    AL1(255),CL8'ANY'                                                
         DC    X'00'                                                            
*&&                                                                             
         EJECT                                                                  
* DSECT TO COVER TEMP W/S                                                       
*                                                                               
WORKD    DSECT                                                                  
MYRELO   DS    A                                                                
AKEYTAB  DS    A                                                                
LINDISP  DS    A                                                                
LINFLDN  DS    X                                                                
LINSUBF  DS    X                                                                
LASTFLD  DS    C                                                                
SCUNFLAG DS    X                                                                
FLDLIST  DS    XL20                                                             
SCANBLK  DS    2XL32                                                            
SCUNTAB  DS    20CL68                                                           
         DS    X                                                                
WORKX    EQU   *                                                                
         SPACE 1                                                                
* DSECT TO COVER A TWA LINE                                                     
*                                                                               
LINED    DSECT                                                                  
LINFNUMH DS    CL8                                                              
LINFNUM  DS    CL3                                                              
LINVALH  DS    CL8                                                              
LINVAL   DS    CL60                                                             
LINLEN   EQU   *-LINED                                                          
         SPACE 1                                                                
* CTLFMACTNS                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTLFMACTNS                                                     
         PRINT ON                                                               
* CTLFMDSECT                                                                    
       ++INCLUDE CTLFMDSECT                                                     
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
         EJECT                                                                  
* CTLFMTWA                                                                      
       ++INCLUDE CTLFMTWA                                                       
* CTLFME7D                                                                      
       ++INCLUDE CTLFME7D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003CTLFM18   05/01/02'                                      
         END                                                                    
