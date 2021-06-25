*          DATA SET SRKWX00    AT LEVEL 005 AS OF 09/03/03                      
*PHASE T14700A                                                                  
*INCLUDE GETBOOK                                                                
*INCLUDE GETLIST                                                                
         TITLE '$KWX - SEND A KWX MESSAGE'                                      
T14700   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,**T147**,RR=RE                                       
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T14700+4096,R9      R9 = 2ND BASE                                
         USING WORKD,RC            RC = W/S                                     
         ST    RE,RELO                                                          
         SR    R0,R0               R0 = USED THROUGHOUT FOR IC'S                
         XC    WORKD(AREGSAVE-WORKD),WORKD                                      
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         MVC   PLB,SPACES                                                       
         MVI   PASAVE,TRMINATE                                                  
         MVI   PBSAVE,TRMINATE                                                  
         ST    RB,ABASE                                                         
         ST    R9,A2NDBASE                                                      
         ST    RD,AREGSAVE                                                      
         ST    R1,APARM                                                         
         MVC   ASYSFAC(24),0(R1)                                                
         L     RA,ATWA                                                          
         USING T147FFD,RA          RA = TWA                                     
         MVC   KWXHEAD,SPACES                                                   
         OI    KWXHEADH+6,X'80'                                                 
         NI    KWXAUTHH+6,X'BF'                                                 
         LA    R1,DESTIDS                                                       
         LA    R1,PBUFFA-DESTIDS-DIDL-1(R1)                                     
         ST    R1,ADIDMAX                                                       
         LA    R1,DIDL+1(R1)                                                    
         ST    R1,APBUFFA                                                       
         LA    R1,PSEQ                                                          
         ST    R1,APSEQ                                                         
         LA    R1,PRINT                                                         
         ST    R1,APRINT                                                        
         LA    R1,ACCOUNT                                                       
         ST    R1,AACCOUNT                                                      
         L     R1,ACOMFACS                                                      
         USING COMFACSD,R1                                                      
         MVC   ADATAMGR,CDATAMGR                                                
         MVC   ACALLOV,CCALLOV                                                  
         MVC   ASCANNER,CSCANNER                                                
         L     R1,AUTL                                                          
         USING UTLD,R1                                                          
         LA    R2,KWXSERVH                                                      
         MVI   FERN,NTLOGGED                                                    
         OC    TUSER,TUSER                                                      
         BZ    ERROR                                                            
         MVC   SAVTUSER,TUSER                                                   
         MVC   SAVTKWID,TUSER                                                   
         TM    TTYPE,X'20'         CANT DO TABS IF TWX                          
         BNZ   *+12                                                             
         TM    TSTAT,X'10'         OR 3270                                      
         BZ    *+8                                                              
         MVI   NOTAB,C'Y'                                                       
         SPACE 1                                                                
T005     LA    R4,KWXDATAH         RESET INPUT LENGTHS TO SIGNIFICANT           
         SR    R5,R5               LENGTHS                                      
T006     ICM   R5,1,5(R4)                                                       
         BZ    T007                                                             
         LA    R6,7(R4,R5)                                                      
         CLI   0(R6),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-12                                                          
         STC   R5,5(R4)                                                         
T007     IC    R5,0(R4)                                                         
         AR    R4,R5                                                            
         CLI   0(R4),0                                                          
         BNE   T006                                                             
         B     T010                                                             
         DROP  R1                                                               
         EJECT                                                                  
*              RESET SCREEN SIZE IF '$KWX,99' INPUT                             
         SPACE 1                                                                
T010     LA    R4,KWXDATAH         SAVE DEFAULT 1ST & LAST LINE & TAB           
         ST    R4,AFRSTHED         FIELD HEADER ADDRESSES                       
         LA    R4,KWXDATLH                                                      
         ST    R4,ALASTHED         HEADER ADDRESSES                             
         LA    R5,KWXTABH                                                       
         ST    R5,ATABHED                                                       
         MVI   LINENUM,19                                                       
         CLC   KWXSERV(4),=C'$KWX'                                              
         BNE   T020                                                             
         CLI   KWXSERV+4,C','                                                   
         BNE   T020                                                             
         ZIC   R3,KWXSERVH+5                                                    
         SH    R3,=H'6'                                                         
         BM    T020                                                             
         CH    R3,=H'1'                                                         
         BH    T020                                                             
         MVC   WORK(2),=C'00'                                                   
         EX    R3,T01MVZ                                                        
         CLC   WORK(2),=C'00'                                                   
         BE    *+12                                                             
         OI    USAGE,TABEQLS       IF NOT NUMERIC ITS A FORMATTED SCRN          
         B     T020                                                             
         EX    R3,T01PACK                                                       
         CVB   R3,DUB                                                           
         LTR   R1,R3                                                            
         BZ    T020                                                             
         BCTR  R3,0                                                             
         MH    R3,=H'86'                                                        
         LA    R3,KWXDATAH(R3)                                                  
         CR    R3,R4                                                            
         BNL   T020                                                             
         STC   R1,LINENUM                                                       
         ST    R3,ALASTHED         RESET ADDRESSES                              
         LA    R3,86(R3)                                                        
         ST    R3,ATABHED                                                       
         USING FLDHDRD,R3                                                       
         MVC   WORK(8),FLDHDRD                                                  
         MVC   FLDHDRD(9),KWXTABH  MOVE A TAB FIELD IN AFTER THE NEW            
         MVC   FLDADR,WORK+2       LAST LINE  - USED TO LOCATE END OF           
         XC    9(3,R3),9(R3)       SET END TO TWA                               
         B     T020                                                             
T01PACK  PACK  DUB,KWXSERV+5(0)                                                 
T01MVZ   MVZ   WORK(0),KWXSERV+5                                                
         DROP  R3                                                               
         EJECT                                                                  
*              VALIDATE AUTHORISATION INPUT                                     
         SPACE 1                                                                
T020     LA    R2,KWXAUTHH                                                      
         CLI   5(R2),0                                                          
         BNE   *+14                                                             
T021     MVC   KWXHEAD(18),=C'ENTER INPUT FIELDS'                               
         B     EXIT                                                             
         CLC   KWXAUTH(6),=C'SHOWME'                                            
         BNE   T022                                                             
         BAS   RE,KEYHELP                                                       
         BNZ   T021                HELP SCREEN AVAILABLE                        
         SPACE 1                                                                
T022     LA    R7,KEY              READ ID RECORD AND SAVE NAME & OFFCE         
         USING CTIREC,R7           & 2-CHAR.AGENCY & KWX ID FOR LISTS           
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),SAVTUSER                                             
         MVI   FERN,INVALID                                                     
         BAS   RE,READ                                                          
         BZ    ERROR                                                            
         LA    R6,CTIDATA                                                       
         SR    R5,R5                                                            
T023     CLI   0(R6),0                                                          
         BE    ERROR                                                            
         CLI   0(R6),X'02'                                                      
         BE    T025                                                             
         CLI   0(R6),X'03'                                                      
         BE    T025B                                                            
         CLI   0(R6),X'06'                                                      
         BE    T025A                                                            
         CLI   0(R6),X'30'                                                      
         BE    T026                                                             
T024     IC    R5,1(R6)                                                         
         AR    R6,R5                                                            
         B     T023                                                             
         USING CTDSCD,R6                                                        
T025     MVC   SAVTNAME,CTDSC      NAME                                         
         MVC   SAVTAGID,CTDSC                                                   
         B     T024                                                             
         USING CTPASD,R6                                                        
T025B    CLI   CTPASLEN,4                                                       
         BNE   T024                                                             
         MVC   SAVTKWID,CTPASDTA   SPECIAL KWX SOURCE ID FOR LISTS              
         B     T024                                                             
         USING CTAGYD,R6                                                        
T025A    MVC   SAVTAGID,CTAGYID                                                 
         B     T024                                                             
         USING CTDSTD,R6                                                        
T026     MVC   SAVTPOW,CTDSTPOW+2  SENDER'S OFFICE                              
         DROP  R6,R7                                                            
         SPACE 1                                                                
T027     DS    0H                  CHECK FOR PRESENCE OF TERMINAL/AUTH          
         USING CTTREC,R7           RECORD AND FOR COMPATIBLE ID                 
         XC    CTTKEY,CTTKEY                                                    
         MVI   CTTKTYP,C'T'                                                     
         L     R3,AUTL                                                          
         GETLA (R3),DUB,ADDR=ALPHA                                              
         CLC   DUB(3),=C'TWX'      FUDGE TWX LINE ID                            
         BNE   *+8                                                              
         MVI   DUB+3,C' '                                                       
         SR    R0,R0                                                            
         MVC   CTTKLINE(8),DUB                                                  
         MVC   CTTKPASS,SPACES                                                  
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CTTKPASS(0),KWXAUTH                                              
         MVC   SAVTAUTH,CTTKPASS                                                
         BAS   RE,READ                                                          
         BZ    ERROR                                                            
         GOTO1 ACALLOV,DMCB,0,X'D9000AFA',0                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)            RF=A(GETIDS) T000AFA                         
         GOTO1 (RF),DMCB,(C'C',CTTREC),ATIA,ADATAMGR                            
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         L     R3,ATIA             SEARCH TABLE OF 12-BYTE COMPATIBLE           
         LA    R4,12               ID NAME ENTRIES                              
         ZIC   RF,DMCB                                                          
         SH    RF,=H'1'                                                         
         BM    ERROR                                                            
         MR    RE,R4                                                            
         LA    R5,0(RF,R3)                                                      
         CLC   SAVTNAME,0(R3)                                                   
         BE    T030                                                             
         BXLE  R3,R4,*-10                                                       
         B     ERROR                                                            
         DROP  R7                                                               
         EJECT                                                                  
*              VALIDATE 'SEND TO AAAA+BBBB-CCCC+DDDD ETC'                       
         SPACE 1                                                                
T030     LA    R2,KWXSENDH                                                      
         MVI   FERN,MISSING                                                     
         CLI   5(R2),0                                                          
         BE    ERROR                                                            
         MVI   FERN,DELIMIT                                                     
         SPACE 1                                                                
T032     LA    R3,8(R2)            BUILD PSEUDO-SCANNER BLOCK FROM              
         LA    R4,1                INPUT, CONTAINING SIGN/L//MYMARK/ID          
         ZIC   R5,5(R2)                                                         
         BCTR  R5,0                                                             
         AR    R5,R3                                                            
         LA    R8,SCANBLCK                                                      
         USING SCAND,R8                                                         
         MVI   SCANSIGN,C'+'                                                    
         CLI   0(R3),C'+'                                                       
         BNE   *+8                                                              
         LA    R3,1(R3)                                                         
         SPACE 1                                                                
T034     LR    R6,R3               R6 = START OF AN ID                          
         SR    R1,R1               R1 = ITS LENGTH                              
T035     CLI   0(R3),C'+'          LOOK FOR DELIMITER + OR -                    
         BE    T036                                                             
         CLI   0(R3),C'-'                                                       
         BE    T036                                                             
         CLI   0(R3),C','                                                       
         BE    ERROR                                                            
         LA    R1,1(R1)                                                         
         BXLE  R3,R4,T035                                                       
T036     MVC   SCANNAME,SPACES     COMPLETE SCANBLCK ENTRY                      
         MVI   SCANMYMK,C'N'       SET MARKER TO Y IF LIST IS FORCED TO         
         CLC   0(3,R6),=C'MY.'     BE UNDER THIS USER'S ID,NOT SAVTKWID         
         BNE   T037                                                             
         LA    R6,3(R6)                                                         
         SH    R1,=H'3'                                                         
         MVI   SCANMYMK,C'Y'                                                    
T037     CH    R1,=H'8'                                                         
         BNH   *+8                                                              
         LA    R1,8                                                             
         STC   R1,SCANLEN                                                       
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BM    T038                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SCANNAME(0),0(R6)                                                
T038     LA    R8,SCANL(R8)        BUMP TO NEXT                                 
         MVC   SCANSIGN,0(R3)      SET UP ITS SIGN                              
         BXLE  R3,R4,T034                                                       
         MVI   SCANSIGN,0          TERMINATE SCANBLCK                           
         SPACE 1                                                                
T040     LA    R8,SCANBLCK                                                      
         MVI   DESTIDS,X'FF'                                                    
         CLI   SCANBLCK+SCANL,0    ONLY ONE SUBFIELD                            
         BE    *+8                                                              
         MVI   FNDX,1                                                           
         SPACE 1                                                                
T042     CLI   0(R8),0             LOOP FOR A SCANBLCK ENTRY                    
         BE    T060                                                             
         MVI   FERN,INVALID                                                     
         CLI   SCANLEN,0           LENGTH MUST BE 1 TO 6                        
         BE    ERROR                                                            
         CLI   SCANLEN,6                                                        
         BH    ERROR                                                            
         SPACE 1                                                                
T043     CLI   SCANMYMK,C'Y'                                                    
         BE    T048                                                             
         CLC   SCANNAME(4),=CL4'ME' '-ME' MEANS NO COPIES FOR SENDER            
         BNE   T044                 '+ME' MEANS 'F' COPY INSTEAD OF 'S'         
         MVI   SCOPY,C'N'                                                       
         CLI   SCANSIGN,C'-'                                                    
         BE    T059                                                             
         MVI   SCOPY,C'F'          'F' = FILE COPY FOR SENDER                   
         L     R7,APBUFFA                                                       
         MVC   0(2,R7),SAVTUSER                                                 
         B     T047                                                             
         SPACE 1                                                                
T044     LA    R7,KEY              LOOK FOR AN ID RECORD                        
         USING CTIREC,R7                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,SPACES                                                    
         ZIC   R1,SCANLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CTIKID(0),SCANNAME                                               
         BAS   RE,READ                                                          
         BZ    T048                                                             
         SR    R5,R5                                                            
         LA    R6,CTIDATA                                                       
T046     CLI   0(R6),X'02'                                                      
         BE    *+14                                                             
         IC    R5,1(R6)                                                         
         AR    R6,R5                                                            
         B     T046                                                             
         USING CTDSCD,R6                                                        
         L     R7,APBUFFA          PUT DEST ID NUMBER IN PBUFFA AND             
         MVC   0(2,R7),CTDSC       GO TO ADD/SUBTRACT IT FROM OUR               
T047     MVI   2(R7),X'FF'         DESTID BLOCK                                 
         MVI   TYPE,C'I'                                                        
         DROP  R6,R7                                                            
         B     T050                                                             
         SPACE 1                                                                
T048     MVC   KEY(2),SAVTKWID     OR LOOK FOR AN ADDRESSEE LIST AND            
         CLI   SCANMYMK,C'Y'                                                    
         BNE   *+10                                                             
         MVC   KEY(2),SAVTUSER                                                  
         MVC   KEY+2(6),SCANNAME   BUILD A STRING OF DESTID'S IN PBUFFA         
         MVI   KEY+8,0                                                          
         GOTO1 =V(GETLIST),DMCB,KEY,APBUFFA,ADATAMGR,RR=RELO                    
         MVI   FERN,LISTNXST                                                    
         CLI   DMCB,X'10'                                                       
         BE    ERROR                                                            
         MVI   FERN,LISTLEVS                                                    
         CLI   DMCB,X'04'                                                       
         BE    ERROR                                                            
         MVI   FERN,INVALID                                                     
         CLI   DMCB,0                                                           
         BNE   ERROR                                                            
         MVI   TYPE,C'A'                                                        
         SPACE 1                                                                
T050     L     R7,APBUFFA          ADD (IF NOT DUPLICATE) OR SUBTRACT           
         USING DID,R6              FROM/TO OUR DEST ID BLOCK                    
         SPACE 1                                                                
T051     CLI   0(R7),X'FF'         LOOP FOR AN INTERMEDIATE BLOCK ENTRY         
         BE    T059                                                             
         LA    R6,DESTIDS                                                       
         CLC   SAVTUSER,0(R7)      IGNORE SENDER UNLESS INDIVIDUALLY            
         BNE   T052                SPECIFIED                                    
         C     R7,APBUFFA                                                       
         BNE   T058                                                             
         CLI   2(R7),X'FF'                                                      
         BNE   T058                                                             
         SPACE 1                                                                
T052     CLI   0(R6),X'FF'         LOOP FOR A DEST ID BLOCK ENTRY               
         BNE   T053                AT END SKIP IF '-', ADD IF '+'               
         CLI   SCANSIGN,C'-'                                                    
         BE    T058                                                             
         MVI   FERN,TOOMANY                                                     
         C     R6,ADIDMAX                                                       
         BNL   ERROR                                                            
         MVC   DINUM,0(R7)                                                      
         MVC   DINAM,SCANNAME                                                   
         MVC   DITYPE,TYPE                                                      
         MVI   DIERR,0                                                          
         MVI   DID+DIDL,X'FF'                                                   
         B     T058                                                             
         SPACE 1                                                                
T053     CLC   DINUM,0(R7)         ON MATCH SKIP IF '+', REMOVE IF '-'          
         BNE   T055                                                             
         CLI   SCANSIGN,C'+'                                                    
         BE    T058                                                             
T054     LA    R5,DIDL(R6)                                                      
         MVC   DID(DIDL),0(R5)                                                  
         CLI   DID,X'FF'                                                        
         BE    T058                                                             
         LA    R6,DIDL(R6)                                                      
         B     T054                                                             
         SPACE 1                                                                
T055     LA    R6,DIDL(R6)         BUMP TO NEXT IN DEST ID BLOCK                
         B     T052                                                             
         SPACE 1                                                                
T058     LA    R7,2(R7)            BUMP TO NEXT IN INTERMEDIATE BLOCK           
         B     T051                                                             
         SPACE 1                                                                
T059     LA    R8,SCANL(R8)        BUMP TO NEXT IN SCANBLCK                     
         ZIC   R1,FNDX                                                          
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         B     T042                                                             
         SPACE 1                                                                
T060     MVI   FNDX,0              COUNT NUMBER OF DESTINATIONS                 
         SR    R1,R1                                                            
         LA    R6,DESTIDS                                                       
T061     CLI   0(R6),X'FF'                                                      
         BE    T062                                                             
         LA    R1,1(R1)                                                         
         LA    R6,DIDL(R6)                                                      
         B     T061                                                             
T062     LTR   R1,R1                                                            
         MVI   FERN,NOADDEES                                                    
         BZ    ERROR                                                            
         CH    R1,=H'100'                                                       
         MVI   FERN,EXADDEES                                                    
         BH    ERROR                                                            
         STH   R1,DESTNUM                                                       
         B     T065                                                             
         DROP  R6,R8                                                            
         EJECT                                                                  
*              HANDLE FORMATTED SCREENS ($KWX,A ETC.)                           
         SPACE 1                                                                
T065     TM    USAGE,TABEQLS       FORMATTED SCREEN                             
         BNO   T070                                                             
         MVI   USAGE,0                                                          
         LA    R2,KWXDATAH                                                      
         LR    R3,R2                                                            
         LA    R4,8(R2)                                                         
         LA    R1,KEYWSPAC         CHECK FOR SPACE=I/3 - NO OTHER               
         BAS   RE,KEYCALL          KEYWORDS RECOGNISED                          
         BZ    ERROR                                                            
         LA    R2,KWXDATAH                                                      
         SPACE 1                                                                
T066     LA    R3,PSEUDTWA         SET UP MESSAGE PART OF TWA AS AN             
         ST    R3,AFRSTHED         UNFORMATTED SET OF 79 BYTE FIELDS            
         LA    R4,19               IN PSEUDTWA                                  
T067     XC    0(79+8,R3),0(R3)    INITIALISE                                   
         MVI   0(R3),79+8                                                       
         LA    R3,79+8(R3)                                                      
         BCT   R4,T067                                                          
         LR    R3,R2                                                            
         SR    R1,R1                                                            
         USING FLDHDRD,R3                                                       
T068     CLI   FLDLEN,0            LOOP TO MOVE A TWA FIELD INTO                
         BE    T085                PSEUDTWA                                     
         TM    FLDATB,X'20'                                                     
         BO    *+12                                                             
         CLI   FLDILEN,0                                                        
         BE    T069                                                             
         LH    R4,FLDADR                                                        
         SH    R4,=H'240'          MESSAGE STARTS ON LINE 4                     
         SRDA  R4,32                                                            
         LR    R6,R5                                                            
         D     R4,=F'80'                                                        
         LA    R5,1(R5)                                                         
         STC   R5,LINENUM                                                       
         MH    R5,=H'7'                                                         
         AR    R5,R6                                                            
         LA    R5,PSEUDTWA(R5)                                                  
         IC    R1,FLDLEN                                                        
         SH    R1,=H'9'                                                         
         EX    R1,T06MVC                                                        
         TM    FLDATB,X'20'                                                     
         BO    T069                                                             
         OI    USAGE,ANY           IF UNPROT INPUT WE WILL SEND KWX NOW         
T069     IC    R1,FLDLEN                                                        
         AR    R3,R1                                                            
         B     T068                                                             
T06MVC   MVC   0(0,R5),FLDDATA                                                  
         DROP  R3                                                               
         EJECT                                                                  
*              VALIDATE TEXT                                                    
         SPACE 1                                                                
T070     LA    R2,KWXDATAH         MOVE TEXT LINES + HEADERS TO TIA             
         L     R3,ATIA                                                          
         LA    R1,KWXTAB+1                                                      
         SR    R1,R2                                                            
         MOVE  ((R3),(R1)),(R2)                                                 
         MVI   1(R3),0             2260 INPUT TRANSLATOR REQUIREMENT            
         SR    R4,R4                                                            
         L     R5,ALASTHED                                                      
         SR    R6,R6                                                            
         SPACE 1                                                                
T071     CLI   5(R2),0             CLEAR THEM IN TWA                            
         BE    T072                                                             
         IC    R6,5(R2)                                                         
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
         MVI   USAGE,ANY                                                        
T072     IC    R4,0(R2)                                                         
         BXLE  R2,R4,T071                                                       
         LA    R2,KWXDATAH                                                      
         CLI   USAGE,0                                                          
         MVI   USAGE,0                                                          
         BE    T081                NO INPUT AT ALL                              
         MVI   ERRORMK,C'N'                                                     
         MVI   TYPE,CHECK                                                       
         SPACE 1                                                                
T075     CLI   ERRORMK,C'Y'        PROCESS A TEXT LINE                          
         BE    T076                IF IT STARTS WITH A KEYWORD CALL THE         
         LA    R1,KEYWTAB          RELEVANT SUBROUTINE, OTHERWISE JUST          
         LA    R4,8(R3)            MOVE IT BACK FROM THE TIA TO THE TWA         
         MVI   FNDX,0                                                           
         BAS   RE,KEYCALL                                                       
         BP    T077                KEYWORD PRESENT & PROCESSED                  
         BM    T076                NO KEYWORD                                   
         ST    R2,ACURSOR          ERROR - SO ALL SUBSEQUENT LINES ARE          
         MVI   ERRORMK,C'Y'        MOVED BACK AS INPUT, KEYWORDS & ALL          
T076     BAS   RE,MOVEIT                                                        
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         IC    R0,0(R3)                                                         
         AR    R3,R0                                                            
T077     CLI   0(R3),9             HAVE WE REACHED THE TAB FIELD                
         BE    T080                                                             
         CLI   0(R2),9                                                          
         BNE   T075                                                             
         SPACE 1                                                                
T080     L     R2,ACURSOR          ERRORS AND 'CHECK=YES' DISPLAYS              
         CLI   ERRORMK,C'Y'                                                     
         BE    ERROR                                                            
         TM    USAGE,CHKEQLS                                                    
         BO    T086                                                             
         B     T082                                                             
         SPACE 1                                                                
T081     MVI   SCANBLCK+32,0       IF NO INPUT LOOK FOR DEFAULT TEXT            
         MVC   SCANBLCK+22(10),SAVTAUTH                                         
         BAS   RE,GETTEXT          PASS TEXT ID=AUTHORIZATION + 0=NO            
         DS    0H                  SUBSTITUTION,VIA SCANBLCK                    
         SPACE 1                                                                
T082     L     R2,ATABHED          SAVE COUNT OF LINES USED AND SET             
         L     R5,ALASTHED         CURSOR TO FIELD AFTER LAST USED              
         LA    R6,L'KWXDATAH+L'KWXDATA                                          
         LNR   R6,R6                                                            
         LA    R7,KWXDATAH-1                                                    
         ZIC   R1,LINENUM          R1 = COUNT                                   
         SR    R3,R3                                                            
T083     IC    R3,0(R5)                                                         
         SH    R3,=H'9'                                                         
         EX    R3,T083OC                                                        
         EX    R3,T083CLC                                                       
         BH    *+12                                                             
         LR    R2,R5               FOR EACH UNUSED LINE RESET CURSOR            
         BCTR  R1,0                AND REDUCE COUNT                             
         BXH   R5,R6,T083                                                       
         STC   R1,LINENUM                                                       
         TM    USAGE,TABEQLS       IF THERE ARE ANY SPECIAL TAB FIELDS          
         BNO   T085                POINT CURSOR TO 1ST MESSAGE LINE             
         LA    R2,KWXDATAH                                                      
         B     T085                                                             
T083OC   OC    8(0,R5),SPACES                                                   
T083CLC  CLC   8(0,R5),SPACES                                                   
         SPACE 1                                                                
T085     TM    USAGE,TEXTEQLS      RETURN SCREEN OR SEND MESSAGE                
         BO    T086                                                             
         TM    USAGE,DISPLAY                                                    
         BO    T086                                                             
         CLI   USAGE,0                                                          
         BNE   T200                MESSAGE READY                                
         MVC   KWXHEAD(L'INVITE1),INVITE1                                       
         B     EXIT                                                             
T086     MVC   KWXHEAD(L'INVITE2),INVITE2                                       
         B     EXIT                                                             
INVITE1 DC CL45'INPUT YOUR MESSAGE AND PRESS ENTER TO SEND IT'                  
INVITE2 DC CL54'COMPLETE/CHECK YOUR MESSAGE AND PRESS ENTER TO SEND IT'         
         EJECT                                                                  
*              ROUTINE TO MOVE A LINE FROM THE TIA TO THE TWA AND               
*              TRANSMIT IT IF NECESSARY, CHECKING FOR POSSIBLE UNEQUAL          
*              LINE LENGTHS                                                     
*              ON ENTRY R2 = A(OUTPUT FIELD HEADER IN TWA)                      
*                       R3 = A(INPUT FIELD HEADER IN TIA)                       
         SPACE 1                                                                
MOVEIT   ZIC   RF,0(R3)                                                         
         CLC   0(1,R3),0(R2)                                                    
         BNH   *+8                                                              
         IC    RF,0(R2)                                                         
         SH    RF,=H'9'                                                         
         EX    RF,MOMVC                                                         
         EX    RF,MOOC                                                          
         BZR   RE                                                               
         OI    6(R2),X'80'                                                      
         CLC   8(7,R2),KEYWREP                                                  
         BER   RE                                                               
         OI    USAGE,ANY                                                        
         BR    RE                                                               
MOMVC    MVC   8(0,R2),8(R3)                                                    
MOOC     OC    8(0,R2),8(R2)                                                    
         EJECT                                                                  
*              GENERAL ROUTINE TO CALL SUBROUTINES TO HANDLE KEYWORDS           
*              ON ENTRY R1 = A(TABLE TO DRIVE PROCESS) - SEE DSECT KWD          
*                       R2 = A(OUTPUT FIELD HEADER IN TWA)                      
*                       R3 = A(INPUT FIELD HEADER IN TIA)                       
*                       R4 = A(FIELD)                                           
*              ON EXIT  CC = EQU IF ERROR - FERN IS SET                         
*                       CC = POS IF OK    - R2/R3 BUMPED IF APPROPRIATE         
*                       CC = NEG IF NO MATCH FOUND                              
         SPACE 1                                                                
KEYCALL  NTR1                                                                   
         SR    RF,RF                                                            
         USING KWD,R1                                                           
KC1      CLI   0(R1),0                                                          
         BNE   *+10                                                             
         LNR   R1,R1                                                            
         B     KEYXIT2                                                          
         IC    RF,KWEXLEN                                                       
         EX    RF,KEYCLC                                                        
         BE    *+12                                                             
         LA    R1,KWDL(R1)                                                      
         B     KC1                                                              
         L     RF,KWASUBR                                                       
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         B     KEYXIT2                                                          
KEYCLC   CLC   KWORD(0),0(R4)                                                   
         DROP  R1                                                               
         SPACE 3                                                                
KEYWTAB  DS    0CL12                                                            
         DC    CL7'TEXT=',AL1(4),AL4(KEYTEXT)                                   
KEYWREP  DC    CL7'REPORT=',AL1(6),AL4(KEYREPT)                                 
         DC    CL7'KWX=',AL1(3),AL4(KEYREPT)                                    
         DC    CL7'STATUS=',AL1(6),AL4(KEYSTAT)                                 
         DC    CL7'CHECK=',AL1(5),AL4(KEYREPT)                                  
KEYWSPAC DC    CL7'SPACE=',AL1(5),AL4(KEYSPACE)                                 
         DC    X'00'                                                            
         SPACE 1                                                                
KEYERR   SR    R0,R0               CC = EQU FOR ERROR                           
ANXIT1   XIT1                                                                   
         SPACE 1                                                                
KEYXIT   LTR   RB,RB               CC = POS FOR OK                              
KEYXIT2  XIT1  REGS=(R2,R3)                                                     
         EJECT                                                                  
*              ROUTINE TO HANDLE 'TEXT=AAAAAAAAAA,NOSHIFT,X=Y' ETC              
*              SEE KEYCALL FOR PARAMETERS                                       
         SPACE 1                                                                
KEYTEXT  NTR1                      SCAN INPUT                                   
         XC    SCANBLCK(250),SCANBLCK                                           
         XC    SCANBLCK+250(70),SCANBLCK+250                                    
         GOTO1 ASCANNER,DMCB,(R3),(9,SCANBLCK)                                  
         MVI   FERN,INVALID                                                     
         CLI   DMCB+4,0                                                         
         BE    KEYERR                                                           
         MVI   NOSHIFT,C'N'                                                     
         SPACE 1                                                                
KT1      MVI   FNDX,2              IF MORE THAN ONE ENTRY, 2ND AND              
         LA    R5,SCANBLCK+32      SUBSEQUENT ONES ARE KEYWORDS OR              
         SR    R6,R6               SUBSTITUTION REQUEST                         
KT3      OC    0(2,R5),0(R5)                                                    
         BZ    KT4                                                              
         LA    R1,TEXTAB                                                        
         LA    R4,12(R5)                                                        
         BAS   RE,KEYCALL                                                       
         MVI   FERN,INVALID                                                     
         BZ    KEYERR                                                           
         BM    KT3A                NO MATCHING KEYWORD                          
         MVC   2(2,R5),=C'**'      ENTRY+2(2) INDICATES NOT SUBSTITUTES         
         B     KT3B                                                             
KT3A     MVI   FERN,SUBSINVL                                                    
         CLI   1(R5),0                                                          
         BE    KEYERR                                                           
         MVI   2(R5),0             CLEAR BYTE FOR HIT MARKER                    
KT3B     IC    R6,FNDX                                                          
         LA    R6,1(R6)                                                         
         STC   R6,FNDX                                                          
         LA    R5,L'SCANBLCK(R5)                                                
         B     KT3                                                              
         SPACE 1                                                                
KT4      MVI   FNDX,0              GET TEXT FROM CTFILE AND SET IT UP           
         BAS   RE,GETTEXT          IN TWA                                       
         BZ    KEYERR                                                           
         SPACE 1                                                                
KT5      CLI   SCANBLCK+32,0       CHECK FOR NO SUBSTITUTION HITS               
         BE    KT8                                                              
         LA    R5,SCANBLCK+32                                                   
         MVI   FNDX,2                                                           
         MVI   FERN,NOHITS                                                      
KT6      CLI   0(R5),0                                                          
         BE    KT8                                                              
         CLI   2(R5),0                                                          
         BE    KEYERR                                                           
         IC    R6,FNDX                                                          
         LA    R6,1(R6)                                                         
         STC   R6,FNDX                                                          
         LA    R5,L'SCANBLCK(R5)                                                
         B     KT6                                                              
         SPACE 1                                                                
KT8      ZIC   R0,0(R3)            BUMP TIA POINTER                             
         AR    R3,R0                                                            
         B     KEYXIT                                                           
         SPACE 1                                                                
TEXTAB   DS    0CL12               KEYWORD TABLE                                
         DC    CL7'NOSHIFT',AL1(6),AL4(SRSHIFT)                                 
         DC    X'00'                                                            
         EJECT                                                                  
*              SUBROUTINES TO HANDLE KEYWORDS IN THE 'TEXT=' STRING             
*              ON EXIT  CC = EQU IF ERROR                                       
*                       CC = POS IF OK                                          
         SPACE 1                                                                
SRSHIFT  MVI   NOSHIFT,C'Y'        SET INDICATOR THAT SUBSTITUTIONS ARE         
         LTR   RB,RB               TO BE PERFORMED WITHOUT SHIFTS               
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO GET TEXT FROM CTFILE, PERFORM SUBSTITUTIONS           
*              IF ANY AND SET IT UP IN THE TWA                                  
*              ON ENTRY SCANBLCK = SCANNED 'TEXT=A' INPUT                       
*                       R2       = A(1ST OUTPUT FIELD HEADER)                   
*              ON EXIT  CC=EQU   = ERROR                                        
*                       R2       = A(NEXT OUTPUT HEADER) IF NO ERROR            
*                       EACH SCANBLCK ENTRY HAS A SUBSTITUTION HIT              
*                       MARKER AT +2                                            
         SPACE 1                                                                
GETTEXT  NTR1                      BUILD KEY FOR GETBOOK CALL                   
         LA    R7,KEY                                                           
         USING CTCREC,R7                                                        
         XC    CTCKEY,CTCKEY                                                    
         MVI   CTCKTYP,C'C'                                                     
         MVC   CTCKUSER,SAVTKWID                                                
         MVI   CTCKSYS,C'T'                                                     
         MVC   CTCKID,SCANBLCK+22                                               
         OC    CTCKID,SPACES                                                    
         GOTO1 =V(GETBOOK),DMCB,KEY,CARD,ADATAMGR,RR=RELO                       
         MVI   FERN,TEXTNFND                                                    
         CLI   DMCB+8,X'10'                                                     
         BE    KEYERR                                                           
         OI    USAGE,TEXTEQLS+ANY                                               
         B     GT2                                                              
         SPACE 1                                                                
GT1      BASR  RE,RF               RE-CALL GETBOOK                              
         CLI   DMCB+8,X'80'        END                                          
         BE    KEYXIT                                                           
GT2      MVI   FERN,INVALID                                                     
         CLI   DMCB+8,0                                                         
         BNE   KEYERR                                                           
         MVI   FERN,NOROOM                                                      
         CLI   0(R2),9             THIS IS THE TAB FIELD SO WEVE RUN            
         BE    KEYERR              OUT OF SPACE                                 
*&&US                                                                           
         CLI   NOTAB,C'Y'          SWAP COLON FOR SEMICOLON TO GIVE             
         BE    GT4                 TAB FACILITY IN US                           
         LA    R6,CARD+71                                                       
         LA    R4,1                                                             
         LNR   R4,R4                                                            
         LA    R5,CARD-1                                                        
GT3      CLI   0(R6),C';'                                                       
         BNE   *+12                                                             
         OI    USAGE,TABEQLS                                                    
         MVI   0(R6),C':'                                                       
         BXH   R6,R4,GT3                                                        
*&&                                                                             
GT4      MVC   CARD+72(8),SPACES   CLEAR SEQUENCE NUMBER                        
         BAS   RE,REPLACE          PERFORM SUBSTITUTIONS                        
         ZIC   R4,0(R2)                                                         
         SH    R4,=H'9'                                                         
         EX    R4,GTMVC            MOVE LINE TO TWA                             
         OI    6(R2),X'80'                                                      
         LA    R2,9(R4,R2)                                                      
         B     GT1                                                              
GTMVC    MVC   8(0,R2),CARD                                                     
         DROP  R7                                                               
         EJECT                                                                  
*              ROUTINE TO PERFORM SUBSTITUTIONS ON A LINE OF TEXT               
*              ON ENTRY CARD     = LINE OF TEXT - 72 BYTES                      
*                       SCANBLCK = (FROM 2ND ENTRY ON) SUBSTITUTIONS            
*              ON EXIT  EACH SCANBLCK ENTRY CONTAINS A SUBSTITUTION HIT         
*                       MARKER AT +2                                            
         SPACE 1                                                                
REPLACE  CLI   SCANBLCK+32,0                                                    
         BER   RE                                                               
         NTR1                                                                   
         LA    R5,SCANBLCK+32                                                   
         LA    R2,1                                                             
         LA    R3,CARD+L'CARD-1                                                 
         SR    R4,R4                                                            
         SR    R7,R7                                                            
         SPACE 1                                                                
RPL1     CLI   0(R5),0             SEARCH FOR A MATCH                           
         BE    ANXIT1                                                           
         CLI   3(R5),C'*'          NOT A SUBSTITUTION REQUEST                   
         BE    RPL4                                                             
         LA    R1,CARD                                                          
         IC    R4,0(R5)                                                         
         BCTR  R4,0                                                             
RPL2     EX    R4,RPLCLC                                                        
         BE    RPL3                                                             
         BXLE  R1,R2,RPL2                                                       
         B     RPL4                                                             
RPLCLC   CLC   0(0,R1),12(R5)                                                   
         SPACE 1                                                                
RPL3     OI    2(R5),1             SET HIT MARKER AND SUBSTITUTE,               
         LA    R6,1(R1,R4)         SHIFTING REMAINDER OF LINE LEFT              
         MVC   CARD2,SPACES        OR RIGHT IF REQUIRED                         
         MVC   WORK(L'CARD),0(R6)                                               
         EX    R4,RPLSPACE                                                      
         IC    R7,1(R5)                                                         
         BCTR  R7,0                                                             
         EX    R7,RPLMVC                                                        
         LA    R1,1(R1,R7)                                                      
         CLI   NOSHIFT,C'Y'                                                     
         BE    RPL2                                                             
         MVC   0(L'CARD,R1),WORK                                                
         B     RPL2                                                             
RPLMVC   MVC   0(0,R1),22(R5)                                                   
RPLSPACE MVC   0(0,R1),SPACES                                                   
         SPACE 1                                                                
RPL4     LA    R5,L'SCANBLCK(R5)                                                
         B     RPL1                                                             
         EJECT                                                                  
*              ROUTINE FOR ALL REPORT HANDLING FUNCTIONS, IE                    
*              REPORT=SPP,9999,FORCE,PAGE=99-99,LINE=99-99,DISPLAY,             
*               NEWPAGE,SENDER                                                  
*              KWX=KKK,9999                                                     
*              CHECK=YES                                                        
*              ON ENTRY TYPE  = CHECK OR REPORT                                 
*                       SCOPY = Z IF WE'RE LOOKING FOR REPORT EXTRACTS          
*                               TO APPEND TO SENDER'S COPY (CC=EQU=NO)          
*              SEE KEYCALL FOR OTHER PARAMETERS                                 
         SPACE 1                                                                
KEYREPT  NTR1                                                                   
         OC    AOLAY,AOLAY                                                      
         BNZ   KR1                                                              
         GOTO1 ACALLOV,DMCB,(3,0),0                                             
         MVI   FERN,INVALID                                                     
         CLI   DMCB+4,X'FF'                                                     
         BE    KEYERR                                                           
         MVC   AOLAY,DMCB                                                       
         SPACE 1                                                                
KR1      L     RF,AOLAY                                                         
         GOTO1 (RF),DMCB,(RC)                                                   
         B     KEYXIT2                                                          
         EJECT                                                                  
*              ROUTINE TO HANDLE 'STATUS=OK'                                    
*              SEE KEYCALL FOR PARAMETERS                                       
         SPACE 1                                                                
KEYSTAT  CLC   15(2,R3),=C'OK'     IF STATUS=OK TURN TEXTEQLS OFF IN            
         BNE   *+8                 USAGE BYTE SO MESSAGE WILL BE SENT           
         NI    USAGE,ALL-TEXTEQLS  THIS TIME (UNLESS THERE ARE MORE             
         IC    R0,0(R3)            TEXT= LINES)                                 
         AR    R3,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO HANDLE 'SPACE=1/2/3' FOR SINGLE/DOUBLE/TRIPLE         
*              LINE-SPACEING                                                    
*              SEE KEYCALL FOR PARAMETERS                                       
         SPACE 1                                                                
KEYSPACE NTR1                      SAVE PRINT CONTROL BYTE IN SPACEING          
         MVI   FERN,INVALID                                                     
         CLI   5(R3),7                                                          
         BNE   KEYERR                                                           
         CLI   6(R4),C'1'                                                       
         BE    KS01                                                             
         MVI   SPACEING,PR1SP2                                                  
         CLI   6(R4),C'2'                                                       
         BE    KS01                                                             
         MVI   SPACEING,PR1SP3                                                  
         CLI   6(R4),C'3'                                                       
         BNE   KEYERR                                                           
KS01     BAS   RE,MOVEIT                                                        
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         IC    R0,0(R3)                                                         
         AR    R3,R0                                                            
         B     KEYXIT                                                           
         EJECT                                                                  
*              ROUTINE TO HANDLE 'SHOWME'                                       
         SPACE 1                                                                
KEYHELP  NTR1                                                                   
         LA    R4,KWXDATLH         DONT TRY IF NOT FULL SCREEN                  
         C     R4,ALASTHED                                                      
         BNE   KEYERR                                                           
         TM    USAGE,TABEQLS       OR IF FORMATTED SCREEN                       
         BO    KEYERR                                                           
         LA    R6,KWXDATAH                                                      
         GOTO1 ACALLOV,DMCB,(1,APBUFFA),0                                       
         CLI   DMCB+4,X'FF'                                                     
         BE    KEYERR                                                           
         L     R5,APBUFFA                                                       
         LA    R4,18                                                            
         CLI   KWXAUTH+6,C'2'      CONTINUATION SCREEN                          
         BNE   KH2                                                              
         LA    R5,78*18(R5)                                                     
         SPACE 1                                                                
KH2      CLI   0(R5),0                                                          
         BE    KH3                                                              
         OI    6(R6),X'80'                                                      
         MVC   8(78,R6),0(R5)                                                   
         LA    R6,86(R6)                                                        
         LA    R5,78(R5)                                                        
         BCT   R4,KH2                                                           
         SPACE 1                                                                
KH3      B     KEYXIT                                                           
         EJECT                                                                  
*              MESSAGE READY TO SEND                                            
         SPACE 1                                                                
T200     L     RF,ACOMFACS         SAVE DATE AND TIME                           
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,0                                                      
         L     R1,0(R1)            R1=A(TIME)                                   
         MVC   FULL,0(R1)                                                       
         UNPK  DUB,FULL                                                         
         MVC   TIMENOW,DUB+2                                                    
         OI    TIMENOW+5,X'F0'                                                  
         LA    R1,4(R1)            R1=A(DATE)                                   
         MVC   TODAY(2),6(R1)                                                   
*&&UK*&& MVC   TODAY+2(2),3(R1)                                                 
*&&UK*&& MVC   TODAY+4(2),0(R1)                                                 
*&&US*&& MVC   TODAY+2(2),0(R1)                                                 
*&&US*&& MVC   TODAY+4(2),3(R1)                                                 
         SPACE 1                                                                
T201     PACK  DUB,TODAY+2(2)      SAVE ALPHAMON & ALPHASEC FOR KWXREF          
         CVB   R1,DUB                                                           
         LA    R1,ALPHATAB-1(R1)                                                
         MVC   ALPHAMON,0(R1)                                                   
         PACK  DUB,TIMENOW+4(2)                                                 
         CVB   R1,DUB                                                           
         LA    R1,1(R1)                                                         
         M     R0,=F'25'                                                        
         D     R0,=F'60'                                                        
         LA    R1,ALPHATAB(R1)                                                  
         MVC   ALPHASEC,0(R1)                                                   
         SR    R0,R0                                                            
         B     T202                                                             
ALPHATAB DC    CL26'ABCDEFGHIJKLMNOPQRSTUVWXYZ'                                 
         SPACE 1                                                                
T202     LA    R8,PLKWX            BUILD INITIALISE PRINT LINE IN PLKWX         
         USING PQPLD,R8                                                         
         MVC   PLKWX,PLREP                                                      
         MVI   QLLPP,68            SET 68 LINES PER PAGE                        
         MVI   QLSTAT,0                                                         
*        CLI   USAGE,REPTEQLS      JUST A COMPLETE REPORT                       
*        BE    T210                                                             
         XC    PLKWX,PLKWX                                                      
         BAS   RE,SETPL                                                         
         SPACE 1                                                                
T210     LA    R7,KEY              BUILD BASIC KWXFILE ACCOUNTING REC           
         USING KWXACD,R7                                                        
         XC    KWXACD(KWXALENQ+4),KWXACD                                        
         MVI   KWXRTYP,KWXRTYPQ                                                 
         MVI   KWXAEL,KWXAELQ                                                   
         MVI   KWXALEN,KWXALENQ                                                 
         MVC   KWXAUSER,SAVTUSER                                                
         MVC   KWXAAUTH,SAVTAUTH                                                
         MVC   KWXADATE,TODAY                                                   
         MVC   KWXATIME,TIMENOW                                                 
         MVC   KWXAREF,QLDESC                                                   
         MVC   KWXACLSS,QLCLASS                                                 
         MVC   KWXASENT,KWXSEND                                                 
         OC    KWXASENT,SPACES                                                  
         LA    R5,IO+500                                                        
         SPACE 1                                                                
T220     LA    R6,DESTIDS          PREPARE FOR DESTID LOOP                      
         USING DID,R6                                                           
         MVI   FIRSTID,C'Y'                                                     
         XC    ERRNUM,ERRNUM                                                    
         MVI   TYPE,REPORT                                                      
         SPACE 1                                                                
T230     CLI   DID,X'FF'           ANY MORE DESTINATIONS                        
         BE    T260                                                             
         MVC   QLSRCID,DINUM                                                    
         MVC   PLB,PQPLD                                                        
*        CLI   USAGE,REPTEQLS      JUST A COMPLETE REPORT                       
*        BNE   T232                                                             
*        BAS   RE,KEYREPT                                                       
*        BZ    T255                                                             
*        B     T252                                                             
         SPACE 1                                                                
T232     CLC   QLSRCID,SAVTUSER    'F' = FILE COPY FOR SENDER                   
         BNE   T235                FOR SENDER                                   
         CLI   SCOPY,C'F'                                                       
         BNE   T235                                                             
         BAS   RE,HEDMESS                                                       
         MVI   PLB+(QLSUBID-PQPLD),C'F'                                         
         MVI   FCOPYNOW,C'Y'                                                    
         B     T275                                                             
         SPACE 1                                                                
T235     BAS   RE,PRINT                                                         
         MVI   PLB,HEADOF                                                       
         CLI   FIRSTID,C'Y'                                                     
         BNE   *+8                                                              
         BAS   RE,ACCOUNT                                                       
         BAS   RE,PRINT                                                         
T238     MVI   PLB,PR1SP2                                                       
         MVC   PLB+1(12),=C'START OF KWX'                                       
         MVC   PLB+14(11),QLDESC                                                
         BAS   RE,PRINT                                                         
T239     L     R2,AFRSTHED                                                      
         ZIC   R4,LINENUM                                                       
         SPACE 1                                                                
T240     MVI   PLB,PR1SP1          LOOP FOR A SCREEN LINE                       
         CLI   SCOPY,C'Y'                                                       
         BE    T245                                                             
         CLC   8(6,R2),KEYWSPAC    SPACE=1/2/3                                  
         BE    T248                                                             
         CLC   8(7,R2),KEYWREP     NESTED REPORT                                
         BE    T242                                                             
         CLI   SCOPY,C'Z'                                                       
         BE    T248                                                             
         CLI   SPACEING,0                                                       
         BE    T245                                                             
         MVC   PLB(1),SPACEING                                                  
         B     T245                                                             
T242     BAS   RE,KEYREPT                                                       
         BNZ   T248                                                             
         CLI   SCOPY,C'Z'                                                       
         BE    T248                                                             
         B     T255                                                             
         SPACE 1                                                                
T245     ZIC   R1,0(R2)            LINE OF TEXT                                 
         SH    R1,=H'9'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PLB+1(0),8(R2)                                                   
*&&US                                                                           
         CLI   SCOPY,C'Y'          CLEAR US TAB CHARACTER                       
         BE    T246                                                             
         LA    R1,PLB+78                                                        
         LA    RE,1                                                             
         LNR   RE,RE                                                            
         LA    RF,PLB                                                           
         CLI   0(R1),C':'          COLON                                        
         BNE   *+8                                                              
         MVI   0(R1),C' '          SEMI-COLON                                   
         BXH   R1,RE,*-12                                                       
*&&                                                                             
T246     CLI   FIRSTID,C'Y'                                                     
         BNE   *+8                                                              
         BAS   RE,ACCOUNT                                                       
         BAS   RE,PRINT                                                         
         SPACE 1                                                                
T248     IC    R0,0(R2)            BUMP TO NEXT LINE                            
         AR    R2,R0                                                            
         BCT   R4,T240                                                          
         SPACE 1                                                                
T250     MVI   PLB,SP1             CLOSE MESSAGE                                
         BAS   RE,PRINT                                                         
         CLI   SCOPY,C'Y'          IF SENDER'S COPY IS BEING GENERATED          
         BNE   T251                AND USAGE BYTE TELLS US REPORT(S)            
         TM    USAGE,SENDER        ARE TO BE APPENDED TO IT SET SCOPY           
         BNO   T251                TO Z TO GO BACK AND PRINT THEM               
         MVI   SCOPY,C'Z'                                                       
         B     T239                                                             
T251     MVI   PLB,PR1SP1                                                       
         MVC   PLB+1(10),=C'END OF KWX'                                         
         MVC   PLB+12(11),QLDESC                                                
         BAS   RE,PRINT                                                         
         MVI   PLB,TRMINATE                                                     
         BAS   RE,PRINT                                                         
         SPACE 1                                                                
T252     CLI   SCOPY,C'Y'          BUMP TO NEXT DESTID                          
         BNL   T280                                                             
         CLI   FCOPYNOW,C'Y'       DONT ACCOUNT FOR 'F'ILE COPY                 
         BE    *+14                                                             
         MVC   0(2,R5),QLSRCID                                                  
         LA    R5,2(R5)                                                         
T253     MVI   FCOPYNOW,0                                                       
         MVI   FIRSTID,C'N'                                                     
T254     LA    R6,DIDL(R6)                                                      
         B     T230                                                             
         SPACE 1                                                                
T255     CLI   SCOPY,C'Y'          DESTID IN ERROR                              
         BNL   T280                                                             
         CLI   FCOPYNOW,C'Y'                                                    
         BE    T253                                                             
         LH    R1,ERRNUM                                                        
         LA    R1,1(R1)                                                         
         STH   R1,ERRNUM                                                        
         MVI   DIERR,C'Y'                                                       
         B     T254                                                             
         SPACE 1                                                                
T260     LA    RE,T262                                                          
HEDMESS  LA    R2,KWXSENDH         COMPLETION MESSAGE                           
         LH    R3,DESTNUM                                                       
         SH    R3,ERRNUM                                                        
         BNZ   *+12                                                             
         MVI   FERN,NOADDEES                                                    
         B     ERROR                                                            
         CLI   SCOPY,C'F'                                                       
         BNE   *+6                                                              
         BCTR  R3,0                                                             
         MVC   KWXHEAD(24),=C'KWX XX-99999999 SENT TO '                         
         MVC   KWXHEAD+4(11),QLDESC                                             
         LA    RF,KWXHEAD+24                                                    
         EDIT  (R3),(3,0(RF)),ALIGN=LEFT,ZERO=NOBLANK                           
         OI    0(RF),C'0'                                                       
         AR    RF,R0                                                            
         SR    R0,R0                                                            
         MVC   1(13,RF),=C' DESTINATIONS'                                       
         CH    R3,=H'1'                                                         
         BNER  RE                                                               
         MVI   13(RF),C' '                                                      
         BR    RE                                                               
         SPACE 1                                                                
T262     LH    R3,DESTNUM          COMPLETE ACCOUNTING RECORD                   
         SH    R3,ERRNUM                                                        
         CLI   SCOPY,C'F'                                                       
         BNE   *+6                                                              
         BCTR  R3,0                                                             
         LTR   R3,R3                                                            
         BZ    T270                                                             
         LA    R3,IO+500                                                        
         LA    R4,2                                                             
         BCTR  R5,0                                                             
         LA    R1,KWXANEXT                                                      
T264     LR    R6,R1               BUILD DEST ID STRING ELEMENTS OF UP          
         USING KWXDEL,R6           TO 125 ID'S PER ELEMENT                      
         LA    R0,125                                                           
         MVI   KWXDEL,KWXDELQ                                                   
         MVI   KWXDLEN,125*2+2     MAX EL LEN                                   
         LA    R1,KWXDID                                                        
         B     *+12                                                             
T266     BCT   R0,*+8                                                           
         B     T264                                                             
         MVC   0(2,R1),0(R3)                                                    
         LA    R1,2(R1)                                                         
         BXLE  R3,R4,T266                                                       
         LR    R0,R1               SET LAST ELEMENT LENGTH                      
         SR    R0,R6                                                            
         STC   R0,KWXDLEN                                                       
         MVI   0(R1),0                                                          
         SR    R1,R7               SET RECORD LENGTH                            
         LA    R1,1(R1)                                                         
         STH   R1,KWXRLEN                                                       
         DROP  R6                                                               
         SPACE 1                                                                
T268     BAS   RE,WRITE            WRITE ACCOUNTING RECORD                      
         MVC   AKWX,DMCB+8                                                      
         SPACE 1                                                                
T270     CLI   SCOPY,0             SENDER'S COPY IF REQUIRED                    
         BNE   T280                                                             
         BAS   RE,SETPL                                                         
         MVI   QLSUBID,C'S'                                                     
         MVC   QLSRCID,SAVTUSER                                                 
         MVI   SCOPY,C'Y'                                                       
         MVC   PLB,PLKWX                                                        
T275     BAS   RE,PRINT                                                         
         MVI   PLB,HEADOF                                                       
         BAS   RE,PRINT                                                         
         MVI   PLB,PR1SP1                                                       
         MVC   PLB+1(L'KWXHEAD),KWXHEAD                                         
         BAS   RE,PRINT                                                         
         MVI   PLB,PR1SP1                                                       
         MVC   PLB+01(L'KWXAUPR),KWXAUPR                                        
         MVC   PLB+15(L'KWXAUTH),KWXAUTH                                        
         MVC   PLB+26(L'KWXSEPR),KWXSEPR                                        
         MVC   PLB+35(L'KWXSEND),KWXSEND                                        
         BAS   RE,PRINT                                                         
         B     T238                                                             
         SPACE 1                                                                
T280     LA    R2,KWXAUTHH                                                      
         B     EXIT                                                             
         EJECT                                                                  
SETPL    XC    PLKWX,PLKWX         SET UP INITIAL PRINT LINE IN PLKWX           
         MVI   QLSOFLAB,X'00'                                                   
         MVI   QLEXTRA,X'FF'       SET NEW CALL                                 
         MVI   QLFLAG,X'00'                                                     
         MVI   QLSUBID,C'R'                                                     
         MVC   QLSUBID+1(2),SAVTPOW                                             
         MVI   QLCLASS,C'K'                                                     
         MVI   QLSTAT,0                                                         
         MVI   QLLINET,X'C0'       FIXED LEN WITH CC                            
         MVI   QLLINEW,132         FIXED LEN 132                                
         MVC   QLRETNL,=H'24'      LIVE RETAIN HOURS                            
         MVC   QLRETND,=H'24'      DEAD RETAIN HOURS                            
         MVC   QLDESC(2),SAVTAGID                                               
         MVI   QLDESC+2,C'-'                                                    
         MVC   QLDESC+3(1),ALPHAMON                                             
         MVC   QLDESC+4(2),TODAY+4                                              
         MVC   QLDESC+6(4),TIMENOW                                              
         MVC   QLDESC+10(1),ALPHASEC                                            
         MVI   QLLPP,68                                                         
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO COUNT PAGES/LINES/WORDS/CHARACTERS IN A KWX           
*              ON ENTRY PLB = PRINT LINE WITH SPACE AT +133                     
*                       R7  = A(ACCOUNT RECORD) COVERED BY KWXACD               
         SPACE 1                                                                
ACCOUNT  NTR1  BASE=ABASE                                                       
         L     R9,A2NDBASE                                                      
         LM    R1,R4,KWXAPAGE                                                   
         CLI   PLB,HEADOF                                                       
         BL    *+8                                                              
         LA    R1,1(R1)            INCREMENT PAGE COUNT                         
         LA    R5,PLB+1                                                         
         LA    RE,1                                                             
         LA    RF,PLB+133                                                       
         MVI   ACCMARK,0           SET MARKER TO INDICATE BLANK LINE            
         SPACE 1                                                                
ACC1     CLI   0(R5),C'A'          INCREMENT CHARACTER COUNT                    
         BL    ACC2                                                             
         MVI   ACCMARK,X'60'       X'40' = WORD, X'20' = LINE                   
         LA    R4,1(R4)                                                         
         B     ACCBXLE                                                          
         SPACE 1                                                                
ACC2     TM    ACCMARK,X'40'       INCREMENT WORD COUNT                         
         BZ    ACCBXLE                                                          
         LA    R3,1(R3)                                                         
         NI    ACCMARK,X'BF'                                                    
ACCBXLE  BXLE  R5,RE,ACC1                                                       
         SPACE 1                                                                
ACCEND   TM    ACCMARK,X'20'       INCREMENT LINE COUNT                         
         BZ    *+8                                                              
         LA    R2,1(R2)                                                         
         STM   R1,R4,KWXAPAGE                                                   
         B     ANXIT1                                                           
         EJECT                                                                  
*              DATAMGR CALLS                                                    
*              ON EXIT CC = EQU AFTER AN ERROR                                  
*              AFTER PRINT PLB IS CLEARED TO SPACES                             
*              AFTER A PRINT ERROR ROUTINE RETURNS TO T255                      
         SPACE 1                                                                
READ     NTR1                                                                   
         LA    R2,=C'DMREAD'                                                    
         LA    R3,=C'CTFILE'                                                    
         LA    R4,KEY                                                           
         B     DM2                                                              
         SPACE 1                                                                
WRITE    NTR1                                                                   
         LA    R2,=C'DMADD'                                                     
         LA    R3,=C'KWXFILE'                                                   
         LA    R4,DUB                                                           
DM2      LA    R5,KEY                                                           
         LA    R6,WORK                                                          
DMALL    STM   R2,R6,DMCB                                                       
         GOTO1 ADATAMGR,DMCB                                                    
         CLI   DMCB+8,0                                                         
         BE    DMOK                                                             
         B     DMERR                                                            
         SPACE 1                                                                
PSEQ     NTR1  BASE=ABASE                                                       
         L     R9,A2NDBASE                                                      
*                                                                               
PSEQ1    CLI   PLA,0               TEST IF CALL TO LOCATE REPORT                
         BNE   PSEQ2                                                            
         CLC   PLA+2(6),=C'LOCATE'                                              
         BNE   PSEQ2                                                            
         XC    NDXA,NDXA           CLEAR INDEX FOR LOCATE                       
         MVC   NDXA+UKSRCID-UKKEY(2),PLA+QLSRCID-PQPLD                          
         MVC   NDXA+UKSUBID-UKKEY(3),PLA+QLSUBID-PQPLD                          
         MVC   NDXA+UKREPNO-UKKEY(2),PLA+QLREPNO-PQPLD                          
         GOTO1 ADATAMGR,DMCB,(X'08',INDEX),PRTQUE,NDXA,PLA,APBUFFA              
         CLI   DMCB+8,0                                                         
         BNE   PSEQ3A                                                           
         XC    PLA(4),PLA          READ REPORT HEADER INTO PLA                  
         MVI   PLA+4,C'L'                                                       
         GOTO1 ADATAMGR,DMCB,(X'00',RANDOM)                                     
         B     PSEQ3A                                                           
*                                                                               
PSEQ2    CLI   PLA,X'FF'           TEST IF CALL TO TERMINATE REPORT             
         BNE   PSEQ3                                                            
         GOTO1 ADATAMGR,DMCB,(X'00',BUFF),PRTQUE,NDXA,PLA,APBUFFA               
         MVI   PLA,X'FF'                                                        
         MVI   DMCB+8,X'80'        RETURN EOF FOR TERMINATE REQUEST             
         B     PSEQ3A                                                           
*                                                                               
PSEQ3    GOTO1 ADATAMGR,DMCB,(X'00',REAF),PRTQUE,NDXA,PLA,APBUFFA               
         TM    DMCB+8,X'80'        TEST END OF REPORT                           
         BZ    PSEQ3A                                                           
         XC    PLA,PLA                                                          
         MVI   PLA,X'FF'           SET END OF REPORT IN PLA                     
         MVI   DMCB+8,X'80'        AND MAKE SURE ONLY EOF RETURNED              
PSEQ3A   MVC   PASAVE,PLA                                                       
         TM    DMCB+8,X'7F'        TEST ERRORS EXCEPT EOF                       
         BZ    DMOK                                                             
*                                                                               
PSEQ4    MVI   PASAVE,X'FF'        TERMINATE/FLUSH WRITE ON ERROR               
         CLI   PBSAVE,X'FF'                                                     
         BE    DMERR                                                            
         MVC   PLB,PLA                                                          
         BAS   RE,PRINT                                                         
         B     DMERR                                                            
         SPACE 1                                                                
PRINT    NTR1  BASE=ABASE                                                       
         L     R9,A2NDBASE                                                      
         GOTO1 ADATAMGR,DMCB,(X'00',DMPRINT),PRTQUE,0,PLB,ATIA                  
         MVC   WORK(1),PLB                                                      
         MVC   PLB,SPACES                                                       
         TM    DMCB+8,X'FE'                                                     
         BNZ   *+14                                                             
         MVC   PBSAVE,WORK                                                      
         B     DMOK                                                             
         CLI   PASAVE,TRMINATE                                                  
         BE    *+12                                                             
         MVI   PLA,TRMINATE        IF ERROR DO A TERMINATE READ                 
         BAS   RE,PSEQ                                                          
         L     RD,AREGSAVE                                                      
         LM    RE,RC,12(RD)                                                     
         B     T255                                                             
         SPACE 1                                                                
DMERR    SR    RC,RC                                                            
DMOK     LTR   RC,RC                                                            
         B     ANXIT1                                                           
         EJECT                                                                  
*              ERROR HANDLING AND EXIT                                          
         SPACE 1                                                                
ERROR    MVC   KWXHEAD(13),=C'*** ERROR ***'                                    
         GOTO1 ACALLOV,DMCB,(2,APBUFFA),0                                       
         CLI   DMCB+4,X'FF'                                                     
         BE    EXIT                                                             
         L     RF,APBUFFA                                                       
         GOTO1 (RF),DMCB,(FERN,ATWA),(FNDX,0)                                   
         SPACE 1                                                                
EXIT     OI    6(R2),X'40'                                                      
         CLI   PASAVE,TRMINATE                                                  
         BE    *+12                                                             
         MVI   PLA,TRMINATE                                                     
         BAS   RE,PSEQ                                                          
         XMOD1 1                                                                
         EJECT                                                                  
RELO     DC    A(0)                                                             
PRTQUE   DC    CL8'PRTQUE'                                                      
DMPRINT  DC    CL8'DMPRINT'                                                     
INDEX    DC    CL8'INDEX'                                                       
REAF     DC    CL8'READ'                                                        
RANDOM   DC    CL8'RANDOM'                                                      
BUFF     DC    CL8'BUFF  '                                                      
         LTORG                                                                  
         EJECT                                                                  
*SRKWXDSECT                                                                     
       ++INCLUDE SRKWXDSECT                                                     
         EJECT                                                                  
*SRKWXACD                                                                       
       ++INCLUDE SRKWXACD                                                       
         EJECT                                                                  
*SRKWXFFD                                                                       
       ++INCLUDE SRKWXFFD                                                       
         EJECT                                                                  
*DMPRTQL                                                                        
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
*DMPRTQK                                                                        
       ++INCLUDE DMPRTQK                                                        
         EJECT                                                                  
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*DDCOMFACS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*DDFLDHDR                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
*FASYSFAC                                                                       
         PRINT OFF                                                              
       ++INCLUDE FASYSFAC                                                       
         PRINT ON                                                               
*FAUTL                                                                          
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SRKWX00   09/03/03'                                      
         END                                                                    
