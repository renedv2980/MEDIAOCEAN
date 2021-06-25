*          DATA SET SRBIG00    AT LEVEL 002 AS OF 09/09/14                      
*PHASE T13200A                                                                  
*INCLUDE EXPAND                                                                 
*INCLUDE TWANG                                                                  
         TITLE '$BIG - ONLINE BIG LETTERS'                                      
         PRINT NOGEN                                                            
BIG      CSECT                                                                  
         NMOD1 WORKX-WORKD,**$BIG**                                             
         USING WORKD,RC                                                         
         ST    R1,APLIST                                                        
*                                                                               
INIT     L     R7,0(R1)            R7=A(SYSFAC)                                 
         MVC   ADATAMGR,VDATAMGR-SYSFACD(R7)                                    
*                                                                               
         L     R7,VSSB-SYSFACD(R7) R7=A(SSB)                                    
         MVI   ALLC,0                                                           
         TM    SSBSTAT6-SSBD(R7),SSB6ALLC                                       
         BZ    *+8                                                              
         OI    ALLC,ALLCHRS        SET ALL CHARACTERS VALID FLAG                
         L     R7,SSBXLAT-SSBD(R7)                                              
         ST    R7,AXLAT            SAVE A(CTRYXLAT TABLES)                      
*                                                                               
         L     R7,8(R1)            R7=A(UTL)                                    
         MVC   CTRY,TCTRY-UTLD(R7)                                              
*                                                                               
INIT1    L     RA,20(R1)           RA=A(TWA)                                    
         USING SRBIGFFD,RA                                                      
         BAS   RE,CLEAR            CLEAR DISPLAY AREA TO SPACES                 
*                                                                               
INIT2    CLC   BIGSRV+4(2),=C',C'  TEST =BIG,C FOR CODEPAGE                     
         BNE   INIT2A                                                           
         MVC   BIGFRST(8),=C'CODEPAGE'                                          
         CLI   BIGSRV+6,C'U'       TEST =BIG,CU FOR CODEPAGE/UPPER              
         BNE   CODEPAGE                                                         
         MVC   BIGSCND(5),=C'UPPER'                                             
         B     CODEPAGE                                                         
INIT2A   CLC   BIGFRST(7),=C'CODEPAGE'                                          
         BE    CODEPAGE                                                         
         EJECT                                                                  
**********************************************************************          
*HANDLE INPUT WORDS                                                  *          
**********************************************************************          
WORDS    LA    R2,BIGFRSTH                                                      
         LA    R3,BIGOUT+74                                                     
         BAS   RE,BIGGER                                                        
         LA    R4,73                                                            
         MHI   R4,9                                                             
         LA    R3,BIGOUT+74                                                     
         AR    R3,R4                                                            
         LA    R2,BIGSCNDH                                                      
         BAS   RE,BIGGER                                                        
         BAS   RE,REVERSE                                                       
         LA    R2,BIGFRSTH                                                      
         LA    R3,MESSA                                                         
         B     XIT                                                              
*                                                                               
ERRB     LA    R3,MESSB                                                         
*                                                                               
XIT      BAS   RE,FOUTEM                                                        
         FOUT  BIGHEADH,(R3),60                                                 
         OI    6(R2),X'40'                                                      
*                                                                               
XITX     XMOD1 1                                                                
         EJECT                                                                  
**********************************************************************          
*HANDLE CODEPAGE/CODEPAGN/CODEPAGO TO DIUSPLAY ALL HEX CHARACTERS    *          
**********************************************************************          
CODEPAGE CLI   BIGFRST+7,C'O'      TEST IF OLD/NEW CODEPAGE WANTED              
         BNE   *+8                                                              
         OI    ALLC,OLDPAGE        SET OLD CODE PAGE WANTED                     
         CLI   BIGFRST+7,C'N'                                                   
         BNE   *+8                                                              
         OI    ALLC,NEWPAGE        SET NEW CODE PAGE WANTED                     
         MVI   CTRYINP,0                                                        
         CLI   BIGFRST+8,C'0'      TEST IF COUNTRY CODE INPUT                   
         BL    CODE1                                                            
         CLI   BIGFRST+8,C'9'                                                   
         BH    CODE1                                                            
         MVC   CTRYINP,BIGFRST+8   CODEPAGE# FOR COUNTRY #                      
         NI    CTRYINP,X'0F'                                                    
         CLC   CTRY,CTRYINP                                                     
         BE    *+8                                                              
         OI    ALLC,NEWCTRY        SET NEW COUNTRY INPUT                        
         MVC   CTRY,CTRYINP                                                     
*                                                                               
CODE1    L     RE,AXLAT            GET A(TRANSLATE TABLES)                      
         TM    ALLC,OLDPAGE                                                     
         BO    CODE1B                                                           
         TM    ALLC,NEWPAGE                                                     
         BO    CODE1A                                                           
         TM    ALLC,ALLCHRS                                                     
         BZ    CODE1B                                                           
CODE1A   AHI   RE,-4               GET A(ALL CHARACTERS VALID TABLES)           
         L     RE,0(RE)                                                         
CODE1B   LLC   R0,CTRY             INDEX INTO COUNTRY                           
         SLL   R0,4                                                             
         AR    RE,R0                                                            
CODE1C   CLC   BIGSCND(5),=C'UPPER'                                             
         BNE   CODE1D                                                           
         OI    ALLC,UPPER          SET USING UPPER CASE                         
         LA    RE,8(RE)            SET UPPER CASE TRANSLATE TABLE               
         B     CODE1F                                                           
CODE1D   CLC   BIGSCND(5),=C'EXTRA'                                             
         BNE   CODE1E                                                           
         OI    ALLC,UPPER+EXTRA    SET USING UPPER CASE FROM XTRAINFO           
         LA    RE,8(RE)            SET UPPER CASE TRANSLATE TABLE               
         L     R1,APLIST                                                        
         L     R1,32(R1)           SRPAR9 HAS A(XTRAINFO)                       
         LT    R0,28(R1)                                                        
         BZ    CODE1F                                                           
         LA    RE,28(R1)                                                        
         B     CODE1F                                                           
CODE1E   LA    RE,12(RE)           SET LOWER CASE TRANSLATE TABLE               
CODE1F   MVC   AXTAB,0(RE)                                                      
*                                                                               
CODE2    LA    R4,BIGBIGH          R4=A(DISPLAY AREA LINE)                      
         MVC   12(64,R4),CODEHDR                                                
         OI    6(R4),X'80'                                                      
         LA    R4,81(R4)                                                        
         LA    R5,16               R5=NUMBER OF LINES TO DISPLAY                
         LA    R6,CODETAB          R6=A(CODE TABLE ENTRY)                       
*                                                                               
CODE3    MVC   WORK,0(R6)          TRANSLATE THE CHARACTERS                     
         L     RE,AXTAB                                                         
         TR    WORK(18),0(RE)                                                   
*                                                                               
CODE4    MVC   08(2,R4),WORK                                                    
         LA    RE,12(R4)           RE=A(NEXT DISPLAY LINE CHARACTER)            
         LA    RF,WORK+2           RF=A(NEXT CODE TABLE CHARACTER)              
         LA    R0,16               R0=NUMBER OF CHARACTERS TO DISPLAY           
*                                                                               
CODE5    MVC   0(1,RE),0(RF)       MOVE CHARACTER FROM TABLE TO DISPLAY         
         LA    RE,4(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,CODE5                                                         
         OI    6(R4),X'80'                                                      
*                                                                               
CODE6    LA    R4,81(R4)           BUMP TO NEXT DISPLAY LINE                    
         LA    R6,18(R6)           BUMP TO NEXT CODE TABLE ENTRY                
         BCT   R5,CODE3                                                         
*                                                                               
CODE7    LA    R3,MESSC            OUTPUT HEADER MESSAGE                        
         FOUT  BIGHEADH,(R3),60                                                 
         LLC   RF,CTRY                                                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  BIGHEAD+39(2),DUB+6(2)                                           
         LA    R2,BIGFRSTH                                                      
         OI    6(R2),X'40'         POSITION CURSOR                              
*                                                                               
CODE8    MVC   WORK(4),BIGREV      TEST TO WRITE SCREEN TO PRINT QUEUE          
         OC    WORK(4),SPACES                                                   
         CLC   WORK(4),=C'PRTQ'                                                 
         BE    CODEPQ                                                           
         CLC   WORK(4),=C'PQ  '                                                 
         BE    CODEPQ                                                           
*                                                                               
CODE9    CLI   BIGREVH+5,0         TEST TRANSLATE TEXT STRING INPUT             
         BE    XITX                                                             
         MVC   WORK(16),BIGREV                                                  
         L     RE,AXTAB                                                         
         TR    WORK(16),0(RE)                                                   
         FOUT  BIGRSLTH,WORK,16                                                 
         B     XITX                                                             
         EJECT                                                                  
**********************************************************************          
*OUTPUT CODEPAGE AS A PRINT QUEUE REPORT                             *          
**********************************************************************          
CODEPQ   GOTO1 =V(TWANG),PARA,(RA),BIGOUT,0,RR=RB                               
*                                                                               
CODEPQ1  LA    R7,PQLINE           SET ATTRIBUTES AND OPEN REPORT               
         USING PQPLD,R7                                                         
         XC    PQLINE,PQLINE                                                    
         MVI   QLEXTRA,X'FF'                                                    
*&&UK*&& MVC   QLSRCID,=H'38'      DDS1                                         
*&&US*&& MVC   QLSRCID,=H'17'      SJR                                          
         MVC   QLSUBID,=C'BIG'                                                  
         MVI   QLCLASS,C'S'                                                     
         MVI   QLLINET,X'C0'                                                    
         MVI   QLLINEW,132                                                      
         MVC   QLRETNL,=H'24'                                                   
         MVC   QLRETND,=H'24'                                                   
         MVC   QLDESC,=CL11'CODEPAGE   '                                        
         TM    ALLC,ALLCHRS+NEWPAGE                                             
         BZ    *+8                                                              
         MVI   QLDESC+09,C'A'      SET ALL CHARACTERS VALID                     
         TM    ALLC,UPPER                                                       
         BZ    *+8                                                              
         MVI   QLDESC+10,C'U'      SET UPPER CASE                               
         MVC   QLMAKER,=C'DSR  '                                                
         GOTO1 ADATAMGR,DMCB,=C'DMPRINT',=C'PRTQUE',0,PQLINE,PQBUFF             
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         XR    R0,R0                                                            
         ICM   R0,3,QLREPRNO       EXTRACT REPORT NUMBER                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
*                                                                               
CODEPQ2  MVI   PQLINE,X'8B'        POSITION TO TOP OF PAGE                      
         MVI   PQLINE+1,C' '                                                    
         MVC   PQLINE+2(131),PQLINE+1                                           
         GOTO1 ADATAMGR,DMCB                                                    
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         LA    R6,BIGOUT           R6=A(NEXT SCREEN LINE)                       
         LA    R0,24                                                            
*                                                                               
CODEPQ3  MVI   PQLINE,X'09'        PRINT ALL SCREEN LINES                       
         MVC   PQLINE+1(80),0(R6)                                               
         GOTO1 ADATAMGR,DMCB                                                    
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         LA    R6,80(R6)                                                        
         BCT   R0,CODEPQ3                                                       
*                                                                               
CODEPQ4  MVI   PQLINE,X'FF'        CLOSE REPORT                                 
         GOTO1 ADATAMGR,DMCB                                                    
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
CODEPQ5  LA    RE,BIGHEAD+29       OUTPUT REPORT ID                             
         MVC   0(7,RE),=C'- PRTQ='                                              
         LA    RE,7(RE)                                                         
*&&UK*&& MVC   0(4,RE),=C'DDS1'                                                 
*&&UK*&& LA    RE,4(RE)                                                         
*&&US*&& MVC   0(3,RE),=C'SJR'                                                  
*&&US*&& LA    RE,3(RE)                                                         
         MVC   0(5,RE),=C',BIG,'                                                
         LA    RE,5(RE)                                                         
         UNPK  0(5,RE),DUB+4(4)                                                 
         B     XITX                                                             
         DROP  R7                                                               
         EJECT                                                                  
**********************************************************************          
*ROUTINE TO CLEAR STORAGE                                            *          
**********************************************************************          
CLEAR    MVI   BIGOUT,C' '                                                      
         MVC   BIGOUT+1(72),BIGOUT                                              
         LA    R4,BIGOUT                                                        
         LA    R5,19                                                            
*                                                                               
CLEAR2   MVC   73(73,R4),0(R4)                                                  
         LA    R4,73(R4)                                                        
         BCT   R5,CLEAR2                                                        
         MVC   REVMASK,BIGOUT                                                   
         MVC   WORK,BIGOUT                                                      
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
*ROUTINE TO EXPAND                                                   *          
**********************************************************************          
BIGGER   MVI   WORK,C' '                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         SR    R4,R4                                                            
         ICM   R4,1,5(R2)                                                       
         BZR   RE                                                               
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),8(R2)                                                    
         LA    R4,WORK             SCAN FOR OBSCENITIES                         
         LA    R5,8                                                             
*                                                                               
OBSCENE  CLC   0(4,R4),=X'C6E4C3D2'                                             
         BE    ERRB                                                             
         CLC   0(4,R4),=X'C3E4D5E3'                                             
         BE    ERRB                                                             
         CLC   0(4,R4),=X'E2C8C9E3'                                             
         BE    ERRB                                                             
         CLC   0(5,R4),=X'D7D9C9C3D2'                                           
         BE    ERRB                                                             
         LA    R4,1(R4)                                                         
         BCT   R5,OBSCENE                                                       
         LR    R0,RE                                                            
         LA    R4,WORK                                                          
         LA    R5,8                                                             
*                                                                               
BIGGER2  GOTO1 =V(EXPAND),PARA,(R4),(65,(R3)),RR=RB                             
         LA    R4,1(R4)                                                         
         LA    R3,9(R3)                                                         
         BCT   R5,BIGGER2                                                       
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
*ROUTINE TO REVERSE PATTERN                                          *          
**********************************************************************          
REVERSE  SR    R4,R4                                                            
         ICM   R4,1,BIGREVH+5                                                   
         BZR   RE                                                               
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         OC    BIGREV(0),SPACES    CONVERT TO UPPER                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   REVMASK(0),BIGREV                                                
         LA    R4,REVMASK(R4)      R4 TO LAST CHARACTER OF MASK                 
         CLC   REVMASK,SPACES                                                   
         BER   RE                                                               
         LA    R5,BIGOUT                                                        
         LA    R6,1387                                                          
*                                                                               
REV2     CLI   0(R5),C' '                                                       
         BE    REV4                                                             
         MVI   0(R5),C' '                                                       
         B     REV6                                                             
*                                                                               
REV4     MVC   0(1,R5),REVMASK                                                  
*                                                                               
REV6     LA    R5,1(R5)                                                         
         IC    R7,REVMASK          ROTATE MASK                                  
         MVC   REVMASK(7),REVMASK+1                                             
         STC   R7,0(R4)                                                         
         BCT   R6,REV2                                                          
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
*ROUTINE TO FOUT TO SCREEN                                           *          
**********************************************************************          
FOUTEM   LA    R4,BIGOUT                                                        
         LA    R5,BIGBIGH                                                       
         LA    R6,19                                                            
*                                                                               
FOUTEM2  CLC   0(73,R4),8(R5)                                                   
         BE    FOUTEM4                                                          
         FOUT  (R5),(R4),73                                                     
*                                                                               
FOUTEM4  LA    R4,73(R4)                                                        
         LA    R5,81(R5)                                                        
         BCT   R6,FOUTEM2                                                       
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
*CONSTANTS, LITERALS AND TABLES                                      *          
**********************************************************************          
         LTORG                                                                  
*                                                                               
SPACES   DC    CL16' '                                                          
MESSA    DC    CL60'Enter one or two words'                                     
MESSB    DC    CL60'Watch your language'                                        
MESSC    DC    CL60'All hex characters displayed - Country#00'                  
*                                                                               
CODEHDR  DC    CL32'0-  1-  2-  3-  4-  5-  6-  7-  '                           
         DC    CL32'8-  9-  A-  B-  C-  D-  E-  F-  '                           
                                                                                
***********************************************************************         
*HEX CHRS 20 THRU 2D ARE DATA DICTIONARY CHRS AND ARE SET TO SPACE    *         
***********************************************************************         
CODETAB  DS    0CL18                                                            
         DC    C'-0',X'00104030405060708090A0B0C0D0E0F0'                        
         DC    C'-1',X'01114031415161718191A1B1C1D1E1F1'                        
         DC    C'-2',X'02124032425262728292A2B2C2D2E2F2'                        
         DC    C'-3',X'03134033435363738393A3B3C3D3E3F3'                        
         DC    C'-4',X'04144034445464748494A4B4C4D4E4F4'                        
         DC    C'-5',X'05154035455565758595A5B5C5D5E5F5'                        
         DC    C'-6',X'06164036465666768696A6B6C6D6E6F6'                        
         DC    C'-7',X'07174037475767778797A7B7C7D7E7F7'                        
         DC    C'-8',X'08184038485868788898A8B8C8D8E8F8'                        
         DC    C'-9',X'09194039495969798999A9B9C9D9E9F9'                        
         DC    C'-A',X'0A1A403A4A5A6A7A8A9AAABACADAEAFA'                        
         DC    C'-B',X'0B1B403B4B5B6B7B8B9BABBBCBDBEBFB'                        
         DC    C'-C',X'0C1C403C4C5C6C7C8C9CACBCCCDCECFC'                        
         DC    C'-D',X'0D1D403D4D5D6D7D8D9DADBDCDDDEDFD'                        
         DC    C'-E',X'0E1E2E3E4E5E6E7E8E9EAEBECEDEEEFE'                        
         DC    C'-F',X'0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFFF'                        
         EJECT                                                                  
**********************************************************************          
*DSECTS FOR MODULE                                                   *          
**********************************************************************          
WORKD    DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
APLIST   DS    A                                                                
AXLAT    DS    A                                                                
AXTAB    DS    A                                                                
ADATAMGR DS    A                                                                
PARA     DS    6F                                                               
DMCB     DS    6F                                                               
WORK     DS    CL32                                                             
REVMASK  DS    CL8                                                              
BIGOUT   DS    2000C                                                            
*                                                                               
PQLINE   DS    CL133                                                            
*                                                                               
ALLC     DS    XL1                                                              
ALLCHRS  EQU   X'80'                                                            
OLDPAGE  EQU   X'40'                                                            
NEWPAGE  EQU   X'20'                                                            
NEWCTRY  EQU   X'10'                                                            
UPPER    EQU   X'08'                                                            
EXTRA    EQU   X'04'                                                            
*                                                                               
CTRY     DS    XL1                                                              
CTRYINP  DS    XL1                                                              
PQBUFF   DS    18432C                                                           
WORKX    EQU   *                                                                
         EJECT                                                                  
SRBIGFFD DSECT                                                                  
         DS    XL64                                                             
       ++INCLUDE SRBIGFFD                                                       
         EJECT                                                                  
*DDFLDIND                                                                       
       ++INCLUDE DDFLDIND                                                       
                                                                                
*DMPRTQL                                                                        
       ++INCLUDE DMPRTQL                                                        
                                                                                
*FASYSFAC                                                                       
       ++INCLUDE FASYSFAC                                                       
                                                                                
*FASSB                                                                          
       ++INCLUDE FASSB                                                          
                                                                                
*FAUTL                                                                          
       ++INCLUDE FAUTL                                                          
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SRBIG00   09/09/14'                                      
         END                                                                    
