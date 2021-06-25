*          DATA SET PELEV00    AT LEVEL 147 AS OF 01/19/07                      
*PHASE TE0A00A                                                                  
*&&      SET   NOP=N                                                            
         PRINT NOGEN                                                            
PELEVX   CSECT                                                                  
         NMODL WORKX-WORKD,**LEVX*,RA,RR=R4,CLEAR=YES                           
         USING WORKD,RC                                                         
         ST    R4,RELO                                                          
         ST    RD,SAVERD                                                        
         MVC   SVPARMS,0(R1)                                                    
         L     R8,ACOMFACS                                                      
         USING COMFACSD,R8         R8=A(COMFACS)                                
         L     R9,ATWA                                                          
         USING PELEVFFD,R9         R9=A(TWA)                                    
         USING TWAD,TWAHDR                                                      
         LA    R7,3072(R9)         R7=A(TWA SAVE AREA)                          
         USING SAVED,R7                                                         
*                                                                               
         USING PEPANLD,PERKEY      SET UP KEY USING                             
*                                                                               
         L     RF,=A(IOAREA-WORKD)                                              
         LA    RF,WORKD(RF)                                                     
         ST    RF,AIOAREA                                                       
*                                                                               
         L     RF,ATIOB            SET CURSOR INF FROM TIOB                     
         USING TIOBD,RF                                                         
         SR    R0,R0                                                            
         ICM   R0,3,TIOBCURD                                                    
         AR    R0,R9               ADD TWA ADDRESS                              
         ST    R0,CURSOR                                                        
         MVC   PFKEY,TIOBAID       SAVE PFKEY DATA                              
         SR    R0,R0                                                            
         IC    R0,PFKEY                                                         
         CLI   PFKEY,12            TEST FOR PF 13 - 24                          
         BNH   *+8                                                              
         SH    R0,=H'12'           CONVERT TO 7 - 12                            
         STC   R0,PFKEY                                                         
*                                                                               
         BAS   RE,VALIDATE                                                      
         CLI   CHANGED,C'Y'        IF REQUEST HAS CHANGED                       
         BE    MAIN020                                                          
*                                                                               
*AIN015  EQU   *     BAS   RE,VALSEL     SEE IF ANYTHING SELECTED               
*        BNE   MAIN030                                                          
*        XC    DISPPAGE,DISPPAGE                                                
         B     MAIN040                                                          
*                                                                               
MAIN020  MVI   MODE,C'L'                                                        
         SR    R1,R1               CLEAR SAVED STORAGE                          
         SR    R0,R0                                                            
         LH    RF,=Y(SAVECLRL)                                                  
         LA    RE,SAVECLR                                                       
         MVCL  RE,R0                                                            
*                                                                               
MAIN040  BAS   RE,LIST             LIST LEVEL INFO                              
         B     EXITX                                                            
*                                                                               
EXITX    L     R1,CURSOR           SET CURSOR FLAG                              
         OI    6(R1),X'40'                                                      
         XMOD1                                                                  
         SPACE 1                                                                
*                                                                               
XITNO    LTR   RB,RB                                                            
         B     XIT1                                                             
XITYES   CR    RB,RB                                                            
*                                                                               
XIT1     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE INPUT FIELD                                                *         
***********************************************************************         
         SPACE 1                                                                
VALIDATE NTR1                                                                   
         MVC   OLDBOOK,NOWBOOK     PRESET SCAN FIELDS                           
         MVC   OLDLIB,NOWLIB                                                    
         MVC   OLDOPT,NOWOPT                                                    
         XC    NOWOPT,NOWOPT                                                    
         XC    NOWBOOK,NOWBOOK                                                  
         XC    NOWLIB,NOWLIB                                                    
         XC    BOOKLEN,BOOKLEN                                                  
*                                                                               
VALID010 SR    R1,R1               ANY LIBRARY FILTER                           
         ICM   R1,1,LEVLIBH+5                                                   
         BZ    VALID020                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8              MOVE LIB TO FIELD                            
         B     *+10                                                             
         MVC   NOWLIB(0),LEVLIB                                                 
         STC   R1,LIBLEN                                                        
         OI    LEVLIBH+6,X'80'     XMIT                                         
*                                                                               
VALID020 SR    R1,R1               ANY BOOK REQUESTED                           
         ICM   R1,1,LEVBOOH+5                                                   
         BZ    VALID025                                                         
         STC   R1,BOOKLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8              MOVE BOOKNAME TO FIELD                       
         B     *+10                                                             
         MVC   NOWBOOK(0),LEVBOO                                                
         OI    LEVBOOH+6,X'80'     XMIT                                         
*                                                                               
VALID025 SR    R1,R1               ANY OPTIONS                                  
         ICM   R1,1,LEVOPTH+5                                                   
         BZ    VALID030                                                         
         MVC   NOWOPT,LEVOPT                                                    
         OI    LEVOPTH+6,X'80'     XMIT                                         
*                                                                               
VALID030 CLC   OLDBOOK,NOWBOOK     HAS REQUEST CHANGED                          
         BNE   VALID050                                                         
         CLC   OLDLIB,NOWLIB                                                    
         BNE   VALID050                                                         
         CLC   OLDOPT,NOWOPT                                                    
         BNE   VALID050                                                         
*                                                                               
VALID040 MVI   CHANGED,C'N'        NO CHANGE                                    
         B     XITYES                                                           
*                                                                               
VALID050 MVI   CHANGED,C'Y'        YES CHANGED                                  
         B     XITYES                                                           
         EJECT                                                                  
***********************************************************************         
* LIST LEVEL INFO                                                     *         
***********************************************************************         
         SPACE 1                                                                
LIST     NTR1                                                                   
         MVC   LEVPFK,SPACES                                                    
         MVC   LEVPFK,LISTPFK                                                   
         OI    LEVPFKH+6,X'80'                                                  
*                                                                               
         MVI   PELPANID,PELPANIQ                                                
         MVC   PELBOOK,NOWBOOK                                                  
*                                                                               
         OC    KEYLAST,KEYLAST     ANY PREVIOUS KEY                             
         BZ    LIST010                                                          
         MVC   PERKEY,KEYLAST                                                   
*                                                                               
LIST005  CLI   PFKEY,8             8 IS SAME AS ENTER                           
         BE    LIST010                                                          
         CLI   PFKEY,7             7 BACK UP ONE PAGE                           
         BNE   LIST010                                                          
*                                                                               
         ICM   RF,15,LISTPAGE      BACK UP ONE PAGE                             
         CHI   RF,1                                                             
         BL    INFO2                                                            
         BNE   *+8                                                              
         LA    RF,2                                                             
         SHI   RF,2                                                             
*                                                                               
         STCM  RF,15,LISTPAGE                                                   
         MHI   RF,36                                                            
         LA    RF,KEYLIST(RF)                                                   
         MVC   PERKEY,0(RF)                                                     
*                                                                               
LIST010  L     RF,LISTPAGE         SAVE TOP KEY FOR THIS PAGE                   
         MHI   RF,36                                                            
         LA    RF,KEYLIST(RF)                                                   
         MVC   0(36,RF),PERKEY                                                  
         LA    RF,1                                                             
         A     RF,LISTPAGE                                                      
         ST    RF,LISTPAGE                                                      
         GOTO1 CDATAMGR,DMCB,DMRDHI,PERDIR,PERKEY,PERKEY                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,LEVSELH                                                       
*                                                                               
LIST015  OI    6(R4),X'80'         ZAP ALL FIELDS TO SPACES                     
         MVC   8(2,R4),SPACES                                                   
         LA    R4,10(R4)                                                        
         OI    6(R4),X'80'                                                      
         MVC   8(75,R4),SPACES                                                  
         LA    R4,83(R4)                                                        
         LA    R1,LEVPFKH          NEXT                                         
         CR    R4,R1                                                            
         BL    LIST015                                                          
*                                                                               
         LA    R4,LEVSELH                                                       
         USING LSTLINED,R4                                                      
*                                                                               
         BAS   RE,CHKBK                                                         
         BNE   LIST991                                                          
*                                                                               
         BAS   RE,CHKLIB                                                        
         BE    LIST050                                                          
*                                                                               
LIST040  GOTO1 CDATAMGR,DMCB,DMRSEQ,PERDIR,PERKEY,PERKEY                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,CHKBK                                                         
         BNE   LIST991                                                          
*                                                                               
         BAS   RE,CHKLIB                                                        
         BNE   LIST040                                                          
*                                                                               
         CLC   BOOKLEVL,PELBOOK    SAME BOOK/LEVEL/DATE                         
         BE    LIST050                                                          
*                                                                               
         MVC   LSTLIN,DASHES                                                    
*                                                                               
         LA    R4,8+2+8+75(R4)                                                  
         LA    R1,LEVPFKH          NEXT                                         
         CR    R4,R1                                                            
         BNL   LIST990                                                          
*                                                                               
LIST050  MVC   BOOKLEVL,PELBOOK                                                 
         MVC   LSTBOOK,PELBOOK                                                  
         EDIT  (B1,PELLEV),(3,LSTLEV),FILL=0                                    
         MVC   HALF,PELDAT                                                      
         XC    HALF,=X'FFFF'                                                    
         GOTO1 CDATCON,DMCB,(2,HALF),(11,LSTDATE)                               
*                                                                               
         CLI   PELTYP,C'A'                                                      
         BE    *+12                                                             
         CLI   PELTYP,C'B'                                                      
         BNE   LIST060                                                          
         MVC   LSTLIB(4),=C'PAN.'                                               
         MVC   LSTLIB+4(4),PELLIB                                               
         MVC   LSTNAME,=C'**SOURCE**'                                           
*                                                                               
         GOTO1 CDATAMGR,DMCB,GETREC,PERFIL,PELDA,AIOAREA,IOWORK                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA                                                       
         LA    R2,PELDATA-PEPANLD(R2)                                           
         LA    R1,INCLAREA                                                      
LIST051  SR    R0,R0                                                            
         CLI   0(R2),0                                                          
         BE    LIST052                                                          
         CLI   0(R2),C'I'                                                       
         BNE   *+14                                                             
         MVC   0(8,R1),2(R2)                                                    
         LA    R1,8(R1)                                                         
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     LIST051                                                          
*                                                                               
LIST052  XC    0(8,R1),0(R1)                                                    
*                                                                               
LIST060  CLI   PELTYP,C'C'                                                      
         BNE   LIST062                                                          
         MVC   LSTLIB(4),=C'PAN.'                                               
         MVC   LSTLIB+4(4),PELLIB                                               
         MVC   LSTNAME(2),=C'RM'                                                
         MVC   LSTNAME+2(8),PELTEST                                             
*                                                                               
LIST062  CLI   PELTYP,C'L'                                                      
         BNE   LIST063                                                          
         MVC   LSTLIB(4),PELLIB                                                 
         MVC   LSTLIB+4(3),=C'LIB'                                              
         MVC   LSTNAME(8),PELTEST                                               
         CLC   PELTEST,SPACES                                                   
         BNE   *+10                                                             
         MVC   LSTNAME(8),PELPROD                                               
*                                                                               
LIST063  CLI   PELTYP,C'P'                                                      
         BNE   LIST065                                                          
         MVC   LSTLIB(4),=C'PROG'                                               
         MVC   BYTE,PELLIB+3                                                    
         MVC   HALF+0(1),PELCTRY                                                
         MVC   HALF+1(1),PELLIB+3                                               
         BAS   RE,GETADV                                                        
         MVC   LSTLIB+4(4),FULL                                                 
         MVC   LSTNAME(8),PELTEST                                               
         CLC   PELTEST,SPACES                                                   
         BNE   *+10                                                             
         MVC   LSTNAME(8),PELPROD                                               
*                                                                               
LIST065  MVC   LSTCTRY,=C'UK'                                                   
         CLI   PELCTRY,C'S'                                                     
         BNE   *+10                                                             
         MVC   LSTCTRY,=C'US'                                                   
*                                                                               
         CLI   PELTYP,C'C'                                                      
         BL    LIST090                                                          
*                                                                               
LIST070  LR    R1,R4               BACK UP R1 TO PREVIOUS LINE                  
         SHI   R1,8+2+8+75                                                      
O        USING LSTLINED,R1                                                      
         CLC   O.LSTBOOK(LSTNAME-LSTBOOK),LSTBOOK                               
         BNE   LIST090                                                          
         LA    RE,LSTNAME                                                       
         LA    RF,O.LSTNAME                                                     
         LA    R0,10                                                            
LIST071  CLC   0(1,RE),0(RF)       COMPARE PHASE NAMES                          
         BNE   LIST072                                                          
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,LIST071                                                       
         B     LIST072A                                                         
*                                                                               
LIST072  CLI   0(RF),C' '          WERE THEY SAME UP TO L'OLD                   
         BE    *+12                                                             
         CLI   1(RF),C' '          WERE WE ON LAST CHR                          
         BNE   LIST090                                                          
*                                                                               
LIST072A LA    R1,O.LSTNAME        COPY THIS PHASE TO PREVIOUS LINE             
         LA    R0,3                                                             
LIST073  CLI   0(R1),C' '                                                       
         BE    LIST074                                                          
         LA    R1,11(R1)           NEXT PHASE                                   
         BCT   R0,LIST073                                                       
         B     LIST090             UNLESS NO ROOM                               
         DROP  O                                                                
*                                                                               
LIST074  MVC   0(10,R1),LSTNAME    COPY IN PHASE                                
         MVC   LSTLIN,SPACES       CLEAR THIS LINE                              
         B     LIST040                                                          
*                                                                               
LIST090  LA    R4,8+2+8+75(R4)                                                  
         LA    R1,LEVPFKH          NEXT                                         
         CR    R4,R1                                                            
         BL    LIST040                                                          
*                                                                               
LIST990  MVC   KEYLAST,PERKEY                                                   
*                                                                               
LIST991  EQU   *                                                                
*                                                                               
LIST999  B     INFO2                                                            
*                                                                               
CHKBK    SR    R1,R1                                                            
         ICM   R1,1,BOOKLEN                                                     
         BZR   RE                                                               
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   NOWBOOK(0),PELBOOK                                               
         BNER  RE                                                               
         BR    RE                                                               
*                                                                               
CHKLIB   SR    R1,R1                                                            
         ICM   R1,1,LIBLEN                                                      
         BZR   RE                                                               
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   NOWLIB(0),PELLIB                                                 
         BNER  RE                                                               
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
* GET SYSTEM NAME FROM GTADV                                          *         
***********************************************************************         
         SPACE 1                                                                
GETADV   NTR1                                                                   
         LA    RE,PGMSTAB                                                       
*                                                                               
GADV02   CLC   HALF,0(RE)          EOT                                          
         BE    GADV03                                                           
         LA    RE,6(RE)                                                         
         CLC   0(2,RE),=C'**'                                                   
         BNE   GADV02                                                           
*                                                                               
GADV03   MVC   FULL,2(RE)          GET NAME                                     
         B     XITYES                                                           
*                                                                               
PGMSTAB  DC    C'KF',C'LCSC'                                                    
         DC    C'KN',C'NEW '                                                    
         DC    C'KS',C'TTS '                                                    
         DC    C'KT',C'LTST'                                                    
         DC    C'KQ',C'LFQA'                                                    
         DC    C'KA',C'LADV'                                                    
         DC    C'S2',C'MEL '                                                    
         DC    C'SA',C'NADV'                                                    
         DC    C'SC',C'NCSC'                                                    
         DC    C'SQ',C'NFQA'                                                    
         DC    C'SR',C'REP '                                                    
         DC    C'ST',C'NTST'                                                    
         DC    C'**',C'????'                                                    
         EJECT                                                                  
***********************************************************************         
* GET SYSTEM NAME FROM GTADV                                          *         
***********************************************************************         
*        SPACE 1                                                                
*ETADV   NTR1                                                                   
*        LA    RE,FACIDTAB                                                      
*        USING FACITABD,RE                                                      
*        LHI   RF,L'FACITAB                                                     
*                                                                               
*ADV02   CLI   FACITAB,255         EOT                                          
*        BE    XITNO                                                            
*        CLC   BYTE,FACISN1        MATCH ADV CHR                                
*        BE    *+8                                                              
*        BXH   RE,RF,GADV02                                                     
*                                                                               
*        MVC   FULL,FACISN4        GET NAME                                     
*        B     XITYES                                                           
*        DROP  RE                                                               
*        EJECT                                                                  
***********************************************************************         
* ERROR EXITS                                                         *         
***********************************************************************         
         SPACE 1                                                                
ERR1     MVC   LEVHDR(25),=C'No matches found         '                         
         B     ERRX                                                             
ERR2     MVC   LEVHDR(25),=C'No more books in list    '                         
         B     ERRX                                                             
ERR3     MVC   LEVHDR(25),=C'This is the first book   '                         
         B     ERRX                                                             
ERR4     MVC   LEVHDR(25),=C'Error - No report created'                         
         B     ERRX                                                             
ERR5     MVC   LEVHDR(25),=C'Error - Loading tempest  '                         
         B     ERRX                                                             
ERR6     MVC   LEVHDR(25),=C'Test systems only please '                         
         B     ERRX                                                             
INFO1    MVC   LEVHDR(25),=C'Enter field to search for'                         
         B     ERRX                                                             
INFO2    MVC   LEVHDR(25),=C'Book details displayed   '                         
         B     ERRX                                                             
INFO3    MVC   LEVHDR(25),=C'Report PAN,              '                         
         EDIT  (B2,HALF),(5,LEVHDR+11),ALIGN=LEFT                               
         LA    R1,LEVHDR+12                                                     
         AR    R1,R0                                                            
         MVC   0(10,R1),=C'Spooled '                                            
         B     ERRX                                                             
*                                                                               
ERRX     OI    LEVHDRH+6,X'80'                                                  
         L     RD,SAVERD                                                        
         B     EXITX                                                            
         EJECT                                                                  
***********************************************************************         
* CONSTANTS AND LITERALS                                              *         
***********************************************************************         
         SPACE 1                                                                
DMOPEN   DC    CL8'OPEN   '                                                     
DMPRINT  DC    CL8'DMPRINT'                                                     
DMCLOSE  DC    CL8'CLOSE  '                                                     
CLOSE    DC    CL8'CLOSE  '                                                     
PRTQUE   DC    CL8'PRTQUE'                                                      
DMRDHI   DC    C'DMRDHI '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
GETREC   DC    C'GETREC '                                                       
PERDIR   DC    C'PERDIR '                                                       
PERFIL   DC    C'PERFIL '                                                       
READ     DC    C'READ   '                                                       
PAN      DC    C'PAN    '                                                       
*                                                                               
DASHES   DC    80C'-'                                                           
DASHES2  DC    40C'- '                                                          
SPACES   DC    CL80' '                                                          
LISTPFK  DC    CL40'PF7=Up PF8=Down                         '                   
         DC    CL38'                                      '                     
DISPPFK  DC    CL40'PF7=Up PF8=Down PF5=Previous Book PF6=Ne'                   
         DC    CL38'xt Book PF3=Return PF4=Print          '                     
BOOKL1   DC    C'- - - - - BOOK=           LEVEL     DATE'                      
         DC    C'=.../.. - - - - -                  '                           
HEADER1  DC    CL40'S      Source Object name  Load librarys'                   
         DC    CL38'   Program dataspaces                 '                     
HEADER2  DC    CL40'L  Cty Lib    and library  Loadlib Testl'                   
         DC    CL38'ib adv tst new tts fqa csc            '                     
         LTORG                                                                  
       ++INCLUDE FACIDTAB                                                       
         EJECT                        adv tst new tts fqa csc                   
***********************************************************************         
* DSECT TO COVER SAVED STORAGE IN TWA                                 *         
***********************************************************************         
         SPACE 1                                                                
SAVED    DSECT                                                                  
*                                                                               
NOWOPT   DS    CL8                 IGNORE BOOKS                                 
NOWBOOK  DS    CL10                BOOK FILTER                                  
BOOKLEN  DS    CL1                 LENGTH OF BOOK FILTER                        
NOWLIB   DS    CL12                LIB FILTER                                   
LIBLEN   DS    CL1                 LENGTH - 1                                   
*                                                                               
BOOK     DS    CL10                                                             
DA       DS    CL4                                                              
*                                                                               
PREVBOOK DS    CL10                                                             
*                                                                               
MODE     DS    C                                                                
*                                                                               
JTODAY   DS    PL4                                                              
LOADJULE DS    PL4                                                              
LOADDATE DS    CL10                                                             
*                                                                               
SAVECLR  DS    0C                                                               
SAVECLRL EQU   SAVECLRX-SAVECLR                                                 
*                                                                               
KEYLIST  DS    100CL36             TOP KEYS 100 PAGES WORTH                     
KEYLAST  DS    CL36                LAST KEY ON PAGE                             
LISTPAGE DS    F                   CURRENT PAGE FOR LIST                        
*                                                                               
SAVECLRX EQU   *                                                                
         SPACE 2                                                                
***********************************************************************         
*DSECT TO COVER WORKING STORAGE                                       *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
SAVERD   DS    A                                                                
SAVERE   DS    A                                                                
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
PFKEY    DS    X                                                                
RELO     DS    A                                                                
CURSOR   DS    A                                                                
ATSARBUF DS    A                                                                
*                                                                               
IOWORK   DS    XL64                                                             
*                                                                               
CHANGED  DS    C                                                                
CODE     DS    C                                                                
NOT      DS    C                                                                
FLUSH    DS    C                                                                
*                                                                               
BOOKLEVL DS    CL13                CURRENT BOOK LEVEL DATE                      
BOOKLINE DS    CL72                                                             
*                                                                               
AIOAREA  DS    A                                                                
VPANIC   DS    A                                                                
VTSAR    DS    A                                                                
*                                                                               
         DS    0A                                                               
SVPARMS  DS    0XL32                                                            
ATIOB    DS    A                                                                
ATWA     DS    A                                                                
ALOCSYS  DS    A                                                                
ATIA     DS    A                                                                
ACOMFACS DS    A                                                                
AXTRA    DS    A                                                                
         DS    A                                                                
         DS    A                                                                
*                                                                               
APAGEBRK DS    A                                                                
*                                                                               
PERKEY   DS    CL44                                                             
*                                                                               
OLDOPT   DS    CL8                 PREVIOUS FILTERS                             
OLDBOOK  DS    CL10                                                             
OLDLIB   DS    CL12                                                             
*                                                                               
DMCB     DS    6F                                                               
WORK     DS    XL1024                                                           
DMWORK   DS    12D                                                              
*                                                                               
NONHDR   DS    CL5                                                              
NONLINE  DS    CL80                                                             
*                                                                               
         DS    0D                                                               
WLINEL   DS    CL8                                                              
WLINE    DS    0CL255                                                           
WCTL     DS    C                                                                
WDAT     DS    CL254                                                            
*                                                                               
INCLADDR DS    A                                                                
INCLAREA DS    500CL8                                                           
*                                                                               
IOAREA   DS    6144C                                                            
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* LIST LINES                                                          *         
***********************************************************************         
         SPACE 1                                                                
LSTLINED DSECT                                                                  
LSTSELH  DS    XL8                                                              
LSTSEL   DS    CL2                                                              
LSTLINH  DS    XL8                                                              
LSTLIN   DS    0CL75                                                            
LSTBOOK  DS    CL10                                                             
         DS    CL1                                                              
LSTLEV   DS    CL3                                                              
         DS    CL1                                                              
LSTDATE  DS    CL8                                                              
         DS    CL1                                                              
LSTCTRY  DS    CL2                                                              
         DS    CL1                                                              
LSTLIB   DS    CL8                                                              
         DS    CL1                                                              
LSTNAME  DS    CL10                                                             
LSTNAMEX DS    CL1                                                              
         DS    CL21                                                             
         SPACE 1                                                                
LSTL2    DSECT                                                                  
LST2SEL  DS    XL8                                                              
LST2SELH DS    CL2                                                              
LST2LINH DS    XL8                                                              
LSTSLIN  DS    0CL75                                                            
LST2CTY  DS    CL3                                                              
         DS    CL1                                                              
LST2SRC  DS    CL6                                                              
         DS    CL1                                                              
LST2OBJ  DS    CL13                                                             
         DS    CL1                                                              
LST2LOAD DS    CL8                                                              
         DS    CL1                                                              
LST2TEST DS    CL8                                                              
         DS    CL1                                                              
LST2DSPC DS    CL23                                                             
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER TWA                                                  *         
***********************************************************************         
         SPACE 1                                                                
PELEVFFD DSECT                                                                  
TWAHDR   DS    CL64                                                             
       ++INCLUDE PELEVFFD                                                       
         SPACE 1                                                                
         DS    CL128' '            SPACE FOR HILITE HEADERS                     
TWAEND   EQU   *                                                                
*DDCOMFACS                                                                      
*DDACCFACS                                                                      
*PEGENPAN                                                                       
*FATIOB                                                                         
*DMPRTQL                                                                        
*FATWA                                                                          
*FACIDTABD                                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDACCFACS                                                      
       ++INCLUDE PEGENPAN                                                       
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE FATWA                                                          
       ++INCLUDE FACIDTABD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'147PELEV00   01/19/07'                                      
         END                                                                    
