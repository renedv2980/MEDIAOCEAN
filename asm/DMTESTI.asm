*          DATA SET DMTESTI    AT LEVEL 003 AS OF 08/14/13                      
*PHASE DMTESTIA                                                                 
*INCLUDE DMDMGR     <== CTBUFF RECORD TYPE TABLE                                
*INCLUDE DMDTFSN    <== COPY OF TEST SYSTEM VERSION                             
*INCLUDE DMSYSFIL                                                               
*INCLUDE DMDADDS                                                                
*INCLUDE DMISDDST   <== VERSION WITH CPU TIMES                                  
*INCLUDE DMIS20                                                                 
*INCLUDE DMEOFCHK                                                               
*INCLUDE DMDDNAME                                                               
*INCLUDE DMDYNDD                                                                
*INCLUDE DMDALINK                                                               
*INCLUDE DMDAPTRS                                                               
*INCLUDE DMLOCKER                                                               
*INCLUDE DMPRTQ                                                                 
*INCLUDE DMPRTQO                                                                
*INCLUDE DMPQGCI                                                                
*INCLUDE DMRCVR                                                                 
*INCLUDE DMRCVUSS                                                               
*INCLUDE DMWRKF                                                                 
*INCLUDE DMWRKR                                                                 
*INCLUDE DMWRKZ                                                                 
*INCLUDE DMACCEMU                                                               
*INCLUDE DMDABUFF                                                               
*INCLUDE DMDANDX                                                                
*INCLUDE DMENQDEQ                                                               
*INCLUDE DMENQCTL                                                               
*INCLUDE ARREDIT                                                                
*INCLUDE CARDS                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DDTRACE                                                                
*INCLUDE DYNALLOC                                                               
*INCLUDE GETRET                                                                 
*INCLUDE GETGIN                                                                 
*INCLUDE LOCKSPC                                                                
*INCLUDE LOCKUP                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE LOADER                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE RECOVR                                                                 
*INCLUDE WKGET                                                                  
         TITLE 'DMTESTI - CPU USED BY ISDDS'                                    
         PRINT NOGEN                                                            
DMTEST   CSECT                                                                  
         ENTRY AMSOON                                                           
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         NBASE 0,DMTEST,RA,WORK=A(WORK)                                         
                                                                                
CARD     GOTO1 =V(CARDS),PLIST,C,=C'RE00'                                       
         CLC   C(2),=C'/*'                                                      
         BE    INIT                                                             
         MVC   P(80),C                                                          
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
*                                                                               
CARD1    CLC   C(7),=C'DSPACE='                                                 
         BNE   CARD1X                                                           
         MVC   DSPACE,C+7                                                       
         CLC   C+9(4),=C'TABS'                                                  
         BNE   CARD                                                             
         MVI   TABS,C'Y'                                                        
         B     CARD                                                             
CARD1X   EQU   *                                                                
*                                                                               
CARD2    CLC   C(6),=C'WRITE='                                                  
         BNE   CARD2X                                                           
         MVC   WRITE,C+6                                                        
         B     CARD                                                             
CARD2X   EQU   *                                                                
*                                                                               
CARD3    CLC   C(4),=C'SYS='                                                    
         BNE   CARD3X                                                           
         GOTO1 =V(DMDDNAME),DMCB,(X'24',=C'DDNAME'),C,0                         
         CLI   8(R1),0                                                          
         BNE   CARDE                                                            
         L     RF,8(R1)            RF=A(FILE/SYSTEM INFO LIST)                  
         USING DDNADATA,RF                                                      
*                                                                               
         CLI   C+4,C'C'            CONTROL SYSTEM                               
         BNE   *+12                                                             
         LA    RE,CONSYS                                                        
         B     CARD3W                                                           
*                                                                               
         B     CARDE               SYSTEM IS NOT SUPPORTED                      
*                                                                               
CARD3W   MVC   0(5,RE),C+4         SET SE NAME                                  
         MVC   5(1,RE),DDNASENO    SET SE NUMBER                                
         B     CARD                                                             
CARD3X   EQU   *                                                                
         DROP  RF                                                               
*                                                                               
CARD4    CLC   C(7),=C'XBUFFS='    SWITCH ON/OFF XBUFFS                         
         BNE   CARD4X                                                           
         MVC   XBUFFS,C+7                                                       
         LT    RE,=V(ISCPTCNT)                                                  
         BZ    CARD4X                                                           
         AHI   RE,-1                                                            
         MVC   0(1,RE),C+7                                                      
         B     CARD                                                             
CARD4X   EQU   *                                                                
*                                                                               
CARD5    CLC   C(6),=C'PRINT='     SWITCH ON/OFF PRINT DETIAL                   
         BNE   CARD5X                                                           
         MVC   PRNT,C+6                                                         
         B     CARD                                                             
CARD5X   EQU   *                                                                
*                                                                               
CARD6    CLC   C(5),=C'LOOP='      LOOP=NNNNN COUNTER                           
         BNE   CARD6X                                                           
         MVC   DUB(5),=C'00000'                                                 
         MVZ   DUB(5),C+5                                                       
         CLC   DUB(5),=C'00000'                                                 
         BNE   CARDE                                                            
         PACK  DUB,C+5(5)                                                       
         CVB   R0,DUB                                                           
         ST    R0,LOOP                                                          
         B     CARD                                                             
CARD6X   EQU   *                                                                
*                                                                               
CARD7    B     CARDE               INVALID CARD                                 
*                                                                               
CARDE    MVC   P(40),=CL40'INVALID PARAMETER CARD'                              
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVI   ERROR,X'08'                                                      
         B     EXIT                                                             
                                                                                
INIT     L     RE,=A(SSB)          SET SSB VALUES                               
         USING SSOOFF,RE                                                        
         MVC   SSODSPAC,DSPACE                                                  
INIT1    CLI   TABS,C'Y'                                                        
         BNE   INIT2                                                            
         GOTO1 =V(LOCKSPC),DUB,X'20008001',0                                    
         L     RE,=A(SSB)                                                       
INIT2    CLI   WRITE,C'Y'                                                       
         BNE   START                                                            
         MVI   SSOSTAT2,SSOSGALO   GLOBAL ALLOCATE                              
         OI    SSOSTAT2,SSOSROLC   OFFLINE COPIES WANTED                        
         OI    SSOSTAT2,SSOSLOCK   READ-FOR-UPDATE LOCKING                      
*NOP*    OI    SSOFLAG1,SSOFRCVR   FULL RECOVERY REQUIRED                       
         DROP  RE                                                               
                                                                                
START    XC    DMCB(24),DMCB                                                    
         GOTO1 VDATAMGR,DMCB,(X'80',DMREAD),=C'SYSFLES'                         
         MVC   WRK(4),DMCB+12                                                   
         MVC   P,SPACES                                                         
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P(40),=CL40'DMREAD/SYSFLES WITH P1=80'                           
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
                                                                                
***********************************************************************         
* CONTROL SYSTEM OPEN/READ/WRITE/CLOSE                                *         
***********************************************************************         
CON      CLI   CONSYS,C' '         TEST IF CON WANTED                           
         BE    CEXIT                                                            
         MVC   DIRNAM,=CL8'CTFILE'                                              
         L     RE,=A(UTL)          CLEAR UTL+4                                  
         MVI   4(RE),0                                                          
         LLC   R0,CONSE            R0=CON SENUM                                 
         XC    DMCB(24),DMCB                                                    
         GOTO1 VDATAMGR,DMCB,DMREAD,=C'SYSFLES',(R0)                            
         MVC   WRK(4),DMCB+12                                                   
         MVC   P,SPACES                                                         
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P(40),=CL40'DMREAD/SYSFLES PASSING SYS SENUM'                    
         MVC   P+23(3),=C'CON'                                                  
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
*                                                                               
         L     RE,=A(UTL)          SET CON SENUM IN UTL                         
         STC   R0,4(RE)                                                         
         XC    DMCB(24),DMCB                                                    
         GOTO1 VDATAMGR,DMCB,DMREAD,=C'SYSFLES'                                 
         MVC   WRK(4),DMCB+12                                                   
         MVC   P(40),=CL40'DMREAD/SYSFLES WITH SYS SE SET'                      
         MVC   P+20(3),=C'CON'                                                  
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
*                                                                               
COPEN    XC    DMCB(24),DMCB       SET FILE OPEN LIST READONLY/UPDATIVE         
         LA    R2,CONLST                                                        
         MVI   00(R2),C' '                                                      
         MVI   08(R2),C' '                                                      
         MVI   16(R2),C' '                                                      
         MVI   24(R2),C' '                                                      
         CLI   WRITE,C'N'                                                       
         BNH   COPEN1                                                           
         MVI   00(R2),C'U'                                                      
         MVI   08(R2),C'U'                                                      
         MVI   16(R2),C'U'                                                      
         MVI   24(R2),C'U'                                                      
*                                                                               
COPEN1   GOTO1 VDATAMGR,DMCB,DMOPEN,=C'CON',(R2),REC                            
         MVC   P(40),=CL40'SYS    OPENED'                                       
         MVC   P(5),CONSYS                                                      
         CLI   WRITE,C'N'                                                       
         BNH   *+10                                                             
         MVC   P+14(6),=C'UPDATE'                                               
         BAS   RE,ISC                                                           
         MVC   P+40(L'ISCDATA),ISCDATA                                          
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         LT    RE,=V(ISCPTCNT)                                                  
         BZ    *+10                                                             
         XC    0(16,RE),0(RE)      CLEAR ISDDS COUNTERS                         
*                                                                               
COPEN2   XC    KEY,KEY             READ FIRST RECORD OF GENDIR                  
         XR    R0,R0                                                            
         GOTO1 VDATAMGR,DMCB,((R0),DMRDHI),=C'GENDIR',KEY,KEY                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P(40),=CL40'GENDIR FIRST RECORD READ'                            
         BAS   RE,ISC                                                           
         MVC   P+40(L'ISCDATA),ISCDATA                                          
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         LT    RE,=V(ISCPTCNT)                                                  
         BZ    *+10                                                             
         XC    0(16,RE),0(RE)      CLEAR ISDDS COUNTERS                         
*                                                                               
COPEN3   XC    KEY,KEY             READ FIRST RECORD OF CTFILE                  
         XR    R0,R0                                                            
         GOTO1 VDATAMGR,DMCB,((R0),DMRDHI),=C'CTFILE',KEY,REC                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P(40),=CL40'CTFILE FIRST RECORD READ'                            
         BAS   RE,ISC                                                           
         MVC   P+40(L'ISCDATA),ISCDATA                                          
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         LT    RE,=V(ISCPTCNT)                                                  
         BZ    *+10                                                             
         XC    0(16,RE),0(RE)      CLEAR ISDDS COUNTERS                         
*                                                                               
COPEN5   MVC   P,SPACES                                                         
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
*                                                                               
CREAD1   XC    KEY,KEY             SET KEY TO READ                              
         MVC   KEY(3),=X'0000D4'                                                
         XR    R0,R0               TEST/SET READ FOR UPDATE                     
         CLI   WRITE,C'Y'                                                       
         BNE   *+8                                                              
         LA    R0,X'80'                                                         
         GOTO1 VDATAMGR,DMCB,((R0),DMRDHI),=C'GENDIR',KEY,KEY                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DSKADR,KEY+36                                                    
         CLI   WRITE,C'Y'                                                       
         BNE   CREAD2                                                           
         GOTO1 VDATAMGR,DMCB,DMWRT,=C'GENDIR',KEY,KEY                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P(40),=CL40'SYSDIR RECORD UPDATED'                               
         MVC   P(3),=C'GEN'                                                     
         BAS   RE,ISC              GET ISDDS COUNTERS                           
         MVC   P+40(L'ISCDATA),ISCDATA                                          
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
*                                                                               
CREAD2   XR    R0,R0               TEST/SET READ FOR UPDATE                     
         CLI   WRITE,C'Y'                                                       
         BNE   *+8                                                              
         LA    R0,X'80'                                                         
         GOTO1 VDATAMGR,DMCB,((R0),GETREC),=C'GENFIL',DSKADR,REC,WRK            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   WRITE,C'Y'                                                       
         BNE   CREAD3                                                           
         GOTO1 VDATAMGR,DMCB,PUTREC,=C'GENFIL',DSKADR,REC,WRK                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P(40),=CL40'SYSFIL RECORD UPDATED'                               
         MVC   P(3),=C'GEN'                                                     
         MVC   P+40(L'ISCDATA),SPACES                                           
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
*                                                                               
CREAD3   XC    KEY,KEY             SET KEY TO READ CTFILE                       
         MVI   KEY,C'I'                                                         
         MVC   KEY+15(10),=CL10'BOBBY'                                          
         XR    R0,R0               TEST/SET READ FOR UPDATE                     
         CLI   WRITE,C'Y'                                                       
         BNE   *+8                                                              
         LA    R0,X'80'                                                         
         GOTO1 VDATAMGR,DMCB,((R0),DMRDHI),=C'CTFILE',KEY,REC                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   WRITE,C'Y'                                                       
         BNE   CREAD4                                                           
         MVI   REC+27,X'01'        RESET LAST USED MONTH IN STATUS BYTE         
         GOTO1 VDATAMGR,DMCB,DMWRT,=C'CTFILE',KEY,REC                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P(40),=CL40'CTFILE RECORD UPDATED'                               
         BAS   RE,ISC              GET ISDDS COUNTERS                           
         MVC   P+40(L'ISCDATA),ISCDATA                                          
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
*                                                                               
CREAD4   B     CREAD5              NOP THIS LOOP                                
         XC    KEY,KEY             SET KEY TO READ CTFILE J RECORDS             
         MVI   KEY,C'J'                                                         
         MVI   KEY+14,C'A'                                                      
         MVC   CLOOP,LOOP          SET LOOP COUNTER                             
         LT    RE,=V(ISCPTCNT)                                                  
         BZ    *+10                                                             
         XC    0(16,RE),0(RE)      CLEAR ISDDS COUNTERS                         
         XC    PREV,PREV                                                        
         XC    NOIO,NOIO                                                        
         XC    EXIO,EXIO                                                        
         MVC   P,SPACES                                                         
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
*                                                                               
CREAD4A  GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),=C'CTFILE',KEY,REC                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   CLOOP,=F'1'         ALWAYS PRINT LAST RECORD                     
         BE    *+12                                                             
         CLI   PRNT,C'N'                                                        
         BE    CREAD4B                                                          
         MVC   P(40),SPACES                                                     
         MVC   P(6),=C'CTFILE'                                                  
         GOTO1 =V(HEXOUT),PLIST,KEY,P+07,01,(24,00)                             
         GOTO1 =V(HEXOUT),PLIST,REC,P+10,01,(24,00)                             
         MVC   P+13(25),REC                                                     
         MVI   ISCWHY,0                                                         
         BAS   RE,ISC              GET ISDDS COUNTERS                           
         MVC   P+40(L'ISCDATA),ISCDATA                                          
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
*                                                                               
CREAD4B  LT    RE,=V(ISCPTCNT)     RE=A(ISDDS DATA)                             
         BZ    CREAD4N                                                          
         MVC   THIS,0(RE)                                                       
         L     R0,THIS+4           R0=EXCPS THIS CALL                           
         S     R0,PREV+4                                                        
         L     R1,THIS+12          R1=CPU USED THIS CALL                        
         S     R1,PREV+12                                                       
         LTR   R0,R0                                                            
         BNZ   CREAD4D                                                          
*                                                                               
CREAD4C  L     RF,NOIO             BUMP NOIO COUNT AND CPU USED                 
         AHI   RF,1                                                             
         ST    RF,NOIO                                                          
         L     RF,NOIO+12                                                       
         AR    RF,R1                                                            
         ST    RF,NOIO+12                                                       
         B     CREAD4E                                                          
*                                                                               
CREAD4D  L     RF,EXIO             BUMP EXCP COUNT AND CPU USED                 
         AHI   RF,1                                                             
         ST    RF,EXIO                                                          
         L     RF,EXIO+4                                                        
         AR    RF,R0                                                            
         ST    RF,EXIO+4                                                        
         L     RF,EXIO+12                                                       
         AR    RF,R1                                                            
         ST    RF,EXIO+12                                                       
*                                                                               
CREAD4E  MVC   PREV,THIS                                                        
*                                                                               
CREAD4N  SR    R1,R1               SET KEY FOR NEXT READ HI                     
         IC    R1,KEY+14                                                        
         AHI   R1,1                                                             
         XC    KEY,KEY                                                          
         MVI   KEY,C'J'                                                         
         STC   R1,KEY+14                                                        
         CLI   KEY+14,C'9'                                                      
         BNH   *+8                                                              
         MVI   KEY+14,C'A'                                                      
         L     R0,CLOOP            DECR LOOP COUNTER                            
         AHI   R0,-1                                                            
         ST    R0,CLOOP                                                         
         LTR   R0,R0                                                            
         BP    CREAD4A                                                          
*                                                                               
CREAD4X  MVC   P,SPACES                                                         
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P(40),=CL40'CTFILE READS WITH NO I/OS'                           
         MVC   P+7(5),=C'J REC'                                                 
         CLI   XBUFFS,C'N'                                                      
         BNE   *+10                                                             
         MVC   P+26(8),=C'XBUFFS=N'                                             
         MVI   ISCWHY,1                                                         
         BAS   RE,ISC                                                           
         MVC   P+40(L'ISCDATA),ISCDATA                                          
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P(40),=CL40'CTFILE READS WITH EXCPS'                             
         MVC   P+7(5),=C'J REC'                                                 
         MVI   ISCWHY,2                                                         
         BAS   RE,ISC                                                           
         MVC   P+40(L'ISCDATA),ISCDATA                                          
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
*                                                                               
CREAD5   XC    KEY,KEY             SET KEY TO READ CTFILE LIST                  
         LA    RE,CLIST                                                         
         ST    RE,ACLIST                                                        
         MVC   KEY(1),0(RE)                                                     
         MVC   CLOOP,LOOP          SET LOOP COUNTER                             
         LT    RE,=V(ISCPTCNT)                                                  
         BZ    *+10                                                             
         XC    0(16,RE),0(RE)      CLEAR ISDDS COUNTERS                         
         XC    PREV,PREV                                                        
         XC    NOIO,NOIO                                                        
         XC    EXIO,EXIO                                                        
         MVC   P,SPACES                                                         
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
*                                                                               
CREAD5A  GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),=C'CTFILE',KEY,REC                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   CLOOP,=F'1'         ALWAYS PRINT LAST RECORD                     
         BE    *+12                                                             
         CLI   PRNT,C'N'                                                        
         BE    CREAD5B                                                          
         MVC   P(40),SPACES                                                     
         MVC   P(6),=C'CTFILE'                                                  
         GOTO1 =V(HEXOUT),PLIST,KEY,P+07,01,(24,00)                             
         GOTO1 =V(HEXOUT),PLIST,REC,P+10,01,(24,00)                             
         MVC   P+13(25),REC                                                     
         MVI   ISCWHY,0                                                         
         BAS   RE,ISC              GET ISDDS COUNTERS                           
         MVC   P+40(L'ISCDATA),ISCDATA                                          
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
*                                                                               
CREAD5B  LT    RE,=V(ISCPTCNT)     RE=A(ISDDS DATA)                             
         BZ    CREAD5N                                                          
         MVC   THIS,0(RE)                                                       
         L     R0,THIS+4           R0=EXCPS THIS CALL                           
         S     R0,PREV+4                                                        
         L     R1,THIS+12          R1=CPU USED THIS CALL                        
         S     R1,PREV+12                                                       
         LTR   R0,R0                                                            
         BNZ   CREAD5D                                                          
*                                                                               
CREAD5C  L     RF,NOIO             BUMP NOIO COUNT AND CPU USED                 
         AHI   RF,1                                                             
         ST    RF,NOIO                                                          
         L     RF,NOIO+12                                                       
         AR    RF,R1                                                            
         ST    RF,NOIO+12                                                       
         B     CREAD5E                                                          
*                                                                               
CREAD5D  L     RF,EXIO             BUMP EXCP COUNT AND CPU USED                 
         AHI   RF,1                                                             
         ST    RF,EXIO                                                          
         L     RF,EXIO+4                                                        
         AR    RF,R0                                                            
         ST    RF,EXIO+4                                                        
         L     RF,EXIO+12                                                       
         AR    RF,R1                                                            
         ST    RF,EXIO+12                                                       
*                                                                               
CREAD5E  MVC   PREV,THIS                                                        
*                                                                               
CREAD5N  L     RE,ACLIST           SET KEY FOR NEXT READ HI                     
         LA    RE,1(RE)                                                         
         ST    RE,ACLIST                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(1),0(RE)                                                     
         CLI   KEY,X'FF'           TEST END OF LIST                             
         BNE   CREAD5P                                                          
         LA    RE,CLIST                                                         
         ST    RE,ACLIST                                                        
         MVC   KEY(1),0(RE)                                                     
CREAD5P  L     R0,CLOOP            DECR LOOP COUNTER                            
         AHI   R0,-1                                                            
         ST    R0,CLOOP                                                         
         LTR   R0,R0                                                            
         BP    CREAD5A                                                          
*                                                                               
CREAD5X  MVC   P,SPACES                                                         
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P(40),=CL40'CTFILE READS WITH NO I/OS'                           
         CLI   XBUFFS,C'N'                                                      
         BNE   *+10                                                             
         MVC   P+26(8),=C'XBUFFS=N'                                             
         MVI   ISCWHY,1                                                         
         BAS   RE,ISC                                                           
         MVC   P+40(L'ISCDATA),ISCDATA                                          
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P(40),=CL40'CTFILE READS WITH EXCPS'                             
         MVI   ISCWHY,2                                                         
         BAS   RE,ISC                                                           
         MVC   P+40(L'ISCDATA),ISCDATA                                          
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
*                                                                               
CCLOSE   BAS   RE,ISX                                                           
         GOTO1 VDATAMGR,DMCB,DMCLSE,=C'CON'                                     
         MVC   P(40),=CL40'SYS    CLOSED'                                       
         MVC   P(5),CONSYS                                                      
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         B     CEXIT                                                            
*                                                                               
CONSYS   DC    CL5' '                                                           
CONSE    DC    X'0A'                                                            
CONLST   DC    C' GENDIR  GENFIL  CTFILE  CTRCVR X'                             
ACLIST   DC    A(CLIST)                                                         
**IST    DC    C'FIPTUW05',X'FF'                                                
CLIST    DS    0X                                                               
**       DC    X'0102'                                                          
         DC    X'040506'                                                        
**       DC    X'070809'                                                        
**       DC    X'14'                                                            
**       DC    X'9799'                                                          
         DC    X'9A9B'                                                          
         DC    X'C1C3C4C5C7C8'                                                  
         DC    X'D1D3D4D5D6D8D9'                                                
         DC    X'E2E7E8E9'                                                      
         DC    X'F3F4F6F7F9'                                                    
         DC    X'FF'                                                            
*                                                                               
CEXIT    DS    0H                                                               
                                                                                
EXIT     XBASE RC=ERROR,RL=1       EXIT WITH CC SET IF ERROR                    
                                                                                
***********************************************************************         
* SUBROUTINE TO FORMAT ISFILE CALLS/EXCPS/CPUTIME                     *         
***********************************************************************         
ISC      NTR1                                                                   
         LT    R5,=V(ISCPTCNT)                                                  
         BZ    ISCX                                                             
         CLI   ISCWHY,1                                                         
         BNE   *+8                                                              
         LA    R5,NOIO                                                          
         CLI   ISCWHY,2                                                         
         BNE   *+8                                                              
         LA    R5,EXIO                                                          
*                                                                               
ISC1     L     R0,0(R5)            NUMBER OF CALLS                              
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ISCCALLS,DUB                                                     
         B     ISC1B                                                            
         LA    R1,ISCCALLS                                                      
         LA    R0,L'ISCCALLS-1                                                  
ISC1A    CLI   0(R1),C'0'                                                       
         BNE   ISC1B                                                            
         MVI   0(R1),C' '                                                       
         LA    R1,1(R1)                                                         
         BCT   R0,ISC1A                                                         
ISC1B    EQU   *                                                                
*                                                                               
ISC2     L     R0,4(R5)            NUMBER OF EXCPS                              
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ISCEXCPS,DUB                                                     
         B     ISC2B                                                            
         LA    R1,ISCEXCPS                                                      
         LA    R0,L'ISCEXCPS-1                                                  
ISC2A    CLI   0(R1),C'0'                                                       
         BNE   ISC2B                                                            
         MVI   0(R1),C' '                                                       
         LA    R1,1(R1)                                                         
         BCT   R0,ISC2A                                                         
ISC2B    EQU   *                                                                
*                                                                               
ISC3     L     R0,12(R5)           CPU TIME IN MICRO SECONDS                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ISCMICRO,DUB                                                     
         B     ISC3B                                                            
         LA    R1,ISCMICRO                                                      
         LA    R0,L'ISCMICRO-1                                                  
ISC3A    CLI   0(R1),C'0'                                                       
         BNE   ISC3B                                                            
         MVI   0(R1),C' '                                                       
         LA    R1,1(R1)                                                         
         BCT   R0,ISC3A                                                         
ISC3B    EQU   *                                                                
*                                                                               
ISC4     MVC   ISCAVG,SPACES       AVERAGE CPU TIME PER CALL                    
         OC    0(4,R5),0(R5)                                                    
         BZ    ISC4B                                                            
         SR    R0,R0               AVERAGE CPU TIME PER CALL                    
         L     R1,12(R5)                                                        
         M     R0,=F'10'           MICROSECS TO 1 DECIMAL PLACE                 
         D     R0,0(R5)                                                         
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  FULL,DUB                                                         
         MVC   ISCAVG(2),FULL+1                                                 
         MVI   ISCAVG+2,C'.'                                                    
         MVC   ISCAVG+3(1),FULL+3                                               
         CLI   ISCAVG,C'0'                                                      
         BNE   *+8                                                              
         MVI   ISCAVG,C' '                                                      
ISC4B    EQU   *                                                                
*                                                                               
ISCX     XIT1                                                                   
*                                                                               
         LG    GR0,8(R5)           CPU TIME IN MICRO SECONDS                    
         CVDG  GR0,BIG                                                          
         OI    BIG+15,X'0F'                                                     
         UNPK  ISCMICRO,BIG                                                     
*                                                                               
ISCDATA  DS    0CL25                                                            
ISCCALLS DC    CL5' '                                                           
         DC    CL1' '                                                           
ISCEXCPS DS    CL5' '                                                           
         DC    CL1' '                                                           
ISCMICRO DC    CL8' '                                                           
         DC    CL1' '                                                           
ISCAVG   DC    CL4' '                                                           
*                                                                               
ISCWHY   DC    X'00'                                                            
         DC    XL3'00'                                                          
***********************************************************************         
* SUBROUTINE TO PRINT ISXTAB FOR ISFILE IN DIRNAM                     *         
***********************************************************************         
ISX      NTR1                                                                   
         GOTO1 VDATAMGR,PLIST,=C'DTFAD',DIRNAM                                  
         SR    R2,R2                                                            
         ICM   R2,7,13(R1)         R2=A(DTF)                                    
         LT    R5,32(R2)           R5=A(ISXTAB)                                 
         BZ    ISXX                                                             
         SR    R6,R6                                                            
         ICM   R6,1,4(R2)          R6=NUM OF EXTRA BUFFS                        
         BZ    ISXX                                                             
         AHI   R6,1                                                             
         MVC   P,SPACES                                                         
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         SAM31                                                                  
ISX2     LA    R7,00(R5)                                                        
         LA    R8,P+00                                                          
         GOTO1 =V(HEXOUT),PLIST,(R7),(R8),20,(31,00)                            
         LA    R7,20(R5)                                                        
         LA    R8,P+41                                                          
         GOTO1 =V(HEXOUT),PLIST,(R7),(R8),04,(31,00)                            
         LA    R7,24(R5)                                                        
         LA    R8,P+50                                                          
         GOTO1 =V(HEXOUT),PLIST,(R7),(R8),08,(31,00)                            
         LA    R7,74(R5)                                                        
         LA    R8,P+67                                                          
         GOTO1 =V(HEXOUT),PLIST,(R7),(R8),02,(31,00)                            
         LA    R7,78(R5)                                                        
         LA    R8,P+72                                                          
         GOTO1 =V(HEXOUT),PLIST,(R7),(R8),02,(31,00)                            
         SAM24                                                                  
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         SAM31                                                                  
         LA    R5,80(R5)                                                        
         BCT   R6,ISX2                                                          
         SAM24                                                                  
         MVC   P,SPACES                                                         
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
ISXX     XIT1                                                                   
                                                                                
***********************************************************************         
* SUBROUTINE TO GET DSN FOR ISFILE IN DIRNAM. SET A(DSN) IN ADSN.     *         
***********************************************************************         
GETDSN   NTR1                                                                   
         GOTO1 VDATAMGR,PLIST,=C'DTFAD',DIRNAM                                  
         SR    R2,R2                                                            
         ICM   R2,7,13(R1)         R2=A(DTF)                                    
         LA    R1,DYNBLK2          SET UP DYNALLOC PARMS                        
         ST    R1,DYNBLK1                                                       
         OI    DYNBLK1,X'80'                                                    
         LA    R1,DYNBLK4                                                       
         ST    R1,DYNBLK3                                                       
         LA    R1,DYNBLK5                                                       
         ST    R1,DYNBLK3+4                                                     
         OI    DYNBLK3+4,X'80'                                                  
         MVC   DYNBLK2,=X'1407000000000000000000000000000018000000'             
         LA    R1,DYNBLK3                                                       
         ST    R1,DYNBLK2+8                                                     
         MVC   DYNBLK4(6),=X'000100010008'                                      
         MVC   DYNBLK4+6(8),22(R2)                                              
         MVC   DYNBLK5,SPACES                                                   
         MVC   DYNBLK5(6),=X'000500010020'                                      
         LA    R1,DYNBLK1                                                       
         LAM   AR0,ARF,ZEROS                                                    
         DYNALLOC                                                               
         STAM  AR0,ARF,ARS                                                      
         LTR   RF,RF                                                            
         BZ    GETDSNX                                                          
         XC    DYNBLK5,DYNBLK5                                                  
GETDSNX  LA    RF,DYNBLK5+6                                                     
         ST    RF,ADSN                                                          
         XIT1                                                                   
                                                                                
         LTORG                                                                  
                                                                                
VDATAMGR DC    V(DATAMGR)                                                       
DMOPEN   DC    CL8'DMOPEN'                                                      
DMCLSE   DC    CL8'DMCLSE'                                                      
DMREAD   DC    CL8'DMREAD'                                                      
DMRDHI   DC    CL8'DMRDHI'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
GETREC   DC    CL8'GETREC'                                                      
PUTREC   DC    CL8'PUTREC'                                                      
COMMIT   DC    CL8'COMMIT'                                                      
DIRNAM   DC    CL8' '                                                           
                                                                                
LOOP     DC    F'1'                                                             
WRITE    DC    C'N'                                                             
DSPACE   DC    C'T'                                                             
TABS     DC    C'N'                                                             
XBUFFS   DC    C'*'                                                             
PRNT     DC    C'Y'                                                             
ERROR    DC    X'00'                                                            
C        DC    CL80' '                                                          
PCC      DC    X'00'                                                            
P        DC    CL132' '                                                         
SPACES   DC    CL132' '                                                         
                                                                                
DUB      DC    D'0'                                                             
FULL     DC    F'0'                                                             
HALF     DC    H'0'                                                             
BYTE     DC    X'00'                                                            
BYTE1    DC    X'00'                                                            
BIG      DC    XL16'00'                                                         
PREV     DC    XL16'00'                                                         
THIS     DC    XL16'00'                                                         
NOIO     DC    XL16'00'                                                         
EXIO     DC    XL16'00'                                                         
CLOOP    DC    F'0'                                                             
PLIST    DC    6F'0'                                                            
WRK      DC    20F'0'                                                           
ZEROS    DC    16F'0'                                                           
ARS      DC    16F'0'                                                           
                                                                                
ADSN     DS    F                                                                
DYNBLK1  DS    F                                                                
DYNBLK2  DS    XL20                                                             
DYNBLK3  DS    XL8                                                              
DYNBLK4  DS    XL14                                                             
DYNBLK5  DS    XL38                                                             
                                                                                
         DS    0D                                                               
         DC    CL8'**DMCB**'                                                    
DMCB     DC    6F'0'                                                            
         DS    0D                                                               
         DC    CL8'*DSKADR*'                                                    
DSKADR   DC    F'0'                                                             
         DS    0D                                                               
         DC    CL8'*KEYKEY*'                                                    
KEY      DC    XL64'00'                                                         
         DS    0D                                                               
         DC    CL8'*IOAIOA*'                                                    
IOA      DC    8000X'00'                                                        
REC      EQU   IOA                                                              
         DS    0D                                                               
AMSOON   DC    C'N',XL7'00'                                                     
         DS    0D                                                               
         DC    CL8'*SSBSSB*'                                                    
SSB      DC    X'0000',X'FF',X'00'                                              
         DC    X'00000000'                                                      
         DC    CL8' '                                                           
         DC    A(0),A(0)                                                        
         DC    A(0),A(0)                                                        
         DC    XL32'00'                                                         
         DC    XL256'00'                                                        
                                                                                
         DS    0D                                                               
         DC    CL8'*UTLUTL*'                                                    
UTL      DC    64F'0'                                                           
                                                                                
         DC    CL8'**WORK**'                                                    
WORK     DC    20000D'0'                                                        
                                                                                
       ++INCLUDE DMGREQUS                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDDNAMED                                                      
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003DMTESTI   08/14/13'                                      
         END                                                                    
