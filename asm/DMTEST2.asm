*          DATA SET DMTEST2    AT LEVEL 003 AS OF 08/14/13                      
*PHASE DMTEST2A                                                                 
*INCLUDE DMDMGR                                                                 
*INCLUDE DMDTFSN    <== COPY OF TEST SYSTEM VERSION                             
*INCLUDE DMSYSFIL                                                               
*INCLUDE DMEOFCHK   <== NEW ROUTINE                                             
*INCLUDE DMDADDS                                                                
*INCLUDE DMISDDS                                                                
*INCLUDE DMIS20                                                                 
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
         TITLE 'DMTEST - TEST NEW DMCHKEOF ROUTINE'                             
         PRINT NOGEN                                                            
DMTEST   CSECT                                                                  
         ENTRY AMSOON                                                           
         ENTRY SSB                                                              
         ENTRY UTL                                                              
         NBASE 0,DMTEST,RA,WORK=A(WORK)                                         
                                                                                
LOOP     GOTO1 =V(CARDS),PLIST,C,=C'RE00'                                       
         CLC   C(2),=C'/*'                                                      
         BE    INIT                                                             
         MVC   P(80),C                                                          
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
*                                                                               
LOOP1    CLC   C(7),=C'DSPACE='                                                 
         BNE   LOOP1X                                                           
         MVC   DSPACE,C+7                                                       
         CLC   C+9(4),=C'TABS'                                                  
         BNE   LOOP                                                             
         MVI   TABS,C'Y'                                                        
         B     LOOP                                                             
LOOP1X   EQU   *                                                                
*                                                                               
LOOP2    CLC   C(6),=C'WRITE='                                                  
         BNE   LOOP2X                                                           
         MVC   WRITE,C+6                                                        
         B     LOOP                                                             
LOOP2X   EQU   *                                                                
*                                                                               
LOOP3    CLC   C(4),=C'SYS='                                                    
         BNE   LOOP3X                                                           
         GOTO1 =V(DMDDNAME),DMCB,(X'24',=C'DDNAME'),C,0                         
         CLI   8(R1),0                                                          
         BNE   LOOPE                                                            
         L     RF,8(R1)            RF=A(FILE/SYSTEM INFO LIST)                  
         USING DDNADATA,RF                                                      
*                                                                               
         CLI   C+4,C'A'            ACC SYSTEM                                   
         BNE   *+12                                                             
         LA    RE,ACCSYS                                                        
         B     LOOP3W                                                           
*                                                                               
         CLI   C+4,C'C'            CONTROL SYSTEM                               
         BNE   *+12                                                             
         LA    RE,CONSYS                                                        
         B     LOOP3W                                                           
*&&UK                                                                           
         CLI   C+4,C'M'            MEDIA SYSTEM                                 
         BNE   *+12                                                             
         LA    RE,MEDSYS                                                        
         B     LOOP3W                                                           
*&&                                                                             
*&&US                                                                           
         CLI   C+4,C'S'            SPOT SYSTEM                                  
         BNE   *+12                                                             
         LA    RE,SPTSYS                                                        
         B     LOOP3W                                                           
         CLI   C+4,C'P'            PRINT SYSTEM                                 
         BNE   *+12                                                             
         LA    RE,PRTSYS                                                        
         B     LOOP3W                                                           
*&&                                                                             
         B     LOOPE               SYSTEM IS NOT SUPPORTED                      
*                                                                               
LOOP3W   MVC   0(5,RE),C+4         SET SE NAME                                  
         MVC   5(1,RE),DDNASENO    SET SE NUMBER                                
         B     LOOP                                                             
LOOP3X   EQU   *                                                                
         DROP  RF                                                               
*                                                                               
LOOP4    B     LOOPE               INVALID CARD                                 
*                                                                               
LOOPE    MVC   P(40),=CL40'INVALID PARAMETER CARD'                              
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
* ACCOUNT SYSTEM OPEN/READ/WRITE/CLOSE                                *         
***********************************************************************         
ACC      CLI   ACCSYS,C' '         TEST IF ACC WANTED                           
         BE    AEXIT                                                            
         MVC   DIRNAM,=CL8'ACCDIR'                                              
         L     RE,=A(UTL)          CLEAR UTL+4                                  
         MVI   4(RE),0                                                          
         LLC   R0,ACCSE            R0=ACC SENUM                                 
         XC    DMCB(24),DMCB                                                    
         GOTO1 VDATAMGR,DMCB,DMREAD,=C'SYSFLES',(R0)                            
         MVC   WRK(4),DMCB+12                                                   
         MVC   P,SPACES                                                         
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P(40),=CL40'DMREAD/SYSFLES PASSING SYS SENUM'                    
         MVC   P+23(3),=C'ACC'                                                  
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
*                                                                               
         L     RE,=A(UTL)          SET ACC SENUM IN UTL                         
         STC   R0,4(RE)                                                         
         XC    DMCB(24),DMCB                                                    
         GOTO1 VDATAMGR,DMCB,DMREAD,=C'SYSFLES'                                 
         MVC   WRK(4),DMCB+12                                                   
         MVC   P(40),=CL40'DMREAD/SYSFLES WITH SYS SE SET'                      
         MVC   P+20(3),=C'ACC'                                                  
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
*                                                                               
AOPEN    XC    DMCB(24),DMCB       SET FILE OPEN LIST READONLY/UPDATIVE         
         LA    R2,ACCLST                                                        
         MVI   00(R2),C' '                                                      
         MVI   08(R2),C' '                                                      
         MVI   16(R2),C' '                                                      
         CLI   WRITE,C'N'                                                       
         BNH   AOPEN1                                                           
         MVI   00(R2),C'U'                                                      
         MVI   08(R2),C'U'                                                      
         MVI   16(R2),C'U'                                                      
AOPEN1   GOTO1 VDATAMGR,DMCB,DMOPEN,=C'ACC',(R2),REC                            
         MVC   P(40),=CL40'SYS    OPENED'                                       
         MVC   P(5),ACCSYS                                                      
         CLI   WRITE,C'N'                                                       
         BNH   *+10                                                             
         MVC   P+14(6),=C'UPDATE'                                               
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
*                                                                               
AEOFCHK  GOTO1 =V(DMEOFCHK),DMCB,=C'EOFCHK',=CL8'ACCMSTT'                       
         L     R5,8(R1)                                                         
         MVC   EOFWRK,0(R5)                                                     
         LA    R5,EOFWRK                                                        
         LLC   R0,11(R5)                                                        
         MVC   P,SPACES                                                         
         GOTO1 =V(HEXOUT),PLIST,DMCB+8,P+0,1,(24,00)                            
         GOTO1 =V(HEXOUT),PLIST,11(R5),P+3,1,(24,00)                            
         GOTO1 =V(HEXOUT),PLIST,EOFDDNAD,P+7,28,(24,00)                         
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P,SPACES                                                         
         GOTO1 =V(HEXOUT),PLIST,EOFPLIST,P+7,24,(24,00)                         
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P,SPACES                                                         
         GOTO1 =V(HEXOUT),PLIST,EOFOTHER,P+7,32,(24,00)                         
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P,SPACES                                                         
*                                                                               
ACLOSE   GOTO1 VDATAMGR,DMCB,DMCLSE,=C'ACC'                                     
         MVC   P(40),=CL40'SYS    CLOSED'                                       
         MVC   P(5),ACCSYS                                                      
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         B     AEXIT                                                            
*                                                                               
ACCSYS   DC    CL5' '                                                           
ACCSE    DS    0X                                                               
*&&UK*&& DC    X'66'               ACC0                                         
*&&US*&& DC    X'06'                                                            
ACCIPTR  DC    X'18CC43',10X'00',CL12'ADJA02',CL13'31',X'C140B110'              
ACCLST   DC    C' ACCDIR  ACCMST  ACCRCV X'                                     
AEXIT    DS    0H                                                               
                                                                                
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
COPEN1   GOTO1 VDATAMGR,DMCB,DMOPEN,=C'CON',(R2),REC                            
         MVC   P(40),=CL40'SYS    OPENED'                                       
         MVC   P(5),CONSYS                                                      
         CLI   WRITE,C'N'                                                       
         BNH   *+10                                                             
         MVC   P+14(6),=C'UPDATE'                                               
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
*                                                                               
CEOFCHK  GOTO1 =V(DMEOFCHK),DMCB,=C'EOFCHK',=CL8'GENFIL'                        
         L     R5,8(R1)                                                         
         MVC   EOFWRK,0(R5)                                                     
         LA    R5,EOFWRK                                                        
         LLC   R0,11(R5)                                                        
         MVC   P,SPACES                                                         
         GOTO1 =V(HEXOUT),PLIST,DMCB+8,P+0,1,(24,00)                            
         GOTO1 =V(HEXOUT),PLIST,11(R5),P+3,1,(24,00)                            
         GOTO1 =V(HEXOUT),PLIST,EOFDDNAD,P+7,28,(24,00)                         
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P,SPACES                                                         
         GOTO1 =V(HEXOUT),PLIST,EOFPLIST,P+7,24,(24,00)                         
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P,SPACES                                                         
         GOTO1 =V(HEXOUT),PLIST,EOFOTHER,P+7,32,(24,00)                         
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P,SPACES                                                         
*                                                                               
CCLOSE   GOTO1 VDATAMGR,DMCB,DMCLSE,=C'CON'                                     
         MVC   P(40),=CL40'SYS    CLOSED'                                       
         MVC   P(5),CONSYS                                                      
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         B     CEXIT                                                            
*                                                                               
CONSYS   DC    CL5' '                                                           
CONSE    DC    X'0A'                                                            
CONLST   DC    C' GENDIR  GENFIL  CTFILE  CTRCVR X'                             
CEXIT    DS    0H                                                               
                                                                                
*&&UK                                                                           
***********************************************************************         
* MEDIA SYSTEM OPEN/READ/WRITE/CLOSE                                  *         
***********************************************************************         
MED      CLI   MEDSYS,C' '         TEST IF MED WANTED                           
         BE    MEXIT                                                            
         MVC   DIRNAM,=CL8'MEDDIR'                                              
         L     RE,=A(UTL)          CLEAR UTL+4                                  
         MVI   4(RE),0                                                          
         LLC   R0,MEDSE            R0=MED SENUM                                 
         XC    DMCB(24),DMCB                                                    
         GOTO1 VDATAMGR,DMCB,DMREAD,=C'SYSFLES',(R0)                            
         MVC   WRK(4),DMCB+12                                                   
         MVC   P,SPACES                                                         
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P(40),=CL40'DMREAD/SYSFLES PASSING SYS SENUM'                    
         MVC   P+23(3),=C'MED'                                                  
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
*                                                                               
         L     RE,=A(UTL)          SET MED SENUM IN UTL                         
         STC   R0,4(RE)                                                         
         XC    DMCB(24),DMCB                                                    
         GOTO1 VDATAMGR,DMCB,DMREAD,=C'SYSFLES'                                 
         MVC   WRK(4),DMCB+12                                                   
         MVC   P(40),=CL40'DMREAD/SYSFLES WITH SYS SE SET'                      
         MVC   P+20(3),=C'MED'                                                  
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
*                                                                               
MOPEN    XC    DMCB(24),DMCB       SET FILE OPEN LIST READONLY/UPDATIVE         
         LA    R2,MEDLST                                                        
         MVI   00(R2),C' '                                                      
         MVI   08(R2),C' '                                                      
         MVI   16(R2),C' '                                                      
         CLI   WRITE,C'N'                                                       
         BNH   MOPEN1                                                           
         MVI   00(R2),C'U'                                                      
         MVI   08(R2),C'U'                                                      
         MVI   16(R2),C'U'                                                      
MOPEN1   GOTO1 VDATAMGR,DMCB,DMOPEN,=C'MED',(R2),REC                            
         MVC   P(40),=CL40'SYS    OPENED'                                       
         MVC   P(5),MEDSYS                                                      
         CLI   WRITE,C'N'                                                       
         BNH   *+10                                                             
         MVC   P+14(6),=C'UPDATE'                                               
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
*                                                                               
MEOFCHK  GOTO1 =V(DMEOFCHK),DMCB,=C'EOFCHK',=CL8'MEDFILT1'                      
         L     R5,8(R1)                                                         
         MVC   EOFWRK,0(R5)                                                     
         LA    R5,EOFWRK                                                        
         LLC   R0,11(R5)                                                        
         MVC   P,SPACES                                                         
         GOTO1 =V(HEXOUT),PLIST,DMCB+8,P+0,1,(24,00)                            
         GOTO1 =V(HEXOUT),PLIST,11(R5),P+3,1,(24,00)                            
         GOTO1 =V(HEXOUT),PLIST,EOFDDNAD,P+7,28,(24,00)                         
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P,SPACES                                                         
         GOTO1 =V(HEXOUT),PLIST,EOFPLIST,P+7,24,(24,00)                         
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P,SPACES                                                         
         GOTO1 =V(HEXOUT),PLIST,EOFOTHER,P+7,32,(24,00)                         
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P,SPACES                                                         
*                                                                               
MCLOSE   GOTO1 VDATAMGR,DMCB,DMCLSE,=C'MED'                                     
         MVC   P(40),=CL40'SYS    CLOSED'                                       
         MVC   P(5),MEDSYS                                                      
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         B     MEXIT                                                            
*                                                                               
MEDSYS   DC    CL5' '                                                           
MEDSE    DC    X'74'               MEDT1                                        
MEDLST   DC    C' MEDDIR  MEDFIL  MEDRCV X'                                     
MEXIT    DS    0H                                                               
*&&                                                                             
                                                                                
*&&US                                                                           
***********************************************************************         
* SPOT SYSTEM OPEN/READ/WRITE/CLOSE                                   *         
***********************************************************************         
SPT      CLI   SPTSYS,C' '         TEST IF SPT WANTED                           
         BE    SEXIT                                                            
         MVC   DIRNAM,=CL8'SPTDIR'                                              
         L     RE,=A(UTL)          CLEAR UTL+4                                  
         MVI   4(RE),0                                                          
         LLC   R0,SPTSE            R0=SPT SENUM                                 
         XC    DMCB(24),DMCB                                                    
         GOTO1 VDATAMGR,DMCB,DMREAD,=C'SYSFLES',(R0)                            
         MVC   WRK(4),DMCB+12                                                   
         MVC   P,SPACES                                                         
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P(40),=CL40'DMREAD/SYSFLES PASSING SYS SENUM'                    
         MVC   P+23(3),=C'SPT'                                                  
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
*                                                                               
         L     RE,=A(UTL)          SET SPT SENUM IN UTL                         
         STC   R0,4(RE)                                                         
         XC    DMCB(24),DMCB                                                    
         GOTO1 VDATAMGR,DMCB,DMREAD,=C'SYSFLES'                                 
         MVC   WRK(4),DMCB+12                                                   
         MVC   P(40),=CL40'DMREAD/SYSFLES WITH SYS SE SET'                      
         MVC   P+20(3),=C'SPT'                                                  
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
*                                                                               
SOPEN    XC    DMCB(24),DMCB       SET FILE OPEN LIST READONLY/UPDATIVE         
         LA    R2,SPTLST                                                        
         MVI   00(R2),C' '                                                      
         MVI   08(R2),C' '                                                      
         MVI   16(R2),C' '                                                      
         CLI   WRITE,C'N'                                                       
         BNH   SOPEN1                                                           
         MVI   00(R2),C'U'                                                      
         MVI   08(R2),C'U'                                                      
         MVI   16(R2),C'U'                                                      
SOPEN1   GOTO1 VDATAMGR,DMCB,DMOPEN,=C'SPO',(R2),REC                            
         MVC   P(40),=CL40'SYS    OPENED'                                       
         MVC   P(5),SPTSYS                                                      
         CLI   WRITE,C'N'                                                       
         BNH   *+10                                                             
         MVC   P+14(6),=C'UPDATE'                                               
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
*                                                                               
SREAD1   XC    KEY,KEY             SET KEY TO READ                              
         MVI   KEY,X'10'                                                        
         XR    R0,R0               TEST/SET READ FOR UPDATE                     
         CLI   WRITE,C'Y'                                                       
         BNE   *+8                                                              
         LA    R0,X'80'                                                         
         GOTO1 VDATAMGR,DMCB,((R0),DMRDHI),=C'SPTDIR',KEY,KEY                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DSKADR,KEY+14                                                    
         CLI   WRITE,C'Y'                                                       
         BNE   SREAD2                                                           
         GOTO1 VDATAMGR,DMCB,DMWRT,=C'SPTDIR',KEY,KEY                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P(40),=CL40'SYSDIR RECORD UPDATED'                               
         MVC   P(3),=C'SPT'                                                     
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
*                                                                               
SREAD2   XR    R0,R0               TEST/SET READ FOR UPDATE                     
         CLI   WRITE,C'Y'                                                       
         BNE   *+8                                                              
         LA    R0,X'80'                                                         
         GOTO1 VDATAMGR,DMCB,((R0),GETREC),=C'SPTFIL',DSKADR,REC,WRK            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   WRITE,C'Y'                                                       
         BNE   SREAD3                                                           
         GOTO1 VDATAMGR,DMCB,PUTREC,=C'SPTFIL',DSKADR,REC,WRK                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P(40),=CL40'SYSFIL RECORD UPDATED'                               
         MVC   P(3),=C'SPT'                                                     
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
*                                                                               
SREAD3   EQU   *                                                                
*                                                                               
SEOFCHK  GOTO1 =V(DMEOFCHK),DMCB,=C'EOFCHK',=CL8'SPTFIL1'                       
         L     R5,8(R1)                                                         
         MVC   EOFWRK,0(R5)                                                     
         LA    R5,EOFWRK                                                        
         LLC   R0,11(R5)                                                        
         MVC   P,SPACES                                                         
         GOTO1 =V(HEXOUT),PLIST,DMCB+8,P+0,1,(24,00)                            
         GOTO1 =V(HEXOUT),PLIST,11(R5),P+3,1,(24,00)                            
         GOTO1 =V(HEXOUT),PLIST,EOFDDNAD,P+7,28,(24,00)                         
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P,SPACES                                                         
         GOTO1 =V(HEXOUT),PLIST,EOFPLIST,P+7,24,(24,00)                         
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P,SPACES                                                         
         GOTO1 =V(HEXOUT),PLIST,EOFOTHER,P+7,32,(24,00)                         
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P,SPACES                                                         
*                                                                               
SEOFCHK1 GOTO1 =V(DMEOFCHK),DMCB,=C'EOFCHK',=CL8'XSPFIL1'                       
         L     R5,8(R1)                                                         
         MVC   EOFWRK,0(R5)                                                     
         LA    R5,EOFWRK                                                        
         LLC   R0,11(R5)                                                        
         MVC   P,SPACES                                                         
         GOTO1 =V(HEXOUT),PLIST,DMCB+8,P+0,1,(24,00)                            
         GOTO1 =V(HEXOUT),PLIST,11(R5),P+3,1,(24,00)                            
         GOTO1 =V(HEXOUT),PLIST,EOFDDNAD,P+7,28,(24,00)                         
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P,SPACES                                                         
         GOTO1 =V(HEXOUT),PLIST,EOFPLIST,P+7,24,(24,00)                         
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P,SPACES                                                         
         GOTO1 =V(HEXOUT),PLIST,EOFOTHER,P+7,32,(24,00)                         
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P,SPACES                                                         
*                                                                               
SCLOSE   GOTO1 VDATAMGR,DMCB,DMCLSE,=C'SPO'                                     
         MVC   P(40),=CL40'SYS    CLOSED'                                       
         MVC   P(5),SPTSYS                                                      
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         B     SEXIT                                                            
*                                                                               
SPTSYS   DC    CL5' '                                                           
SPTSE    DC    X'02'                                                            
SPTLST   DC    C' SPTDIR  SPTFIL  RECV   X'                                     
SEXIT    DS    0H                                                               
*&&                                                                             
*&&US                                                                           
***********************************************************************         
* PRNT SYSTEM OPEN/READ/WRITE/CLOSE                                   *         
***********************************************************************         
PRT      CLI   PRTSYS,C' '         TEST IF PRT WANTED                           
         BE    PEXIT                                                            
         MVC   DIRNAM,=CL8'PRTDIR'                                              
         L     RE,=A(UTL)          CLEAR UTL+4                                  
         MVI   4(RE),0                                                          
         LLC   R0,PRTSE            R0=PRT SENUM                                 
         XC    DMCB(24),DMCB                                                    
         GOTO1 VDATAMGR,DMCB,DMREAD,=C'SYSFLES',(R0)                            
         MVC   WRK(4),DMCB+12                                                   
         MVC   P,SPACES                                                         
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P(40),=CL40'DMREAD/SYSFLES PASSING SYS SENUM'                    
         MVC   P+23(3),=C'PRT'                                                  
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
*                                                                               
         L     RE,=A(UTL)          SET PRT SENUM IN UTL                         
         STC   R0,4(RE)                                                         
         XC    DMCB(24),DMCB                                                    
         GOTO1 VDATAMGR,DMCB,DMREAD,=C'SYSFLES'                                 
         MVC   WRK(4),DMCB+12                                                   
         MVC   P(40),=CL40'DMREAD/SYSFLES WITH SYS SE SET'                      
         MVC   P+20(3),=C'PRT'                                                  
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
*                                                                               
POPEN    XC    DMCB(24),DMCB       SET FILE OPEN LIST READONLY/UPDATIVE         
         LA    R2,PRTLST                                                        
         MVI   00(R2),C' '                                                      
         MVI   08(R2),C' '                                                      
         MVI   16(R2),C' '                                                      
         CLI   WRITE,C'N'                                                       
         BNH   POPEN3                                                           
         MVI   00(R2),C'U'                                                      
         MVI   08(R2),C'U'                                                      
         MVI   16(R2),C'U'                                                      
*                                                                               
POPEN1   CLI   WRITE,C'Y'          MUST BE TEST ADV - PRINT NOT GLOBAL          
         BNE   POPEN3                                                           
         CLI   DSPACE,C'T'                                                      
         BE    POPEN2                                                           
         CLI   DSPACE,C'Q'                                                      
         BE    POPEN2                                                           
         CLI   DSPACE,C'C'                                                      
         BE    POPEN2                                                           
         DC    H'0'                                                             
POPEN2   GOTO1 VDATAMGR,DMCB,=C'UPDID'                                          
         L     R1,12(R1)                                                        
         MVC   0(2,R1),=C'TT'                                                   
*                                                                               
POPEN3   GOTO1 VDATAMGR,DMCB,DMOPEN,=C'PRI',(R2),REC                            
         MVC   P(40),=CL40'SYS    OPENED'                                       
         MVC   P(5),PRTSYS                                                      
         CLI   WRITE,C'N'                                                       
         BNH   *+10                                                             
         MVC   P+14(6),=C'UPDATE'                                               
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
*                                                                               
PEOFCHK  GOTO1 =V(DMEOFCHK),DMCB,=C'EOFCHK',=CL8'PRTFIL2'                       
         L     R5,8(R1)                                                         
         MVC   EOFWRK,0(R5)                                                     
         LA    R5,EOFWRK                                                        
         LLC   R0,11(R5)                                                        
         MVC   P,SPACES                                                         
         GOTO1 =V(HEXOUT),PLIST,DMCB+8,P+0,1,(24,00)                            
         GOTO1 =V(HEXOUT),PLIST,11(R5),P+3,1,(24,00)                            
         GOTO1 =V(HEXOUT),PLIST,EOFDDNAD,P+7,28,(24,00)                         
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P,SPACES                                                         
         GOTO1 =V(HEXOUT),PLIST,EOFPLIST,P+7,24,(24,00)                         
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P,SPACES                                                         
         GOTO1 =V(HEXOUT),PLIST,EOFOTHER,P+7,32,(24,00)                         
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         MVC   P,SPACES                                                         
*                                                                               
PCLOSE   GOTO1 VDATAMGR,DMCB,DMCLSE,=C'PRI'                                     
         MVC   P(40),=CL40'SYS    CLOSED'                                       
         MVC   P(5),PRTSYS                                                      
         GOTO1 =V(PRINT),PLIST,PCC,=C'BL01'                                     
         B     PEXIT                                                            
*                                                                               
PRTSYS   DC    CL5' '                                                           
PRTSE    DC    X'04'                                                            
PRTLST   DC    C' PRTDIR  PRTFIL  PRECV  X'                                     
PEXIT    DS    0H                                                               
*&&                                                                             
                                                                                
EXIT     XBASE RC=ERROR,RL=1       EXIT WITH CC SET IF ERROR                    
                                                                                
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
                                                                                
WRITE    DC    C'N'                                                             
DSPACE   DC    C'T'                                                             
TABS     DC    C'N'                                                             
ERROR    DC    X'00'                                                            
C        DC    CL80' '                                                          
PCC      DC    X'00'                                                            
P        DC    CL132' '                                                         
SPACES   DC    CL132' '                                                         
                                                                                
DUB      DC    D'0'                                                             
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
         DC    CL8'*EOFWRK*'                                                    
EOFWRK   DS    0CL116                                                           
EOFDDNAD DS    CL60                                                             
EOFPLIST DS    CL24                                                             
EOFOTHER DS    CL32                                                             
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
**PAN#1  DC    CL21'003DMTEST2   08/14/13'                                      
         END                                                                    
