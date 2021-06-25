*          DATA SET PEMAP05    AT LEVEL 013 AS OF 05/01/02                      
*PHASE TE1B05A                                                                  
         TITLE 'TE1B05 - PROJECT RECORD MAINTENANCE'                            
TE1B05   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**PJMN**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,RELO05                                                        
         SPACE 1                                                                
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LIST                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   XIT                                                              
         L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO05                                                        
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         CLI   OFFLINE,C'Y'                                                     
         BE    *+6                 WE'RE OFF-LINE.                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,XIT              SET DOUBLE EXIT                              
***********************************************************************         
* THIS IS A SPECIAL APPLICATION 'NTR1' TO GRAB 45000 BYTES OF W/S     *         
         STM   RE,RC,12(RD)        POINTING RF TO THE START OF IT     *         
         LR    RE,RD                                                            
         A     RE,=F'45072'                                           *         
         ST    RD,4(RE)                                               *         
         LA    RF,72(RD)                                              *         
         MVC   0(4,RD),=C'PJWK'                                       *         
         ST    RE,8(RD)                                               *         
         LR    RD,RE                                                  *         
***********************************************************************         
         ST    RF,ASORTAR          THIS IS SORTERS AREA                         
         B     REPORT                                                           
         EJECT                                                                  
*              VALIDATE AND DISPLAY KEY                                         
         SPACE 3                                                                
VKEY     LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         CLI   ACTEQU,1                                                         
         BNE   LKEY1                                                            
         USING MAPKEY,R4                                                        
         MVC   MAPAGY,AGENCY                                                    
         MVI   MAPSYS,C'B'                                                      
         MVI   MAPKTYP,X'05'                                                    
         LA    R2,PJRSYSH                                                       
         GOTO1 VALISYS                                                          
         MVC   WORK(8),SAVSYSCD    DISPLAY FULL CODE                            
         BAS   RE,GENDISP                                                       
         LA    R2,PJRCODH                                                       
         GOTO1 ANY                                                              
         MVC   PRJSYS,SAVSYSCD                                                  
         MVC   PRJCODE,WORK                                                     
         B     XIT                                                              
         SPACE 1                                                                
DKEY     LA    R4,KEY              DISPLAY KEY (FOR SELECT)                     
         LA    R2,PJRSYSH                                                       
         MVC   WORK(8),PRJSYS                                                   
         BAS   RE,GENDISP                                                       
         LA    R2,PJRCODH                                                       
         MVC   WORK(8),PRJCODE                                                  
         BAS   RE,GENDISP                                                       
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE KEY FOR LIST OR REPORTS                                 
         SPACE 3                                                                
LKEY1    XC    QSYS,QSYS           PRESET FIELDS                                
         XC    QPROJ,QPROJ                                                      
         XC    QUSER,QUSER                                                      
         XC    QCOOR,QCOOR                                                      
         XC    QPERD,QPERD                                                      
         MVI   QDONE,0                                                          
         MVI   QSCHED,0                                                         
         MVI   SORTYPE,0                                                        
         SPACE 1                                                                
         LA    R2,PJLSYSH          SYSTEM                                       
         CLI   5(R2),0                                                          
         BE    LKEY2                                                            
         GOTO1 VALISYS                                                          
         MVC   QSYS,SAVSYSCD                                                    
         SPACE 1                                                                
         LA    R2,PJLCODH          PROJECT                                      
         CLI   5(R2),0                                                          
         BE    LKEY2                                                            
         GOTO1 VALIPROJ                                                         
         MVC   QPROJ,SAVPJCOD                                                   
         SPACE 1                                                                
LKEY2    LA    R2,PJLUSERH         ANY USER?                                    
         CLI   5(R2),0                                                          
         BE    LKEY4                                                            
         GOTO1 VALIUSER                                                         
         MVC   QUSER,WORK                                                       
         SPACE 1                                                                
LKEY4    LA    R2,PJLSCHEH         SCHEDULED?                                   
         CLI   5(R2),0                                                          
         BE    LKEY6                                                            
         MVC   QSCHED,8(R2)                                                     
         CLI   QSCHED,C'Y'         S/B Y OR N                                   
         BE    LKEY6                                                            
         CLI   QSCHED,C'N'                                                      
         BE    LKEY6                                                            
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         SPACE 1                                                                
LKEY6    LA    R2,PJLDONEH         DONE?                                        
         CLI   5(R2),0                                                          
         BE    LKEY8                                                            
         MVC   QDONE,8(R2)                                                      
         CLI   QDONE,C'Y'          S/B Y OR N                                   
         BE    LKEY8                                                            
         CLI   QDONE,C'N'                                                       
         BE    LKEY8                                                            
         MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         SPACE 1                                                                
LKEY8    LA    R2,PJLCOORH         CO-ORDINATOR?                                
         CLI   5(R2),0                                                          
         BE    LKEY10                                                           
         GOTO1 VALIPERS                                                         
         MVC   QCOOR,WORK                                                       
         SPACE 1                                                                
LKEY10   LA    R2,PJLPERH         PERIOD?                                       
         CLI   5(R2),0                                                          
         BE    LKEY12                                                           
         GOTO1 VALIPER,DMCB,QPERD                                               
         SPACE 1                                                                
LKEY12   LA    R2,PJLOTHH                                                       
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         XC    BLOCK(80),BLOCK                                                  
         GOTO1 SCANNER,DMCB,(R2),(3,BLOCK)                                      
         ZICM  R0,DMCB+4,1                                                      
         MVI   ERROR,INVALID                                                    
         BZ    TRAPERR                                                          
         LA    R5,BLOCK                                                         
*                                                                               
         USING OPTTABD,R4                                                       
LKEY14   LA    R4,OPTTAB                                                        
LKEY16   CLI   OPTKWORD,X'FF'                                                   
         BE    TRAPERR                                                          
         ZIC   R1,0(R5)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   OPTKWORD(0),12(R5)                                               
         BE    LKEY20                                                           
         LA    R4,OPTABLNQ(R4)                                                  
         B     LKEY16                                                           
LKEY18   LA    R5,32(R5)                                                        
         BCT   R0,LKEY14                                                        
         B     XIT                                                              
*                                                                               
LKEY20   ZICM  RF,OPTVALR,4                                                     
         A     RF,RELO05                                                        
         BASR  RE,RF                                                            
         BE    LKEY18                                                           
         B     TRAPERR                                                          
         DROP  R4                                                               
         EJECT                                                                  
OVALSORT NTR1                                                                   
         LA    RE,SORTTAB                                                       
OSORT2   CLI   0(RE),X'FF'                                                      
         BE    BADEX                                                            
         ZIC   R1,1(R5)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),22(R5)                                                   
         BE    OSORT4                                                           
         LA    RE,L'SORTTAB(RE)                                                 
         B     OSORT2                                                           
*                                                                               
OSORT4   MVC   SORTYPE,8(RE)                                                    
         B     GUDEX                                                            
         EJECT                                                                  
*              VALIDATE AND DISPLAY CHAT FIELDS                                 
         SPACE 3                                                                
         USING MAPKEYD,R4                                                       
VREC     DS    0H                                                               
DREC     LA    R2,PJRDESCH         DESCRIPTION                                  
         CLI   MODE,DISPREC                                                     
         BE    DREC2                                                            
         GOTO1 ANY                                                              
DREC2    MVI   ELCODE,X'50'                                                     
         MVI   MAX,3                                                            
         MVI   OPTION,0                                                         
         GOTO1 VALICHAT                                                         
         SPACE 1                                                                
         LA    R2,PJRAPPH          APPRAISAL                                    
         MVI   ELCODE,X'56'                                                     
         MVI   MAX,3                                                            
         GOTO1 VALICHAT                                                         
         SPACE 1                                                                
         LA    R2,PJRCOMMH         COMPLETION COMMENTS                          
         MVI   ELCODE,X'58'                                                     
         MVI   MAX,2                                                            
         GOTO1 VALICHAT                                                         
         EJECT                                                                  
*              VALIDATE PROJECT STATUS                                          
         SPACE 3                                                                
         MVI   ELCODE,X'52'        PROJECT ELEMENT                              
         USING PROJELD,R6                                                       
         LA    R6,ELEMENT                                                       
         GOTO1 REMELEM                                                          
         CLI   MODE,DISPREC                                                     
         BE    DREC62                                                           
         XC    PROJEL(24),PROJEL                                                
         MVI   PROJEL,X'52'        VALIDATE                                     
         MVI   PROJLEN,24                                                       
         SPACE 1                                                                
VREC2    LA    R2,PJRDURH          DURATION                                     
         CLI   5(R2),0                                                          
         BE    VREC4                                                            
         GOTO1 VALINUM                                                          
         MVI   ERROR,INVALID                                                    
         CLI   ACTUAL,99                                                        
         BH    TRAPERR                                                          
         MVC   PROJDUR,ACTUAL                                                   
         SPACE 1                                                                
VREC4    LA    R2,PJRSCHDH         SCHEDULED PERIOD                             
         CLI   5(R2),0                                                          
         BE    VREC6                                                            
         MVI   ERROR,INVALID                                                    
         GOTO1 VALIPER,DMCB,(2,WORK)                                            
         CLC   WORK(6),=6C'0'                                                   
         BE    TRAPERR             NO START DATE                                
         CLC   WORK+6(4),=C'9912'                                               
         BE    TRAPERR             NO END DATE                                  
         GOTO1 DATCON,DMCB,(0,WORK),(1,PROJSCST)                                
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,WORK)                                  
         MVC   PROJSCND,WORK                                                    
         SPACE 1                                                                
VREC6    LA    R2,PJRCOORH         CO-ORDINATOR                                 
         CLI   5(R2),0                                                          
         BE    VREC8                                                            
         GOTO1 VALIPERS                                                         
         MVC   PROJCOOR,WORK                                                    
         SPACE 1                                                                
VREC8    LA    R2,PJRCOMPH         PERCENT COMPLETE                             
         CLI   5(R2),0                                                          
         BE    VUSER                                                            
         GOTO1 VALINUM                                                          
         CLC   ACTUAL,PROJOKPC     NOTE DATE IF A CHANGE                        
         BE    VUSER                                                            
         MVC   PROJOKPC,ACTUAL                                                  
         GOTO1 DATCON,DMCB,(5,0),(1,PROJOKDT)                                   
         B     VUSER                                                            
         EJECT                                                                  
*              DISPLAY PROJECT STATUS                                           
         SPACE 3                                                                
         USING PROJELD,R6                                                       
DREC4    DS    0H                                                               
         SPACE 1                                                                
DREC6    BAS   RE,GENDISP                                                       
DREC62   LA    R2,PJRDURH          DURATION                                     
         MVC   WORK(8),SPACES                                                   
         EDIT  (1,PROJDUR),(2,WORK),WRK=DMCB,ALIGN=LEFT                         
         BAS   RE,GENDISP                                                       
         SPACE 1                                                                
         LA    R2,PJRSCHDH         SCHEDULED DATE (MMMYY-MMMYY)                 
         MVC   WORK(16),SPACES                                                  
         OC    PROJSCND,PROJSCND                                                
         BZ    DREC6C                                                           
         MVC   DUB(2),PROJSCST                                                  
         MVI   DUB+2,1                                                          
         GOTO1 DATCON,DMCB,(1,DUB),(9,WORK)                                     
*&&US*&& MVC   WORK+3(3),WORK+4                                                 
         CLC   PROJSCND,PROJSCST                                                
         BE    DREC6C                                                           
         MVI   WORK+5,C'-'                                                      
         MVC   DUB(2),PROJSCND                                                  
         MVI   DUB+2,1                                                          
         GOTO1 DATCON,DMCB,(1,DUB),(9,WORK+6)                                   
*&&US*&& MVC   WORK+9(3),WORK+10                                                
         SPACE 1                                                                
DREC6C   BAS   RE,GENDISP                                                       
         LA    R2,PJRCOORH                                                      
         MVC   WORK(8),PROJCOOR                                                 
         BAS   RE,GENDISP                                                       
         SPACE 1                                                                
         LA    R2,PJRCOMPH         COMPLETE PCT                                 
         EDIT  (1,PROJOKPC),(3,WORK),WRK=DMCB,ALIGN=LEFT                        
         BAS   RE,GENDISP                                                       
         LA    R2,PJRONH           ON DATE                                      
         CLI   PROJOKDT,0                                                       
         BE    DREC8                                                            
         MVC   WORK(2),=C'ON'                                                   
         GOTO1 DATCON,DMCB,(1,PROJOKDT),(8,WORK+3)                              
         SPACE 1                                                                
DREC8    BAS   RE,GENDISP                                                       
         B     DUSER                                                            
         EJECT                                                                  
*              HANDLE USERS                                                     
         SPACE 3                                                                
VUSER    GOTO1 ADDELEM             VALIDATION                                   
         MVI   ELCODE,X'54'                                                     
         GOTO1 REMELEM                                                          
         LA    R6,ELEMENT                                                       
         USING PUSELD,R6                                                        
         LA    R2,PJRUSERH                                                      
         LA    R3,9                (MAX 9)                                      
         SPACE 1                                                                
VUSER2   CLI   5(R2),0                                                          
         BE    XIT                                                              
         XC    BLOCK(80),BLOCK                                                  
         GOTO1 SCANNER,DMCB,(R2),(3,BLOCK)                                      
         CLI   DMCB+4,2                                                         
         MVI   ERROR,INVALID                                                    
         BL    TRAPERR                                                          
         MVC   WORK(8),BLOCK+12    GET USER ID                                  
         CLC   WORK(8),=CL8'ALL'   ALLOW KEYWORD 'ALL'                          
         BE    VUSER4                                                           
         MVI   OPTION,C'W'                                                      
         GOTO1 VALIUSER                                                         
VUSER4   XC    ELEMENT,ELEMENT                                                  
         MVI   PUSEL,X'54'                                                      
         MVI   PUSLEN,16                                                        
         MVC   PUSER,WORK                                                       
         GOTO1 DATVAL,DMCB,(2,BLOCK+44),DUB COMPULSORY REQUESTED DATE           
         MVI   ERROR,INVALID                                                    
         OC    DMCB(4),DMCB                                                     
         BZ    TRAPERR                                                          
         MVC   DUB+4(2),=C'01'                                                  
         GOTO1 DATCON,DMCB,(0,DUB),(1,WORK)                                     
         MVC   PUSYM,WORK                                                       
         MVC   PUSPRI,BLOCK+71     OPTIONAL USER PRIORITY                       
         GOTO1 ADDELEM                                                          
         BAS   RE,BUMP                                                          
         BCT   R3,VUSER2                                                        
         B     XIT                                                              
         SPACE 1                                                                
DUSER    L     R6,AIO              DISPLAY                                      
         MVI   ELCODE,X'54'                                                     
         LA    R2,PJRUSERH                                                      
         LA    R3,9                                                             
         BAS   RE,GETEL                                                         
         B     DUSER4                                                           
         SPACE 1                                                                
DUSER2   BAS   RE,NEXTEL                                                        
         SPACE 1                                                                
DUSER4   BNE   DUSER8                                                           
         MVC   BLOCK(60),SPACES                                                 
         MVC   BLOCK(8),PUSER                                                   
         MVC   WORK(2),PUSYM       GET DUMMY DAY NO.                            
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(9,BLOCK+20)                                
*&&US*&& MVC   BLOCK+23(3),BLOCK+24                                             
         LA    R4,2                SET NUMBER OF UNSCAN FIELDS                  
         CLI   PUSPRI,0                                                         
         BE    DUSER6              NO PRIORITY                                  
         EDIT  (B1,PUSPRI),(2,BLOCK+40),ALIGN=LEFT                              
         LA    R4,3                BUMP NUMBER OF FIELDS                        
DUSER6   GOTO1 UNSCAN,DMCB,((R4),BLOCK),(R2),0                                  
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
         BCT   R3,DUSER2                                                        
         B     XIT                                                              
         SPACE 1                                                                
DUSER8   MVC   WORK(17),SPACES                                                  
         BAS   RE,GENDISP                                                       
         BAS   RE,BUMP                                                          
         BCT   R3,DUSER8                                                        
         B     XIT                                                              
         EJECT                                                                  
*              LIST RECORDS                                                     
         SPACE 3                                                                
LIST     LA    R4,KEY                                                           
         OC    KEY,KEY             WAS A KEY PROVIDED?                          
         BNZ   LIST4                                                            
         USING MAPKEYD,R4                                                       
         MVC   MAPAGY,AGENCY                                                    
         MVI   MAPSYS,C'B'                                                      
         MVI   MAPKTYP,X'05'                                                    
         MVC   PRJSYS,QSYS                                                      
         MVC   PRJCODE,QPROJ                                                    
         GOTO1 HIGH                                                             
         B     LIST4                                                            
         SPACE 1                                                                
LIST2    GOTO1 SEQ                                                              
         SPACE 1                                                                
LIST4    CLC   KEY(3),KEYSAVE      CHECKMAIN C/B                                
         BNE   XIT                                                              
         CLI   QSYS,0              IF SYSTEM WAS REQUESTED                      
         BE    LIST6                                                            
         CLC   KEY(11),KEYSAVE     SHOULD MATCH ON THIS                         
         BNE   XIT                                                              
         SPACE 1                                                                
LIST6    GOTO1 GETREC                                                           
         OC    QUSER,QUSER                                                      
         BZ    LIST66              NO USER FILTER                               
         USING PUSELD,R6                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,X'54'        SEARCH FOR USER ELEMENT(S)                   
         BAS   RE,GETEL                                                         
         B     LIST64                                                           
         SPACE 1                                                                
LIST62   BAS   RE,NEXTEL                                                        
LIST64   BNE   LIST2               NO SUCH USER                                 
         CLC   PUSER(4),=C'ALL '                                                
         BE    LIST66                                                           
         CLC   QUSER,PUSER                                                      
         BE    LIST66              MATCHED THE USER FILTER                      
         B     LIST62                                                           
         SPACE 1                                                                
LIST66   MVC   LISTAR,SPACES       SHOW CODE                                    
         LA    R3,LISTAR                                                        
         LA    R5,PJLHED                                                        
         OI    PJLHEDH+6,X'80'                                                  
         MVC   0(8,R3),PRJSYS                                                   
         MVC   0(6,R5),=C'SYSTEM'                                               
         SPACE 1                                                                
         LA    R3,9(R3)                                                         
         LA    R5,9(R5)                                                         
         MVC   0(8,R3),PRJCODE                                                  
         MVC   0(7,R5),=C'PROJECT'                                              
         SPACE 1                                                                
         LA    R3,9(R3)            DESCRIPTION                                  
         LA    R5,9(R5)                                                         
         MVI   ELCODE,X'50'                                                     
         MVI   OPTION,C'W'                                                      
         MVI   MAX,1                                                            
         GOTO1 DISPCHAT                                                         
         MVC   0(25,R3),WORK                                                    
         MVC   0(11,R5),=C'DESCRIPTION'                                         
         SPACE 1                                                                
         LA    R3,26(R3)           SCHEDULED                                    
         LA    R5,26(R5)                                                        
         LA    R6,ELEMENT                                                       
         MVI   ELCODE,X'52'                                                     
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
         USING PROJELD,R6                                                       
LIST68   MVC   0(5,R5),=C'SCHED'   SCHEDULED PERIOD                             
         OC    PROJSCST(4),PROJSCST                                             
         BZ    LIST682                                                          
         CLI   QSCHED,C'N'                                                      
         BE    LIST2               SCHEDULED, WE DON'T WANT IT                  
         B     LIST684                                                          
LIST682  CLI   QSCHED,C'Y'                                                      
         BE    LIST2               UNSCHEDULED, WE DON'T WANT IT.               
         B     LIST6A4                                                          
         SPACE 1                                                                
LIST684  CLI   PROJSCST,0                                                       
         BE    LIST6A              NO START DATE                                
         MVC   WORK(2),PROJSCST                                                 
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(9,(R3))                                    
*&&US*&& MVC   12(3,R3),13(R3)                                                  
         CLC   PROJSCST,PROJSCND                                                
         BE    LIST6A4                                                          
         CLC   PROJSCND,=X'9912'                                                
         BE    *+12                                                             
         MVI   3(R3),C'-'                                                       
         B     LIST6A2                                                          
         MVC   6(2,R3),=C'ON'      INDEFINITE END DATE                          
         B     LIST6A4                                                          
         SPACE 1                                                                
LIST6A   MVC   0(3,R3),=C'TIL'                                                  
LIST6A2  MVC   WORK(2),PROJSCND                                                 
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(9,4(R3))                                   
*&&US*&& MVC   7(3,R3),8(R3)                                                    
         SPACE 1                                                                
LIST6A4  LA    R3,10(R3)           CO-ORDINATOR                                 
         LA    R5,10(R5)                                                        
         OC    QCOOR,QCOOR                                                      
         BZ    *+14                NO COORDINATOR FILTER                        
         CLC   PROJCOOR,QCOOR                                                   
         BNE   LIST2                                                            
         MVC   0(8,R3),PROJCOOR                                                 
         MVC   0(5,R5),=C'COORD'                                                
         SPACE 1                                                                
         LA    R3,9(R3)                                                         
         LA    R5,9(R5)                                                         
         MVC   0(4,R5),=C'DONE'                                                 
         CLI   PROJOKPC,100        PERCENT DONE                                 
         BE    LIST6G                                                           
         CLI   PROJOKPC,0                                                       
         BE    LIST6C                                                           
         EDIT  (1,PROJOKPC),(3,0(R3))                                           
         MVI   3(R3),C'%'                                                       
LIST6C   CLI   QDONE,C'Y'                                                       
         BE    LIST2               INCOMPLETE, SO IGNORE                        
         B     LIST8                                                            
         SPACE 1                                                                
LIST6G   GOTO1 DATCON,DMCB,(1,PROJOKDT),(9,0(R3))                               
*&&US                                                                           
         MVC   3(3,R3),4(R3)       REMOVE US STYLE /                            
*&&                                                                             
         CLI   QDONE,C'N'          COMPLETE                                     
         BE    LIST2               SO IGNORE IF NO REQUESTED                    
         SPACE 1                                                                
LIST8    GOTO1 LISTMON                                                          
         B     LIST2                                                            
         EJECT                                                                  
*              PRINT REPORT                                                     
*                                                                               
REPORT   LA    R4,KEY                                                           
         MVC   REPSTDT(4),=X'FFFF0000'                                          
         XC    REPMWKS,REPMWKS     CLEAR MAN WEEKS                              
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,(43,ASORTAR)                        
         OC    KEY,KEY             WAS A KEY PROVIDED?                          
         BNZ   SORT4                                                            
         USING MAPKEYD,R4                                                       
         MVC   MAPAGY,AGENCY                                                    
         MVI   MAPSYS,MAPSYSQ                                                   
         MVI   MAPKTYP,PRJKTYPQ                                                 
         MVC   PRJSYS,QSYS                                                      
         MVC   PRJCODE,QPROJ                                                    
         GOTO1 HIGH                                                             
         B     SORT4                                                            
*                                                                               
SORT2    GOTO1 SEQ                                                              
*                                                                               
SORT4    LA    R4,KEY                                                           
         CLC   KEY(3),KEYSAVE      CHECKMAIN C/B                                
         BNE   SORTEND                                                          
         CLI   QSYS,0              IF SYSTEM WAS REQUESTED                      
         BE    SORT6                                                            
         CLC   PRJSYS,KEYSAVE+(PRJSYS-MAPKEY) WE SHOULD MATCH                   
         BNE   SORTEND                                                          
*                                                                               
SORT6    GOTO1 GETREC                                                           
         XC    SORTREC,SORTREC                                                  
         MVC   SORTSYS,PRJSYS                                                   
         MVC   SORTPROJ,PRJCODE                                                 
         MVC   SORTADDR,DMDSKADD                                                
         CLI   SORTYPE,0                                                        
         BNE   *+10                                                             
         MVC   SORTHIGH(8),PRJSYS                                               
*                                                                               
         MVI   USEROK,0            SET TO FAIL USER FILTER                      
         CLI   QUSER,0                                                          
         BNE   *+8                 USER FILTER ACTIVE                           
         MVI   USEROK,1            IF NO USER FILTER, SET TO PASS               
         USING PUSELD,R6                                                        
         L     R6,AIO                                                           
         XC    USERBLK,USERBLK                                                  
         LA    R5,USERBLK          R5=A(LIST OF UNSCANNED USER DATA)            
         MVI   ELCODE,X'54'        SEARCH FOR USER ELEMENT(S)                   
         BAS   RE,GETEL                                                         
         BNE   SORT66              NO USER ELEMENTS                             
         B     SORT64                                                           
*                                                                               
SORT62   BAS   RE,NEXTEL                                                        
         BNE   SORT66              END OF USERS                                 
         CLI   SORTYPE,C'U'                                                     
         BNE   SORT64B             NOT INTERESTED IN USER DETAILS YET           
SORT64   MVC   0(17,R5),SPACES                                                  
         MVC   0(8,R5),PUSER       UNSCAN THIS USER                             
         MVC   WORK(2),PUSYM       GET DUMMY DAY NO.                            
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(9,12(R5))                                  
*&&US*&& MVC   15(2,R5),16(R5)                                                  
*&&US*&& MVI   17(R5),C' '                                                      
         MVI   9(R5),X'FF'         NO PRI GET HIGHEST KEY VALUE                 
         CLI   PUSPRI,0                                                         
         BE    SORT64A             NO PRIORITY                                  
         LA    RF,10               INVERT PRIORITY TO GET RECORDS               
         ZIC   RE,PUSPRI           IN DESCENDING PRIORITY ORDER                 
         SR    RF,RE                                                            
         STC   RF,9(R5)                                                         
SORT64A  LA    R5,17(R5)                                                        
*                                                                               
SORT64B  CLI   QUSER,0                                                          
         BE    SORT62              NOT FILTERING                                
         CLC   PUSER(4),=C'ALL '                                                
         BE    SORT64C                                                          
         CLC   QUSER,PUSER                                                      
         BNE   SORT62              DOESN'T MATCH THE USER FILTER                
SORT64C  MVI   USEROK,1            DOES MATCH                                   
         B     SORT62                                                           
         SPACE 1                                                                
SORT66   CLI   USEROK,0                                                         
         BE    SORT2               FAILED THE USER FILTER                       
         L     R4,AIO                                                           
*                                                                               
         USING PROJELD,R6                                                       
SORT678  LA    R6,ELEMENT                                                       
         MVI   ELCODE,X'52'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
SORT68   OC    PROJSCST(4),PROJSCST                                             
         BZ    SORT682                                                          
         CLI   QSCHED,C'N'                                                      
         BE    SORT2               SCHEDULED, WE DON'T WANT IT                  
         B     SORT684                                                          
SORT682  CLI   QSCHED,C'Y'                                                      
         BE    SORT2               UNSCHEDULED, WE DON'T WANT IT.               
         B     SORT6A                                                           
*                                                                               
SORT684  OC    QPERD,QPERD                                                      
         BZ    SORT6A              NO PERIOD FILTER                             
         CLC   PROJSCND,QPERD                                                   
         BL    SORT2               END DATE BEFORE FILTER START                 
         CLC   PROJSCST,QPERD+2                                                 
         BH    SORT2               START DATE BEFORE FILTER END                 
*                                                                               
SORT6A   OC    QCOOR,QCOOR                                                      
         BZ    *+14                NO COORDINATOR FILTER                        
         CLC   PROJCOOR,QCOOR                                                   
         BNE   SORT2                                                            
         CLI   SORTYPE,C'C'        ORDERING BY CO-ORDINATOR                     
         BNE   *+10                NO                                           
         MVC   SORTHIGH(8),PROJCOOR                                             
*                                                                               
         CLI   PROJOKPC,0          PERCENT DONE                                 
         BE    SORT6C                                                           
         CLI   PROJOKPC,100        PERCENT DONE                                 
         BE    SORT6G                                                           
SORT6C   CLI   QDONE,C'Y'                                                       
         BE    SORT2               INCOMPLETE, SO IGNORE                        
         B     SORT8                                                            
*                                                                               
SORT6G   CLI   QDONE,C'N'          COMPLETE                                     
         BE    SORT2               SO IGNORE IF NOT REQUESTED                   
*                                                                               
SORT8    CLI   SORTYPE,C'U'                                                     
         BE    SORT84              SPECIAL USER ORDER SET UP                    
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         B     SORT2                                                            
*                                                                               
SORT84   LA    R5,USERBLK          PUT D/A TO SORT FOR EACH USER                
         LA    R0,9                                                             
SORTA    CLI   0(R5),0                                                          
         BE    SORT2                                                            
         CLI   QUSER,0             IF FILTERING ONLY USE FILTERED USER          
         BE    SORTC                                                            
         CLC   QUSER,0(R5)                                                      
         BNE   SORTE                                                            
SORTC    MVC   SORTHIGH(17),0(R5)                                               
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         MVI   SORTDUPE,1                                                       
SORTE    LA    R5,17(R5)                                                        
         BCT   R0,SORTA                                                         
         B     SORT2                                                            
         EJECT                                                                  
*              READ SORTED RECORDS, GETREC AND PRINT REPORT                     
*                                                                               
SORTEND  GOTO1 SORTER,DMCB,=C'GET',0                                            
         ZICM  R5,DMCB+4,4                                                      
         BZ    REPTTOT                                                          
         MVC   SORTREC,0(R5)                                                    
*                                                                               
         USING MAPKEYD,R4                                                       
         CLC   LASTSORT,SORTHIGH                                                
         BE    REPT2                                                            
         MVI   FORCEHED,C'Y'                                                    
         MVC   LASTSORT,SORTHIGH                                                
         B     REPT22                                                           
REPT2    GOTO1 SPOOL,DMCB,ASPOOLD  SPACE BETWEEN PROJECTS                       
REPT22   LA    R4,KEY              SET FOR GENCONS GETREC                       
         XC    KEY,KEY                                                          
         MVC   MAPDA,SORTADDR                                                   
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
*                                                                               
         USING PUSELD,R6                                                        
         L     R6,AIO                                                           
         XC    USERBLK,USERBLK                                                  
         LA    R5,USERBLK          R5=A(LIST OF UNSCANNED USER DATA)            
         MVI   ELCODE,X'54'        SEARCH FOR USER ELEMENT(S)                   
         BAS   RE,GETEL                                                         
         BNE   REPT66              NO USER ELEMENTS                             
         B     REPT622                                                          
*                                                                               
REPT62   BAS   RE,NEXTEL                                                        
         BNE   REPT66              END OF USERS                                 
REPT622  CLI   SORTYPE,C'U'        IF SORTING ON USER, DON'T SHOW OTHER         
         BNE   REPT64              USERS ON THIS PAGE                           
         CLC   PUSER,LASTSORT                                                   
         BNE   REPT62                                                           
REPT64   MVC   0(17,R5),SPACES                                                  
         MVC   0(8,R5),PUSER       UNSCAN THIS USER                             
         MVC   WORK(2),PUSYM       GET DUMMY DAY NO.                            
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(9,12(R5))                                  
*&&US*&& MVC   15(3,R5),16(R5)                                                  
         CLI   PUSPRI,0                                                         
         BE    REPT64A             NO PRIORITY                                  
         EDIT  (B1,PUSPRI),(2,9(R5))                                            
REPT64A  LA    R5,17(R5)                                                        
         B     REPT62                                                           
*                                                                               
         USING CHATELD,R6                                                       
REPT66   MVC   P+1(8),PRJCODE      SHOW CODE                                    
         MVI   ELCODE,X'50'        GET DESCRIPTION                              
         L     R6,AIO                                                           
         LA    R5,P+10                                                          
         LA    R0,3                                                             
         BAS   RE,GETEL                                                         
         B     *+8                                                              
REPT67   BAS   RE,NEXTEL                                                        
         BNE   REPT674                                                          
         ZIC   R1,CHATLEN                                                       
         SH    R1,=Y(CHAT+1-CHATEL)                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),CHAT                                                     
         LA    R5,132(R5)                                                       
         BCT   R0,REPT67                                                        
*                                                                               
REPT674  MVI   ELCODE,X'56'        GET APPRAISAL                                
         L     R6,AIO                                                           
         LA    R5,P+59                                                          
         LA    R0,3                                                             
         BAS   RE,GETEL                                                         
         B     *+8                                                              
REPT676  BAS   RE,NEXTEL                                                        
         BNE   REPT678                                                          
         ZIC   R1,CHATLEN                                                       
         SH    R1,=Y(CHAT+1-CHATEL)                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),CHAT                                                     
         LA    R5,132(R5)                                                       
         BCT   R0,REPT676                                                       
*                                                                               
         USING PROJELD,R6                                                       
REPT678  LA    R6,ELEMENT                                                       
         MVI   ELCODE,X'52'                                                     
         GOTO1 REMELEM                                                          
         EDIT  (B1,PROJDUR),(2,P+90) DURATION                                   
         OC    PROJSCST(4),PROJSCST                                             
         BZ    REPT6A                                                           
         CLI   SORTDUPE,1                                                       
         BE    REPT688             FOR ORIGINAL RECORDS ONLY                    
         ZIC   R0,PROJDUR          ACCUMULATE THE MAN WEEKS IF PROJECT          
         AH    R0,REPMWKS          IS SCHEDULED                                 
         STH   R0,REPMWKS                                                       
*                                                                               
REPT68   OC    QPERD,QPERD                                                      
         BZ    REPT686             NO PERIOD FILTER                             
         CLC   PROJSCND,QPERD                                                   
         BL    SORTEND             END DATE BEFORE FILTER START                 
         CLC   PROJSCST,QPERD+2                                                 
         BH    SORTEND             START DATE BEFORE FILTER END                 
*                                                                               
REPT686  CLC   PROJSCST,REPSTDT    ENSURE REPORT DATE RANGE INCLUDES            
         BNL   *+10                ALL OF THIS PROJECT.                         
         MVC   REPSTDT,PROJSCST                                                 
         CLC   PROJSCND,REPNDDT                                                 
         BNH   *+10                                                             
         MVC   REPNDDT,PROJSCND                                                 
*                                                                               
REPT688  MVC   WORK(2),PROJSCST    EDIT START DATE                              
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(9,P+94)                                    
*&&US*&& MVC   P+97(3),P+98                                                     
         CLC   PROJSCST,PROJSCND                                                
         BE    REPT6A                                                           
         MVI   P+97,C'-'                                                        
*                                                                               
         MVC   WORK(2),PROJSCND    EDIT END DATE                                
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(9,P+98)                                    
*&&US*&& MVC   P+101(3),P+102                                                   
*                                                                               
REPT6A   MVC   P2+94(8),PROJCOOR                                                
*                                                                               
         CLI   PROJOKPC,0          PERCENT DONE                                 
         BE    REPT8                                                            
         EDIT  (1,PROJOKPC),(3,P+104)                                           
         GOTO1 DATCON,DMCB,(1,PROJOKDT),(9,P2+104)                              
*&&US                                                                           
         MVC   P2+107(3),P2+108      REMOVE US STYLE /                          
         MVI   P+107,C'%'                                                       
*&&                                                                             
*                                                                               
REPT8    LA    R5,USERBLK          SET USER INFO IN PRINT LINES                 
         LA    R6,P+41             4 LINES AT A TIME                            
         LA    R0,4                                                             
REPTA    CLI   0(R5),0                                                          
         BE    REPTC                                                            
         MVC   0(17,R6),0(R5)                                                   
         LA    R5,17(R5)                                                        
         LA    R6,132(R6)                                                       
         BCT   R0,REPTA                                                         
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
         LA    R6,P+41                                                          
         LA    R0,4                                                             
         CLI   0(R5),0                                                          
         BE    SORTEND             LAST USER LINE PRINTED                       
         B     REPTA               MORE TO PRINT                                
*                                                                               
REPTC    GOTO1 SPOOL,DMCB,ASPOOLD  PRINT THE REMAINDER                          
         B     SORTEND                                                          
         EJECT                                                                  
*              PRINT MAN/WEEK TOTALS                                            
*                                                                               
REPTTOT  GOTO1 SPOOL,DMCB,ASPOOLD                                               
*&&US*&& B     XIT                 NOT APPROPRIATE FOR US                       
         MVC   P+1(25),=C'TOTAL MAN/WEEKS ALLOCATED'                            
         MVC   P+35(40),=C'INCLUDES 35PCT OF PERIOD AS MAINTAINANCE'            
         MVC   P2+1(21),=C'TOTAL WEEKS AVAILABLE'                               
*                                                                               
         MVC   WORK(2),REPSTDT     GET STANDARD REPORT START DATE               
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(0,WORK+3)                                  
         MVC   WORK(2),REPNDDT     GET STANDARD REPORT END DATE                 
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(0,WORK+15)                                 
         GOTO1 ADDAY,DMCB,WORK+15,WORK+9,31 WITH END DATE OF MONTH              
         MVC   WORK+13(2),=C'01'                                                
         L     RF,=F'-1'                                                        
         GOTO1 ADDAY,DMCB,WORK+9,WORK+15,(RF)                                   
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 CPERVERT,DMCB,WORK+3,WORK+15,0                                   
         USING PERVERTD,R1                                                      
         LH    R3,PERVWKS                                                       
         EDIT  (R3),(4,P2+28),ALIGN=LEFT                                        
*                                                                               
         LR    RF,R3               NOW ADD MAINTAINANCE TO ALLOCATED            
         M     RE,=F'70'           MAN WEEKS CALCULATED AT 35PCT                
         D     RE,=F'100'          OF REPORT PERIOD.                            
         AH    RF,=H'1'                                                         
         SRA   RF,1                                                             
         AH    RF,REPMWKS                                                       
         EDIT  (RF),(4,P+28),ALIGN=LEFT                                         
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
         B     XIT                                                              
         DROP  R1,RF                                                            
         EJECT                                                                  
*              ODDMENTS                                                         
*                                                                               
GENDISP  ZIC   R1,0(R2)            GENERAL DISPLAY                              
         SH    R1,=H'9'                                                         
         EX    R1,GDCLC                                                         
         BE    GENDISP2                                                         
         EX    R1,GDMVC                                                         
         OI    6(R2),X'80'                                                      
*                                                                               
GENDISP2 MVC   WORK,SPACES                                                      
         BR    RE                                                               
*                                                                               
GDCLC    CLC   8(0,R2),WORK                                                     
GDMVC    MVC   8(0,R2),WORK                                                     
*                                                                               
BUMP     ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BR    RE                                                               
*                                                                               
BADEX    LTR   RB,RB                                                            
         B     XIT                 CC OF NEQ = ERROR EXIT                       
*                                                                               
GUDEX    CR    RB,RB                                                            
         B     XIT                 CC OF EQ = GOOD EXIT                         
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINE ROUTINES                                                
*                                                                               
HOOK     NTR1                                                                   
         MVC   H5+1(11),=C'SYSTEM   - '                                         
         CLI   SORTYPE,0                                                        
         BE    HOOKEND                                                          
         LA    RE,SORTTAB                                                       
HOOK2    CLI   0(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   SORTYPE,8(RE)                                                    
         BE    HOOK4                                                            
         LA    RE,L'SORTTAB(RE)                                                 
         B     HOOK2                                                            
HOOK4    MVC   H5+1(8),0(RE)                                                    
*                                                                               
HOOKEND  MVC   H5+12(8),LASTSORT                                                
         GOTO1 SQUASHER,DMCB,H5+1,19                                            
         L     R5,ABOX             TURN ON BOXES                                
         USING BOXD,R5                                                          
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXOFF,0                                                         
         MVI   BOXCOLS+000,C'L'                                                 
         MVI   BOXCOLS+009,C'C'                                                 
         MVI   BOXCOLS+040,C'C'                                                 
         MVI   BOXCOLS+058,C'C'                                                 
         MVI   BOXCOLS+089,C'C'                                                 
         MVI   BOXCOLS+093,C'C'                                                 
         MVI   BOXCOLS+103,C'C'                                                 
         MVI   BOXCOLS+109,C'R'                                                 
         MVI   BOXROWS+7,C'T'                                                   
         MVI   BOXROWS+10,C'M'                                                  
         MVI   BOXROWS+58,C'B'                                                  
         B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
OPTTAB   DS    0CL12                                                            
         DC    CL8'ORDER   ',A(OVALSORT)                                        
         DC    AL1(255)                                                         
*                                                                               
SORTTAB  DS    0CL9                                                             
         DC    CL8'SYSTEM  ',AL1(0)                                             
         DC    CL8'USER    ',CL1'U'                                             
         DC    CL8'COORD   ',CL1'C'                                             
         DC    CL8'PERSON  ',CL1'C'                                             
         DC    AL1(255)                                                         
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,33,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(38,,38)'                              
         EJECT                                                                  
HEDSPECS SSPEC H1,2,C'MAP SYSTEM'                                               
         SSPEC H2,2,C'----------'                                               
         SSPEC H1,40,C'PROJECT REPORT'                                          
         SSPEC H2,40,C'--------------'                                          
         SSPEC H1,77,REPORT                                                     
         SSPEC H1,96,REQUESTOR                                                  
         SSPEC H2,77,RUN                                                        
         SSPEC H2,103,PAGE                                                      
         SSPEC H9,2,C'PROJECT  DESCRIPTION OF PROJECT'                          
         SSPEC H9,42,C'---REQUESTORS---- SYSTEMS APPRAISAL'                     
         SSPEC H9,91,C'MAN SCHEDULED PCNT'                                      
*&&UK*&& SSPEC H10,2,C'-------  ----------------------'                         
         SSPEC H10,42,C'USER/PRIORTY/DATE'                                      
         SSPEC H10,91,C'WKS CO-ORD    DONE'                                     
         DC    X'00'               END MARKER FOR SPECS                         
         EJECT                                                                  
*                                  LOCAL DSECTS                                 
*                                                                               
OPTTABD  DSECT                                                                  
OPTKWORD DS    CL8                 OPTION KEYWORD                               
OPTVALR  DS    A                   A(OPTION VALIDATION ROUTINE)                 
OPTABLNQ EQU   *-OPTTABD                                                        
         EJECT                                                                  
       ++INCLUDE PEMAPFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE PEMAPF5D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE PEMAPE5D                                                       
         EJECT                                                                  
* DDCOMFACS                                                                     
* DDBIGBOX                                                                      
* DDPERVERTD                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* PEMAPFILE                                                                     
* PEMAPWORKD ********************* MUST BE THE LAST ++INCLUDE                   
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDPERVERTD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE PEMAPFILE                                                      
       ++INCLUDE PEMAPWORKD                                                     
         PRINT ON                                                               
RELO05   DS    A                                                                
ASORTAR  DS    A                                                                
REPMWKS  DS    H                                                                
QSYS     DS    CL(L'PRJSYS)                                                     
QPROJ    DS    CL(L'PRJCODE)                                                    
QUSER    DS    CL(L'PUSER)                                                      
QCOOR    DS    CL(L'PROJCOOR)                                                   
QPERD    DS    CL4                                                              
QDONE    DS    CL1                                                              
QSCHED   DS    CL1                                                              
SORTYPE  DS    CL1                                                              
LASTSORT DS    CL8                                                              
USEROK   DS    CL1                                                              
REPSTDT  DS    CL2                 REPORT START DATE                            
REPNDDT  DS    CL2                 REPORT END DATE                              
USERBLK  DS    CL((15*17)+1)                                                    
*                                                                               
SORTREC  DS    0CL38                                                            
SORTKEY  DS    0CL33                                                            
SORTHIGH DS    CL17                HIGH ORDER SORT KEY FIELD                    
SORTSYS  DS    CL8                 LO ORDER IS ALWAYS SYS/PROJ                  
SORTPROJ DS    CL8                                                              
SORTDUPE DS    CL1                 0=ORIGINAL,1=DUPLICATE                       
SORTADDR DS    CL4                                                              
         SPACE                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013PEMAP05   05/01/02'                                      
         END                                                                    
