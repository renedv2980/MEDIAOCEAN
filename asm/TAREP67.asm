*          DATA SET TAREP67    AT LEVEL 004 AS OF 02/28/14                      
*PHASE T70367A,*                                                                
*INCLUDE DLFLD                                                                  
         TITLE 'T70367 - PMUSIC RECORD ACTIVITY REPORT'                         
T70367   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70302,R6                                                      
         L     RC,0(R1)            RC=A(CONTROLLER W/S)                         
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(SCREEN)                                 
         USING T703FFD,RA                                                       
         L     R9,ASUBSYSD         R9=A(SYSTEM W/S)                             
         USING SUBSYSD,R9                                                       
         L     R8,ASPOOLD          R8=A(SPOOL DSECT)                            
         USING SPOOLD,R8                                                        
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING MYD,R7                                                           
***********************************************************************         
*        MODE CONTROLLED ROUTINES                                     *         
***********************************************************************         
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
*                                                                               
         CLI   MODE,PRINTREP                                                    
         JNE   YES                                                              
         BRAS  RE,PREP             PRINT REPORT                                 
         J     XIT                                                              
*                                                                               
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         LTORG                                                                  
***********************************************************************         
*        PRINT REPORT                                                 *         
***********************************************************************         
PREP     NTR1                                                                   
         USING DLCBD,R5                                                         
         LA    R5,DLCB                                                          
         BAS   RE,INITDOWN                                                      
         BAS   RE,DWNHDR           DOWNLOAD COLUMN HEADERS                      
*                                                                               
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
         OPEN  (RECVIN,(INPUT))    OPEN THE INPUT FILE                          
*                                                                               
         BAS   RE,PROCRCV          PROCESS RECOVERY FILE                        
         BAS   RE,GETSORT          GET RECS FROM SORTER & DOWNLOAD              
*                                                                               
PREPX    BAS   RE,EOLDOWN                                                       
         J     XIT                                                              
         DROP  R5                                                               
***********************************************************************         
*        DOWNLOAD COLUMN HEADERS                                      *         
***********************************************************************         
DWNHDR   NTR1                                                                   
         MVC   WORK(10),=C'Music     '                                          
         GOTO1 OUTPDOWN,DMCB,(C'T',WORK),10                                     
         MVC   WORK(10),=C'Field     '                                          
         GOTO1 OUTPDOWN,DMCB,(C'T',WORK),10                                     
         MVC   WORK(10),=C'Action    '                                          
         GOTO1 OUTPDOWN,DMCB,(C'T',WORK),10                                     
         MVC   WORK(10),=C'Old Value '                                          
         GOTO1 OUTPDOWN,DMCB,(C'T',WORK),10                                     
         MVC   WORK(10),=C'New Value '                                          
         GOTO1 OUTPDOWN,DMCB,(C'T',WORK),10                                     
         MVC   WORK(10),=C'Staff ID  '                                          
         GOTO1 OUTPDOWN,DMCB,(C'T',WORK),10                                     
         MVC   WORK(10),=C'Date      '                                          
         GOTO1 OUTPDOWN,DMCB,(C'T',WORK),10                                     
         MVC   WORK(10),=C'Time      '                                          
         GOTO1 OUTPDOWN,DMCB,(C'T',WORK),10                                     
         BAS   RE,EOLDOWN                                                       
DWNHDRX  J     XIT                                                              
***********************************************************************         
*        READ RECOVERY FILE AND PUT RECORDS TO SORTER                 *         
***********************************************************************         
PROCRCV  NTR1                                                                   
PRCVNXT  LA    R2,RCVREC           R2 = A(INPUT AREA)                           
         GET   RECVIN,(R2)                                                      
         LA    R2,4(R2)            R2 = A(RECOVERY HEADER)                      
         USING RCVD,R2                                                          
         XC    SORTREC,SORTREC                                                  
S        USING SRTD,SORTREC                                                     
*                                                                               
         CLI   RFILTY,X'72'        ONLY INTERESTED IN TALFILE                   
         JNE   PRCVNXT                                                          
*                                                                               
         LA    R4,RDATA            R4 = A(ACTUAL RECORD)                        
         USING TLMUD,R4                                                         
         CLI   TLMUCD,TLMUCDQ      ONLY INTERESTED IN MUSIC RECORDS             
         JNE   PRCVNXT                                                          
*                                                                               
         CLI   RRECTY,X'01'        IF THIS IS A COPY                            
         JE    PRCVCPY                                                          
         CLI   RRECTY,X'02'        IF THIS IS A CHANGE?                         
         JE    PRCVCHG                                                          
         CLI   RRECTY,X'03'        IF THIS IS AN ADD?                           
         JNE   PRCVNXT                                                          
         MVC   S.SRTACT,=C'ADD     '                                            
         J     PRCVADEL                                                         
*                                                                               
PRCVCPY  L     RE,AIO1             PUT COPY VALUES IN AIO1                      
         LA    RF,4000                                                          
         XCEF                                                                   
*                                                                               
         MVC   AIO,AIO1                                                         
         BAS   RE,GETVALS          GET VALUES FROM COPY RECORD                  
         J     PRCVNXT                                                          
*                                                                               
PRCVCHG  LA    R4,RDATA                                                         
         TM    TLMUSTAT,X'80'      RECORD DELETED?                              
         JZ    *+14                                                             
         MVC   S.SRTACT,=C'DELETE  '                                            
         J     PRCVADEL                                                         
*                                                                               
         L     RF,AIO1                                                          
         USING SVVALD,RF                                                        
         TM    SVSTAT,X'80'        RECORD RESTORED?                             
         JZ    *+14                                                             
         MVC   S.SRTACT,=C'RESTORED'                                            
         J     PRCVADEL                                                         
         DROP  RF                                                               
*                                                                               
         L     RE,AIO2             PUT CHANGE VALUES IN AIO2                    
         LA    RF,4000                                                          
         XCEF                                                                   
*                                                                               
         MVC   AIO,AIO2                                                         
         BAS   RE,GETVALS          GET VALUES FROM CHANGE RECORD                
         BAS   RE,COMPARE          COMPARE CHANGE RECORD WITH COPY              
         J     PRCVNXT                                                          
*                                                                               
PRCVADEL LA    R4,RDATA            MUSIC RECORD ADD                             
         USING TLMUD,R4                                                         
*                                                                               
         MVC   S.SRTMUS,TLMUMUS    MUSIC CODE                                   
*                                                                               
         LA    R4,RDATA                                                         
         MVI   ELCODE,TAACELQ      GET ACTIVITY ELEMENT                         
         BRAS  RE,GETEL                                                         
         JNE   PRCVA10                                                          
         USING TAACD,R4                                                         
*                                                                               
         MVC   S.SRTSTAFF,TAACSTAF   STAFF ID                                   
         GOTO1 DATCON,DMCB,(1,TAACCDTE),(8,S.SRTDATE) DATE                      
         GOTO1 TIMECON,DMCB,TAACCTIM,TAACCDTE,(8,S.SRTTIME) TIME                
         DROP  R4,S                                                             
*                                                                               
PRCVA10  GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         J     PRCVNXT                                                          
***********************************************************************         
*        GET RECORDS FROM SORTER AND DOWNLOAD                         *         
***********************************************************************         
GETSORT  NTR1                                                                   
GSORT05  GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   R2,15,4(R1)         R2 = A(SORTED RECORD)                        
         JZ    GETSORTX                                                         
         USING SRTD,R2                                                          
*                                                                               
         GOTO1 OUTPDOWN,DMCB,(C'T',SRTMUS),L'SRTMUS                             
         GOTO1 OUTPDOWN,DMCB,(C'T',SRTFLD),L'SRTFLD                             
         GOTO1 OUTPDOWN,DMCB,(C'T',SRTACT),L'SRTACT                             
         GOTO1 OUTPDOWN,DMCB,(C'T',SRTOVAL),L'SRTOVAL                           
         GOTO1 OUTPDOWN,DMCB,(C'T',SRTNVAL),L'SRTNVAL                           
         GOTO1 OUTPDOWN,DMCB,(C'T',SRTSTAFF),L'SRTSTAFF                         
         GOTO1 OUTPDOWN,DMCB,(C'T',SRTDATE),L'SRTDATE                           
         GOTO1 OUTPDOWN,DMCB,(C'T',SRTTIME),L'SRTTIME                           
         BAS   RE,EOLDOWN                                                       
         J     GSORT05                                                          
         DROP  R2                                                               
*                                                                               
GETSORTX GOTO1 SORTER,DMCB,=C'END'                                              
         J     XIT                                                              
***********************************************************************         
*        GET VALUES FROM RECOVERY RECORD                              *         
*        AIO = AIO1 FOR COPY RECORD                                   *         
*        AIO = AIO2 FOR CHANGE RECORD                                 *         
***********************************************************************         
GETVALS  NTR1                                                                   
         L     R3,AIO              SAVE VALUES INTO AIO AREA                    
         USING SVVALD,R3                                                        
*                                                                               
         LA    R4,RDATA                                                         
         USING TLMUD,R4                                                         
*                                                                               
         MVC   SVMUS,TLMUMUS       MUSIC CODE                                   
         MVC   SVSTAT,TLMUSTAT     STATUS                                       
*                                                                               
         LA    R4,RDATA                                                         
         USING TAPMD,R4                                                         
         MVI   ELCODE,TAPMELQ      LOOK FOR INFO/HISTORY ELEMENT                
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SVCAGY,TAPMCAGY     CURRENT AGENCY                               
         MVC   SVPAGY,TAPMPAGY     PREVIOUS AGENCY                              
*                                                                               
         LA    R4,RDATA                                                         
         USING TANAD,R4                                                         
         MVI   ELCODE,TANAELQ      LOOK FOR COMPOSITION NAME ELEMENT            
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   R1,TANALEN                                                       
         SHI   R1,2                                                             
*                                                                               
         CHI   R1,L'SVNAME                                                      
         JL    GVAL10                                                           
         MVC   SVNAME(L'SVNAME),TANANAME     COMPOSITION NAME 1                 
         SHI   R1,L'SVNAME                                                      
         CHI   R1,0                                                             
         JNH   GVAL20                                                           
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   SVNAME2(0),TANANAME+L'SVNAME  COMPOSITION NAME 2                 
         J     GVAL20                                                           
*                                                                               
GVAL10   SHI   R1,1                                                             
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   SVNAME(0),TANANAME     COMPOSITION NAME                          
*                                                                               
GVAL20   LA    R2,SVAUTH                                                        
         LA    R4,RDATA                                                         
         USING TAMUD,R4                                                         
         MVI   ELCODE,TAMUELQ      MUSIC ELEMENTS                               
         BRAS  RE,GETEL                                                         
         JE    GVAL24                                                           
         DC    H'0'                                                             
GVAL22   BRAS  RE,NEXTEL                                                        
GVAL24   JNE   GVAL30                                                           
*                                                                               
         CLI   TAMUTYPE,TAMUTAUT   AUTHOR?                                      
         JNE   GVAL22                                                           
         ZIC   R1,TAMULEN          ELEMENT LENGTH                               
         SH    R1,=Y(TAMULNQ)      R1 = L'NAME                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R2),TAMUNAME                   AUTHOR                        
         MVC   L'SVAUTH1(L'SVAUTHL1,R2),TAMULIC   LISCENSOR                     
         AHI   R2,L'SVAUTH1+L'SVAUTHL1                                          
         J     GVAL22                                                           
*                                                                               
GVAL30   LA    R2,SVCOMP                                                        
         LA    R4,RDATA                                                         
         USING TAMUD,R4                                                         
         MVI   ELCODE,TAMUELQ      MUSIC ELEMENTS                               
         BRAS  RE,GETEL                                                         
         JE    GVAL34                                                           
         DC    H'0'                                                             
GVAL32   BRAS  RE,NEXTEL                                                        
GVAL34   JNE   GVAL40                                                           
*                                                                               
         CLI   TAMUTYPE,TAMUTCOM   COMPOSER?                                    
         JNE   GVAL32                                                           
         ZIC   R1,TAMULEN          ELEMENT LENGTH                               
         SH    R1,=Y(TAMULNQ)      R1 = L'NAME                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R2),TAMUNAME                   COMPOSER                      
         MVC   L'SVCOMP1(L'SVCOMPL1,R2),TAMULIC   LISCENSOR                     
         AHI   R2,L'SVCOMP1+L'SVCOMPL1                                          
         J     GVAL32                                                           
*                                                                               
GVAL40   LA    R2,SVPUB                                                         
         LA    R4,RDATA                                                         
         USING TAMUD,R4                                                         
         MVI   ELCODE,TAMUELQ      MUSIC ELEMENTS                               
         BRAS  RE,GETEL                                                         
         JE    GVAL44                                                           
         DC    H'0'                                                             
GVAL42   BRAS  RE,NEXTEL                                                        
GVAL44   JNE   GVAL50                                                           
*                                                                               
         CLI   TAMUTYPE,TAMUTPUB   PUBLISHER?                                   
         JNE   GVAL42                                                           
         ZIC   R1,TAMULEN          ELEMENT LENGTH                               
         SH    R1,=Y(TAMULNQ)      R1 = L'NAME                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   0(0,R2),TAMUNAME                 PUBLISHER                       
         MVC   L'SVPUB1(L'SVPUBL1,R2),TAMULIC   LISCENSOR                       
         AHI   R2,L'SVPUB1+L'SVPUBL1                                            
         J     GVAL42                                                           
*                                                                               
GVAL50   LA    R4,RDATA                                                         
         USING TAFND,R4                                                         
         MVI   ELCODE,TAFNELQ      GET CLIENT/PRODUCT                           
         BRAS  RE,GETEL                                                         
         J     *+8                                                              
GVAL52   BRAS  RE,NEXTEL                                                        
         JNE   GVAL60                                                           
         ZIC   R1,TAFNLEN                                                       
         SHI   R1,TAFNLNQ                                                       
         SHI   R1,1                                                             
*                                                                               
         CLI   TAFNTYPE,TAFNTCLI                                                
         JNE   GVAL54                                                           
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   SVCLI(0),TAFNNAME                                                
         J     GVAL52                                                           
*                                                                               
GVAL54   CLI   TAFNTYPE,TAFNTPRD                                                
         JNE   GVAL52                                                           
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   SVPRD(0),TAFNNAME                                                
         J     GVAL52                                                           
*                                                                               
GVAL60   LA    R4,RDATA                                                         
         USING TACMD,R4                                                         
         MVI   ELCODE,TACMELQ      LOOK FOR COMMENT ELEMENT                     
         BRAS  RE,GETEL                                                         
         JNE   GETVALSX                                                         
*                                                                               
         OC    SVCMT1,SPACES                                                    
         OC    SVCMT2,SPACES                                                    
*                                                                               
         ZIC   R1,TACMLEN                                                       
         SHI   R1,TACMLNQ                                                       
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   SVCMT1(0),TACMCOMM                                               
         DROP  R3,R4                                                            
*                                                                               
GETVALSX J     XIT                                                              
***********************************************************************         
*        COMPARE CHANGE RECORD TO COPY                                *         
*        AIO1 = COPY RECORD VALUES                                    *         
*        AIO2 = CHANGE RECORD VALUES                                  *         
***********************************************************************         
COMPARE  NTR1                                                                   
         XC    CURVALS(CURVALNQ),CURVALS                                        
*                                                                               
         LA    R4,RDATA                                                         
         USING TLMUD,R4                                                         
*                                                                               
         MVC   CURMUS,TLMUMUS      MUSIC CODE                                   
*                                                                               
         LA    R4,RDATA                                                         
         MVI   ELCODE,TAACELQ      GET ACTIVITY ELEMENT                         
         BRAS  RE,GETEL                                                         
         JNE   COMP10                                                           
         USING TAACD,R4                                                         
*                                                                               
         MVC   CURSTAFF,TAACSTAF   STAFF ID                                     
         GOTO1 DATCON,DMCB,(1,TAACCDTE),(8,CURDATE) DATE                        
         GOTO1 TIMECON,DMCB,TAACCTIM,TAACCDTE,(8,CURTIME) TIME                  
*                                                                               
COMP10   L     R2,AIO1             COPY RECORD VALUES                           
         USING SVVALD,R2                                                        
         L     R3,AIO2             CHANGE RECORD VALUES                         
         AHI   R3,L'SVMUS                                                       
*                                                                               
         CLC   SVCAGY,0(R3)        CURRENT AGY?                                 
         JE    CMPPAGY                                                          
         MVC   CUROVAL,SPACES                                                   
         MVC   CURNVAL,SPACES                                                   
         MVC   CURFLD,=C'CAGY'                                                  
         MVC   CUROVAL(L'SVCAGY),SVCAGY                                         
         MVC   CURNVAL(L'SVCAGY),0(R3)                                          
         BAS   RE,PUTSORT                                                       
*                                                                               
CMPPAGY  AHI   R3,L'SVCAGY                                                      
         CLC   SVPAGY,0(R3)        PREVIOUS AGY?                                
         JE    CMPNAME                                                          
         MVC   CUROVAL,SPACES                                                   
         MVC   CURNVAL,SPACES                                                   
         MVC   CURFLD,=C'PAGY'                                                  
         MVC   CUROVAL(L'SVPAGY),SVPAGY                                         
         MVC   CURNVAL(L'SVPAGY),0(R3)                                          
         BAS   RE,PUTSORT                                                       
*                                                                               
CMPNAME  AHI   R3,L'SVPAGY                                                      
         CLC   SVNAME(L'SVNAME+L'SVNAME2),0(R3)  COMPOSITION NAME?              
         JE    CMPCLI                                                           
         MVC   CUROVAL,SPACES                                                   
         MVC   CURNVAL,SPACES                                                   
         MVC   CURFLD,=C'COMP'                                                  
         MVC   CUROVAL(L'SVNAME+L'SVNAME2),SVNAME                               
         MVC   CURNVAL(L'SVNAME+L'SVNAME2),0(R3)                                
         BAS   RE,PUTSORT                                                       
*                                                                               
CMPCLI   AHI   R3,L'SVNAME+L'SVNAME2                                            
         CLC   SVCLI,0(R3)         CLIENT?                                      
         JE    CMPPRD                                                           
         MVC   CUROVAL,SPACES                                                   
         MVC   CURNVAL,SPACES                                                   
         MVC   CURFLD,=C'CLI '                                                  
         MVC   CUROVAL(L'SVCLI),SVCLI                                           
         MVC   CURNVAL(L'SVCLI),0(R3)                                           
         BAS   RE,PUTSORT                                                       
*                                                                               
CMPPRD   AHI   R3,L'SVCLI                                                       
         CLC   SVPRD,0(R3)         PRODUCT?                                     
         JE    CMPCMT1                                                          
         MVC   CUROVAL,SPACES                                                   
         MVC   CURNVAL,SPACES                                                   
         MVC   CURFLD,=C'PRD '                                                  
         MVC   CUROVAL(L'SVPRD),SVPRD                                           
         MVC   CURNVAL(L'SVPRD),0(R3)                                           
         BAS   RE,PUTSORT                                                       
*                                                                               
CMPCMT1  AHI   R3,L'SVPRD                                                       
         CLC   SVCMT1,0(R3)        COMMENT 1?                                   
         JE    CMPCMT2                                                          
         MVC   CUROVAL,SPACES                                                   
         MVC   CURNVAL,SPACES                                                   
         MVC   CURFLD,=C'COM1'                                                  
         MVC   CUROVAL(L'SVCMT1),SVCMT1                                         
         MVC   CURNVAL(L'SVCMT1),0(R3)                                          
         BAS   RE,PUTSORT                                                       
*                                                                               
CMPCMT2  AHI   R3,L'SVCMT1                                                      
         CLC   SVCMT2,0(R3)        COMMENT 2?                                   
         JE    CMPAUT1                                                          
         MVC   CUROVAL,SPACES                                                   
         MVC   CURNVAL,SPACES                                                   
         MVC   CURFLD,=C'COM2'                                                  
         MVC   CUROVAL(L'SVCMT2),SVCMT2                                         
         MVC   CURNVAL(L'SVCMT2),0(R3)                                          
         BAS   RE,PUTSORT                                                       
*                                                                               
CMPAUT1  AHI   R3,L'SVCMT2+L'SVSTAT                                             
         CLC   SVAUTH1,0(R3)       AUTHOR 1?                                    
         JE    CMPAUTL1                                                         
         MVC   CUROVAL,SPACES                                                   
         MVC   CURNVAL,SPACES                                                   
         MVC   CURFLD,=C'A1  '                                                  
         MVC   CUROVAL(L'SVAUTH1),SVAUTH1                                       
         MVC   CURNVAL(L'SVAUTH1),0(R3)                                         
         BAS   RE,PUTSORT                                                       
*                                                                               
CMPAUTL1 AHI   R3,L'SVAUTH1                                                     
         CLC   SVAUTHL1,0(R3)      AUTHOR LISCENSOR 1?                          
         JE    CMPAUT2                                                          
         MVC   CUROVAL,SPACES                                                   
         MVC   CURNVAL,SPACES                                                   
         MVC   CURFLD,=C'A1L '                                                  
         MVC   CUROVAL(L'SVAUTHL1),SVAUTHL1                                     
         MVC   CURNVAL(L'SVAUTHL1),0(R3)                                        
         BAS   RE,PUTSORT                                                       
*                                                                               
CMPAUT2  AHI   R3,L'SVAUTHL1                                                    
         CLC   SVAUTH2,0(R3)       AUTHOR 2?                                    
         JE    CMPAUTL2                                                         
         MVC   CUROVAL,SPACES                                                   
         MVC   CURNVAL,SPACES                                                   
         MVC   CURFLD,=C'A2  '                                                  
         MVC   CUROVAL(L'SVAUTH2),SVAUTH2                                       
         MVC   CURNVAL(L'SVAUTH2),0(R3)                                         
         BAS   RE,PUTSORT                                                       
*                                                                               
CMPAUTL2 AHI   R3,L'SVAUTH2                                                     
         CLC   SVAUTHL2,0(R3)      AUTHOR LISCENSOR 2?                          
         JE    CMPCOM1                                                          
         MVC   CUROVAL,SPACES                                                   
         MVC   CURNVAL,SPACES                                                   
         MVC   CURFLD,=C'A2L '                                                  
         MVC   CUROVAL(L'SVAUTHL2),SVAUTHL2                                     
         MVC   CURNVAL(L'SVAUTHL2),0(R3)                                        
         BAS   RE,PUTSORT                                                       
*                                                                               
CMPCOM1  AHI   R3,L'SVAUTHL2                                                    
         CLC   SVCOMP1,0(R3)       COMPOSER 1?                                  
         JE    CMPCOML1                                                         
         MVC   CUROVAL,SPACES                                                   
         MVC   CURNVAL,SPACES                                                   
         MVC   CURFLD,=C'C1  '                                                  
         MVC   CUROVAL(L'SVCOMP1),SVCOMP1                                       
         MVC   CURNVAL(L'SVCOMP1),0(R3)                                         
         BAS   RE,PUTSORT                                                       
*                                                                               
CMPCOML1 AHI   R3,L'SVCOMP1                                                     
         CLC   SVCOMPL1,0(R3)      COMPOSER LISCENSOR 1?                        
         JE    CMPCOM2                                                          
         MVC   CUROVAL,SPACES                                                   
         MVC   CURNVAL,SPACES                                                   
         MVC   CURFLD,=C'C1L '                                                  
         MVC   CUROVAL(L'SVCOMPL1),SVCOMPL1                                     
         MVC   CURNVAL(L'SVCOMPL1),0(R3)                                        
         BAS   RE,PUTSORT                                                       
*                                                                               
CMPCOM2  AHI   R3,L'SVCOMPL1                                                    
         CLC   SVCOMP2,0(R3)       COMPOSER 2?                                  
         JE    CMPCOML2                                                         
         MVC   CUROVAL,SPACES                                                   
         MVC   CURNVAL,SPACES                                                   
         MVC   CURFLD,=C'C2  '                                                  
         MVC   CUROVAL(L'SVCOMP2),SVCOMP2                                       
         MVC   CURNVAL(L'SVCOMP2),0(R3)                                         
         BAS   RE,PUTSORT                                                       
*                                                                               
CMPCOML2 AHI   R3,L'SVCOMP2                                                     
         CLC   SVCOMPL2,0(R3)      COMPOSER LISCENSOR 2?                        
         JE    CMPCOM3                                                          
         MVC   CUROVAL,SPACES                                                   
         MVC   CURNVAL,SPACES                                                   
         MVC   CURFLD,=C'C2L '                                                  
         MVC   CUROVAL(L'SVCOMPL2),SVCOMPL2                                     
         MVC   CURNVAL(L'SVCOMPL2),0(R3)                                        
         BAS   RE,PUTSORT                                                       
*                                                                               
CMPCOM3  AHI   R3,L'SVCOMPL2                                                    
         CLC   SVCOMP3,0(R3)       COMPOSER 3?                                  
         JE    CMPCOML3                                                         
         MVC   CUROVAL,SPACES                                                   
         MVC   CURNVAL,SPACES                                                   
         MVC   CURFLD,=C'C3  '                                                  
         MVC   CUROVAL(L'SVCOMP3),SVCOMP3                                       
         MVC   CURNVAL(L'SVCOMP3),0(R3)                                         
         BAS   RE,PUTSORT                                                       
*                                                                               
CMPCOML3 AHI   R3,L'SVCOMP3                                                     
         CLC   SVCOMPL3,0(R3)      COMPOSER LISCENSOR 3?                        
         JE    CMPCOM4                                                          
         MVC   CUROVAL,SPACES                                                   
         MVC   CURNVAL,SPACES                                                   
         MVC   CURFLD,=C'C3L '                                                  
         MVC   CUROVAL(L'SVCOMPL3),SVCOMPL3                                     
         MVC   CURNVAL(L'SVCOMPL3),0(R3)                                        
         BAS   RE,PUTSORT                                                       
*                                                                               
CMPCOM4  AHI   R3,L'SVCOMPL3                                                    
         CLC   SVCOMP4,0(R3)       COMPOSER 4?                                  
         JE    CMPCOML4                                                         
         MVC   CUROVAL,SPACES                                                   
         MVC   CURNVAL,SPACES                                                   
         MVC   CURFLD,=C'C4  '                                                  
         MVC   CUROVAL(L'SVCOMP4),SVCOMP4                                       
         MVC   CURNVAL(L'SVCOMP4),0(R3)                                         
         BAS   RE,PUTSORT                                                       
*                                                                               
CMPCOML4 AHI   R3,L'SVCOMP4                                                     
         CLC   SVCOMPL4,0(R3)      COMPOSER LISCENSOR 4?                        
         JE    CMPPUB1                                                          
         MVC   CUROVAL,SPACES                                                   
         MVC   CURNVAL,SPACES                                                   
         MVC   CURFLD,=C'C4L '                                                  
         MVC   CUROVAL(L'SVCOMPL4),SVCOMPL4                                     
         MVC   CURNVAL(L'SVCOMPL4),0(R3)                                        
         BAS   RE,PUTSORT                                                       
*                                                                               
CMPPUB1  AHI   R3,L'SVCOMPL4                                                    
         CLC   SVPUB1,0(R3)        PUBLISHER 1?                                 
         JE    CMPPUBL1                                                         
         MVC   CUROVAL,SPACES                                                   
         MVC   CURNVAL,SPACES                                                   
         MVC   CURFLD,=C'P1  '                                                  
         MVC   CUROVAL(L'SVPUB1),SVPUB1                                         
         MVC   CURNVAL(L'SVPUB1),0(R3)                                          
         BAS   RE,PUTSORT                                                       
*                                                                               
CMPPUBL1 AHI   R3,L'SVPUB1                                                      
         CLC   SVPUBL1,0(R3)       PUBLISTHER LISCENSOR 1?                      
         JE    CMPPUB2                                                          
         MVC   CUROVAL,SPACES                                                   
         MVC   CURNVAL,SPACES                                                   
         MVC   CURFLD,=C'P1L '                                                  
         MVC   CUROVAL(L'SVPUBL1),SVPUBL1                                       
         MVC   CURNVAL(L'SVPUBL1),0(R3)                                         
         BAS   RE,PUTSORT                                                       
*                                                                               
CMPPUB2  AHI   R3,L'SVPUBL1                                                     
         CLC   SVPUB2,0(R3)        PUBLISHER 2?                                 
         JE    CMPPUBL2                                                         
         MVC   CUROVAL,SPACES                                                   
         MVC   CURNVAL,SPACES                                                   
         MVC   CURFLD,=C'P2  '                                                  
         MVC   CUROVAL(L'SVPUB2),SVPUB2                                         
         MVC   CURNVAL(L'SVPUB2),0(R3)                                          
         BAS   RE,PUTSORT                                                       
*                                                                               
CMPPUBL2 AHI   R3,L'SVPUB2                                                      
         CLC   SVPUBL2,0(R3)       PUBLISTHER LISCENSOR 2?                      
         JE    CMPPUB3                                                          
         MVC   CUROVAL,SPACES                                                   
         MVC   CURNVAL,SPACES                                                   
         MVC   CURFLD,=C'P2L '                                                  
         MVC   CUROVAL(L'SVPUBL2),SVPUBL2                                       
         MVC   CURNVAL(L'SVPUBL2),0(R3)                                         
         BAS   RE,PUTSORT                                                       
*                                                                               
CMPPUB3  AHI   R3,L'SVPUBL2                                                     
         CLC   SVPUB3,0(R3)        PUBLISTHER 3?                                
         JE    CMPPUBL3                                                         
         MVC   CUROVAL,SPACES                                                   
         MVC   CURNVAL,SPACES                                                   
         MVC   CURFLD,=C'P3  '                                                  
         MVC   CUROVAL(L'SVPUB3),SVPUB3                                         
         MVC   CURNVAL(L'SVPUB3),0(R3)                                          
         BAS   RE,PUTSORT                                                       
*                                                                               
CMPPUBL3 AHI   R3,L'SVPUB3                                                      
         CLC   SVPUBL3,0(R3)       PUBLISTHER LISCENSOR 3?                      
         JE    COMPAREX                                                         
         MVC   CUROVAL,SPACES                                                   
         MVC   CURNVAL,SPACES                                                   
         MVC   CURFLD,=C'P3L '                                                  
         MVC   CUROVAL(L'SVPUBL3),SVPUBL3                                       
         MVC   CURNVAL(L'SVPUBL3),0(R3)                                         
         BAS   RE,PUTSORT                                                       
         DROP  R2                                                               
*                                                                               
COMPAREX J     XIT                                                              
***********************************************************************         
*        PUT VALUES TO SORTER                                         *         
***********************************************************************         
PUTSORT  NTR1                                                                   
         OC    CURFLD,SPACES                                                    
         OC    CURACT,SPACES                                                    
         OC    CUROVAL,SPACES                                                   
         OC    CURNVAL,SPACES                                                   
*                                                                               
         LA    R2,SORTREC                                                       
         USING SRTD,R2                                                          
*                                                                               
         MVC   SRTMUS,CURMUS       MUSIC CODE                                   
         MVC   SRTDATE,CURDATE     DATE                                         
         MVC   SRTTIME,CURTIME     TIME                                         
         MVC   SRTFLD,CURFLD       FIELD THAT CHANGED                           
         MVC   SRTACT,=C'CHANGE  '                                              
         MVC   SRTOVAL,CUROVAL     ORIGINAL VALUE                               
         MVC   SRTNVAL,CURNVAL     NEW VALUE                                    
         MVC   SRTSTAFF,CURSTAFF   STAFF                                        
         DROP  R2                                                               
*                                                                               
         GOTO1 SORTER,DMCB,=C'PUT',SORTREC                                      
         J     XIT                                                              
***********************************************************************         
*        INITIALIZE DOWNLOAD SETTINGS                                 *         
*        ON ENTRY ... R5=A(DOWNLOAD BLOCK)                            *         
***********************************************************************         
         USING DLCBD,R5                                                         
INITDOWN NTR1                                                                   
         XC    DLCBD(DLCBXLX),DLCBD                                             
         MVI   DLCBACT,DLCBINIT    INITIALIZE FOR DOWNLOAD                      
         LA    R1,PRTDOWN          A(HOOK ROUTINE FOR PRINTING)                 
         ST    R1,DLCBAPR                                                       
         LA    R1,P                A(PRINT LINE)                                
         MVC   P,SPACES                                                         
         ST    R1,DLCBAPL                                                       
         MVC   DLCBAED,EDITOR                                                   
         MVC   DLCXMAXL,=Y(L'P)    MAXIMUM LENGTH OF PRINT LINE                 
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      TEXT DELIMITER ALTERNATE                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON FOR END OF LINE                   
         MVI   DLCXEORC,C':'       END OF REPORT                                
         OI    DLCBFLG1,DLCBFXTN                                                
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
         DROP  R5                                                               
***********************************************************************         
*        USER SUPPLIED PRINT ROUTINE                                  *         
***********************************************************************         
PRTDOWN  NTR1                                                                   
         MVI   LINE,1              PREVENT PAGE BREAK                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         J     XIT                                                              
***********************************************************************         
*        OUTPUT DOWNLOAD ENTRY                                        *         
*        ON ENTRY ... P1, B0    = TYPE TO PASS                        *         
*                         B1-B3 = ADDRESS OF DATA                     *         
*                     P2        = LENGTH                              *         
*                     R5=A(DOWNLOAD BLOCK)                            *         
***********************************************************************         
         USING DLCBD,R5                                                         
OUTPDOWN NTR1                                                                   
         MVI   DLCBACT,DLCBPUT                                                  
         MVC   DLCBTYP,0(R1)                                                    
         OI    DLCBFLG1,DLCBFXFL                                                
*                                                                               
         L     RF,0(R1)            RF=A(DOWNLOAD FIELD)                         
         L     RE,4(R1)            RE=L'DOWNLOAD FIELD                          
*                                                                               
         LR    R1,RF                                                            
         AR    R1,RE                                                            
OPD10    SHI   R1,1                                                             
         CLI   0(R1),C' '                                                       
         JNE   OPD20                                                            
         BCT   RE,OPD10                                                         
         LHI   RE,1                                                             
OPD20    STC   RE,DLCBLEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         MVC   DLCBFLX(0),0(RF)                                                 
         GOTO1 =V(DLFLD),DLCBD                                                  
         J     XIT                                                              
         DROP  R5                                                               
***********************************************************************         
*        FINISH LINE OF DOWNLOAD OUPUT                                *         
*        ON ENTRY ... R5=A(DOWNLOAD BLOCK)                            *         
***********************************************************************         
         USING DLCBD,R5                                                         
EOLDOWN  NTR1                                                                   
         MVI   DLCBACT,DLCBEOL      END OF LINE                                 
         GOTO1 =V(DLFLD),DLCBD      (DON'T USE RF, BASR RE,RF BELOW)            
         J     XIT                                                              
         DROP  R5                                                               
***********************************************************************         
*        END DOWNLOAD                                                 *         
*        ON ENTRY ... R5=A(DOWNLOAD BLOCK)                            *         
***********************************************************************         
         USING DLCBD,R5                                                         
ENDDOWN  NTR1                                                                   
         MVI   DLCBACT,DLCBEOR      END OF REPORT                               
         GOTO1 =V(DLFLD),DLCBD      LAST FOR REPORT                             
         J     XIT                                                              
         DROP  R5                                                               
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
         LTORG                                                                  
***********************************************************************         
*        SORTER CARDS                                                 *         
***********************************************************************         
RECVIN   DCB   DDNAME=RECVIN,DSORG=PS,MACRF=GM,EODAD=XIT                        
SORTCARD DC    CL80'SORT FIELDS=(1,28,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=200'                                   
***********************************************************************         
*        WORKING STORAGE                                                        
***********************************************************************         
MYD      DSECT                                                                  
SORTREC  DS    XL200                                                            
DLCB     DS    CL(DLCBXLX)         DOWNLOAD BLOCK                               
*                                                                               
CURVALS  DS    0X                                                               
CURMUS   DS    CL8                                                              
CURDATE  DS    CL8                                                              
CURTIME  DS    CL8                                                              
CURFLD   DS    CL4                                                              
CURACT   DS    CL8                                                              
CUROVAL  DS    CL72                                                             
CURNVAL  DS    CL72                                                             
CURSTAFF DS    CL8                                                              
CURVALNQ EQU   *-CURVALS                                                        
*                                                                               
RCVREC   DS    0H                  RECOVERY RECORD                              
RLEN     DS    H                                                                
         DS    H                                                                
RHEAD    DS    XL24                                                             
RDATA    DS    2500C                                                            
*                                                                               
SRTD     DSECT                                                                  
SRTMUS   DS    CL8                 MUSIC                                        
SRTDATE  DS    CL8                 DATE                                         
SRTTIME  DS    CL8                 TIME                                         
SRTFLD   DS    CL4                 FIELD                                        
SRTKEYQ  EQU   *-SRTD                                                           
SRTACT   DS    CL8                 ACTION                                       
SRTOVAL  DS    CL72                OLD VALUE                                    
SRTNVAL  DS    CL72                NEW VALUE                                    
SRTSTAFF DS    CL8                 STAFF ID                                     
SRTDLQ   EQU   *-SRTD                                                           
*                                                                               
* MUSIC FIELDS FROM RECORD                                                      
*                                                                               
* IF A FIELD IS ADDED IN SVVALD, MAKE SURE BUMP IS TAKEN INTO                   
* ACCOUNT IN "COMPARE" ROUTINE                                                  
*                                                                               
SVVALD   DSECT                                                                  
SVVALS   DS    0C                                                               
SVMUS    DS    CL(L'TLMUMUS)       MUSIC CODE                                   
SVCAGY   DS    CL(L'TLMUAGY)       CURRENT AGY                                  
SVPAGY   DS    CL(L'TLMUAGY)       PREVIOUS AGY                                 
SVNAME   DS    CL36                COMPOSITION NAME                             
SVNAME2  DS    CL36                COMPOSITION NAME                             
SVCLI    DS    CL36                CLIENT                                       
SVPRD    DS    CL36                PRODUCT                                      
SVCMT1   DS    CL60                COMMENT 1                                    
SVCMT2   DS    CL60                COMMENT 2                                    
SVSTAT   DS    XL1                 STATUS                                       
SVVALSLQ EQU   *-SVVALS                                                         
*                                                                               
SVAUTH   DS    0C                                                               
SVAUTH1  DS    CL36                AUTHOR 1                                     
SVAUTHL1 DS    CL1                 AUTHOR LISCENSOR 1                           
SVAUTH2  DS    CL36                AUTHOR 2                                     
SVAUTHL2 DS    CL1                 AUTHOR LISCENSOR 2                           
SVAUTHLQ EQU   *-SVAUTH                                                         
*                                                                               
SVCOMP   DS    0C                                                               
SVCOMP1  DS    CL36                COMPOSER 1                                   
SVCOMPL1 DS    CL1                 COMPOSER LISCENSOR 1                         
SVCOMP2  DS    CL36                COMPOSER 2                                   
SVCOMPL2 DS    CL1                 COMPOSER LISCENSOR 2                         
SVCOMP3  DS    CL36                COMPOSER 3                                   
SVCOMPL3 DS    CL1                 COMPOSER LISCENSOR 3                         
SVCOMP4  DS    CL36                COMPOSER 4                                   
SVCOMPL4 DS    CL1                 COMPOSER LISCENSOR 4                         
SVCOMPLQ EQU   *-SVCOMP                                                         
*                                                                               
SVPUB    DS    0C                                                               
SVPUB1   DS    CL36                PUBLISHER 1                                  
SVPUBL1  DS    CL1                 PUBLISHER LISCENSOR 1                        
SVPUB2   DS    CL36                PUBLISHER 2                                  
SVPUBL2  DS    CL1                 PUBLISHER LISCENSOR 2                        
SVPUB3   DS    CL36                PUBLISHER 3                                  
SVPUBL3  DS    CL1                 PUBLISHER LISCENSOR 3                        
SVPUBLQ  EQU   *-SVPUB                                                          
*                                                                               
RCVD     DSECT                                                                  
       ++INCLUDE DMRCVRHDR                                                      
RCVEXTD  DSECT                                                                  
       ++INCLUDE DMRCVREXT                                                      
*                                                                               
       ++INCLUDE TAREPFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPEAD                                                       
*DDGENTWA  (MUST FOLLOW LAST SCREEN)                                            
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*TAGENFILE                                                                      
*DDPERVALD                                                                      
*TASYSDSECT                                                                     
*TASYSEQUS                                                                      
*DDDLCB                                                                         
*DDTWADCONS                                                                     
*TAREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDDLCB                                                         
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004TAREP67   02/28/14'                                      
         END                                                                    
