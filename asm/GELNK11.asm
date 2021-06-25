*          DATA SET GELNK11    AT LEVEL 002 AS OF 11/29/12                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 041173.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE TA0611A                                                                  
GELNK11  TITLE '- CFM RECORD MAINTENANCE'                                       
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=U,CODE=ENTRY,RLEN=100,REQUEST=*,WORKERKEY=CTFM,    *        
               LINKIO=Y,BLOCKS=(B#SAVED,SAVED),SYSTEM=CTLSYSQ                   
         EJECT                                                                  
ENTRY    NMOD1 0,**GL11**,RR=RE                                                 
         USING LP_D,R1             R1=A(LP_D)                                   
         L     R7,LP_ARUNP                                                      
         USING RUNPARMD,R7         R7=A(RUNPARMS)                               
         SR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
         BASR  R5,0                                                             
         AHI   R5,GLOBALS-*                                                     
         USING GLOBALS,R5          R5=A(GLOBAL LITERALS)                        
         L     R9,LP_ABLK1                                                      
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         L     R8,LP_ABLK2                                                      
         USING SAVED,R8            R8=A(SAVE W/S)                               
         L     RA,ATWA                                                          
         USING TWAD,RA             RA=A(TWA)                                    
                                                                                
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
                                                                                
         CLI   RUNPMODE,RINIREQQ   TEST 'INITIALIZE' MODE                       
         BE    INIT                                                             
         CLI   RUNPMODE,RRUNREQQ   TEST 'RUN REQUEST' MODE                      
         BE    INPUT                                                            
         CLI   RUNPMODE,RRUNENDQ   TEST 'LAST TIME' MODE                        
         JNE   EXITY                                                            
         GOTOR GOIO                                                             
         J     EXITY                                                            
                                                                                
INIT     LA    R0,SAVED            CLEAR SAVE STORAGE                           
         LHI   R1,SAVEL                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         GOTOR VDATCON,DMCB,(5,0),(2,TODAYC)                                    
                                                                                
         L     R0,AIO4                                                          
         LHI   R1,IOLENQ                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               CLEAR I/O AREA 4 (RECORD ID TABLE)           
                                                                                
         L     R1,ALP              RESTORE A(LP_D)                              
         MVC   ALIOB,LP_ALIOB      EXTRACT A(LIOB) FROM LP_D                    
         L     RF,LP_ACOM          EXTRACT A(LINKIO) FROM COMFACS               
         MVC   LINKIO,CLINKIO-COMFACSD(RF)                                      
                                                                                
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS AN UPLOAD RECORD                                            *         
***********************************************************************         
                                                                                
INPUT    BASR  RF,0                                                             
         AHI   RF,RECTAB-*                                                      
         USING RECTABD,RF                                                       
         LHI   R0,RECTABN                                                       
         BASR  RE,0                                                             
         CLC   RECTMAP#,LP_QMAPN   LOOK UP RECORD MAP CODE IN TABLE             
         BE    *+12                                                             
         AHI   RF,RECTABL                                                       
         BCTR  R0,RE                                                            
         DC    H'0'                                                             
         MVC   RECTYPE,RECTTYPE    SET RECORD TYPE                              
                                                                                
         SR    R0,R0               BUILD DOWNLOAD MAP ELEMENT                   
         ICM   R0,3,LP_QMAPN                                                    
         AHI   R0,1                                                             
         DROP  R1                                                               
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTMAP',(R0))                   
                                                                                
         GOTOR UPDREC              PROCESS THE INPUT RECORD                     
                                                                                
         J     EXITY               EXIT BACK TO DDLINK                          
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* GO TO UPLOAD RECORD HANDLING ROUTINE                                *         
***********************************************************************         
                                                                                
UPDREC   NTR1  BASE=*,LABEL=*                                                   
         SR    RF,RF                                                            
         IC    RF,RECTYPE                                                       
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         CHI   RF,UPDTABL                                                       
         BL    *+6                                                              
         DC    H'0'                                                             
         B     UPDTAB(RF)                                                       
                                                                                
UPDTAB   DS    0XL4                                                             
         J     UPDLNM              MAINTAIN LEVEL NAMES                         
         J     UPDSTR              MAINTAIN STRUCTURE RECORDS                   
         J     UPDMED              MAINTAIN MEDIA POINTER RECORDS               
         J     UPDCLT              MAINTAIN CLIENT POINTERS                     
         J     UPDBRD              MAINTAIN BRAND POINTERS                      
         J     UPDVEN              MAINTAIN VENDOR POINTERS                     
UPDTABL  EQU   *-UPDTAB                                                         
                                                                                
UPDRECY  J     EXITY                                                            
                                                                                
UPDRECN  XC    PENDVALS(PENDVALL),PENDVALS                                      
         J     EXITN                                                            
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* MAINTAIN LEVEL NAMES ON AGENCY HEADER RECORD                        *         
***********************************************************************         
                                                                                
UPDLNM   BASE  ,                                                                
         LA    R2,KEYWORK                                                       
         USING CFMRECD,R2                                                       
         XC    CFMKEY,CFMKEY                                                    
         MVI   CFMKTYPE,CFMKTYPQ                                                
         MVC   CFMKAGY,TWAAGY                                                   
         MVI   CFMKSUBR,CFMKSHDR                                                
         LHI   R0,1                                                             
         STCM  R0,15,CFMKRNOD                                                   
         CLC   CFMKEY,PENDKEY      TEST RECORD INITIALIZED                      
         BE    UPDLNM04                                                         
                                                                                
         GOTOR GOIO                FLUSH ANY PENDING I/O                        
                                                                                
         MVC   IOKEY(L'CFMKEY),CFMKEY                                           
         GOTOR (#IOEXEC,AIOEXEC),'IORDUPD+IODIR+IO1'                            
         BE    UPDLNM02            RECORD FOUND                                 
         BH    *+16                                                             
         GOTOR PUTERR,CE#AHRNF                                                  
         J     EXITN                                                            
         TM    IOERR,IOERNF        HEADER RECORD CANNOT BE DELETED              
         BNZ   *+16                                                             
         GOTOR PUTERR,CE#AHRNF                                                  
         J     EXITN                                                            
                                                                                
         MVC   PENDKEY,CFMKEY                                                   
         MVC   IOKEY(L'CFMKEY),CFMKEY                                           
         GOTOR BLDVIR              BUILD VIRGIN RECORD IN I/O AREA 1            
         B     UPDLNM04                                                         
                                                                                
UPDLNM02 GOTOR GETREC              READ HEADER RECORD                           
                                                                                
UPDLNM04 L     R2,AIO1                                                          
         USING CFMRECD,R2          R2=A(CFM RECORD)                             
         CLI   RECORD,0            TEST WE HAVE RECORD CODE                     
         BNE   *+16                                                             
         GOTOR PUTERR,CE#INVR#                                                  
         J     EXITN                                                            
                                                                                
         CLI   ACTION,ACTADDQ      TEST ADDING NEW LEVEL NAME                   
         BNE   UPDLNM06                                                         
         GOTOR VHELLO,DMCB,(C'G',GENFIL),('LNMELQ',CFMRECD),           *        
               (L'LNMREC,RECORD)                                                
         CLI   12(R1),6            TEST THIS ELEMENT DOESN'T EXIST              
         BE    UPDLNM08                                                         
         GOTOR PUTERR,CE#RECAE                                                  
         J     EXITN                                                            
                                                                                
UPDLNM06 GOTOR VHELLO,DMCB,(C'D',GENFIL),('LNMELQ',CFMRECD),           *        
               (L'LNMREC,RECORD)                                                
         CLI   12(R1),0                                                         
         BE    UPDLNM08                                                         
         GOTOR PUTERR,CE#RECNF                                                  
         J     EXITN                                                            
                                                                                
UPDLNM08 CLI   ACTION,ACTDELQ      TEST DELETING LEVEL NAME                     
         BE    UPDLNMX                                                          
                                                                                
         LA    R3,ELEM             BUILD AND ADD NEW ELEMENT                    
         USING LNMD,R3                                                          
         MVI   LNMEL,LNMELQ                                                     
         MVC   LNMREC,RECORD                                                    
         MVC   LNMNAME(L'RNAME),RNAME                                           
         XC    RNAME,RNAME                                                      
         LA    R1,LNMNAME+L'RNAME-1                                             
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         AHI   R1,1                                                             
         SR    R1,R3                                                            
         CHI   R1,LNMLNQ                                                        
         BH    *+8                                                              
         LHI   R1,LNMLNQ                                                        
         STC   R1,LNMLN                                                         
         GOTOR VHELLO,DMCB,(C'P',GENFIL),CFMRECD,LNMD,0                         
         CLI   12(R1),0                                                         
         BE    UPDLNMX                                                          
         DC    H'0'                                                             
                                                                                
UPDLNMX  J     UPDRECY                                                          
         DROP  R2,R3,RB                                                         
         EJECT                                                                  
***********************************************************************         
* MAINTAIN STRUCTURE RECORDS                                          *         
***********************************************************************         
                                                                                
UPDSTR   BASE  ,                                                                
         GOTOR GOIO                FLUSH ANY PENDING I/O                        
                                                                                
         CLI   ACTION,ACTMOVQ      TEST MOVING RECORD TO NEW PARENT             
         BNE   UPDSTR02                                                         
                                                                                
         OC    PNODE,PNODE         TEST WE HAVE NEW PARENT NODE                 
         BNZ   *+16                                                             
         GOTOR PUTERR,CE#NNRAM                                                  
         J     UPDRECN                                                          
                                                                                
CHKNEW   USING CFMPAS,IOKEY        CHECK NEW PARENT EXISTS                      
         XC    CHKNEW.CFMPAS,CHKNEW.CFMPAS                                      
         MVI   CHKNEW.CFMPTYPE,CFMPTYPQ                                         
         MVC   CHKNEW.CFMPAGY,TWAAGY                                            
         MVC   CHKNEW.CFMPRNOD,PNODE                                            
         XC    CHKNEW.CFMPRNOD,EFFS                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   CHKNEW.CFMPAS(CFMPPNOD-CFMKEY),IOKEYSAV                          
         BE    *+16                                                             
         GOTOR PUTERR,CE#NPNNF                                                  
         J     UPDRECN                                                          
         DROP  CHKNEW                                                           
                                                                                
OLDPAS   USING CFMPAS,IOKEY        DELETE OLD PARENT PASSIVE                    
         XC    OLDPAS.CFMPAS,OLDPAS.CFMPAS                                      
         MVI   OLDPAS.CFMPTYPE,CFMPTYPQ                                         
         MVC   OLDPAS.CFMPAGY,TWAAGY                                            
         OC    RNODE,RNODE         TEST WE HAVE CURRENT NODE                    
         BNZ   *+16                                                             
         GOTOR PUTERR,CE#INVRN                                                  
         J     UPDRECN                                                          
         MVC   OLDPAS.CFMPRNOD,RNODE                                            
         XC    OLDPAS.CFMPRNOD,EFFS                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUP+IODIR+IO1'                             
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   OLDPAS.CFMPAS(CFMPPNOD-CFMKEY),IOKEYSAV                          
         BE    *+16                                                             
         GOTOR PUTERR,CE#NPPNF                                                  
         J     UPDRECN                                                          
         CLC   OLDPAS.CFMPPNOD,PNODE                                            
         BNE   *+16                                                             
         GOTOR PUTERR,CE#NPNSO                                                  
         J     UPDRECN                                                          
         CLC   PNODE,RNODE         ENSURE NOT POINTING TO SELF                  
         BNE   *+16                                                             
         GOTOR PUTERR,CE#INVPA                                                  
         J     UPDRECN                                                          
                                                                                
         GOTOR GETREC              READ EXISTING RECORD                         
         L     R2,AIO1             AND POINT TO IT                              
         USING CFMRECD,R2                                                       
         CLC   CFMKSUBR,RECORD     ENSURE WE HAVE A GOOD RECORD                 
         BE    *+16                                                             
         GOTOR PUTERR,CE#INVRE                                                  
         J     UPDRECN                                                          
         CLI   CFMKSUBR,CFMKSAG1   CAN'T MOVE AGENCY LEVEL 1 RECORD             
         BNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         OI    OLDPAS.CFMKSTAT,X'80'                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO1'                            
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  OLDPAS                                                           
                                                                                
OLDACT   USING CFMKEY,IOKEY        DELETE OLD ACTIVE KEY                        
         MVC   OLDACT.CFMKEY,CFMKEY                                             
         GOTOR (#IOEXEC,AIOEXEC),'IORDUP+IODIR+IO1'                             
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    OLDACT.CFMKSTAT,X'80'                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO1'                            
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    OLDACT.CFMKSTAT,FF-X'80'                                         
         DROP  OLDACT                                                           
                                                                                
         MVC   CFMKPNOD,PNODE      SET NEW PARENT                               
         OC    RCODE,RCODE         TEST NEW RECORD CODE PROVIDED                
         BNZ   *+10                                                             
         MVC   RCODE,CFMKCODE      NO - USE OLD CODE                            
         MVC   CFMKCODE,RCODE      SET NEW CODE (IF ANY)                        
         GOTOR UPDNAM              UPDATE NAME                                  
                                                                                
NEWACT   USING CFMKEY,IOKEY                                                     
         MVC   NEWACT.CFMKEY,CFMKEY                                             
         GOTOR (#IOEXEC,AIOEXEC),'IORDUPD+IODIR+IO1'                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   NEWACT.CFMKEY(CFMKLENQ),IOKEYSAV                                 
         MVC   PENDKEY,NEWACT.CFMKEY                                            
         TM    IOERR,IOEDEL        IF NEW KEY FOUND AND DELETED                 
         JNZ   UPDRECY             JUST EXIT                                    
         TM    IOERR,IOERNF        IF NEW KEY NOT FOUND                         
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IODIR+IO1'                              
         JE    UPDRECY                                                          
         DC    H'0'                                                             
         DROP  NEWACT                                                           
                                                                                
UPDSTR02 LA    R2,KEYWORK          BUILD ACTIVE KEY                             
         USING CFMRECD,R2                                                       
         XC    CFMKEY,CFMKEY                                                    
         MVI   CFMKTYPE,CFMKTYPQ                                                
         MVC   CFMKAGY,TWAAGY                                                   
                                                                                
         CLI   ACTION,ACTADDQ      TEST ADDING NEW RECORD                       
         BNE   UPDSTR12                                                         
         CLI   RECORD,0            TEST WE HAVE RECORD CODE                     
         JNE   *+16                                                             
         GOTOR PUTERR,CE#INVR#                                                  
         J     UPDRECN                                                          
         MVC   CFMKSUBR,RECORD                                                  
                                                                                
         OC    PID#,PID#           TEST PARENT RECORD ID PROVIDED               
         BZ    UPDSTR04                                                         
         SR    R1,R1               YES - LOOK-UP FROM ID TABLE                  
         ICM   R1,3,PID#                                                        
         BZ    UPDSTR04                                                         
         CHI   R1,IOLENQ/L'PNODE                                                
         BNH   *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         A     R1,AIO4                                                          
         OC    CFMKPNOD,0(R1)                                                   
         BNZ   UPDSTR06                                                         
         DC    H'0'                                                             
                                                                                
UPDSTR04 OC    PNODE,PNODE         TEST WE HAVE PARENT NODE                     
         BNZ   *+16                                                             
         GOTOR PUTERR,CE#INVPA                                                  
         J     UPDRECN                                                          
         MVC   CFMKPNOD,PNODE                                                   
                                                                                
UPDSTR06 OC    RCODE,RCODE         TEST WE HAVE RECORD CODE                     
         BNZ   *+16                                                             
         GOTOR PUTERR,CE#INVRC                                                  
         J     UPDRECN                                                          
         MVC   CFMKCODE,RCODE                                                   
                                                                                
CHKCOD   USING CFMKEY,IOKEY                                                     
         MVC   CHKCOD.CFMKEY,CFMKEY                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUPD+IODIR+IO1'                            
         BE    *+14                                                             
         TM    IOERR,IOEDEL                                                     
         BNZ   UPDSTR08                                                         
         DC    H'0'                                                             
         CLC   CHKCOD.CFMKEY(CFMKRNOD-CFMKEY),CFMKEY                            
         BNE   UPDSTR08                                                         
         GOTOR PUTERR,CE#INVRC                                                  
         J     UPDRECN                                                          
         DROP  CHKCOD                                                           
                                                                                
UPDSTR08 GOTOR NXTNOD              ESTABLISH RECORD NODE                        
                                                                                
         SR    R1,R1                                                            
         ICM   R1,3,RID#           UPDATE RECORD ID TABLE IF REQUIRED           
         BZ    UPDSTR10                                                         
         CHI   R1,IOLENQ/L'NODEWORK                                             
         BNH   *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         A     R1,AIO4                                                          
         MVC   0(L'NODEWORK,R1),NODEWORK                                        
                                                                                
UPDSTR10 MVC   CFMKRNOD,NODEWORK                                                
         CLC   CFMKPNOD,CFMKRNOD                                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   PENDKEY,CFMKEY                                                   
         GOTOR BLDVIR              BUILD VIRGIN RECORD                          
         GOTOR UPDNAM              ADD NAME ELEMENT IF NECESSARY                
         GOTOR PUTNOD              BUILD DOWNLOAD ELEMENT FOR NODE              
         J     UPDRECY                                                          
                                                                                
UPDSTR12 OC    RNODE,RNODE         TEST RECORD NODE GIVEN                       
         BNZ   *+16                                                             
         GOTOR PUTERR,CE#INVRN                                                  
         J     UPDRECN                                                          
                                                                                
GETPAS   USING CFMPAS,PASWORK      GET RECORD PASSIVE POINTER                   
         XC    GETPAS.CFMPAS,GETPAS.CFMPAS                                      
         MVI   GETPAS.CFMPTYPE,CFMPTYPQ                                         
         MVC   GETPAS.CFMPAGY,TWAAGY                                            
         MVC   GETPAS.CFMPRNOD,RNODE                                            
         XC    GETPAS.CFMPRNOD,EFFS                                             
         MVC   IOKEY(L'CFMPAS),GETPAS.CFMPAS                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUP+IODIR+IO1'                             
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   GETPAS.CFMPAS(CFMPPNOD-CFMPAS),IOKEYSAV                          
         BE    *+16                                                             
         GOTOR PUTERR,CE#INVRN                                                  
         J     UPDRECN                                                          
         DROP  GETPAS                                                           
                                                                                
         GOTOR GETREC              READ EXISTING RECORD                         
         L     R2,AIO1             AND POINT TO IT                              
                                                                                
         CLI   ACTION,ACTDELQ      TEST DELETING THIS RECORD                    
         BNE   *+12                                                             
         OI    CFMRSTAT,X'80'      YES - TURN ON DELETED FLAG                   
         B     UPDSTR18                                                         
                                                                                
         OC    RCODE,RCODE         TEST RECORD CODE GIVEN                       
         BNZ   *+10                                                             
         MVC   RCODE,CFMKCODE      NO - USE OLD RECORD CODE                     
         CLC   CFMKCODE,RCODE      TEST CHANGE OF RECORD CODE                   
         BE    UPDSTR16            NO - JUST UPDATE THE RECORD                  
                                                                                
OLDACT   USING CFMKEY,IOKEY        DELETE OLD ACTIVE KEY                        
         MVC   OLDACT.CFMKEY,CFMKEY                                             
         GOTOR (#IOEXEC,AIOEXEC),'IORDUP+IODIR+IO1'                             
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    OLDACT.CFMKSTAT,X'80'                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO1'                            
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  OLDACT                                                           
                                                                                
NEWACT   USING CFMKEY,IOKEY        READ NEW ACTIVE KEY                          
         MVC   NEWACT.CFMKEY,CFMKEY                                             
         MVC   NEWACT.CFMKCODE,RCODE                                            
         GOTOR (#IOEXEC,AIOEXEC),'IORDUPD+IODIR+IO1'                            
         BNE   *+6                                                              
         DC    H'0'                NEW KEY CAN'T EXIST                          
         TM    IOERR,IOEDEL        UNLESS IT IS DELETED                         
         BNZ   UPDSTR14            IF SO JUST WRITE THE RECORD BACK             
         TM    IOERR,IOERNF        TEST NOT FOUND                               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   NEWACT.CFMKEY,IOKEYSAV                                           
         MVC   NEWACT.CFMKSTAT,CFMRSTAT                                         
         MVC   NEWACT.CFMKDA,IODA                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IODIR+IO1'                              
         BE    UPDSTR14                                                         
         DC    H'0'                                                             
UPDSTR14 MVC   CFMKEY,NEWACT.CFMKEY                                             
         DROP  NEWACT                                                           
                                                                                
UPDSTR16 GOTOR UPDNAM              UPDATE NAME                                  
                                                                                
UPDSTR18 MVC   PENDKEY,CFMKEY      SET PENDING I/O KEY                          
         J     UPDRECY                                                          
         DROP  R2,RB                                                            
         EJECT                                                                  
***********************************************************************         
* MAINTAIN MEDIA POINTERS                                             *         
***********************************************************************         
                                                                                
UPDMED   BASE  ,                                                                
         GOTOR GOIO                FLUSH ANY PENDING I/O                        
                                                                                
CHKHDR   USING CFMRECD,IOKEY       ENSURE AGENCY HEADER EXISTS                  
         XC    CHKHDR.CFMKEY,CHKHDR.CFMKEY                                      
         MVI   CHKHDR.CFMKTYPE,CFMKTYPQ                                         
         MVC   CHKHDR.CFMKAGY,TWAAGY                                            
         MVI   CHKHDR.CFMKSUBR,CFMKSHDR                                         
         LHI   R0,1                                                             
         STCM  R0,15,CHKHDR.CFMKRNOD                                            
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         BE    *+16                                                             
         GOTOR PUTERR,CE#AHRNF                                                  
         J     UPDRECN                                                          
         DROP  CHKHDR                                                           
                                                                                
         CLI   ACTION,ACTADDQ      TEST ADDING NEW RECORD                       
         BNE   UPDMED02                                                         
         LA    R2,KEYWORK          BUILD ACTIVE KEY                             
         USING CFMRECD,R2                                                       
         XC    CFMKEY,CFMKEY                                                    
         MVI   CFMKTYPE,CFMKTYPQ                                                
         MVC   CFMKAGY,TWAAGY                                                   
         MVI   CFMKSUBR,CFMKSMED   TEST WE HAVE RECORD CODE                     
                                                                                
         OC    RCODE,RCODE         TEST RECORD CODE PASSED                      
         BNZ   *+16                                                             
         GOTOR PUTERR,CE#INVRC                                                  
         J     UPDRECN                                                          
         MVC   CFMKCODE,RCODE      SET MEDIA RECORD CODE                        
CHKCOD   USING CFMKEY,IOKEY                                                     
         MVC   CHKCOD.CFMKEY,CFMKEY                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUP+IODIR+IO1'                             
         BE    *+14                                                             
         TM    IOERR,IOEDEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   CHKCOD.CFMKEY(CFMKRNOD-CFMKEY),CFMKEY                            
         BNE   *+16                                                             
         GOTOR PUTERR,CE#INVRC                                                  
         J     UPDRECN                                                          
         DROP  CHKCOD                                                           
                                                                                
         GOTOR NXTNOD              ESTABLISH RECORD NODE                        
         MVC   CFMKRNOD,NODEWORK                                                
         MVC   PENDKEY,CFMKEY                                                   
         GOTOR BLDVIR              BUILD VIRGIN RECORD                          
         GOTOR UPDNAM              ADD NAME ELEMENT IF NECESSARY                
         L     R2,AIO1             POINT TO RECORD                              
                                                                                
         LA    RF,ELEM                                                          
         USING MPTRD,RF                                                         
         MVI   MPTREL,MPTRELQ                                                   
         MVI   MPTRLN,MPTRLNQ                                                   
         CLI   SYSTEM,MPTRSSPT                                                  
         BE    *+8                                                              
         CLI   SYSTEM,MPTRSNET                                                  
         BE    *+8                                                              
         CLI   SYSTEM,MPTRSPRT                                                  
         BE    *+8                                                              
         CLI   SYSTEM,MPTRSACC                                                  
         BE    *+16                                                             
         GOTOR PUTERR,CE#INVSY                                                  
         J     UPDRECN                                                          
         MVC   MPTRSYS,SYSTEM                                                   
         CLI   MEDIA,0                                                          
         BNE   *+16                                                             
         GOTOR PUTERR,CE#INVME                                                  
         J     UPDRECN                                                          
         MVC   MPTRMED,MEDIA                                                    
         GOTOR VHELLO,DMCB,(C'P',GENFIL),CFMRECD,MPTRD,0                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR PUTNOD              BUILD DOWNLOAD ELEMENT FOR NODE              
         J     UPDRECY                                                          
                                                                                
UPDMED02 OC    RNODE,RNODE         TEST RECORD NODE GIVEN                       
         BNZ   *+16                                                             
         GOTOR PUTERR,CE#INVRN                                                  
         J     UPDRECN                                                          
                                                                                
GETPAS   USING CFMPAS,PASWORK      GET RECORD FROM ITS PASSIVE                  
         XC    GETPAS.CFMPAS,GETPAS.CFMPAS                                      
         MVI   GETPAS.CFMPTYPE,CFMPTYPQ                                         
         MVC   GETPAS.CFMPAGY,TWAAGY                                            
         MVC   GETPAS.CFMPRNOD,RNODE                                            
         XC    GETPAS.CFMPRNOD,EFFS                                             
         MVC   IOKEY(L'CFMPAS),GETPAS.CFMPAS                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUP+IODIR+IO1'                             
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   GETPAS.CFMPAS(CFMPPNOD-CFMPAS),IOKEYSAV                          
         BE    *+16                                                             
         GOTOR PUTERR,CE#INVRN                                                  
         J     UPDRECN                                                          
                                                                                
         GOTOR GETREC              READ RECORD                                  
         L     R2,AIO1             AND POINT TO IT                              
         CLI   CFMKSUBR,CFMKSMED   ENSURE THIS IS A MEDIA RECORD                
         BE    *+16                                                             
         GOTOR PUTERR,CE#INVRE                                                  
         J     UPDRECN                                                          
                                                                                
         CLI   ACTION,ACTDELQ      TEST DELETING MEDIA POINTER RECORD           
         BNE   *+12                                                             
         OI    CFMRSTAT,X'80'                                                   
         J     UPDRECY                                                          
                                                                                
         CLI   ACTION,ACTCHAQ      TEST ACTION CHANGE                           
         BE    *+16                                                             
         GOTOR PUTERR,CE#INVAC                                                  
         J     UPDRECN                                                          
                                                                                
         OC    RCODE,RCODE         TEST CODE GIVEN                              
         BNZ   *+10                                                             
         MVC   RCODE,CFMKCODE                                                   
         CLC   CFMKCODE,RCODE      TEST CHANGE OF RECORD CODE                   
         BE    UPDMED06                                                         
                                                                                
CHKCOD   USING CFMKEY,IOKEY        ENSURE NEW CODE NOT USED                     
         XC    CHKCOD.CFMKEY,CHKCOD.CFMKEY                                      
         MVC   CHKCOD.CFMKEY(CFMKCODE-CFMKEY),CFMKEY                            
         MVC   CHKCOD.CFMKCODE,RCODE                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   CHKCOD.CFMKEY(CFMKRNOD-CFMKEY),IOKEYSAV                          
         BNE   *+16                                                             
         GOTOR PUTERR,CE#RECAE                                                  
         J     UPDRECN                                                          
         DROP  CHKCOD                                                           
                                                                                
OLDACT   USING CFMKEY,IOKEY        DELETE OLD ACTIVE KEY                        
         MVC   OLDACT.CFMKEY,CFMKEY                                             
         GOTOR (#IOEXEC,AIOEXEC),'IORDUP+IODIR+IO1'                             
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    OLDACT.CFMKSTAT,X'80'                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO1'                            
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    OLDACT.CFMKSTAT,FF-X'80'                                         
         DROP  OLDACT                                                           
                                                                                
NEWACT   USING CFMKEY,IOKEY        READ NEW ACTIVE KEY                          
         MVC   NEWACT.CFMKEY,CFMKEY                                             
         MVC   NEWACT.CFMKCODE,RCODE                                            
         GOTOR (#IOEXEC,AIOEXEC),'IORDUPD+IODIR+IO1'                            
         BNE   *+6                                                              
         DC    H'0'                NEW KEY CAN'T EXIST                          
         TM    IOERR,IOEDEL        UNLESS IT IS DELETED                         
         BNZ   UPDMED04            IF SO JUST WRITE THE RECORD BACK             
         TM    IOERR,IOERNF        TEST NOT FOUND                               
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   NEWACT.CFMKEY,IOKEYSAV                                           
         MVC   NEWACT.CFMKSTAT,CFMRSTAT                                         
         MVC   NEWACT.CFMKDA,IODA                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IODIR+IO1'                              
         BE    UPDMED04                                                         
         DC    H'0'                                                             
                                                                                
UPDMED04 MVC   CFMKEY,NEWACT.CFMKEY                                             
         DROP  NEWACT                                                           
                                                                                
UPDMED06 GOTOR UPDNAM              UPDATE NAME                                  
         J     UPDRECY                                                          
         DROP  R2,RB                                                            
         EJECT                                                                  
***********************************************************************         
* MAINTAIN CLIENT POINTERS                                            *         
***********************************************************************         
                                                                                
UPDCLT   BASE  ,                                                                
         GOTOR GOIO                FLUSH ANY PENDING I/O                        
                                                                                
         SR    R1,R1                                                            
         ICM   R1,3,RID#           TEST RECORD ID PROVIDED                      
         BZ    UPDCLT02                                                         
         XC    RNODE,RNODE                                                      
         CHI   R1,IOLENQ/L'CFMPRNOD                                             
         BNH   *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         A     R1,AIO4                                                          
         OC    RNODE,0(R1)                                                      
         BNZ   UPDCLT02                                                         
         DC    H'0'                                                             
                                                                                
UPDCLT02 GOTOR VALMED              VALIDATE MEDIA NODE                          
         JNE   EXITN                                                            
                                                                                
         OC    CLIENT,CLIENT       TEST CLIENT CODE PASSED                      
         BNZ   *+16                                                             
         GOTOR PUTERR,CE#INVCL                                                  
         J     EXITN                                                            
                                                                                
         OC    RNODE,RNODE         TEST RECORD NODE PASSED                      
         BNZ   *+16                                                             
         GOTOR PUTERR,CE#INVRN                                                  
         J     EXITN                                                            
                                                                                
GETPAS   USING CFMPAS,IOKEY                                                     
         XC    GETPAS.CFMPAS,GETPAS.CFMPAS                                      
         MVI   GETPAS.CFMPTYPE,CFMPTYPQ                                         
         MVC   GETPAS.CFMPAGY,TWAAGY                                            
         MVC   GETPAS.CFMPRNOD,RNODE                                            
         XC    GETPAS.CFMPRNOD,EFFS                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   GETPAS.CFMPAS(CFMPPNOD-CFMPAS),IOKEYSAV                          
         BE    *+16                                                             
         GOTOR PUTERR,CE#INVRN                                                  
         J     EXITN                                                            
         DROP  GETPAS                                                           
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO2'                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO2                                                          
         USING CFMRECD,R2                                                       
         CLI   CFMKSUBR,CFMKSAD1   TEST ADVERTISER RECORD                       
         BL    *+12                                                             
         CLI   CFMKSUBR,CFMKSMV1                                                
         BNH   *+16                                                             
         GOTOR PUTERR,CE#INVRE                                                  
         J     EXITN                                                            
                                                                                
         LA    R2,KEYWORK          BUILD ACTIVE CLIENT POINTER KEY              
         XC    CFMKEY,CFMKEY                                                    
         MVI   CFMKTYPE,CFMKTYPQ                                                
         MVC   CFMKAGY,TWAAGY                                                   
         MVC   CFMKPNOD,RNODE                                                   
         MVI   CFMKSUBR,CFMKSCLT                                                
         MVC   CFMKCMED,MNODE                                                   
         MVC   CFMKCLTE,CLIENT                                                  
         MVC   CFMKCLTI,ICLIENT                                                 
                                                                                
CHKPAS   USING CLTPAS,IOKEY        BUILD PASSIVE CLIENT POINTER KEY             
         XC    CHKPAS.CLTPAS,CHKPAS.CLTPAS                                      
         MVI   CHKPAS.CLTPTYPE,CLTPTYPQ                                         
         MVC   CHKPAS.CLTPAGY,TWAAGY                                            
         MVC   CHKPAS.CLTPMED,MNODE                                             
         MVC   CHKPAS.CLTPCLT,CLIENT                                            
         DROP  CHKPAS                                                           
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORDUPD+IODIR+IO1'                            
         BNE   UPDCLT04                                                         
         CLI   ACTION,ACTDELQ      TEST DELETE CLIENT POINTER                   
         BE    *+16                                                             
         GOTOR PUTERR,CE#RECAE                                                  
         J     EXITN                                                            
         GOTOR GETREC                                                           
         L     R2,AIO1                                                          
         OI    CFMRSTAT,X'80'                                                   
         MVC   PENDKEY,CFMKEY                                                   
         MVI   PENDACTN,PENDACHA                                                
         J     UPDRECY                                                          
                                                                                
UPDCLT04 TM    IOERR,IOERNF+IOEDEL TEST RECORD NOT FOUND/DELETED                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   ACTION,ACTADDQ      TEST ADDING NEW POINTER                      
         BE    *+16                                                             
         GOTOR PUTERR,CE#RECNF     NO - NOT GOOD                                
         J     EXITN                                                            
                                                                                
CHKKEY   USING CFMKEY,IOKEY        READ FOR ACTIVE CLIENT POINTER               
         MVC   CHKKEY.CFMKEY,CFMKEY                                             
         DROP  CHKKEY                                                           
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORDUPD+IODIR+IO1'                            
         TM    IOERR,IOEDEL        TEST FOUND DELETED CLIENT POINTER            
         BZ    UPDCLT06                                                         
         GOTOR GETREC                                                           
         L     R2,AIO1                                                          
         NI    CFMRSTAT,FF-X'80'                                                
         MVC   PENDKEY,CFMKEY                                                   
         MVI   PENDACTN,PENDACHA                                                
         J     UPDRECY                                                          
                                                                                
UPDCLT06 TM    IOERR,IOERNF        TEST RECORD NOT FOUND                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   PENDKEY,CFMKEY      BUILD NEW ACTIVE CLIENT POINTER              
         GOTOR BLDVIR                                                           
         J     UPDRECY                                                          
         DROP  R2,RB                                                            
         EJECT                                                                  
***********************************************************************         
* MAINTAIN BRAND POINTERS                                             *         
***********************************************************************         
                                                                                
GETPAS   USING CFMPAS,IOKEY                                                     
                                                                                
UPDBRD   BASE  ,                                                                
         GOTOR GOIO                                                             
                                                                                
         SR    R1,R1                                                            
         ICM   R1,3,RID#           TEST RECORD ID PROVIDED                      
         BZ    UPDBRD02                                                         
         XC    RNODE,RNODE                                                      
         CHI   R1,IOLENQ/L'CFMPRNOD                                             
         BNH   *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         A     R1,AIO4                                                          
         OC    RNODE,0(R1)                                                      
         BNZ   UPDBRD02                                                         
         DC    H'0'                                                             
                                                                                
UPDBRD02 OC    RNODE,RNODE                                                      
         BNZ   *+16                                                             
         GOTOR PUTERR,CE#INVRN                                                  
         J     EXITN                                                            
                                                                                
         XC    GETPAS.CFMPAS,GETPAS.CFMPAS                                      
         MVI   GETPAS.CFMPTYPE,CFMPTYPQ                                         
         MVC   GETPAS.CFMPAGY,TWAAGY                                            
         MVC   GETPAS.CFMPRNOD,RNODE                                            
         XC    GETPAS.CFMPRNOD,EFFS                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   GETPAS.CFMPAS(CFMPPNOD-CFMPAS),IOKEYSAV                          
         BE    *+16                                                             
         GOTOR PUTERR,CE#INVRN                                                  
         J     EXITN                                                            
         DROP  GETPAS                                                           
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO2'                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO2                                                          
         USING CFMRECD,R2                                                       
         CLI   CFMKSUBR,CFMKSAD1   TEST ADVERTISER RECORD                       
         BL    *+12                                                             
         CLI   CFMKSUBR,CFMKSMV1                                                
         BNH   *+16                                                             
         GOTOR PUTERR,CE#INVRE                                                  
         J     EXITN                                                            
                                                                                
         SR    R1,R1                                                            
         ICM   R1,3,AID#           TEST ADVERTISER RECORD ID PROVIDED           
         BZ    UPDBRD04                                                         
         CHI   R1,IOLENQ/L'ANODE                                                
         BNH   *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         A     R1,AIO4                                                          
         MVC   ANODE,0(R1)                                                      
         OC    ANODE,ANODE                                                      
         BNZ   UPDBRD06                                                         
         DC    H'0'                                                             
                                                                                
UPDBRD04 OC    ANODE,ANODE         VALIDATE ADVERTISER NODE                     
         BNZ   *+16                                                             
         GOTOR PUTERR,CE#INVAN                                                  
         J     EXITN                                                            
                                                                                
CHKADV   USING CFMPAS,IOKEY                                                     
UPDBRD06 XC    CHKADV.CFMPAS,CHKADV.CFMPAS                                      
         MVI   CHKADV.CFMPTYPE,CFMPTYPQ                                         
         MVC   CHKADV.CFMPAGY,TWAAGY                                            
         MVC   CHKADV.CFMPRNOD,ANODE                                            
         XC    CHKADV.CFMPRNOD,EFFS                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO2'                               
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   CHKADV.CFMPAS(CFMPPNOD-CFMPAS),IOKEYSAV                          
         BE    *+16                                                             
         GOTOR PUTERR,CE#INVAN                                                  
         J     EXITN                                                            
         DROP  CHKADV                                                           
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO2'                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO2                                                          
         CLI   CFMKSUBR,CFMKSAD1   TEST THIS IS A ADVERTISER RECORD             
         BL    *+12                                                             
         CLI   CFMKSUBR,CFMKSMV1                                                
         BNH   *+16                                                             
         GOTOR PUTERR,CE#INVAN                                                  
         J     EXITN                                                            
                                                                                
         GOTOR VALMED              VALIDATE MEDIA NODE                          
         JNE   EXITN                                                            
                                                                                
         OC    CLIENT,CLIENT       TEST CLIENT CODE PASSED                      
         BNZ   *+16                                                             
         GOTOR PUTERR,CE#INVCL                                                  
         J     EXITN                                                            
                                                                                
CHKCLT   USING CFMKEY,IOKEY                                                     
         XC    CHKCLT.CFMKEY,CHKCLT.CFMKEY                                      
         MVI   CHKCLT.CFMKTYPE,CFMKTYPQ                                         
         MVC   CHKCLT.CFMKAGY,TWAAGY                                            
         MVC   CHKCLT.CFMKPNOD,ANODE                                            
         MVI   CHKCLT.CFMKSUBR,CFMKSCLT                                         
         MVC   CHKCLT.CFMKCMED,MNODE                                            
         MVC   CHKCLT.CFMKCLTE,CLIENT                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   CHKCLT.CFMKEY(CFMKCLTI-CFMKEY),IOKEYSAV                          
         BE    *+16                                                             
         GOTOR PUTERR,CE#MAPNF                                                  
         J     EXITN                                                            
         DROP  CHKCLT                                                           
                                                                                
         OC    BRAND,BRAND         TEST BRAND CODE PASSED                       
         BNZ   *+16                                                             
         GOTOR PUTERR,CE#INVBR                                                  
         J     EXITN                                                            
                                                                                
         LA    R2,KEYWORK                                                       
         USING CFMKEY,R2                                                        
         XC    CFMKEY,CFMKEY                                                    
         MVI   CFMKTYPE,CFMKTYPQ                                                
         MVC   CFMKAGY,TWAAGY                                                   
         MVC   CFMKPNOD,RNODE                                                   
         MVI   CFMKSUBR,CFMKSBRD                                                
         MVC   CFMKBMED,MNODE                                                   
         MVC   CFMKBADV,ANODE                                                   
         MVC   CFMKBCLT,CLIENT                                                  
         MVC   CFMKBRDE,BRAND                                                   
         MVC   CFMKBRDI,IBRAND                                                  
                                                                                
CHKPAS   USING BRDPAS,IOKEY                                                     
         XC    CHKPAS.BRDPAS,CHKPAS.BRDPAS                                      
         MVI   CHKPAS.BRDPTYPE,BRDPTYPQ                                         
         MVC   CHKPAS.BRDPAGY,TWAAGY                                            
         MVC   CHKPAS.BRDPMED,MNODE                                             
         MVC   CHKPAS.BRDPCLT,CLIENT                                            
         MVC   CHKPAS.BRDPBRD,BRAND                                             
         DROP  CHKPAS                                                           
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORDUPD+IODIR+IO1'                            
         BNE   UPDBRD08                                                         
         CLI   ACTION,ACTDELQ      TEST DELETE BRAND POINTER                    
         BE    *+16                                                             
         GOTOR PUTERR,CE#RECAE                                                  
         J     EXITN                                                            
         GOTOR GETREC                                                           
         L     R2,AIO1                                                          
         OI    CFMRSTAT,X'80'                                                   
         MVC   PENDKEY,CFMKEY                                                   
         MVI   PENDACTN,PENDACHA                                                
         J     UPDRECY                                                          
                                                                                
UPDBRD08 TM    IOERR,IOERNF+IOEDEL TEST RECORD NOT FOUND/DELETED                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   ACTION,ACTADDQ      TEST ADDING NEW POINTER                      
         BE    *+16                                                             
         GOTOR PUTERR,CE#RECNF     NO - NOT GOOD                                
         J     EXITN                                                            
                                                                                
CHKKEY   USING CFMKEY,IOKEY        READ FOR ACTIVE BRAND POINTER                
         MVC   CHKKEY.CFMKEY,CFMKEY                                             
         DROP  CHKKEY                                                           
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORDUPD+IODIR+IO1'                            
         TM    IOERR,IOEDEL        TEST FOUND DELETED BRAND POINTER             
         BZ    UPDBRD10                                                         
         GOTOR GETREC                                                           
         L     R2,AIO1                                                          
         NI    CFMRSTAT,FF-X'80'                                                
         MVC   PENDKEY,CFMKEY                                                   
         MVI   PENDACTN,PENDACHA                                                
         J     UPDRECY                                                          
                                                                                
UPDBRD10 TM    IOERR,IOERNF        TEST RECORD NOT FOUND                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   PENDKEY,CFMKEY      BUILD NEW ACTIVE BRAND POINTER               
         GOTOR BLDVIR                                                           
         J     UPDRECY                                                          
         DROP  R2,RB                                                            
         EJECT                                                                  
***********************************************************************         
* MAINTAIN VENDOR POINTERS                                            *         
***********************************************************************         
                                                                                
UPDVEN   BASE  ,                                                                
         GOTOR GOIO                FLUSH ANY PENDING I/O                        
                                                                                
         LA    R2,KEYWORK          BUILD KEY                                    
         USING CFMRECD,R2                                                       
         XC    CFMKEY,CFMKEY                                                    
         MVI   CFMKTYPE,CFMKTYPQ                                                
         MVC   CFMKAGY,TWAAGY                                                   
         MVI   CFMKSUBR,CFMKSVEN                                                
                                                                                
         OC    PID#,PID#           TEST PARENT RECORD ID PROVIDED               
         BZ    UPDVEN02                                                         
         SR    R1,R1               YES - LOOK-UP FROM ID TABLE                  
         ICM   R1,3,PID#                                                        
         BZ    UPDVEN02                                                         
         CHI   R1,IOLENQ/L'PNODE                                                
         BNH   *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         A     R1,AIO4                                                          
         OC    CFMKPNOD,0(R1)                                                   
         BNZ   UPDVEN04                                                         
         DC    H'0'                                                             
                                                                                
UPDVEN02 OC    PNODE,PNODE         TEST WE HAVE PARENT NODE                     
         BNZ   *+16                                                             
         GOTOR PUTERR,CE#INVPA                                                  
         J     UPDRECN                                                          
         MVC   CFMKPNOD,PNODE                                                   
                                                                                
UPDVEN04 OC    RCODE,RCODE         TEST WE HAVE VENDOR CODE                     
         BNZ   *+16                                                             
         GOTOR PUTERR,CE#INVRC                                                  
         J     UPDRECN                                                          
         OC    RCODE,SPACES        SPACE FILL VENDOR CODE                       
         MVC   CFMKCODE,RCODE                                                   
                                                                                
CHKNEW   USING CFMPAS,IOKEY        CHECK GOOD PARENT EXISTS                     
         XC    CHKNEW.CFMPAS,CHKNEW.CFMPAS                                      
         MVI   CHKNEW.CFMPTYPE,CFMPTYPQ                                         
         MVC   CHKNEW.CFMPAGY,TWAAGY                                            
         MVC   CHKNEW.CFMPRNOD,CFMKPNOD                                         
         XC    CHKNEW.CFMPRNOD,EFFS                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO1'                               
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   CHKNEW.CFMPAS(CFMPPNOD-CFMKEY),IOKEYSAV                          
         BE    *+16                                                             
         GOTOR PUTERR,CE#INVPA                                                  
         J     UPDRECN                                                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO1'                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         CLI   CFMKSUBR,CFMKSMV1   ENSURE PARENT IS A VENDOR                    
         BL    *+12                                                             
         CLI   CFMKSUBR,CFMKSMED                                                
         BL    *+16                                                             
         GOTOR PUTERR,CE#INVPA                                                  
         J     UPDRECN                                                          
                                                                                
         LA    R2,KEYWORK          POINT BACK TO KEY                            
                                                                                
         GOTOR VALMED              VALIDATE MEDIA NODE                          
         JNE   EXITN                                                            
         MVC   CFMKVMED,MNODE                                                   
                                                                                
         CLI   ACTION,ACTADDQ      TEST ADDING NEW RECORD                       
         BNE   UPDVEN10                                                         
                                                                                
CHKKEY   USING CFMKEY,IOKEY                                                     
         MVC   CHKKEY.CFMKEY,CFMKEY                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUPD+IODIR+IO1'                            
         BE    *+14                                                             
         TM    IOERR,IOEDEL                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   CHKKEY.CFMKEY,CFMKEY                                             
         BNE   UPDVEN08                                                         
         TM    IOERR,IOEDEL        ALLOW IF DELETED                             
         BNZ   UPDVEN06                                                         
         GOTOR PUTERR,CE#RECAE                                                  
         J     UPDRECN                                                          
         DROP  CHKKEY                                                           
                                                                                
UPDVEN06 GOTOR GETREC                                                           
         L     R2,AIO1                                                          
         NI    CFMRSTAT,FF-X'80'                                                
         MVC   PENDKEY,CFMKEY                                                   
         GOTOR UPDNAM                                                           
         J     UPDRECY                                                          
                                                                                
UPDVEN08 MVC   PENDKEY,CFMKEY                                                   
         GOTOR BLDVIR              BUILD VIRGIN RECORD                          
         GOTOR UPDNAM              ADD NAME ELEMENT IF NECESSARY                
         J     UPDRECY                                                          
                                                                                
UPDVEN10 CLI   ACTION,ACTDELQ      MUST BE DELETING                             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY(L'CFMKEY),CFMKEY                                           
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IODIR+IO1'                               
         BE    *+16                                                             
         GOTOR PUTERR,CE#RECNF                                                  
         J     EXITN                                                            
         GOTOR GETREC                                                           
         L     R2,AIO1                                                          
         OI    CFMRSTAT,X'80'                                                   
         MVC   PENDKEY,CFMKEY      SET PENDING I/O KEY                          
         J     UPDRECY                                                          
         DROP  R2,RB                                                            
         EJECT                                                                  
***********************************************************************         
* BUILD VIRGIN RECORD IN I/O AREA 1 - SET PENDING FLAG TO 'ADD'       *         
***********************************************************************         
                                                                                
BLDVIR   L     R1,AIO1                                                          
         USING CFMRECD,R1                                                       
         MVC   CFMKEY,PENDKEY                                                   
         XC    CFMRSTAT(L'CFMRSTAT+L'CFMRLINK),CFMRSTAT                         
         LA    RF,CFMFIRST                                                      
         USING CFMAD,RF                                                         
         MVI   CFMAEL,CFMAELQ                                                   
         MVI   CFMALN,CFMALNQ                                                   
         MVC   CFMADATE,TODAYC                                                  
         MVC   CFMAUSER,TWAUSRID                                                
         MVC   CFMAPERS,TWAAUTH                                                 
         MVI   CFMATYPE,CFMATADD                                                
         MVI   CFMAD+CFMALNQ,0                                                  
         AHI   RF,CFMALNQ+1                                                     
         SR    RF,R1                                                            
         STCM  RF,3,CFMRECLN                                                    
         MVI   PENDACTN,PENDAADD                                                
         BR    RE                                                               
         DROP  R1,RF                                                            
                                                                                
***********************************************************************         
* READ EXISTING RECORD - SET PENDING ACTION TO CHANGE                 *         
***********************************************************************         
                                                                                
GETREC   LR    R0,RE                                                            
         MVI   PENDACTN,PENDACHA                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOFIL+IO1'                           
         JE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE OR ADD A NAME ELEMENT TO THE RECORD IN IO1                   *         
***********************************************************************         
                                                                                
UPDNAM   LR    R0,RE                                                            
         OC    RNAME,RNAME         TEST NAME PRESENT                            
         JZ    UPDNAMX                                                          
         GOTOR VHELLO,DMCB,(C'D',GENFIL),('RNMELQ',AIO1),0,0                    
         LA    RF,ELEM             BUILD NAME ELEMENT                           
         USING RNMD,RF                                                          
         MVI   RNMEL,RNMELQ                                                     
         MVC   RNMNAME(L'RNAME),RNAME                                           
         XC    RNAME,RNAME                                                      
         LA    R1,RNMNAME+L'RNAME-1                                             
         LHI   RE,L'RNAME                                                       
UPDNAM02 CLI   0(R1),C' '                                                       
         JH    UPDNAM04                                                         
         BRCT  RE,*+8                                                           
         J     UPDNAMX                                                          
         BRCT  R1,UPDNAM02                                                      
UPDNAM04 AHI   R1,1                                                             
         SR    R1,RF                                                            
         STC   R1,RNMLN                                                         
         GOTOR VHELLO,DMCB,(C'P',GENFIL),AIO1,RNMD,0                            
         CLI   12(R1),0                                                         
         JE    UPDNAMX                                                          
         DC    H'0'                                                             
UPDNAMX  LR    RE,R0                                                            
         BR    RE                                                               
         DROP  RF                                                               
                                                                                
***********************************************************************         
* ESTABLISH NEXT RECORD NODE                                          *         
***********************************************************************         
                                                                                
NXTNOD   LR    R0,RE                                                            
NODKEY   USING CFMPAS,IOKEY                                                     
         XC    NODKEY.CFMPAS,NODKEY.CFMPAS                                      
         MVI   NODKEY.CFMPTYPE,CFMPTYPQ                                         
         MVC   NODKEY.CFMPAGY,TWAAGY                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUPD+IODIR+IO1'                            
         JE    *+14                                                             
         TM    IOERR,IOEDEL                                                     
         JNZ   *+6                                                              
         DC    H'0'                                                             
         SR    RF,RF                                                            
         CLC   NODKEY.CFMPAS(CFMPRNOD-CFMPAS),IOKEYSAV                          
         JNE   *+12                                                             
         ICM   RF,15,NODKEY.CFMPRNOD                                            
         X     RF,EFFS                                                          
         AHI   RF,1                                                             
         STCM  RF,15,NODEWORK                                                   
         LR    RE,R0                                                            
         BR    RE                                                               
         DROP  NODKEY                                                           
         EJECT                                                                  
***********************************************************************         
* ISSUE PENDING I/O AND CREATE/UPDATE PASSIVE POINTERS                *         
***********************************************************************         
                                                                                
GOIO     CLI   PENDACTN,0                                                       
         BER   RE                                                               
GOION    NTR1  ,                                                                
         CLI   PENDACTN,PENDAADD   TEST ADDING NEW RECORD                       
         JNE   GOIO02                                                           
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOFIL+IO1'                           
         JE    GOIO06                                                           
         DC    H'0'                                                             
                                                                                
GOIO02   GOTOR (#IOEXEC,AIOEXEC),'IOPUTREC+IOFIL+IO1'                           
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO1                                                          
         MVC   IOKEY,0(R1)                                                      
         GOTOR (#IOEXEC,AIOEXEC),'IORDUPD+IODIR+IO1'                            
         JE    GOIO04                                                           
         TM    IOERR,IOEDEL                                                     
         JNZ   GOIO04                                                           
         DC    H'0'                                                             
                                                                                
GOIO04   L     R1,AIO1             UPDATE ACTIVE DIRECTORY POINTER              
         USING CFMRECD,R1                                                       
GOKEY    USING CFMKEY,PASWORK                                                   
         MVC   GOKEY.CFMKEY,CFMKEY                                              
         MVC   GOKEY.CFMKSTAT,CFMRSTAT                                          
         MVC   GOKEY.CFMKDA,IODA                                                
         CLC   GOKEY.CFMKEY(CFMKLENQ),IOKEY                                     
         JE    GOIO06                                                           
         MVC   IOKEY(CFMKLENQ),GOKEY.CFMKEY                                     
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IODIR+IO1'                            
         JE    GOIO06                                                           
         DC    H'0'                                                             
                                                                                
GOIO06   L     R1,AIO1             DEAL WITH PASSIVE POINTERS                   
         CLI   CFMKSUBR,CFMKSMED   TEST IF A STRUCTURE RECORD                   
         JH    GOIO08                                                           
         OC    CFMKRNOD,CFMKRNOD   TEST NEED NODE PASSIVE                       
         JZ    GOIOX                                                            
         XC    GOKEY.CFMPAS,GOKEY.CFMPAS                                        
         MVI   GOKEY.CFMPTYPE,CFMPTYPQ                                          
         MVC   GOKEY.CFMPAGY,CFMKAGY                                            
         MVC   GOKEY.CFMPRNOD,CFMKRNOD                                          
         XC    GOKEY.CFMPRNOD,EFFS                                              
         MVC   GOKEY.CFMPPNOD,CFMKPNOD                                          
         MVC   GOKEY.CFMKSTAT,CFMRSTAT                                          
         MVC   GOKEY.CFMKDA,IODA                                                
         MVC   IOKEY(L'CFMPAS),GOKEY.CFMPAS                                     
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORDUPD+IODIR+IO1'                            
         JNE   *+14                                                             
         CLC   IOKEY(CFMKLENQ),GOKEY.CFMPAS                                     
         JE    GOIOX                                                            
                                                                                
         MVC   IOKEY(CFMKLENQ),GOKEY.CFMPAS                                     
         LHI   R1,IOADD+IODIR+IO1                                               
         TM    IOERR,IOERNF                                                     
         JNZ   *+8                                                              
         LHI   R1,IOWRITE+IODIR+IO1                                             
         GOTOR (#IOEXEC,AIOEXEC),(R1)                                           
         JE    GOIOX                                                            
         DC    H'0'                                                             
                                                                                
GOIO08   CLI   CFMKSUBR,CFMKSVEN   TEST IF A VENDOR POINTER                     
         JE    GOIOX                                                            
         CLI   CFMKSUBR,CFMKSCLT   TEST IF A CLIENT POINTER                     
         JE    GOIO10                                                           
         CLI   CFMKSUBR,CFMKSBRD   TEST IF A BRAND POINTER                      
         JE    GOIO12                                                           
         J     GOIOX                                                            
                                                                                
GOIO10   XC    GOKEY.CLTPAS,GOKEY.CLTPAS                                        
         MVI   GOKEY.CLTPTYPE,CLTPTYPQ                                          
         MVC   GOKEY.CLTPAGY,CFMKAGY                                            
         MVC   GOKEY.CLTPMED,CFMKCMED                                           
         MVC   GOKEY.CLTPCLT,CFMKCLTE                                           
         J     GOIO14                                                           
                                                                                
GOIO12   XC    GOKEY.BRDPAS,GOKEY.BRDPAS                                        
         MVI   GOKEY.BRDPTYPE,BRDPTYPQ                                          
         MVC   GOKEY.BRDPAGY,CFMKAGY                                            
         MVC   GOKEY.BRDPMED,CFMKBMED                                           
         MVC   GOKEY.BRDPCLT,CFMKBCLT                                           
         MVC   GOKEY.BRDPBRD,CFMKBRDE                                           
         J     GOIO14                                                           
                                                                                
GOIO14   MVC   GOKEY.CFMKSTAT,CFMRSTAT                                          
         MVC   GOKEY.CFMKDA,IODA                                                
         MVC   IOKEY(L'CFMPAS),GOKEY.CFMPAS                                     
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORDUPD+IODIR+IO1'                            
         JNE   *+14                                                             
         CLC   IOKEY(CFMKLENQ),GOKEY.CFMPAS                                     
         JE    GOIOX                                                            
                                                                                
         MVC   IOKEY(CFMKLENQ),GOKEY.CFMPAS                                     
         LHI   R1,IOADD+IODIR+IO1                                               
         TM    IOERR,IOERNF                                                     
         JNZ   *+8                                                              
         LHI   R1,IOWRITE+IODIR+IO1                                             
         GOTOR (#IOEXEC,AIOEXEC),(R1)                                           
         JE    GOIOX                                                            
         DC    H'0'                                                             
                                                                                
GOIOX    XC    PENDVALS(PENDVALL),PENDVALS                                      
         J     EXITY                                                            
         DROP  R1,GOKEY                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE MEDIA NODE                                                 *         
***********************************************************************         
                                                                                
VALMED   NTR1  ,                                                                
         OC    MNODE,MNODE         VALIDATE MEDIA NODE                          
         JNZ   *+16                                                             
         GOTOR PUTERR,CE#INVMN                                                  
         J     EXITN                                                            
CHKMED   USING CFMPAS,IOKEY                                                     
         XC    CHKMED.CFMPAS,CHKMED.CFMPAS                                      
         MVI   CHKMED.CFMPTYPE,CFMPTYPQ                                         
         MVC   CHKMED.CFMPAGY,TWAAGY                                            
         MVC   CHKMED.CFMPRNOD,MNODE                                            
         XC    CHKMED.CFMPRNOD,EFFS                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOHI+IODIR+IO3'                               
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   CHKMED.CFMPAS(CFMPPNOD-CFMPAS),IOKEYSAV                          
         JE    *+16                                                             
         GOTOR PUTERR,CE#INVMN                                                  
         J     EXITN                                                            
         DROP  CHKMED                                                           
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOFIL+IO3'                              
         JE    *+6                                                              
         DC    H'0'                                                             
         L     R2,IOADDR                                                        
         CLI   CFMKSUBR-CFMRECD(R2),CFMKSMED                                    
         JE    EXITY                                                            
         GOTOR PUTERR,CE#INVMN                                                  
         J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* CALL LINKIO TO BUILD NODE RETURN ELEMENT                            *         
***********************************************************************         
                                                                                
PUTNOD   NTR1  LABEL=NO                                                         
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTRAW',D#UPLNOD),     *        
               ('LD_UBINQ',NODEWORK),(L'NODEWORK,0)                             
         J     EXITY                                                            
                                                                                
***********************************************************************         
* CALL LINKIO TO BUILD ERROR RETURN ELEMENT                           *         
***********************************************************************         
                                                                                
PUTERR   NTR1  LABEL=NO                                                         
         STCM  R1,3,WORK                                                        
         GOTOR LINKIO,DMCB,('LIOAPUT',ALIOB),('LIOTERR',D#UPLERR),     *        
               WORK,0                                                           
PUTERRX  J     EXITY                                                            
                                                                                
EXITN    LHI   RE,0                                                             
         J     EXITCC                                                           
EXITY    LHI   RE,1                                                             
EXITCC   CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
         EJECT                                                                  
GLOBALS  DS    0D                  ** GLOBAL LITERALS **                        
         LTORG                                                                  
                                                                                
RANDOM   DC    C'RANDOM'                                                        
REC#     DC    C'REC#'                                                          
GENFIL   DC    C'GENFIL  '                                                      
                                                                                
D#UPLERR EQU   1                                                                
D#UPLNOD EQU   2                                                                
                                                                                
RECTAB   DS    0XL(RECTABL)        ** RECORD TABLE **                           
         DC    AL2(I#MFMMLN),AL1(RECTTMLN)                                      
         DC    AL2(I#MFMMSR),AL1(RECTTMSR)                                      
         DC    AL2(I#MFMMMP),AL1(RECTTMMP)                                      
         DC    AL2(I#MFMMCP),AL1(RECTTMCP)                                      
         DC    AL2(I#MFMMBP),AL1(RECTTMBP)                                      
         DC    AL2(I#MFMMVP),AL1(RECTTMVP)                                      
RECTABN  EQU   (*-RECTAB)/L'RECTAB                                              
                                                                                
RECTABD  DSECT                     ** DSECT TO COVER RECORD TABLE **            
RECTMAP# DS    AL2                 RECORD MAP NUMBER                            
RECTTYPE DS    AL1                 ** RECORD TYPE **                            
RECTTMLN EQU   1                   MAINTAIN LEVEL NAMES                         
RECTTMSR EQU   2                   MAINTAIN STRUCTURE RECORDS                   
RECTTMMP EQU   3                   MAINTAIN MEDIA POINTERS                      
RECTTMCP EQU   4                   MAINTAIN CLIENT POINTERS                     
RECTTMBP EQU   5                   MAINTAIN BRAND POINTERS                      
RECTTMVP EQU   6                   MAINTAIN VENDOR POINTERS                     
RECTABL  EQU   *-RECTABD                                                        
SVRDEF   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* REQUEST MAP FOR MAINTAIN LEVEL NAMES                                *         
***********************************************************************         
                                                                                
CFMMLN   LKMAP H,I#MFMMLN,NEWREC=Y                                              
ActCd    LKMAP F,1,CHAR,CT#CFMAC,OUTPUT=(D,B#SAVED,ACTION)                      
RecTy    LKMAP F,2,UBIN,CT#RECTY,OUTPUT=(D,B#SAVED,RECORD)                      
LvNam    LKMAP F,3,VSTR,CT#LVNAM,OUTPUT=(D,B#SAVED,RNAME),LOWERCASE=Y           
         LKMAP E                                                                
                                                                                
***********************************************************************         
* REQUEST MAP FOR MAINTAIN STRUCTURE RECORDS                          *         
***********************************************************************         
                                                                                
CFMMSR   LKMAP H,I#MFMMSR,NEWREC=Y                                              
ActCd    LKMAP F,1,CHAR,CT#CFMAC,OUTPUT=(D,B#SAVED,ACTION)                      
RecTy    LKMAP F,2,UBIN,CT#RECTY,OUTPUT=(D,B#SAVED,RECORD)                      
RNode    LKMAP F,3,UBIN,CT#RCONT,OUTPUT=(D,B#SAVED,RNODE)                       
NNode    LKMAP F,4,UBIN,CT#NCONT,OUTPUT=(D,B#SAVED,PNODE)                       
RCode    LKMAP F,5,CHAR,CT#RCODE,OUTPUT=(D,B#SAVED,RCODE),LOWERCASE=Y           
RName    LKMAP F,6,VSTR,CT#RNAME,OUTPUT=(D,B#SAVED,RNAME),LOWERCASE=Y, *        
               MAXLEN=L'RNAME                                                   
RIDNo    LKMAP F,10,UBIN,CT#RID#,OUTPUT=(D,B#SAVED,RID#)                        
PIDNo    LKMAP F,11,UBIN,CT#PID#,OUTPUT=(D,B#SAVED,PID#)                        
         LKMAP E                                                                
                                                                                
***********************************************************************         
* REQUEST MAP FOR MAINTAIN MEDIA POINTERS                             *         
***********************************************************************         
                                                                                
CFMMMP   LKMAP H,I#MFMMMP,NEWREC=Y                                              
ActCd    LKMAP F,1,CHAR,CT#CFMAC,OUTPUT=(D,B#SAVED,ACTION)                      
RNode    LKMAP F,2,UBIN,CT#RCONT,OUTPUT=(D,B#SAVED,RNODE)                       
RCode    LKMAP F,3,CHAR,CT#MEDC,OUTPUT=(D,B#SAVED,RCODE),LOWERCASE=Y            
RName    LKMAP F,4,VSTR,CT#MEDN,OUTPUT=(D,B#SAVED,RNAME),LOWERCASE=Y,  *        
               MAXLEN=L'RNAME                                                   
MedSy    LKMAP F,5,CHAR,CT#CFMMS,OUTPUT=(D,B#SAVED,SYSTEM)                      
Media    LKMAP F,6,CHAR,CT#CFMMM,OUTPUT=(D,B#SAVED,MEDIA)                       
         LKMAP E                                                                
                                                                                
***********************************************************************         
* REQUEST MAP FOR MAINTAIN CLIENT POINTERS                            *         
***********************************************************************         
                                                                                
CFMMCP   LKMAP H,I#MFMMCP,NEWREC=Y                                              
ActCd    LKMAP F,1,CHAR,CT#CFMAC,OUTPUT=(D,B#SAVED,ACTION)                      
RNode    LKMAP F,2,UBIN,CT#RCONT,OUTPUT=(D,B#SAVED,RNODE)                       
NNode    LKMAP F,3,UBIN,CT#MCONT,OUTPUT=(D,B#SAVED,MNODE)                       
CliCd    LKMAP F,4,CHAR,CT#CFMCM,OUTPUT=(D,B#SAVED,CLIENT)                      
CliHx    LKMAP F,5,HEXD,CT#CFMIC,OUTPUT=(D,B#SAVED,ICLIENT)                     
RIDNo    LKMAP F,10,UBIN,CT#RID#,OUTPUT=(D,B#SAVED,RID#)                        
         LKMAP E                                                                
                                                                                
***********************************************************************         
* REQUEST MAP FOR MAINTAIN BRAND POINTERS                             *         
***********************************************************************         
                                                                                
CFMMBP   LKMAP H,I#MFMMBP,NEWREC=Y                                              
ActCd    LKMAP F,1,CHAR,CT#CFMAC,OUTPUT=(D,B#SAVED,ACTION)                      
RNode    LKMAP F,2,UBIN,CT#RCONT,OUTPUT=(D,B#SAVED,RNODE)                       
ANode    LKMAP F,3,UBIN,CT#ACONT,OUTPUT=(D,B#SAVED,ANODE)                       
MNode    LKMAP F,4,UBIN,CT#MCONT,OUTPUT=(D,B#SAVED,MNODE)                       
CliCd    LKMAP F,5,CHAR,CT#CFMCM,OUTPUT=(D,B#SAVED,CLIENT)                      
PrdCd    LKMAP F,6,CHAR,CT#CFMBM,OUTPUT=(D,B#SAVED,BRAND)                       
PrdHx    LKMAP F,7,HEXD,CT#CFMIB,OUTPUT=(D,B#SAVED,IBRAND)                      
RIDNo    LKMAP F,10,UBIN,CT#RID#,OUTPUT=(D,B#SAVED,RID#)                        
AIDNo    LKMAP F,11,UBIN,CT#AID#,OUTPUT=(D,B#SAVED,AID#)                        
         LKMAP E                                                                
                                                                                
***********************************************************************         
* REQUEST MAP FOR MAINTAIN VENDOR POINTERS                            *         
***********************************************************************         
                                                                                
CFMMSP   LKMAP H,I#MFMMVP,NEWREC=Y                                              
ActCd    LKMAP F,1,CHAR,CT#CFMAC,OUTPUT=(D,B#SAVED,ACTION)                      
RNode    LKMAP F,2,UBIN,CT#RCONT,OUTPUT=(D,B#SAVED,PNODE)                       
MNode    LKMAP F,3,UBIN,CT#MCONT,OUTPUT=(D,B#SAVED,MNODE)                       
SCode    LKMAP F,4,CHAR,CT#CFMSC,OUTPUT=(D,B#SAVED,RCODE)                       
MktNo    LKMAP F,6,CHAR,CT#SMKTN,OUTPUT=(D,B#SAVED,RMKT)                        
StaCd    LKMAP F,7,CHAR,CT#SSTAC,OUTPUT=(D,B#SAVED,RSTA)                        
CblCh    LKMAP F,8,CHAR,CT#SSTAC,OUTPUT=(D,B#SAVED,RCBL)                        
VName    LKMAP F,5,VSTR,CT#LVNAM,OUTPUT=(D,B#SAVED,RNAME),LOWERCASE=Y, *        
               MAXLEN=L'RNAME                                                   
RIDNo    LKMAP F,10,UBIN,CT#RID#,OUTPUT=(D,B#SAVED,RID#)                        
         LKMAP E                                                                
                                                                                
         LKMAP X                                                                
         EJECT                                                                  
SAVED    DSECT                                                                  
                                                                                
ALIOB    DS    A                                                                
LINKIO   DS    A                                                                
                                                                                
TODAYC   DS    XL2                 TODAY'S DATE - COMPRESSED                    
                                                                                
RECTYPE  DS    AL(L'RECTTYPE)      RECORD TYPE                                  
                                                                                
ACTION   DS    C                   ** ACTION CODE **                            
ACTADDQ  EQU   C'A'                ADD A RECORD                                 
ACTCHAQ  EQU   C'C'                CHANGE A RECORD                              
ACTDELQ  EQU   C'D'                DELETE A RECORD                              
ACTMOVQ  EQU   C'M'                MOVE A RECORD                                
                                                                                
RECORD   DS    XL(L'CFMKSUBR)      RECORD TYPE                                  
                                                                                
RID#     DS    XL2                 RECORD ID#                                   
PID#     DS    XL2                 PARENT ID#                                   
AID#     DS    XL2                 ADVERTISER ID#                               
                                                                                
RNODE    DS    XL(L'CFMKPNOD)      RECORD NODE                                  
PNODE    DS    XL(L'CFMKPNOD)      PARENT NODE                                  
ANODE    DS    XL(L'CFMKPNOD)      ADVERTISER NODE                              
MNODE    DS    XL(L'CFMKPNOD)      MEDIA NODE                                   
RCODE    DS    CL(L'CFMKCODE)      RECORD CODE                                  
         ORG   RCODE                                                            
RMKT     DS    CL(L'CFMKSMKT)      SPOTPAK MARKET NUMBER                        
RSTA     DS    CL(L'CFMKSSTA)      SPOTPAK STATION CODE                         
RCBL     DS    CL(L'CFMKSCBL)      SPOTPAK CABLE CHANNEL                        
         ORG                                                                    
RNAME    DS    CL(L'LNMNAME)       RECORD NAME                                  
                                                                                
SYSTEM   DS    C                   MEDIA SYSTEM                                 
MEDIA    DS    CL(L'MPTRMED)       MEDIA CODE                                   
CLIENT   DS    CL(L'CFMKCLTE)      CLIENT CODE                                  
ICLIENT  DS    XL(L'CFMKCLTI)      INTERNAL CLIENT CODE                         
BRAND    DS    CL(L'CFMKBRDE)      BRAND CODE                                   
IBRAND   DS    XL(L'CFMKBRDI)      INTERNAL BRAND CODE                          
                                                                                
PENDVALS DS    0X                  ** PENDING VALUES **                         
                                                                                
PENDKEY  DS    XL(L'CFMKEY)        PENDING KEY                                  
PENDACTN DS    C                   ** PENDING ACTION **                         
PENDAADD EQU   ACTADDQ             ADD                                          
PENDACHA EQU   ACTCHAQ             CHANGE                                       
                                                                                
PENDVALL EQU   *-PENDVALS                                                       
                                                                                
KEYWORK  DS    XL(L'IOKEY)                                                      
PASWORK  DS    XL(L'IOKEY)                                                      
NODEWORK DS    XL(L'CFMKRNOD)                                                   
                                                                                
SAVEL    EQU   *-SAVED                                                          
                                                                                
* INCLUDED DSECTS                                                               
         PRINT OFF                                                              
       ++INCLUDE GELNKWRK                                                       
       ++INCLUDE GEMSGEQUS                                                      
       ++INCLUDE DDLINKIOD                                                      
         PRINT ON                                                               
                                                                                
TWAD     DSECT                                                                  
         ORG   TWAUSER                                                          
SVVALS   DS    0X                  ** SAVED VALUES **                           
SVVALL   EQU   *-SVVALS                                                         
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002GELNK11   11/29/12'                                      
         END                                                                    
