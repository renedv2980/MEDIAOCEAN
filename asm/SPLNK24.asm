*          DATA SET SPLNK24    AT LEVEL 010 AS OF 09/12/17                      
*PHASE T21E24A                                                                  
SPLNK24  TITLE 'DDS Desktop - Order records upload'                             
         PRINT NOGEN                                                            
SVRDEF   CSECT                                                                  
         LKSVR TYPE=U,CODE=CODE,REQUEST=*,SYSTEM=SPTSYSQ,LINKIO=Y,     +        
               WORKERKEY=SPCB,RLEN=512,                                +        
               BLOCKS=(B#SAVED,SAVED,B#WORKD,WORKD)                             
                                                                                
CODE     NMOD1 0,**SL24**,RR=RE                                                 
         LR    R7,R1                                                            
         USING LP_D,R7             R7=A(LP_D)                                   
         L     RF,LP_ARUNP                                                      
         USING RUNPARMD,RF         RF=A(RUNPARMS)                               
         SR    R6,R6                                                            
         ICM   R6,7,RUNPARUN                                                    
         USING RUNFACSD,R6         R6=A(RUNFACS)                                
         L     R9,LP_ABLK1                                                      
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         L     R8,LP_ABLK2                                                      
         USING SAVED,R8            R8=A(SAVE W/S)                               
         LARL  RA,GLOBALS                                                       
         USING GLOBALS,RA          RA=A(GLOBAL LITERALS)                        
         ST    RE,SRVRRELO         SAVE PROGRAM RELOCATION FACTOR               
         L     R2,AIO3             POINT R2 TO RECORD/R3 TO ELEMENT             
         LA    R3,CMPFRST-CMPRECD(R2)                                           
         STM   R2,RB,LP_R2RB       SAVE REGISTERS FOR SUB-ROUTINES              
                                                                                
         CLI   RUNPMODE,RINIREQQ   TEST 'INITIALIZE' MODE                       
         JNE   EXITY                                                            
         DROP  RF                                                               
                                                                                
         LA    R0,SAVED            INITIALIZE SAVED STORAGE                     
         LHI   R1,SAVEL                                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
         L     RF,LP_ACOM          EXTRACT A(LINKIO) FROM COMFACS               
         MVC   LINKIO,CLINKIO-COMFACSD(RF)                                      
         MVC   RECUP,CRECUP-COMFACSD(RF)                                        
         MVC   GETFACT,CGETFACT-COMFACSD(RF)                                    
         J     EXITY                                                            
         DROP  R6,RB                                                            
         EJECT                                                                  
***********************************************************************         
* MARKET MAPPING RECORD - X'02A0'                                               
***********************************************************************         
                                                                                
MMPREC   LKREQ H,I#COMKMP,NEWREC=Y,NEXTREQ=SMPREC,ROUTINE=BLDCMP                
                                                                                
Action   LKREQ F,1,(D,B#SAVED,QACTION),(R,VALACTCD),                   +        
               OLEN=L'QACTION,TEXT=SP#ACTN                                      
Media    LKREQ F,2,(D,B#WORKD,QMEDX),(U,#VALMED,$VALMED),TEXT=SP#MED            
MktNam   LKREQ F,3,(D,B#SAVED,QMAPTEXT),CHAR,TEXT=SP#MKNAM,            +        
               MAXLEN=L'QMAPTEXT                                                
DDSMkt   LKREQ F,4,(D,B#SAVED,QMKT),UBIN,TEXT=SP#MKT                            
                                                                                
         LKREQ E                                                                
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE AND BUILD CANADIAN MAPPING RECORDS                        
***********************************************************************         
                                                                                
         USING DMPRECD,R2                                                       
BLDCMP   XC    EORADD,EORADD       SET NO RE-ENTRY POINT                        
         CLI   FATALERR,C'Y'                                                    
         JE    EXITY                                                            
         OC    QMAPTEXT,SPACES                                                  
                                                                                
K        USING DMPKEY,IOKEY        BUILD KEY FROM INPUT DATA                    
         XC    K.DMPKEY,K.DMPKEY                                                
         MVI   K.DMPKTYPE,DMPKTYPQ  X'0D' - RECORD TYPE                         
         MVC   K.DMPKAGMD,QMEDX                                                 
         MVC   K.DMPKTEXT,QMAPTEXT                                              
*                                                                               
         MVI   K.DMPKSTYP,DMPKSTMQ  X'0D' - MARKET NAME MAPPING                 
         CLC   LP_QMAPN,M#COSTMP   Test station mapping                         
         JNE   *+8                                                              
         MVI   K.DMPKSTYP,DMPKSTSQ  X'0E' - STATION NAME APPING                 
         CLC   LP_QMAPN,M#CODMMP   TEST demo mapping                            
         JNE   BLDCMPHI                                                         
         MVI   K.DMPKSTYP,DMPKSTDQ  X'0F' - DEMOCAT NAME MAPPING                
         L     R3,AAGYREC                                                       
         USING AGYRECD,R3                                                       
         MVC   K.DMPKAGMD,AGYMEDBT                                              
         NI    K.DMPKAGMD,X'F0'     NO MEDIA FOR DEMO, JUST AGY HEX             
         DROP  R3                                                               
         DROP  R2                                                               
*                                                                               
BLDCMPHI GOTOR (#IOEXEC,AIOEXEC),'IOHID+IOXSPDIR+IO3'                           
         J     BLDCMP10                                                         
BLDCMPSQ GOTOR (#IOEXEC,AIOEXEC),'IOSQD+IOXSPDIR+IO3'                           
*                                                                               
BLDCMP10 CLC   IOKEYSAV(DMPKSEQ-DMPKEY),IOKEY                                   
         JNE   BLDCMP20            No match on the entered text                 
         GOTOR (#IOEXEC,AIOEXEC),'IOGET+IOXSPFIL+IO3+IORDEL'                    
         JE    BLDCMP13                                                         
         TM    IOERR,IOEDEL                                                     
         JZ    BLDCMPH0            ALL OTHER ERROR DIE                          
*                                                                               
BLDCMP13 L     R6,AIO3                                                          
         USING DMPKEY,R6                                                        
         LA    R6,DMPFRST                                                       
BLDCMP16 CLI   0(R6),0             EOR?                                         
         JE    BLDCMPSQ            Yes, check next record                       
         CLI   0(R6),DMPDSELQ      X'01' - Description element                  
         JE    BLDCMP19                                                         
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         J     BLDCMP16                                                         
*                                                                               
         USING DMPDSELD,R6                                                      
BLDCMP19 CLC   QMAPTEXT,DMPDTEXT   ALL TEXT MAPPINGS ARE DISPLACED HERE         
         JE    BLDCMP50            We have a match, record exists               
         J     BLDCMPSQ                                                         
         DROP  R6                                                               
*********                                                                       
* Record does NOT exist on file                                                 
*********                                                                       
BLDCMP20 CLI   QACTION,QACTADD     Adding this non-existent rec?                
         JNE   EM#NOMMP            No, can't modify a non-existent rec          
         L     RF,AIO3             INITIALIZE A RECORD                          
         XC    0(256,RF),0(RF)                                                  
         MVC   0(L'DMPKEY,RF),IOKEYSAV                                          
         MVC   DMPKSEQ-DMPKEY(2,RF),=X'FFFF'   DEFAULT FOR 1ST MAPPING          
         LHI   R0,DMPFRST+1-DMPKEY                                              
         STCM  R0,3,DMPLEN-DMPRECD(RF)                                          
         MVC   DMPAGY-DMPRECD(,RF),LP_AGY                                       
         LA    R3,DMPFRST-DMPKEY(RF)  R3=A(1st element on record)               
*                                                                               
         LA    R6,ELEM                                                          
         USING DMPDSELD,R6                                                      
         XC    ELEM,ELEM                                                        
         MVI   DMPDSEL,DMPDSELQ    X'01' elem                                   
         MVI   DMPDSLEN,DMPDSLNQ                                                
         MVC   DMPDTEXT,QMAPTEXT                                                
         GOTOR VDATCON,DMCB,(5,0),(2,DMPDSADT)                                  
         MVC   QDATTIM(L'DMPDSADT),DMPDSADT  save off date for later            
         LA    R4,WORK2            only want first 4 bytes                      
         LA    R5,WORK                                                          
         TIME  BIN,(R4),MF=(E,(R5)),LINKAGE=SYSTEM                              
         MVC   DMPDSATM,WORK2                                                   
*                                                                               
         GOTOR GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   DMPDSABY,FAPASSWD                                                
         DROP  R1                                                               
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(L'DMPDSADT+L'DMPDSATM),DMPDSADT                             
         MVC   WORK+L'DMPDSADT+L'DMPDSATM(L'DMPDSABY),DMPDSABY                  
         DROP  R6                                                               
*                                                                               
         GOTOR RECUP,DMCB,(X'FE',AIO3),ELEM,0(R3),=X'002A00200F87'              
*                                                                               
         LLC   R0,1(R3)            POINT TO NEXT POSITION FOR ELEM              
         AR    R3,R0                                                            
*                                                                               
         LA    R6,ELEM             Layout for all 3 types of X'10' are          
         USING DMPMKELD,R6            basically the same except for the         
         MVI   DMPMKEL,DMPMKELQ       same except for the data @ end            
         MVC   DMPMKADT(L'DMPDSADT+L'DMPDSATM),WORK  Use date/time              
*                                                                               
         MVI   DMPMKLEN,DMPMKLNQ                                                
         MVC   DMPMKMKT,QMKT                                                    
         CLC   LP_QMAPN,M#COSTMP   TEST STATION MAPPING                         
         JNE   BLDCMP25                                                         
         USING DMPSTELD,R6                                                      
         CLI   QMEDA,C'N'          TEST NETWORK                                 
         JNE   *+8                                                              
         MVI   QSTA+4,C'N'                                                      
         GOTOR (#VALSTA,AVALSTA),DMCB,QSTA,L'QSTA,SVBSTA                        
         JNE   EM#INVST                                                         
         MVI   DMPSTLEN,DMPSTLNQ                                                
         MVC   DMPSTSTA,SVBSTA                                                  
         J     BLDCMP30                                                         
*                                                                               
BLDCMP25 MVI   K.DMPKSTYP,DMPKSTSQ  X'0E' - STATION NAME APPING                 
         CLC   LP_QMAPN,M#CODMMP   TEST DEMO MAPPING                            
         JNE   BLDCMP30                                                         
         USING DMPDMELD,R6                                                      
         MVI   DMPDMLEN,DMPDMLNQ                                                
         MVC   DMPDMDMO,QDMO                                                    
         DROP  R6                                                               
*                                                                               
BLDCMP30 GOTOR RECUP,DMCB,(X'FE',AIO3),ELEM,0(R3),=X'002A00200F87'              
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(DMPKSEQ-DMPKEY),IOKEYSAV  RD4UPD FOR NEXT SEQ #            
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUPD+IOXSPDIR+IO3'                         
         CLC   IOKEY(DMPKSEQ-DMPKEY),IOKEYSAV                                   
         JNE   BLDCMP40                                                         
         XR    RF,RF                                                            
         ICM   RF,3,IOKEY+DMPKSEQ-DMPKEY                                        
         JZ    BLDCMPH0            Die if sequence is already 0                 
         BCTR  RF,0                                                             
         L     R6,AIO3                                                          
         STCM  RF,3,DMPKSEQ-DMPKEY(R6)                                          
*                                                                               
BLDCMP40 L     R6,AIO3              BUILD PRIMARY KEY IN THE COPY               
         XC    K.DMPKEY,K.DMPKEY                                                
         MVI   K.DMPKTYPE,DMPKTYPQ  X'0D' - RECORD TYPE                         
*                                                                               
         MVI   K.DMPKSTYP,DMPKSTMQ  X'0D' - MARKET NAME MAPPING                 
         MVC   K.DMPKAGMD,QMEDX                                                 
         MVC   K.DMPKTEXT,QMAPTEXT                                              
         MVC   K.DMPKSEQ,DMPKSEQ-DMPKEY(R6)                                     
         CLC   LP_QMAPN,M#COSTMP   TEST STATION MAPPING                         
         JNE   *+8                                                              
         MVI   K.DMPKSTYP,DMPKSTSQ  X'0E' - STATION NAME APPING                 
         CLC   LP_QMAPN,M#CODMMP   TEST DEMO MAPPING                            
         JNE   BLDCMP45                                                         
         MVI   K.DMPKSTYP,DMPKSTDQ  X'0F' - DEMOCAT NAME MAPPING                
         L     R3,AAGYREC                                                       
         USING AGYRECD,R3                                                       
         MVC   K.DMPKAGMD,AGYMEDBT                                              
         NI    K.DMPKAGMD,X'F0'     NO MEDIA FOR DEMO, JUST AGY HEX             
         DROP  R3                                                               
*                                                                               
BLDCMP45 MVC   0(L'DMPKEY,R6),IOKEY   PRIMARY KEY                               
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOADDREC+IOXSPFIL+IO3'                        
         JNE   BLDCMPH0                                                         
         MVC   FULL2,IODA          SAVE DISK ADDRESS                            
*                                                                               
         XC    K.DMPKEY,K.DMPKEY                                                
         MVI   K.DMPKTYPE,DMPKTYPQ  X'0D' - RECORD TYPE                         
         MVC   K.DMPPKAM,QMEDX                                                  
*                                                                               
         MVI   K.DMPPKSTY,DMPPKSMQ  X'8D' - MARKET NAME MAPPING PSV             
         CLC   LP_QMAPN,M#COSTMP    TEST STATION MAPPING                        
         JNE   *+8                                                              
         MVI   K.DMPPKSTY,DMPPKSSQ  X'8E' - STATION NAME MAPPING PSV            
         CLC   LP_QMAPN,M#CODMMP    TEST DEMO MAPPING                           
         JNE   BLDCMP47                                                         
         MVI   K.DMPPKSTY,DMPPKSDQ  X'8F' - DEMOCAT NAME MAPPING PSV            
         L     R3,AAGYREC                                                       
         USING AGYRECD,R3                                                       
         MVC   K.DMPPKAM,AGYMEDBT                                               
         NI    K.DMPPKAM,X'F0'      NO MEDIA FOR DEMO, JUST AGY HEX             
         DROP  R3                                                               
*                                                                               
BLDCMP47 MVC   K.DMPPKADT,QDATTIM                                               
         MVC   K.DMPPKTXT,QMAPTEXT                                              
         MVC   K.DMPPKSEQ,DMPKSEQ-DMPKEY(R6)                                    
         MVC   K.DMPKAGY,LP_AGY                                                 
         MVC   K.DMPKDA,FULL2                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOXSPDIR+IO3'                           
         JNE   BLDCMPH0                                                         
         J     BLDCMPX                                                          
*********                                                                       
* Record exists on file                                                         
*********                                                                       
BLDCMP50 DS    0H                                                               
         LA    R6,ELEM                                                          
         USING DMPDSELD,R6                                                      
         XC    ELEM,ELEM                                                        
         MVI   DMPDSEL,DMPDSELQ    X'01' elem                                   
         MVI   DMPDSLEN,DMPDSLNQ                                                
         MVC   DMPDTEXT,QMAPTEXT                                                
         GOTOR VDATCON,DMCB,(5,0),(2,DMPDSADT)                                  
         MVC   QDATTIM(L'DMPDSADT),DMPDSADT                                     
         LA    R4,WORK2            ONLY WANT FIRST 4 BYTES                      
         LA    R5,WORK                                                          
         TIME  BIN,(R4),MF=(E,(R5)),LINKAGE=SYSTEM                              
         MVC   DMPDSATM,WORK2                                                   
*                                                                               
         GOTOR GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   DMPDSABY,FAPASSWD                                                
         DROP  R1                                                               
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(L'DMPDSADT+L'DMPDSATM),DMPDSADT                             
         MVC   WORK+L'DMPDSADT+L'DMPDSATM(L'DMPDSABY),DMPDSABY                  
*                                                                               
         CLI   QACTION,QACTADD     ADDING THIS EXISTENT REC?                    
         JNE   BLDCMP55            No                                           
         L     R6,AIO3                                                          
         TM    DMPSTTS-DMPKEY(R6),X'80'  REC PREVIOUSLY DELETED?                
         JZ    EM#CNTAD                 No, Give PC program an error            
         J     BLDCMP60                                                         
*                                                                               
BLDCMP55 MVC   DMPDSCDT(DMPDSCDT-DMPDSADT),WORK                                 
         DROP  R6                                                               
*                                                                               
BLDCMP60 GOTOR (#IOEXEC,AIOEXEC),'IOHIUPD+IOXSPDIR+IO3'                         
         CLI   QACTION,QACTADD                                                  
         JNE   BLDCMP63                                                         
         NI    IOKEY+DMPKSTAT-DMPKEY,X'FF'-X'80'                                
         GOTOR (#IOEXEC,AIOEXEC),'IOWRT+IOXSPDIR+IO3'                           
         JNE   BLDCMPH0                                                         
*                                                                               
BLDCMP63 GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOXSPFIL+IO3+IORDEL'                 
         MVC   FULL2,IODA          SAVE DISK ADDRESS                            
*                                                                               
         L     R6,AIO3                                                          
         USING DMPKEY,R6                                                        
         NI    DMPSTTS,X'FF'-X'80' remove deleted bit                           
         LA    R6,DMPFRST                                                       
BLDCMP65 CLI   0(R6),0             EOR?                                         
         JE    BLDCMPH0            Yes, check next record                       
         CLI   0(R6),DMPDSELQ      X'01' - Description element                  
         JE    BLDCMP70                                                         
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         J     BLDCMP65                                                         
*                                                                               
BLDCMP70 LR    R3,R6                                                            
         USING DMPDSELD,R6                                                      
         CLI   QACTION,QACTADD                                                  
         JNE   BLDCMP73                                                         
         MVC   DMPDSEL(DMPDSLNQ),ELEM   Ovrwrt current X'01' with new           
         LHI   R0,DMPFRST+1-DMPKEY+DMPDSLNQ   NEW RECORD LENGTH                 
         L     RF,AIO3                                                          
         STCM  R0,3,DMPLEN-DMPKEY(RF)                                           
         LLC   R0,1(R6)                                                         
         AR    R3,R0                                                            
         XC    0(256,R3),0(R3)          CLEAR ANY REMAINING X'10' ELEMS         
         J     BLDCMP80                                                         
*                                                                               
* Not an add, so modify the change activity data                                
BLDCMP73 MVC   DMPDSCDT(DMPDSCDT-DMPDSADT),ELEM+DMPDSCDT-DMPDSEL                
*                                                                               
BLDCMP76 LR    R3,R6                                                            
         LLC   R0,1(R6)                                                         
         AR    R3,R0               Adjust R3 to 1st X'10' elem                  
*                                                                               
         CLI   QACTION,QACTDEL     Test delete                                  
         JNE   BLDCMP80                                                         
         GOTOR DELREC                                                           
         J     BLDCMPX                                                          
*                                                                               
BLDCMP80 LA    R6,ELEM                                                          
         USING DMPMKELD,R6                                                      
         MVI   DMPMKEL,DMPMKELQ    X'10' elem                                   
         MVC   DMPMKADT(L'DMPDSADT+L'DMPDSATM),WORK  Use date/time              
*                                                                               
         MVI   DMPMKLEN,DMPMKLNQ                                                
         MVC   DMPMKMKT,QMKT                                                    
         CLC   LP_QMAPN,M#COSTMP   TEST STATION MAPPING                         
         JNE   BLDCMP85                                                         
         USING DMPSTELD,R6                                                      
         CLI   QMEDA,C'N'          TEST NETWORK                                 
         JNE   *+8                                                              
         MVI   QSTA+4,C'N'                                                      
         GOTOR (#VALSTA,AVALSTA),DMCB,QSTA,L'QSTA,SVBSTA                        
         JNE   EM#INVST                                                         
         MVI   DMPSTLEN,DMPSTLNQ                                                
         MVC   DMPSTSTA,SVBSTA                                                  
         J     BLDCMP90                                                         
*                                                                               
BLDCMP85 MVI   K.DMPKSTYP,DMPKSTSQ  X'0E' - STATION MAPPING                     
         CLC   LP_QMAPN,M#CODMMP   TEST DEMO MAPPING                            
         JNE   BLDCMP90                                                         
         USING DMPDMELD,R6                                                      
         MVI   DMPDMLEN,DMPDMLNQ                                                
         MVC   DMPDMDMO,QDMO                                                    
         DROP  R6                                                               
*                                                                               
BLDCMP90 GOTOR RECUP,DMCB,(X'FE',AIO3),ELEM,0(R3),=X'002A00200F87'              
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOPUT+IOXSPFIL+IO3'                           
*                                                                               
***   SEE IF WE NEED TO ADD PASSIVE KEY.  DATE MIGHT BE THE SAME AS             
***     CHANGES CAN HAPPEN ON THE SAME DAY                                      
*                                                                               
         L     R6,AIO3                                                          
         XC    K.DMPKEY,K.DMPKEY                                                
         MVI   K.DMPKTYPE,DMPKTYPQ  X'0D' - RECORD TYPE                         
         MVC   K.DMPPKAM,QMEDX                                                  
*                                                                               
         MVI   K.DMPPKSTY,DMPPKSMQ  X'8D' - MARKET NAME MAPPING PSV             
         CLC   LP_QMAPN,M#COSTMP    TEST STATION MAPPING                        
         JNE   *+8                                                              
         MVI   K.DMPPKSTY,DMPPKSSQ  X'8E' - STATION NAME MAPPING PSV            
         CLC   LP_QMAPN,M#CODMMP    TEST DEMO MAPPING                           
         JNE   BLDCMP91                                                         
         MVI   K.DMPPKSTY,DMPPKSDQ  X'8F' - DEMOCAT NAME MAPPING PSV            
         L     R3,AAGYREC                                                       
         USING AGYRECD,R3                                                       
         MVC   K.DMPPKAM,AGYMEDBT                                               
         NI    K.DMPPKAM,X'F0'      NO MEDIA FOR DEMO, JUST AGY HEX             
         DROP  R3                                                               
BLDCMP91 MVC   K.DMPPKADT,QDATTIM                                               
         MVC   K.DMPPKTXT,QMAPTEXT                                              
         MVC   K.DMPPKSEQ,DMPKSEQ-DMPKEY(R6)                                    
*                                                                               
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUPD+IOXSPDIR+IO3'                         
         CLC   IOKEY(L'DMPKEY),IOKEYSAV   DATE, TEXT, & SEQ EXISTS?             
         JNE   BLDCMP93                   NO, TIME TO ADD IT THEN               
*                                                                               
         TM    K.DMPKSTAT,X'80'     WAS IT DELETED?                             
         JZ    BLDCMPX              NO, THEN WE'RE DONE WITH PASSIVES           
         NI    K.DMPKSTAT,X'FF'-X'80' REMOVE THE DELETED BIT                    
         GOTOR (#IOEXEC,AIOEXEC),'IOWRT+IOXSPDIR+IO3'                           
         JNE   BLDCMPH0                                                         
         L     R6,AIO3              IF I HAD TO UNDELETE, MIGHT HAVE TO         
         XC    IOKEY,IOKEY            ALSO UNDELETE PRIMARY KEY                 
         MVC   IOKEY(L'DMPKEY),0(R6)                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUPD+IOXSPDIR+IO3'                         
         CLC   IOKEY(L'DMPKEY),IOKEYSAV   SAME PRIMARY KEY?                     
         JNE   BLDCMPH0                   IT BETTER BE OUT THERE                
*                                                                               
         TM    K.DMPKSTAT,X'80'       WAS IT DELETED?                           
         JZ    BLDCMPX              NO, THEN WE'RE DONE                         
         NI    K.DMPKSTAT,X'FF'-X'80' REMOVE THE DELETED BIT                    
         GOTOR (#IOEXEC,AIOEXEC),'IOWRT+IOXSPDIR+IO3'                           
         JNE   BLDCMPH0                                                         
         J     BLDCMPX                                                          
*                                                                               
BLDCMP93 MVC   IOKEY,IOKEYSAV                                                   
         MVC   K.DMPKAGY,LP_AGY                                                 
         MVC   K.DMPKDA,FULL2                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOXSPDIR+IO3'                           
         JNE   BLDCMPH0                                                         
*                                                                               
***   ALSO UNDELETE THE PASSIVE KEY OR ADD A NEW PASSIVE WITH NEW DATE          
*                                                                               
         XR    R1,R1                                                            
         L     R6,AIO3                                                          
         LA    R6,DMPFRST-DMPKEY(R6)                                            
BLDCMP94 CLI   0(R6),0                                                          
         JE    BLDCMPX              NOTHING TO DELETE                           
         CLI   0(R6),DMPMKELQ       X'10' - DATA ELEMENT?                       
         JE    BLDCMP96                                                         
BLDCMP95 LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         J     BLDCMP94                                                         
*                                                                               
BLDCMP96 CHI   R1,1                 WE WANT PREVIOUS ACTIVITY                   
         JNL   BLDCMP97                                                         
         LA    R1,1                 ON CURRENT DATA, SKIP TO THE NEXT           
         J     BLDCMP95                                                         
*                                                                               
BLDCMP97 MVC   K.DMPPKADT,DMPMKADT-DMPMKEL(R6)                                  
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUPD+IOXSPDIR+IO3'                         
         CLC   IOKEY(L'DMPKEY),IOKEYSAV   DATE, TEXT, & SEQ EXISTS?             
         JNE   BLDCMPX                    NO, DON'T WORRY ABOUT IT              
         OI    K.DMPKSTAT,X'80'       WAS IT DELETED?                           
         GOTOR (#IOEXEC,AIOEXEC),'IOWRT+IOXSPDIR+IO3'                           
         JNE   BLDCMPH0                                                         
*                                                                               
BLDCMPX  J     EXITY                                                            
*                                                                               
BLDCMPH0 DC    H'0'                                                             
         DROP  K                                                                
         EJECT                                                                  
***********************************************************************         
* STATION MAPPING RECORD - X'02A1'                                              
***********************************************************************         
                                                                                
SMPREC   LKREQ H,I#COSTMP,NEWREC=Y,NEXTREQ=DMPREC,ROUTINE=BLDCMP                
                                                                                
Action   LKREQ F,1,(D,B#SAVED,QACTION),(R,VALACTCD),                   +        
               OLEN=L'QACTION,TEXT=SP#ACTN                                      
Media    LKREQ F,2,(D,B#WORKD,QMEDX),(U,#VALMED,$VALMED),TEXT=SP#MED            
StaNam   LKREQ F,3,(D,B#SAVED,QMAPTEXT),CHAR,TEXT=SP#MKNAM,            +        
               MAXLEN=L'QMAPTEXT                                                
Station  LKREQ F,4,(D,B#SAVED,QSTA),CHAR,TEXT=SP#STNET                          
                                                                                
         LKREQ E                                                                
                                                                                
***********************************************************************         
* DEMO CATEGORY MAPPING RECORD - X'02A2'                                        
***********************************************************************         
                                                                                
DMPREC   LKREQ H,I#CODMMP,NEWREC=Y,NEXTREQ=CAMREC,ROUTINE=BLDCMP                
                                                                                
Action   LKREQ F,1,(D,B#SAVED,QACTION),(R,VALACTCD),                   +        
               OLEN=L'QACTION,TEXT=SP#ACTN                                      
DmoNam   LKREQ F,2,(D,B#SAVED,QMAPTEXT),CHAR,TEXT=SP#MKNAM,            +        
               MAXLEN=L'QMAPTEXT                                                
DDSDemo  LKREQ F,3,(D,B#SAVED,QDMO),UBIN,TEXT=SP#MKT                            
                                                                                
         LKREQ E                                                                
                                                                                
***********************************************************************         
* CAMPAIGN RECORD - MAPCODE X'0291'                                   *         
***********************************************************************         
                                                                                
CAMREC   LKREQ H,I#COCAMP,NEWREC=Y,NEXTREQ=SESREC,ROUTINE=BLDCAM                
                                                                                
Action   LKREQ F,1,(D,B#SAVED,QACTION),CHAR,TEXT=SP#ACTN                        
Media    LKREQ F,2,(D,B#WORKD,QMEDX),(U,#VALMED,$VALMED),TEXT=SP#MED            
Client   LKREQ F,3,(D,B#WORKD,QCLTX),(U,#VALCLT,$VALCLT),TEXT=SP#CLI            
Campaign LKREQ F,4,(D,B#SAVED,QCAMX),UBIN,TEXT=(*,TCAMNUM)                      
Descrptn LKREQ F,5,(I,B#SAVED,QADESC),VSTR,TEXT=(*,TCAMDESC),          +        
               LOWERCASE=Y                                                      
StrDate  LKREQ F,6,(D,B#SAVED,QCMPDSDT),BDAT,TEXT=(*,TSTART)                    
EndDate  LKREQ F,7,(D,B#SAVED,QCMPDEDT),BDAT,TEXT=(*,TENDDATE)                  
Product1 LKREQ F,8,(D,B#SAVED,QCMPDPR1),CHAR,TEXT=(*,TPRD1)                     
Product2 LKREQ F,9,(D,B#SAVED,QCMPDPR2),CHAR,TEXT=(*,TPRD2)                     
Status   LKREQ F,10,(D,B#SAVED,QCMPDSTA),HEXD,TEXT=(*,TSTATUS)                  
BuyType  LKREQ F,17,(D,B#SAVED,QCMPDTYP),CHAR,TEXT=(*,TBUYTYPE)                 
DayOfWk  LKREQ F,18,(D,B#SAVED,QCMPDDAY),CHAR,TEXT=(*,TDAYOFWK)                 
DptMenu  LKREQ F,19,(D,B#SAVED,QCMPDDPT),CHAR,TEXT=(*,TDPTMENU)                 
SourceBk LKREQ F,20,(D,B#SAVED,QCMPDSBK),BDAT,TEXT=(*,TBOOK)                    
ConvPro  LKREQ F,21,(D,B#SAVED,QCMPIDCP),LBIN,TEXT=(*,TCMPIDCP)                 
SpclPro  LKREQ F,22,(D,B#SAVED,QCMPIDSP),LBIN,TEXT=(*,TCMPIDSP)                 
Comment  LKREQ F,12,(I,B#SAVED,QACOMMS),VSTR,TEXT=(*,TCOMMENT),        +        
               LOWERCASE=Y                                                      
SpotLens LKREQ F,13,(I,B#SAVED,QASLNS),UBIN,OLEN=L'CMPSLSLN,LIST=F,    +        
               TEXT=(*,TVALSLN)                                                 
Demos    LKREQ F,14,(I,B#SAVED,QADEMS),(U,#VALDCD,$VALDCD),LIST=F,     +        
               OLEN=L'CMPDMDEM,SORT=NO,TEXT=(*,TDEMLIST)                        
FltStrt  LKREQ F,15,(I,B#SAVED,QAFLTS),BDAT,OLEN=L'CMPFLSDT,           +        
               ARRAY=S,TEXT=(*,TFLTSTRT)                                        
FltEnd   LKREQ F,16,,BDAT,OLEN=L'CMPFLEDT,TEXT=(*,TFLTEND),ARRAY=E              
                                                                                
         LKREQ E                                                                
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE AND BUILD CAMPAIGN RECORD                       *         
***********************************************************************         
                                                                                
         USING CMPRECD,R2                                                       
BLDCAM   LARL  RE,BLDCAM30         SET RE-ENTRY POINT                           
         ST    RE,EORADD                                                        
         CLI   FATALERR,C'Y'                                                    
         JE    EXITY                                                            
                                                                                
         CLI   QACTION,0           TEST ACTION GIVEN                            
         JNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
K        USING CMPKEY,IOKEY        BUILD CAMPAIGN KEY FROM INPUT DATA           
         XC    K.CMPKEY,K.CMPKEY                                                
         MVI   K.CMPKTYPE,CMPKTYPQ                                              
         MVI   K.CMPKSTYP,CMPKSTYQ                                              
         MVC   K.CMPKAGMD,QMEDX                                                 
*                                                                               
         MVC   WORK(1),QMEDX                                                    
         NI    WORK,X'0F'          ISOLATE BINARY MEDIA                         
         CLI   WORK,X'02'          REQUESTED RADIO?                             
         JE    BLDCAM05                                                         
         CLI   WORK,X'04'            OR NTWK RADIO?                             
         JE    BLDCAM05                                                         
*                                                                               
         NI    K.CMPKAGMD,X'F0'    NO, NETWORK AND TV SHOULD BE SAVED           
         OI    K.CMPKAGMD,X'08'      AS MEDIA 'C'                               
*                                                                               
BLDCAM05 MVC   K.CMPKCLT,QCLTX                                                  
         MVC   K.CMPKCAM,QCAMX                                                  
         XC    K.CMPKCAM,EFFS      GET FF COMPLEMENT                            
                                                                                
         CLI   QACTION,QACTADD     TEST ADDING A NEW RECORD                     
         JNE   BLDCAM20                                                         
                                                                                
         XC    K.CMPKCAM,K.CMPKCAM                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUPD+IOXSPDIR'                             
                                                                                
         CLC   IOKEY(CMPKCAM-CMPKEY),IOKEYSAV                                   
         JE    BLDCAM10                                                         
         MVC   IOKEY,IOKEYSAV      RESTORE KEY                                  
         LHI   R0,1                                                             
         STCM  R0,15,QCAMX         SET CAMPAIGN 1                               
         MVC   K.CMPKCAM,QCAMX                                                  
         XC    K.CMPKCAM,EFFS                                                   
         J     BLDCAM15                                                         
                                                                                
BLDCAM10 ICM   R0,15,K.CMPKCAM     GET CAMPAIGN NUMBER                          
         SHI   R0,1                INCREMENT                                    
         STCM  R0,15,QCAMX                                                      
         MVC   K.CMPKCAM,QCAMX                                                  
         XC    QCAMX,EFFS          QCAMX IS NOT COMPLEMENTED                    
                                                                                
BLDCAM15 MVC   SVCAMX,QCAMX        SAVE FOR LATER                               
         GOTOR INIREC,K.CMPKEY     INITIALIZE FOR NEW RECORD                    
         J     BLDCAM25                                                         
                                                                                
***********************************************************************         
* READ EXISTING CAMPAIGN                                              *         
***********************************************************************         
                                                                                
BLDCAM20 MVC   SVCAMX,QCAMX        SAVE FOR LATER                               
         GOTOR (#IOEXEC,AIOEXEC),'IORDUP+IOXSPDIR+IO3'                          
         JNE   EM#NOCAM                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOXSPFIL+IO3'                        
         JNE   EM#NOCAM                                                         
                                                                                
         CLI   QACTION,QACTDEL     TEST DELETE                                  
         JNE   BLDCAM25                                                         
         XC    EORADD,EORADD                                                    
         GOTOR DELREC                                                           
         J     EXITY                                                            
                                                                                
***********************************************************************         
* ADD DATA ELEMENTS TO CAMPAIGN RECORD                                *         
***********************************************************************         
                                                                                
BLDCAM25 GOTOR SAVINI              SAVE F1 AND DELETE ALL ELEMENTS              
                                                                                
         USING CMPIDELD,ELEM                                                    
         MVI   CMPIDEL,CMPIDELQ                                                 
         MVI   CMPIDLEN,CMPIDL2Q                                                
         MVC   CMPIDSDT(QCMPVALL),QCMPVALS                                      
         GOTOR NXTADD                                                           
                                                                                
         GOTOR MOVEIT,DMCB,('CMPDMELQ',QADEMS),('CMPDMDEM-CMPDMELD',0)          
         GOTOR MOVEIT,(R1),('CMPSLELQ',QASLNS),('CMPSLSLN-CMPSLELD',0)          
         GOTOR MOVEIT,(R1),('CMPFLELQ',QAFLTS),('CMPFLLST-CMPFLELD',0)          
         GOTOR MOVEIT,(R1),('CMPDSELQ',QADESC),('CMPDSDES-CMPDSELD',0)          
         GOTOR MOVEIT,(R1),('CMPCMELQ',QACOMMS),('CMPCMCOM-CMPCMELD',0)         
         J     EXITY                                                            
                                                                                
BLDCAM30 MVI   EORFLAG,C'N'                                                     
         GOTOR BLDACT              BUILD ACTIVITY ELEMENT                       
                                                                                
         CLI   QACTION,QACTADD     TEST ADD                                     
         JNE   BLDCAM35                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOXSPFIL+IO3'                           
         JE    BLDCAM40                                                         
         DC    H'0'                                                             
                                                                                
BLDCAM35 GOTOR (#IOEXEC,AIOEXEC),'IOPUT+IOXSPFIL+IO3'                           
         JE    BLDCAM40                                                         
         DC    H'0'                                                             
                                                                                
BLDCAM40 GOTOR LINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTMAP',1)                   
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTRAW',1),           +        
               ('LD_LBINQ',QCAMX),(L'QCAMX,0)                                   
         J     SENDDTTM                                                         
         EJECT                                                                  
***********************************************************************         
* CAMPAIGN STATION-ESTIMATE - MAPCODE X'0292'                         *         
***********************************************************************         
                                                                                
SESREC   LKREQ H,I#COSTES,NEWREC=Y,NEXTREQ=CSLREC,ROUTINE=BLDSES                
                                                                                
Action   LKREQ F,1,(D,B#SAVED,QACTION),CHAR,TEXT=SP#ACTN                        
Media    LKREQ F,2,(D,B#WORKD,QMEDX),(U,#VALMED,$VALMED),TEXT=SP#MED            
Client   LKREQ F,3,(D,B#WORKD,QCLTX),(U,#VALCLT,$VALCLT),TEXT=SP#CLI            
Campaign LKREQ F,4,(D,B#SAVED,QCAMX),UBIN,TEXT=(*,TCAMNUM)                      
StaType  LKREQ F,5,(I,B#SAVED,QASTAS),UBIN,OLEN=L'CSEKSTTY,            +        
               ARRAY=S,TEXT=(*,TSTATYPE)                                        
Station  LKREQ F,6,,CHAR,OLEN=L'CSEKSTA,TEXT=SP#STNET,ARRAY=E                   
Product  LKREQ F,7,(D,B#WORKD,QPRDX),(U,#VALPRD,$VALPRD),              +        
               TEXT=(*,TPRD)                                                    
Week     LKREQ F,10,(I,B#SAVED,QAWEEKS),CDAT,OLEN=L'CSESTWK,ARRAY=S,   +        
               TEXT=(*,TWEEKS)                                                  
Estimate LKREQ F,11,,UBIN,OLEN=L'CSESTEST,TEXT=(*,TESTLST),ARRAY=E              
                                                                                
         LKREQ E                                                                
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE AND BUILD STATION-ESTIMATE RECORDS              *         
***********************************************************************         
                                                                                
         USING CSERECD,R2                                                       
BLDSES   XC    EORADD,EORADD       SET NO RE-ENTRY POINT                        
         CLI   FATALERR,C'Y'                                                    
         JE    EXITY                                                            
                                                                                
         MVI   QTYPE,0             INITIALIZE FIELDS                            
         XC    QSTA5,QSTA5                                                      
                                                                                
         CLI   QACTION,0           TEST ACTION GIVEN                            
         JNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
         OC    QCAMX,QCAMX         MAY NOT BE PRESENT ON ADD                    
         JNZ   *+10                                                             
         OC    QCAMX,SVCAMX        SO USE SAVED IF NOT                          
         JNZ   *+6                                                              
         DC    H'0'                BUT IT'S GOT TO BE AROUND                    
                                                                                
K        USING CSEKEY,IOKEY        BUILD CAMPAIGN KEY FROM INPUT DATA           
         XC    K.CSEKEY,K.CSEKEY                                                
         MVI   K.CSEKTYPE,CSEKTYPQ                                              
         MVI   K.CSEKSTYP,CSEKSTYQ                                              
         MVC   K.CSEKAGMD,QMEDX                                                 
         MVC   K.CSEKCLT,QCLTX                                                  
         MVC   K.CSEKCAM,QCAMX                                                  
         XC    K.CSEKCAM,EFFS      SET FF COMPLEMENT                            
                                                                                
         SR    R5,R5               R5=NUMBER OF STATIONS                        
         ICM   R6,7,QASTAS+1       R6=A(STATIONS)                               
         JZ    BLDSES02                                                         
         ICM   R5,3,LW_NUMN-LW_D(R6)                                            
         AHI   R6,LW_LN2Q          R6=A(FIRST STATION)                          
                                                                                
BLDSES02 LTR   R5,R5               TEST ANY STATIONS GIVEN                      
         JNZ   BLDSES04                                                         
         CLI   QACTION,QACTDEL     DELETE REQUIRES NO STATIONS                  
         JE    BLDSES14                                                         
         DC    H'0'                ADD/CHANGE DO                                
                                                                                
BLDSES04 MVC   QTYPSTA,0(R6)       EXTRACT TYPE AND STATION                     
         MVC   K.CSEKSTTY(CSEKPRD-CSEKSTTY),0(R6)                               
         AHI   R6,L'QTYPSTA        POINT TO NEXT TYPE AND STATION               
                                                                                
         MVC   K.CSEKPRD,QPRDX     SET PRODUCT                                  
         CLI   K.CSEKPRD,FF                                                     
         JNE   *+8                                                              
         MVI   K.CSEKPRD,0                                                      
                                                                                
         CLI   QACTION,QACTDEL     TEST DELETE                                  
         JE    BLDSES14                                                         
                                                                                
***********************************************************************         
* TEST IF RECORD ALREADY EXISTS                                       *         
***********************************************************************         
                                                                                
BLDSES06 GOTOR (#IOEXEC,AIOEXEC),'IORDUPD+IOXSPDIR+IO3'                         
         JE    BLDSES10            READ FOR UPDATE IF FOUND                     
         TM    IOERR,IOEDEL                                                     
         JNZ   BLDSES12            READ FOR UPDATE IF DELETED                   
         TM    IOERR,IOERNF        OTHERWISE MUST BE NOT FOUND                  
         JNZ   *+6                                                              
         DC    H'0'                                                             
         MVI   QACTION,QACTADD     SET TO ADD NEW RECORD                        
         MVC   K.CSEKEY,IOKEYSAV                                                
                                                                                
BLDSES08 GOTOR INIREC,K.CSEKEY     INITIALIZE FOR NEW RECORD                    
         J     BLDSES20                                                         
                                                                                
BLDSES10 CLI   QACTION,QACTADD     CAN'T BE FOUND AND UNDELETED IF ADD          
         JNE   BLDSES12                                                         
         DC    H'0'                                                             
                                                                                
BLDSES12 GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOXSPFIL+IO3'                        
         JE    *+6                                                              
         DC    H'0'                                                             
         CLI   QACTION,QACTADD     TEST ORIGINAL ACTION WAS ADD                 
         JNE   *+8                                                              
         MVI   QACTION,QACTADR     YES - SET ADDING A DELETED RECORD            
         J     BLDSES20                                                         
                                                                                
***********************************************************************         
* DELETE RECORD FOR THIS STATION OR ALL STATIONS FOR THIS TYPE OR ALL *         
* STATIONS OF ANY TYPE                                                *         
***********************************************************************         
                                                                                
BLDSES14 GOTOR (#IOEXEC,AIOEXEC),'IOHIUP+IOXSPDIR+IO3'                          
                                                                                
BLDSES16 LHI   RF,CSEKSTTY-CSEKEY-1             KEYLEN FOR ALL                  
         CLI   QTYPE,0                          TEST TYPE GIVEN                 
         JE    *+8                              NO                              
         LHI   RF,CSEKSTTY+L'CSEKSTTY-CSEKEY-1  KEYLEN FOR STATYPE              
         OC    QSTA5,QSTA5                      TEST STATION GIVEN              
         JZ    *+8                              NO                              
         LHI   RF,CSEKSTA+L'CSEKSTA-CSEKEY-1    KEYLEN FOR STATION              
         BASR  RE,0                                                             
         CLC   IOKEY(0),IOKEYSAV                                                
         EX    RF,0(RE)                                                         
         JNE   BLDSES18                                                         
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOXSPFIL+IO3'                        
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR DELREC              DELETE THE RECORD                            
                                                                                
         OC    QSTA5,QSTA5         TEST STATION GIVEN                           
         JNZ   BLDSES26            YES - GET NEXT STATION                       
         GOTOR (#IOEXEC,AIOEXEC),'IOSQUPD+IOXSPDIR+IO3'                         
         J     BLDSES16                                                         
                                                                                
BLDSES18 CLI   QTYPE,0             TEST TYPE GIVEN                              
         JNE   BLDSES26            YES - GET NEXT TYPE/STATION                  
         J     BLDSES28            ELSE ALL DONE                                
                                                                                
***********************************************************************         
* ADD DATA ELEMENTS                                                   *         
***********************************************************************         
                                                                                
BLDSES20 GOTOR SAVINI              SAVE F1 AND DELETE ALL ELEMENTS              
         XC    SVACTEL,SVACTEL     WE DON'T CHECKSUM THIS RECORD                
                                                                                
         OC    QAWEEKS,QAWEEKS     TEST WE HAVE WEEKS                           
         JZ    BLDSES22                                                         
         LA    R0,SVWEEKS          YES - SAVE THEM                              
         ICM   RE,7,QAWEEKS+1                                                   
         LLH   R1,LW_LN-LW_D(RE)                                                
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
                                                                                
BLDSES22 GOTOR MOVEIT,DMCB,('CSEWKELQ',SVWEEKS),('CSESTLST-CSEWKELD',0)         
         JE    *+6                                                              
         DC    H'0'                WE MUST HAVE WEEKS                           
                                                                                
         GOTOR BLDACT              BUILD ACTIVITY ELEMENT                       
                                                                                
         CLI   QACTION,QACTADD     TEST ADD                                     
         JNE   BLDSES24                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOXSPFIL+IO3'                           
         JE    BLDSES26            DO NEXT STATION                              
         DC    H'0'                CAN'T ADD RECORD                             
                                                                                
BLDSES24 NI    CSESTAT,X'FF'-X'80'                                              
         GOTOR (#IOEXEC,AIOEXEC),'IOPUT+IOXSPFIL+IO3'                           
         JE    *+6                                                              
         DC    H'0'                CAN'T PUT RECORD                             
         TM    IOKEY+(CSEKSTAT-CSERECD),X'80'                                   
         JZ    BLDSES26                                                         
*                                  UNDELETE DIRECTORY RECORD                    
         NI    IOKEY+(CSEKSTAT-CSERECD),X'FF'-X'80'                             
         GOTOR (#IOEXEC,AIOEXEC),'IOWRT+IOXSPDIR+IO3'                           
         JE    BLDSES26            DO NEXT STATION                              
         DC    H'0'                CAN'T WRITE RECORD                           
                                                                                
BLDSES26 JCT   R5,BLDSES04         PROCESS NEXT STATION                         
                                                                                
BLDSES28 GOTOR LINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTMAP',1)                   
         J     SENDDTTM                                                         
         EJECT                                                                  
***********************************************************************         
* CAMPAIGN STATION LIST - MAPCODE X'0299'                             *         
***********************************************************************         
                                                                                
CSLREC   LKREQ H,I#COSTEL,NEWREC=Y,NEXTREQ=PGMREC,ROUTINE=BLDCSL                
                                                                                
Action   LKREQ F,1,(D,B#SAVED,QACTION),CHAR,TEXT=SP#ACTN                        
Media    LKREQ F,2,(D,B#WORKD,QMEDX),(U,#VALMED,$VALMED),TEXT=SP#MED            
Client   LKREQ F,3,(D,B#WORKD,QCLTX),(U,#VALCLT,$VALCLT),TEXT=SP#CLI            
Campaign LKREQ F,4,(D,B#SAVED,QCAMX),UBIN,TEXT=(*,TCAMNUM)                      
ListNum  LKREQ F,5,(D,B#SAVED,QLISTNUM),UBIN,TEXT=(*,TSTALIST)                  
Station  LKREQ F,6,(I,B#SAVED,QASTAS),CHAR,OLEN=L'CSLSTSTA,            +        
               LIST=F,TEXT=SP#STNET                                             
Product  LKREQ F,7,(I,B#SAVED,QAPRDS),(U,#VALPRD,$VALPRD),LIST=F,      +        
               TEXT=(*,TPRD),OLEN=L'CSLPRPRD                                    
Week     LKREQ F,8,(I,B#SAVED,QAWEEKS),CDAT,OLEN=L'CSLWEWSD,ARRAY=S,   +        
               TEXT=(*,TWEEKS)                                                  
Estimate LKREQ F,9,,UBIN,OLEN=L'CSLWEEST,TEXT=(*,TESTLST),ARRAY=E               
                                                                                
         LKREQ E                                                                
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE AND BUILD STATION LIST RECORD                   *         
***********************************************************************         
                                                                                
BLDCSL   XC    EORADD,EORADD       SET NO RE-ENTRY POINT                        
         CLI   FATALERR,C'Y'       EXIT NOW ON FATAL ERROR                      
         JE    EXITY                                                            
                                                                                
         L     R2,AIO3                                                          
         USING CSLRECD,R2          R2=A(LIST RECORD)                            
         LA    R3,CSLFRST          R3=A(FIRST ELEMENT ON RECORD)                
                                                                                
         OC    QCAMX,QCAMX         ESTABLISH CAMPAIGN NUMBER                    
         JNZ   *+10                                                             
         OC    QCAMX,SVCAMX        USE SAVED VALUE IF NOT GIVEN                 
         JNZ   *+6                                                              
         DC    H'0'                CAMPAIGN NOT SUPPLIED                        
         MVC   SVCAMX,QCAMX        SAVE CURRENT CAMPAIGN NUMBER                 
                                                                                
K        USING CSLKEY,IOKEY        BUILD KEY OF LIST RECORD                     
         XC    K.CSLKEY,K.CSLKEY                                                
         MVI   K.CSLKTYPE,CSLKTYPQ                                              
         MVI   K.CSLKSTYP,CSLKSTYQ                                              
         MVC   K.CSLKAGMD,QMEDX                                                 
         MVC   K.CSLKCLT,QCLTX                                                  
         MVC   K.CSLKCAM,QCAMX                                                  
         XC    K.CSLKCAM,EFFS                                                   
         MVC   K.CSLKLIST,QLISTNUM                                              
                                                                                
         CLI   QACTION,QACTDEL                                                  
         JE    *+8                                                              
         MVI   QACTION,QACTCHA     SET ACTION TO CHANGE IF NOT DELETE           
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORDUPD+IOXSPDIR+IO3'                         
         JE    BLDCSL04                                                         
*                                                                               
         CLI   QACTION,QACTDEL     TEST DELETE ACTION                           
         JNE   *+6                                                              
         DC    H'0'                YES - RECORD MUST BE FOUND                   
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         JNZ   BLDCSL02                                                         
         TM    IOERR,IOERNF        MUST BE NOT FOUND                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         XC    IOKEY,IOKEY                                                      
         MVC   K.CSLKEY,IOKEYSAV                                                
         MVI   QACTION,QACTADD     SET TO ADD RECORD                            
         J     BLDCSL06                                                         
                                                                                
BLDCSL02 NI    K.CSLKSTAT,FF-X'80' UNDELETE DIRECTORY RECORD                    
         MVI   QACTION,QACTADR     SET RECORD IS DELETED                        
                                                                                
BLDCSL04 GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOXSPFIL+IO3'                        
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLI   QACTION,QACTDEL     TEST DELETING A LIST                         
         JNE   BLDCSL06                                                         
                                                                                
         OI    K.CSLKSTAT,X'80'    DELETE DIRECTORY RECORD                      
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IOXSPDIR+IO3'                         
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         OI    CSLSTAT,X'80'       DELETE FILE RECORD                           
         GOTOR (#IOEXEC,AIOEXEC),'IOPUT+IOXSPFIL+IO3'                           
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOSQUP+IOXSPDIR+IO3'                          
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   K.CSLKEY(CSLKSEQN-CSLKEY),IOKEYSAV                               
         JE    BLDCSL04            DELETE ALL RECORDS FOR THIS LIST             
         J     BLDCSL28                                                         
                                                                                
BLDCSL06 GOTOR INIREC,K.CSLKEY     INITIALIZE STATION LIST RECORD               
                                                                                
         GOTOR MOVEIT,DMCB,('CSLWKELQ',QAWEEKS),('CSLWELST-CSLWKELD',0)         
         GOTOR MOVEIT,(R1),('CSLPRELQ',QAPRDS),('CSLPRPRD-CSLPRELD',0)          
                                                                                
         SR    R6,R6               BUILD STATION LIST ELEMENTS                  
         ICM   R6,7,QASTAS+1                                                    
         JNZ   *+6                                                              
         DC    H'0'                                                             
         LLH   R5,LW_NUMN-LW_D(R6) R5=NUMBER OF STATIONS IN LIST                
         AHI   R6,LW_LN2Q          R6=A(STATION LIST)                           
                                                                                
BLDCSL08 LR    R1,R5               R1=NUMBER OF STATIONS IN ELEMENT             
         CHI   R5,(255-(CSLSTSTA-CSLSTELD))/L'CSLSTSTA                          
         JNH   *+8                                                              
         LHI   R1,(255-(CSLSTSTA-CSLSTELD))/L'CSLSTSTA                          
         SR    R5,R1               DECREMENT NUMBER OF STATIONS                 
                                                                                
         USING CSLSTELD,ELEM       BUILD STATION LIST ELEMENT                   
         MVI   CSLSTEL,CSLSTELQ                                                 
         LA    R0,CSLSTSTA         POINT TO STATION LIST IN ELEMNT              
         MHI   R1,L'CSLSTSTA       R1=LENGTH OF STATION CHUNK                   
         LR    RE,R6               POINT TO STATION LIST IN INPUT ARRAY         
         AR    R6,R1               POINT TO NEXT STATION CHUNK                  
         LA    RF,CSLSTSTA-CSLSTELD(R1)                                         
         STC   RF,CSLSTLEN         SET ELEMENT LENGTH                           
         LR    RF,R1               SET FROM LENGTH=TO LENGTH                    
         MVCL  R0,RE               MOVE STATIONS TO ELEMENT                     
                                                                                
         LLH   R0,CSLLEN           GET RECORD LENGTH                            
         LLC   R1,ELEM+(CSLSTLEN-CSLSTELD)                                      
         AR    R1,R0                                                            
         MVI   BYTE1,0             SET RECORD NOT FULL CONDITION                
         CHI   R1,4000             TEST STATION LIST ELEMENT WILL FIT           
         JNH   BLDCSL22                                                         
         MVI   BYTE1,1             NO - SET RECORD FULL CONDITION               
                                                                                
BLDCSL10 CLI   QACTION,QACTADD     ADD/WRITE CURRENT RECORD                     
         JNE   BLDCSL12                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOXSPFIL+IO3'                           
         JE    BLDCSL14                                                         
         DC    H'0'                                                             
                                                                                
BLDCSL12 GOTOR (#IOEXEC,AIOEXEC),'IOPUT+IOXSPFIL+IO3'                           
         JE    *+6                                                              
         DC    H'0'                                                             
         CLI   QACTION,QACTADR     TEST DIRECTORY IS DELETED                    
         JNE   BLDCSL14            NO - DON'T WRITE ON CHANGE                   
         GOTOR (#IOEXEC,AIOEXEC),'IOWRITE+IOXSPDIR+IO3'                         
         JE    BLDCSL14                                                         
         DC    H'0'                                                             
                                                                                
BLDCSL14 CLI   BYTE1,2             TEST FLUSH CALL                              
         JE    BLDCSL24                                                         
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOXSPDIR+IO3'                            
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVI   QACTION,QACTCHA     SET TO CHANGE RECORD                         
         GOTOR (#IOEXEC,AIOEXEC),'IOSQUPD+IOXSPDIR+IO3'                         
         JE    *+14                                                             
         TM    IOERR,IOEDEL                                                     
         JNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   K.CSLKEY(CSLKSEQN-CSLKEY),IOKEYSAV                               
         JE    BLDCSL16                                                         
                                                                                
         XC    IOKEY,IOKEY         CREATE A NEW KEY                             
         MVC   K.CSLKEY,IOKEYSAV   RESTORE KEY                                  
         LLC   R0,K.CSLKSEQN       AND BUMP SEQUENCE NUMBER                     
         AHI   R0,1                                                             
         STC   R0,K.CSLKSEQN                                                    
         MVI   QACTION,QACTADD     SET TO ADD RECORD                            
         J     BLDCSL20                                                         
                                                                                
BLDCSL16 NI    K.CSLKSTAT,FF-X'80' UNDELETE DIRECTORY RECORD                    
         TM    IOERR,IOEDEL        IF RECORD IS DELETED                         
         JZ    *+8                                                              
         MVI   QACTION,QACTADR     SET TO RE-USE DELETED RECORD                 
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOXSPFIL+IO3'                        
         JE    BLDCSL20                                                         
         DC    H'0'                                                             
                                                                                
BLDCSL20 GOTOR INIREC,K.CSLKEY     INITIALIZE A NEW RECORD                      
         LA    R3,CSLFRST          R3=A(FIRST ELEMENT ON RECORD)                
                                                                                
BLDCSL22 GOTOR NXTADD              ADD ELEMENT TO RECORD                        
         LTR   R5,R5               TEST ANY MORE STATIONS TO ADD                
         JNZ   BLDCSL08            YES - DO NEXT CHUNK                          
         MVI   BYTE1,2             SET TO FLUSH LAST PENDING RECORD             
         J     BLDCSL10                                                         
                                                                                
***********************************************************************         
* DELETE ANY HIGHER SEQUENCE RECORDS THAT MAY BE ON FILE              *         
***********************************************************************         
                                                                                
BLDCSL24 GOTOR (#IOEXEC,AIOEXEC),'IORD+IOXSPDIR+IO3'                            
         JE    BLDCSL26                                                         
         DC    H'0'                                                             
                                                                                
BLDCSL26 GOTOR (#IOEXEC,AIOEXEC),'IOSQUP+IOXSPDIR+IO3'                          
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   K.CSLKEY(CSLKSEQN-CSLKEY),IOKEYSAV                               
         JNE   BLDCSL28                                                         
         OI    K.CSLKSTAT,X'80'                                                 
         GOTOR (#IOEXEC,AIOEXEC),'IOWRT+IOXSPDIR+IO3'                           
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOXSPFIL+IO3'                        
         JE    *+6                                                              
         DC    H'0'                                                             
         OI    CSLSTAT,X'80'                                                    
         GOTOR (#IOEXEC,AIOEXEC),'IOPUT+IOXSPFIL+IO3'                           
         JE    BLDCSL26                                                         
         DC    H'0'                                                             
                                                                                
BLDCSL28 GOTOR LINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTMAP',1)                   
         J     EXITY                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PROGRAM RECORD - MAPCODE X'0293'                                    *         
***********************************************************************         
                                                                                
PGMREC   LKREQ H,I#COPROG,NEWREC=Y,NEXTREQ=PCTREC,ROUTINE=BLDPGM                
                                                                                
Action   LKREQ F,1,(D,B#SAVED,QACTION),CHAR,TEXT=SP#ACTN                        
Media    LKREQ F,2,(D,B#WORKD,QMEDX),(U,#VALMED,$VALMED),TEXT=SP#MED            
Client   LKREQ F,3,(D,B#WORKD,QCLTX),(U,#VALCLT,$VALCLT),TEXT=SP#CLI            
Campaign LKREQ F,4,(D,B#SAVED,QCAMX),UBIN,TEXT=(*,TCAMNUM)                      
Market   LKREQ F,5,(D,B#SAVED,QMKT),UBIN,TEXT=SP#MKT                            
Station  LKREQ F,6,(D,B#SAVED,QSTA),CHAR,TEXT=SP#STNET                          
Line     LKREQ F,7,(D,B#SAVED,QLIN),UBIN,TEXT=(*,TLINE)                         
Token    LKREQ F,27,(D,B#SAVED,QTOKEN),CHAR,TEXT=(*,TTOKEN)                     
EstOvrd  LKREQ F,8,(D,B#SAVED,QDPRSTOV),UBIN,TEXT=(*,TESTOVRD)                  
StrDate  LKREQ F,9,(D,B#SAVED,QDPRDSDT),BDAT,TEXT=(*,TSTART)                    
EndDate  LKREQ F,10,(D,B#SAVED,QDPRDEDT),BDAT,TEXT=(*,TENDDATE)                 
Days     LKREQ F,11,(D,B#SAVED,QDPRDDAY),UBIN,TEXT=(*,TDAYS)                    
StrTime  LKREQ F,12,(D,B#SAVED,QDPRDSTM),UBIN,TEXT=(*,TSTRTIME)                 
EndTime  LKREQ F,13,(D,B#SAVED,QDPRDETM),UBIN,TEXT=(*,TENDTIME)                 
Daypart  LKREQ F,14,(D,B#SAVED,QDPRDDPT),CHAR,TEXT=(*,TDAYPART)                 
Product1 LKREQ F,15,(D,B#SAVED,QDPRDPRD),CHAR,TEXT=(*,TPRD1)                    
Product2 LKREQ F,16,(D,B#SAVED,QDPRDPR2),CHAR,TEXT=(*,TPRD2)                    
SpotLen  LKREQ F,17,(D,B#SAVED,QDPRDSLN),UBIN,TEXT=(*,TSLN)                     
TotalLen LKREQ F,18,(D,B#SAVED,QDPRDSL2),UBIN,TEXT=(*,TTOTLEN)                  
Showcode LKREQ F,19,(D,B#SAVED,QDPRDSHO),CHAR,TEXT=(*,TSHOWCD)                  
Program  LKREQ F,20,(D,B#SAVED,QDPRDPGM),CHAR,TEXT=(*,TPROGRAM)                 
Cost     LKREQ F,21,(D,B#SAVED,QDPRDCOS),UBIN,TEXT=(*,TCOST)                    
AdjCode  LKREQ F,22,(D,B#SAVED,QDPRDADJ),(R,VALADJ),TEXT=(*,TADJCODE)           
BuyID    LKREQ F,23,(D,B#SAVED,QDPRPDID),CHAR,TEXT=(*,TBUYID)                   
Order#   LKREQ F,24,(D,B#SAVED,QDPRDORD),UBIN,TEXT=(*,TORDNUM)                  
RepCode  LKREQ F,26,(D,B#SAVED,QDPRDREP),CHAR,TEXT=(*,TREPCD)                   
RepCost  LKREQ F,33,(D,B#SAVED,QDPRDRCS),UBIN,TEXT=(*,TREPCOST)                 
State    LKREQ F,28,(D,B#SAVED,QSTATE),HEXD,TEXT=(*,TSTATE)                     
Comments LKREQ F,25,(I,B#SAVED,QACOMMS),VSTR,TEXT=(*,TCOMMENT),        +        
               MAXLEN=L'CMPCMCOM,LOWERCASE=Y                                    
SpotWeek LKREQ F,30,(I,B#SAVED,QASPOTS),CDAT,OLEN=L'DPRSW2WK,ARRAY=S,  +        
               TEXT=(*,TSPTDTS)                                                 
SpotSpot LKREQ F,31,,UBIN,OLEN=L'DPRSW2NM,TEXT=(*,TSPTCNTS),ARRAY=E             
UnaWeeks LKREQ F,32,(I,B#SAVED,QAWEEKS),CDAT,OLEN=L'DPRUNWK,LIST=F,    +        
               TEXT=(*,TUNAVWK)                                                 
RepComms LKREQ F,34,(I,B#SAVED,QARCOM),VSTR,TEXT=(*,TREPCOM),          +        
               MAXLEN=L'DPRRCMCM,LOWERCASE=Y                                    
AvailID  LKREQ F,35,(I,B#SAVED,QAAVAIL),VSTR,TEXT=(*,TAVAILID),        +        
               MAXLEN=L'DPRAVID                                                 
OfferTyp LKREQ F,36,(I,B#SAVED,QAOFFER),VSTR,TEXT=(*,TOFFERTY),        +        
               MAXLEN=L'DPROFTYP                                                
PID      LKREQ F,100,(D,B#SAVED,QPID),VSTR,TEXT=(*,TPID),              +        
               MAXLEN=L'DPRACPID                                                
TimeStmp LKREQ F,101,(D,B#SAVED,QDATTIM),VSTR,TEXT=(*,TTIMESTP),       +        
               MAXLEN=L'DPRACDTM                                                
                                                                                
         LKREQ E                                                                
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE AND BUILD PROGRAM RECORD                        *         
***********************************************************************         
                                                                                
         USING DPRRECD,R2                                                       
BLDPGM   LARL  RE,BLDPGM20         SET RE-ENTRY POINT                           
         ST    RE,EORADD                                                        
         CLI   FATALERR,C'Y'                                                    
         JE    EXITY                                                            
                                                                                
         CLI   QACTION,0           TEST ACTION KNOWN                            
         JNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
K        USING DPRKEY,SVKEY        BUILD KEY FROM INPUT DATA                    
         XC    K.DPRKEY,K.DPRKEY                                                
         MVI   K.DPRKTYPE,DPRKTYPQ                                              
         MVI   K.DPRKSTYP,DPRKSTYQ                                              
         MVC   K.DPRKAGMD,QMEDX                                                 
         MVC   K.DPRKCLT,QCLTX                                                  
         MVC   K.DPRKCAM,QCAMX                                                  
         XC    K.DPRKCAM,EFFS      GET COMPLEMENT                               
                                                                                
         CLI   QMEDA,C'N'          TEST CANADIAN NETWORK MEDIA                  
         JE    BLDPGM02            YES - NETWORK BUYS HAVE NO MARKET            
                                                                                
         MVC   K.DPRKMKT,QMKT                                                   
         OC    K.DPRKMKT,K.DPRKMKT                                              
         JZ    EM#MSGMK                                                         
                                                                                
BLDPGM02 CLI   QMEDA,C'N'          TEST NETWORK                                 
         JNE   *+8                                                              
         MVI   QSTA+4,C'N'                                                      
         GOTOR (#VALSTA,AVALSTA),DMCB,QSTA,L'QSTA,SVBSTA                        
         JNE   EM#INVST                                                         
                                                                                
BLDPGM04 MVC   K.DPRKSTA,SVBSTA                                                 
         OC    K.DPRKSTA,K.DPRKSTA                                              
         JZ    EM#MSGSN                                                         
                                                                                
         SR    R0,R0                                                            
         ICM   R0,3,QLIN                                                        
         JNZ   BLDPGM06                                                         
         CLI   QACTION,QACTADD     TEST ADDING A NEW RECORD                     
         JE    BLDPGM06                                                         
         DC    H'0'                NO - LINE NUMBER MUST BE GIVEN               
                                                                                
BLDPGM06 STCM  R0,3,K.DPRKLIN                                                   
                                                                                
         CLI   QACTION,QACTADD     TEST ADDING A NEW RECORD                     
         JNE   BLDPGM14                                                         
         MVC   IOKEY(L'DPRKEY),SVKEY                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUPD+IOXSPDIR'                             
                                                                                
BLDPGM08 CLC   IOKEY(DPRKLIN-DPRKEY),IOKEYSAV  SAME TY/A-M/C/MKT/STA            
         JNE   BLDPGM10                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOSQUPD+IOXSPDIR'                             
         J     BLDPGM08                                                         
                                                                                
BLDPGM10 MVC   IOKEY,IOKEYSAV      RESTORE HIGH KEY FOUND                       
                                                                                
         LLH   R0,IOKEY+DPRKLIN-DPRKEY      GET LINE                            
         AHI   R0,1                         INCREMENT                           
         STCM  R0,3,K.DPRKLIN               SET IN SVKEY                        
         STCM  R0,3,QLIN                    AND SAVE SO CAN RETURN              
                                                                                
BLDPGM12 GOTOR INIREC,K.DPRKEY     INITIALIZE NEW RECORD                        
         J     BLDPGM18                                                         
                                                                                
***********************************************************************         
* READ EXISTING LINE                                                  *         
***********************************************************************         
                                                                                
BLDPGM14 MVC   IOKEY(L'DPRKEY),SVKEY                                            
         GOTOR (#IOEXEC,AIOEXEC),'IORDUP+IOXSPDIR+IO3'                          
         JNE   EM#NOPGM                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOXSPFIL+IO3'                        
         JNE   EM#NOPGM                                                         
                                                                                
         CLI   QACTION,QACTDEL     TEST DELETE                                  
         JNE   BLDPGM18                                                         
         XC    EORADD,EORADD                                                    
         GOTOR DELREC              DELETE PROGRAM RECORD                        
                                                                                
***********************************************************************         
* NOW DELETE ASSOCIATED DEMO RECORDS                                  *         
***********************************************************************         
                                                                                
K        USING DDMKEY,IOKEY                                                     
         XC    K.DDMKEY,K.DDMKEY                                                
         MVI   K.DDMKTYPE,DDMKTYPQ                                              
         MVI   K.DDMKSTYP,DDMKSTYQ                                              
         MVC   K.DDMKAGMD(DDMKLCL-DDMKAGMD),SVKEY+DPRKAGMD-DPRKEY               
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUPD+IOXSPDIR+IO3'                         
                                                                                
BLDPGM16 CLC   IOKEY(DDMKLCL-DDMKEY),IOKEYSAV  COMPARE THROUGH LINE             
         JNE   BLDPGM24                                                         
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOXSPFIL+IO3'                        
         JE    *+6                                                              
         DC    H'0'                                                             
         GOTOR DELREC                                                           
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOSQUPD+IOXSPDIR+IO3'                         
         J     BLDPGM16                                                         
                                                                                
***********************************************************************         
* ADD DATA ELEMENTS TO RECORD                                         *         
***********************************************************************         
                                                                                
BLDPGM18 GOTOR SAVINI              SAVE F1 AND DELETE ALL ELEMENTS              
                                                                                
         USING DPRPDELD,ELEM                                                    
         MVI   DPRPDEL,DPRPDELQ                                                 
         MVI   DPRPDLEN,DPRPDLNQ                                                
         MVC   DPRESTOV(QDPRVALL),QDPRVALS                                      
         GOTOR NXTADD                                                           
                                                                                
         USING DPRPXELD,ELEM                                                    
         XC    DPRPXELD(DPRPXLNQ),DPRPXELD                                      
         MVI   DPRPXEL,DPRPXELQ                                                 
         MVI   DPRPXLEN,DPRPXLNQ                                                
         MVC   DPRPXST,QSTATE                                                   
         GOTOR NXTADD                                                           
                                                                                
         USING DPRACTD,ELEM                                                     
         XC    DPRACTD(DPRACLNQ),DPRACTD                                        
         MVI   DPRACEL,DPRACELQ                                                 
         MVI   DPRACLEN,DPRACLNQ                                                
         MVC   DPRACPID,QPID                                                    
         MVC   DPRACDTM,QDATTIM                                                 
         GOTOR NXTADD                                                           
                                                                                
         GOTOR MOVEIT,DMCB,('DPRSW2EQ',QASPOTS),('DPRSW2WK-DPRSW2ED',0)         
         GOTOR MOVEIT,(R1),('DPRUNELQ',QAWEEKS),('DPRUNWK-DPRUNELD',0)          
         GOTOR MOVEIT,(R1),('DPRCMELQ',QACOMMS),('DPRCMCOM-DPRCMELD',0)         
         GOTOR MOVEIT,(R1),('DPRRCMLQ',QARCOM),('DPRRCMCM-DPRRCOMD',0)          
         GOTOR MOVEIT,(R1),('DPRAVELQ',QAAVAIL),('DPRAVID-DPRAVELD',0)          
         GOTOR MOVEIT,(R1),('DPROFELQ',QAOFFER),('DPROFTYP-DPROFELD',0)         
         J     EXITY                                                            
                                                                                
BLDPGM20 MVI   EORFLAG,C'N'                                                     
         GOTOR BLDACT              BUILD ACTIVITY ELEMENT                       
                                                                                
         CLI   QACTION,QACTADD     TEST ADD                                     
         JNE   BLDPGM22                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOXSPFIL+IO3'                           
         JE    BLDPGM24                                                         
         DC    H'0'                                                             
                                                                                
BLDPGM22 GOTOR (#IOEXEC,AIOEXEC),'IOPUT+IOXSPFIL+IO3'                           
         JE    BLDPGM24                                                         
         DC    H'0'                                                             
                                                                                
BLDPGM24 GOTOR LINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTMAP',1)                   
                                                                                
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTRAW',2),           +        
               ('LD_LBINQ',QLIN),(L'QLIN,0)                                     
                                                                                
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTRAW',7),           +        
               ('LD_CHARQ',QSTA),(L'QSTA,0)                                     
                                                                                
         CLI   QACTION,QACTADD     TEST ACTION IS ADD                           
         JNE   SENDDTTM                                                         
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTRAW',3),           +        
               ('LD_CHARQ',QTOKEN),(L'QTOKEN,0)                                 
         MVC   SVLIN,QLIN          SAVE LINE NUMBER ASSIGNED                    
         J     SENDDTTM                                                         
         EJECT                                                                  
***********************************************************************         
* NETWORK PERCENTAGES - MAP CODE X'0295'                              *         
***********************************************************************         
                                                                                
PCTREC   LKREQ H,I#CONPCT,NEWREC=Y,NEXTREQ=DEMREC,ROUTINE=BLDPCT                
                                                                                
Market   LKREQ F,10,(I,B#SAVED,QADATA),UBIN,OLEN=L'NPCTMKT,            +        
               ARRAY=S,TEXT=SP#MKT                                              
Station  LKREQ F,11,,CHAR,OLEN=L'NPCTSTA,TEXT=SP#STNET                          
Province LKREQ F,12,,CHAR,OLEN=L'NPCTPROV,TEXT=(*,TPROVCD)                      
Percent  LKREQ F,13,,UBIN,OLEN=L'NPCTPCT,TEXT=(*,TSTAPCT),ARRAY=E               
                                                                                
         LKREQ E                                                                
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE NETWORK PRORATIONS                              *         
***********************************************************************         
                                                                                
         USING DPRRECD,R2                                                       
BLDPCT   MVI   EORFLAG,C'N'                                                     
         LLH   R3,DPRLEN                                                        
         LA    R3,DPRRECD(R3)                                                   
         BCTR  R3,0                POINT TO END OF RECORD                       
                                                                                
         USING DPRNPELD,ELEM                                                    
         MVI   DPRNPEL,DPRNPELQ                                                 
         MVI   DPRNPLEN,DPRNPLNQ                                                
         ICM   R4,15,QADATA                                                     
         JNZ   *+6                                                              
         DC    H'0'                                                             
         USING LW_D,R4                                                          
         LLH   R5,LW_NUMN          GET NUMBER OF ENTRIES                        
         LA    R4,LW_DATA2         POINT TO FIRST ROW                           
         USING NPCTD,R4                                                         
BLDPCT02 MVC   DPRNPMKT,NPCTMKT                                                 
         MVC   QLOCAL(L'NPCTSTA),NPCTSTA                                        
         CLI   QLOCAL,C' '         IF NO STATION                                
         JH    *+10                                                             
         MVC   QLOCAL(4),QSTA      USE CABLE NETWORK                            
         MVI   QLOCAL+4,C'N'                                                    
         MVC   QPROVCD,NPCTPROV                                                 
         GOTOR GOSTAP                                                           
         MVC   DPRNPSTA,SVLOCALX                                                
         MVC   DPRNPPCT,NPCTPCT                                                 
         GOTOR NXTADD                                                           
         AHI   R4,NPCTLEN          BUMP TO NEXT INPUT ENTRY                     
         JCT   R5,BLDPCT02                                                      
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* DEMO VALUES - MAPCODE X'0294'                                       *         
***********************************************************************         
                                                                                
DEMREC   LKREQ H,I#CODEMO,NEWREC=Y,NEXTREQ=SPLREC,ROUTINE=BLDDEM                
                                                                                
Action   LKREQ F,1,(D,B#SAVED,QACTION),CHAR,TEXT=SP#ACTN                        
Media    LKREQ F,2,(D,B#WORKD,QMEDX),(U,#VALMED,$VALMED),TEXT=SP#MED            
Client   LKREQ F,3,(D,B#WORKD,QCLTX),(U,#VALCLT,$VALCLT),TEXT=SP#CLI            
Campaign LKREQ F,4,(D,B#SAVED,QCAMX),UBIN,TEXT=(*,TCAMNUM)                      
Market   LKREQ F,5,(D,B#SAVED,QMKT),UBIN,TEXT=SP#MKT                            
Station  LKREQ F,6,(D,B#SAVED,QSTA),CHAR,TEXT=SP#STNET                          
Line#    LKREQ F,7,(D,B#SAVED,QLIN),UBIN,TEXT=(*,TLINE)                         
Token    LKREQ F,27,(D,B#SAVED,QTOKEN),CHAR,TEXT=(*,TTOKEN)                     
Local    LKREQ F,8,(D,B#SAVED,QLOCAL),CHAR,TEXT=(*,TLOCAL)                      
Province LKREQ F,9,(D,B#SAVED,QPROVCD),CHAR,TEXT=(*,TPROVCD)                    
LkUpSpil LKREQ F,10,(D,B#SAVED,QSPLMKT),UBIN,TEXT=(*,TSPLMKT)                   
LkUpBook LKREQ F,11,(D,B#SAVED,QDDMDBOK),BDAT,TEXT=(*,TBOOK)                    
LkUpBTyp LKREQ F,12,(D,B#SAVED,QDDMDBTY),CHAR,TEXT=(*,TBOOKTYP)                 
LkUpProg LKREQ F,13,(D,B#SAVED,QDDMDPGM),CHAR,TEXT=(*,TPROGRAM),       +        
               MAXLEN=L'QDDMDPGM                                                
LkUpMrkt LKREQ F,14,(D,B#SAVED,QDDMDMKT),CHAR,TEXT=(*,TLKUPMKT)                 
LkUpSta  LKREQ F,15,(D,B#SAVED,QDDMDSTA),CHAR,TEXT=(*,TLKUPSTA)                 
LkUpRSrv LKREQ F,16,(D,B#SAVED,QDDMDRSV),CHAR,TEXT=(*,TLKUPRSV)                 
DemoCode LKREQ F,20,(I,B#SAVED,QADEMS),(U,#VALDCD,$VALDCD),            +        
               OLEN=L'DDMDVDEM,SORT=NO,TEXT=SP#DEMO,ARRAY=S                     
EstDemo  LKREQ F,21,,(R,VALDVL),OLEN=L'DDMDVEST,TEXT=(*,TDEMOEST)               
LkUpDemo LKREQ F,22,,(R,VALDVL),OLEN=L'DDMDVLKU,TEXT=(*,TDEMLKUP)               
RepDemo  LKREQ F,23,,(R,VALDVL),OLEN=L'DDMDVREP,TEXT=(*,TDEMREP),      +        
               ARRAY=E                                                          
                                                                                
         LKREQ E                                                                
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE AND BUILD DEMO RECORD                           *         
***********************************************************************         
                                                                                
         USING DDMRECD,R2                                                       
BLDDEM   LARL  RE,BLDDEM18         SET RE-ENTRY POINT                           
         ST    RE,EORADD                                                        
         CLI   FATALERR,C'Y'                                                    
         JE    EXITY                                                            
                                                                                
         CLC   LP_QMAPN,M#COSPL    TEST SPILL DEMOS                             
         JE    BLDDEM16                                                         
                                                                                
K        USING DDMKEY,SVKEY        BUILD KEY FROM INPUT DATA                    
         XC    K.DDMKEY,K.DDMKEY                                                
         MVI   K.DDMKTYPE,DDMKTYPQ                                              
         MVI   K.DDMKSTYP,DDMKSTYQ                                              
         MVC   K.DDMKAGMD,QMEDX                                                 
         MVC   K.DDMKCLT,QCLTX                                                  
         MVC   K.DDMKCAM,QCAMX                                                  
         XC    K.DDMKCAM,EFFS      GET FF COMPLEMENT                            
                                                                                
         CLI   QMEDA,C'N'          TEST CANADIAN NETWORK MEDIA                  
         JE    BLDDEM02            YES - NETWORK BUYS HAVE NO MARKET            
         OC    K.DDMKMKT,QMKT                                                   
         JZ    EM#MSGMK                                                         
                                                                                
BLDDEM02 CLI   QMEDA,C'N'          TEST NETWORK                                 
         JNE   *+8                                                              
         MVI   QSTA+4,C'N'                                                      
         GOTOR (#VALSTA,AVALSTA),DMCB,QSTA,L'QSTA,SVBSTA                        
         JNE   EM#INVST                                                         
                                                                                
BLDDEM04 MVC   K.DDMKSTA,SVBSTA                                                 
         OC    K.DDMKSTA,K.DDMKSTA                                              
         JZ    EM#MSGSN                                                         
                                                                                
         OC    QLIN,QLIN           IF NO LINE NUMBER THIS TIME                  
         JNZ   *+10                                                             
         MVC   QLIN,SVLIN          THEN USE PREVIOUS LINE                       
         SR    R0,R0                                                            
         ICM   R0,3,QLIN                                                        
         JNZ   BLDDEM06                                                         
         DC    H'0'                                                             
                                                                                
BLDDEM06 STCM  R0,3,K.DDMKLIN                                                   
         CLI   QMEDA,C'N'          TEST NETWORK                                 
         JNE   BLDDEM12                                                         
         MVI   QSTA+4,C'N'                                                      
         CLI   QPROVCD,C' '        IF PROVINCE GIVEN, THEN CABLE                
         JH    BLDDEM08                                                         
         CLI   QLOCAL,C' '         IF NO LOCAL STA THEN NETWORK DEMOS           
         JNH   BLDDEM12                                                         
         J     BLDDEM10                                                         
                                                                                
BLDDEM08 MVC   QLOCAL,QSTA         COPY STATION TO LOCAL                        
                                                                                
BLDDEM10 GOTOR GOSTAP                                                           
                                                                                
         MVC   K.DDMKLCL,SVLOCALX                                               
                                                                                
***********************************************************************         
* READ FOR EXISTING LINE - IF NOT ON FILE, ADD NEW                    *         
***********************************************************************         
                                                                                
BLDDEM12 MVC   IOKEY(L'DDMKEY),SVKEY                                            
         GOTOR (#IOEXEC,AIOEXEC),'IORDUPD+IOXSPDIR+IO3'                         
         JE    BLDDEM13                                                         
         NI    IOKEY+(DDMKSTAT-DDMKEY),X'FF'-X'80'                              
         TM    IOERR,IOEDEL                                                     
         JZ    *+12                                                             
         MVI   QACTION,QACTADR     SET ACTION TO RESTORE                        
         J     BLDDEM13                                                         
         CLI   QACTION,QACTDEL     MUST BE PRESENT FOR DELETE                   
         JNE   *+6                                                              
         DC    H'0'                                                             
         MVI   QACTION,QACTADD     SET TO ADD NEW RECORD                        
         J     BLDDEM14                                                         
                                                                                
BLDDEM13 MVC   SVKEY,IOKEY        SAVE KEY AND DISK ADDRESS                     
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOXSPFIL+IO3'                        
         JNE   EM#NODMV                                                         
                                                                                
         CLI   QACTION,QACTADR     TEST RESTORING RECORD                        
         JE    BLDDEM14                                                         
         CLI   QACTION,QACTDEL     TEST DELETE                                  
         JE    *+12                                                             
         MVI   QACTION,QACTCHA     ASSUME ACTION CHANGE                         
         J     BLDDEM14                                                         
                                                                                
         XC    EORADD,EORADD       RESET RE-ENTRY POINT                         
         GOTOR DELREC                                                           
         J     EXITY               EXIT WITH NO DATA RETURENED                  
                                                                                
BLDDEM14 GOTOR INIREC,K.DDMKEY     INITIALIZE NEW RECORD                        
                                                                                
         USING DDMDDELD,ELEM       BUILD & ADD DEMO DESCRIPTION ELEMENT         
         MVI   DDMDDEL,DDMDDELQ                                                 
         MVI   DDMDDLEN,DDMDDLNQ                                                
         MVC   DDMDDBOK(QDDMVALL),QDDMVALS                                      
         GOTOR NXTADD                                                           
                                                                                
         USING DDMDVELD,ELEM                                                    
BLDDEM16 MVC   DDMDVMKT,QSPLMKT    SPILL MARKET (0=ORIGINATING)                 
         LLH   R3,DDMLEN                                                        
         LA    R3,DDMRECD(R3)                                                   
         BCTR  R3,0                ADD ELEMENT TO END OF RECORD                 
         GOTOR MOVEIT,DMCB,('DDMDVELQ',QADEMS),('DDMDVDEM-DDMDVELD',0)          
         J     EXITY                                                            
                                                                                
BLDDEM18 MVI   EORFLAG,C'N'                                                     
                                                                                
         CLI   QACTION,QACTADD     TEST ADD                                     
         JNE   BLDDEM20                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOXSPFIL+IO3'                           
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVKEY+(DDMKDA-DDMKEY)(L'DDMKDA),IODA                             
         J     BLDDEM22                                                         
                                                                                
BLDDEM20 GOTOR (#IOEXEC,AIOEXEC),'IOPUT+IOXSPFIL+IO3'                           
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLI   QACTION,QACTADR     TEST RESTORING RECORD                        
         JNE   BLDDEM22                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOWRT+IOXSPDIR+IO3'                           
         JE    BLDDEM22                                                         
         DC    H'0'                                                             
                                                                                
BLDDEM22 LA    R3,DDMFRST          CREATE PASSIVE POINTERS                      
         USING DDMDVELD,R3                                                      
         SR    R4,R4               R4=ELEMENT COUNT                             
         SR    R0,R0                                                            
                                                                                
BLDDEM24 CLI   DDMDVEL,0           TEST END OF RECORD                           
         JE    EXITY                                                            
         CLI   DDMDVEL,DDMDVELQ    TEST DEMO DESCRIPTION ELEMENT                
         JNE   BLDDEM28                                                         
         AHI   R4,1                                                             
         OC    DDMDVMKT,DDMDVMKT   TEST ORIGINATING DEMOS                       
         JZ    BLDDEM28            YES - SKIP                                   
S        USING DDMKEY,IOKEY                                                     
         MVC   IOKEY,SVKEY         SET TO ADD NEW SPILL POINTER                 
         MVC   S.DDMKMKT,DDMDVMKT  SET SPILL MARKET                             
         CLI   QMEDA,C'N'                                                       
         JNE   *+12                                                             
         CHI   R4,1                                                             
         JNH   *+8                                                              
         MVI   S.DDMKSPL,DDMKSPLQ  SET SPILL INDICATOR                          
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IORDUP+IOXSPDIR'                              
         JNE   BLDDEM26                                                         
         CLC   S.DDMKDA,IOKEYSAV+(DDMKDA-DDMRECD)                               
         JE    BLDDEM28                                                         
         MVC   S.DDMKDA,IOKEYSAV+(DDMKDA-DDMRECD)                               
         GOTOR (#IOEXEC,AIOEXEC),'IOWRT+IOXSPDIR'                               
         JE    BLDDEM28                                                         
         DC    H'0'                                                             
                                                                                
BLDDEM26 MVC   IOKEY,IOKEYSAV                                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOXSPDIR'                               
         JE    BLDDEM28                                                         
         DC    H'0'                DIE ON OTHER ERRORS                          
         DROP  S                                                                
                                                                                
BLDDEM28 LLC   R0,DDMDVLEN         BUMP TO NEXT ELEMENT                         
         AR    R3,R0                                                            
         J     BLDDEM24                                                         
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* SPILL DEMO VALUES - MAPCODE X'029B'                                 *         
***********************************************************************         
                                                                                
SPLREC   LKREQ H,I#COSPL,NEWREC=Y,NEXTREQ=ORDREC,ROUTINE=BLDDEM                 
                                                                                
Market   LKREQ F,10,(D,B#SAVED,QSPLMKT),UBIN,TEXT=(*,TSPLMKT)                   
DemoBook LKREQ F,11,(D,B#SAVED,QDDMDBOK),BDAT,TEXT=(*,TBOOK)                    
BookType LKREQ F,12,(D,B#SAVED,QDDMDBTY),CHAR,TEXT=(*,TBOOKTYP)                 
Program  LKREQ F,13,(D,B#SAVED,QDDMDPGM),CHAR,TEXT=(*,TPROGRAM),       +        
               MAXLEN=L'QDDMDPGM                                                
LkUpMrkt LKREQ F,14,(D,B#SAVED,QDDMDMKT),CHAR,TEXT=(*,TLKUPMKT)                 
LkUpSTa  LKREQ F,15,(D,B#SAVED,QDDMDSTA),CHAR,TEXT=(*,TLKUPSTA)                 
LkUpRSrv LKREQ F,16,(D,B#SAVED,QDDMDRSV),CHAR,TEXT=(*,TLKUPRSV)                 
DemoCode LKREQ F,20,(I,B#SAVED,QADEMS),(U,#VALDCD,$VALDCD),            +        
               OLEN=L'DDMDVDEM,SORT=NO,TEXT=SP#DEMO,ARRAY=S                     
EstDemo  LKREQ F,21,,(R,VALDVL),OLEN=L'DDMDVEST,TEXT=(*,TDEMOEST)               
LkUpDemo LKREQ F,22,,(R,VALDVL),OLEN=L'DDMDVLKU,TEXT=(*,TDEMLKUP)               
RepDemo  LKREQ F,23,,(R,VALDVL),OLEN=L'DDMDVREP,TEXT=(*,TDEMREP),      +        
               ARRAY=E                                                          
                                                                                
         LKREQ E                                                                
         EJECT                                                                  
***********************************************************************         
* ORDERS - MAPCODE X'0298'                                            *         
***********************************************************************         
                                                                                
ORDREC   LKREQ H,I#COORDR,NEWREC=Y,NEXTREQ=CONREC,ROUTINE=BLDORD                
                                                                                
Action   LKREQ F,1,(D,B#SAVED,QACTION),CHAR,TEXT=SP#ACTN                        
Media    LKREQ F,2,(D,B#WORKD,QMEDX),(U,#VALMED,$VALMED),TEXT=SP#MED            
Client   LKREQ F,3,(D,B#WORKD,QCLTX),(U,#VALCLT,$VALCLT),TEXT=SP#CLI            
Campaign LKREQ F,4,(D,B#SAVED,QCAMX),UBIN,TEXT=(*,TCAMNUM)                      
Order#   LKREQ F,5,(D,B#SAVED,QORDX),UBIN,TEXT=(*,TORDNUM)                      
Market   LKREQ F,12,(D,B#SAVED,QMKT),UBIN,TEXT=SP#MKT                           
Station  LKREQ F,13,(D,B#SAVED,QSTA),CHAR,TEXT=SP#STNET                         
Office   LKREQ F,14,(D,B#SAVED,QOFFC),CHAR,TEXT=(*,TOFFICE)                     
Buyer    LKREQ F,15,(D,B#SAVED,QBUYER),CHAR,TEXT=(*,TBUYER)                     
Name     LKREQ F,16,(I,B#SAVED,QANAME),VSTR,TEXT=(*,TNAME),            +        
               LOWERCASE=Y                                                      
EstList  LKREQ F,20,(I,B#SAVED,QADATA),UBIN,OLEN=L'CORESLST,LIST=F,    +        
               TEXT=(*,TESTLST)                                                 
Status   LKREQ F,10,(D,B#SAVED,QCORSTA),UBIN,TEXT=(*,TORSTAT)                   
Flag     LKREQ F,21,(D,B#SAVED,QCORACTF),CHAR,TEXT=(*,TACTFLG)                  
Spots    LKREQ F,24,(D,B#SAVED,QCORSPT),UBIN,TEXT=(*,TORDSPT)                   
Cost     LKREQ F,25,(D,B#SAVED,QCORCOS),UBIN,TEXT=(*,TORDDOL)                   
ExpryDt  LKREQ F,26,(D,B#SAVED,QCORTEXD),BDAT,TEXT=(*,TORDEDT)                  
ExpryTm  LKREQ F,27,(D,B#SAVED,QCORTEXT),UBIN,TEXT=(*,TORDETM)                  
Lines    LKREQ F,29,(D,B#SAVED,QCORTLNS),UBIN,TEXT=(*,TTOTLNS)                  
Comments LKREQ F,23,(I,B#SAVED,QACOMMS),VSTR,TEXT=(*,TCOMMENT),        +        
               LOWERCASE=Y                                                      
URL      LKREQ F,30,(I,B#SAVED,QAURL),VSTR,TEXT=(*,TORDURL),           +        
               MAXLEN=L'CORURL,LOWERCASE=Y                                      
Rcpients LKREQ F,28,(I,B#SAVED,QAWHO),CHAR,OLEN=L'CORRLWHO,            +        
               LIST=F,TEXT=(*,TRCPTS)                                           
                                                                                
         LKREQ E                                                                
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE AND BUILD ORDER RECORD                          *         
***********************************************************************         
                                                                                
         USING CORRECD,R2                                                       
BLDORD   LARL  RE,BLDOR080         SET RE-ENTRY POINT                           
         ST    RE,EORADD                                                        
         CLI   FATALERR,C'Y'                                                    
         JE    EXITY                                                            
                                                                                
K        USING CORKEY,SVKEY        BUILD ORDER KEY FROM INPUT DATA              
         XC    K.CORKEY,K.CORKEY                                                
         MVI   K.CORKTYPE,CORKTYPQ                                              
         MVI   K.CORKSTYP,CORKSTYQ                                              
         MVC   K.CORKAGMD,QMEDX                                                 
         MVC   K.CORKCLT,QCLTX                                                  
         MVC   K.CORKCAM,QCAMX                                                  
         XC    K.CORKCAM,EFFS      GET FF COMPLEMENT                            
         MVC   K.CORKORD,QORDX                                                  
         XC    K.CORKORD,EFFS                                                   
                                                                                
         CLI   QACTION,QACTADD     TEST ADDING A NEW ORDER                      
         JNE   BLDOR020                                                         
                                                                                
***********************************************************************         
* READ SEQNUM RECORD TO GET NEXT ORDER NUMBER                         *         
***********************************************************************         
                                                                                
         MVC   K.CORKREV,=H'-2'    SET REVISION 1                               
         XC    IOKEY(L'CORKEY),IOKEY                                            
         MVC   IOKEY(CORKCLT-CORKEY),SVKEY    MOVE TYPE/A-M                     
         NI    IOKEY+CORKAGMD-CORKEY,X'F0'    DROP MEDIA                        
         OI    IOKEY+CORKAGMD-CORKEY,X'08'    SET MEDIA=C                       
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUPD+IOXSPDIR+IO3'                         
         CLC   IOKEY(CORKORD-CORKEY),IOKEYSAV SAME TYPE/A-M                     
         JE    BLDOR010                                                         
                                                                                
***********************************************************************         
* FIRST ORDER FOR THIS AGENCY                                         *         
***********************************************************************         
                                                                                
         GOTOR INIREC,IOKEYSAV                                                  
                                                                                
         USING CORSQELD,ELEM                                                    
         MVI   CORSQEL,CORSQELQ                                                 
         MVI   CORSQLEN,CORSQLNQ                                                
         MVC   CORSQNUM,=F'1'      SET ORDNUM=1                                 
         GOTOR NXTADD                                                           
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOXSPFIL+IO3'                           
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   K.CORKORD,=F'-2'    SET ORDNUM=1 IN KEY                          
         J     BLDOR020                                                         
                                                                                
BLDOR010 GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOXSPFIL+IO3'                        
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING CORSQELD,R3                                                      
         CLI   0(R3),CORSQELQ      ENSURE POINTING TO CORRECT ELEMENT           
         JE    *+6                                                              
         DC    H'0'                                                             
         ICM   R0,15,CORSQNUM      UPDATE SEQUENCE NUMBER                       
         AHI   R0,1                                                             
         STCM  R0,15,CORSQNUM                                                   
         STCM  R0,15,K.CORKORD     SET AS NEW ORDER NUMBER                      
         XC    K.CORKORD,EFFS      COMPLEMENT IT IN THE KEY                     
         GOTOR (#IOEXEC,AIOEXEC),'IOPUT+IOXSPFIL+IO3'                           
         JE    BLDOR020                                                         
         DC    H'0'                                                             
                                                                                
BLDOR020 CLI   QACTION,QACTADD     TEST ADDING NEW ORDER                        
         JNE   BLDOR030                                                         
         GOTOR INIREC,K.CORKEY     INITIALIZE RECORD                            
         J     BLDOR040                                                         
                                                                                
***********************************************************************         
* READ EXISTING ORDER (READ HIGH FOR CURRENT REVISION)                *         
***********************************************************************         
                                                                                
BLDOR030 MVC   IOKEY(L'CORKEY),SVKEY                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUP+IOXSPDIR+IO3'                          
         JNE   EM#NOORD                                                         
         CLC   IOKEY(CORKREV-CORKEY),IOKEYSAV                                   
         JE    *+6                                                              
         DC    H'0'                ORDER DOESN'T EXIST                          
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOXSPFIL+IO3'                        
         JNE   EM#NOORD                                                         
                                                                                
         LLH   R0,CORKREV          UPDATE REVISION NUMBER                       
         SHI   R0,1                                                             
         STCM  R0,3,CORKREV                                                     
                                                                                
***********************************************************************         
* ADD DATA ELEMENTS TO ORDER RECORD                                   *         
***********************************************************************         
                                                                                
BLDOR040 GOTOR SAVINI              SAVE F1 AND DELETE ALL ELEMENTS              
                                                                                
         USING CORIDEL,ELEM                                                     
         XC    CORIDEL(CORIDLNQ),CORIDEL                                        
         MVI   CORIDEL,CORIDELQ                                                 
         MVI   CORIDLEN,CORIDLNQ                                                
         MVC   CORIDMKT,QMKT                                                    
                                                                                
         USING STAPACKD,WORK       GET PACKED STATION                           
         XC    STAPACKD(STAPACKL),STAPACKD                                      
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,LP_AGY                                                   
         MVI   STAPCTRY,C'C'                                                    
*                                                                               
         GOTOR (#EDTMED,AEDTMED),DMCB,QMEDX,0,QMEDA                             
         MVC   STAPMED,QMEDA                                                    
*&&DO                                                                           
         MVI   STAPMED,C'N'                                                     
         TM    QMEDX,X'03'                                                      
         JO    *+8                                                              
         MVI   STAPMED,C'T'                                                     
*&&                                                                             
                                                                                
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPQMKT,=C'0000'                                                
         MVC   STAPQSTA,QSTA                                                    
         GOTOR VSTAPACK,STAPACKD                                                
         CLI   STAPERR,0                                                        
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   CORIDSTA,STAPMKST+2                                              
         MVC   CORIDOFC,QOFFC                                                   
         MVC   CORIDBYR,QBUYER                                                  
         GOTOR NXTADD                                                           
                                                                                
         GOTOR MOVEIT,DMCB,('CORNMELQ',QANAME),('CORNMNAM-CORNMELD',0)          
         GOTOR MOVEIT,(R1),('CORESELQ',QADATA),('CORESLST-CORESELD',0)          
         GOTOR MOVEIT,(R1),('CORCMELQ',QACOMMS),('CORCMCMT-CORCMELD',0)         
                                                                                
         USING CORSTELD,ELEM       STATUS ELEMENT                               
         XC    CORSTELD(CORSTLNQ),CORSTELD                                      
         MVI   CORSTEL,CORSTELQ                                                 
         MVI   CORSTLEN,CORSTLNQ                                                
         MVC   CORSTST(QCORVALL),QCORVALS                                       
         GOTOR NXTADD                                                           
                                                                                
         ICM   R4,15,QAWHO         RECIPIENT ELEMENTS                           
         JZ    BLDOR070                                                         
         USING LW_D,R4                                                          
         LLH   R5,LW_NUMN          GET NUMBER OF ENTRIES                        
         LA    R4,LW_DATA2         POINT TO DATA FOR LIST INPUT                 
         USING CORRLELD,ELEM                                                    
BLDOR060 MVI   CORRLEL,CORRLELQ                                                 
         MVC   CORRLWHO,0(R4)                                                   
         LA    RF,CORRLWHO+L'CORRLWHO-1                                         
         CLI   0(RF),C' '                                                       
         JH    *+8                                                              
         JCT   RF,*-8                                                           
         LA    R0,CORRLEL                                                       
         SR    RF,R0                                                            
         AHI   RF,1                                                             
         STC   RF,CORRLLEN         SET ELEMENT LENGTH                           
         GOTOR NXTADD              ADD ELEMENT                                  
         AHI   R4,L'CORRLWHO       BUMP TO NEXT RECIPIENT                       
         JCT   R5,BLDOR060                                                      
                                                                                
BLDOR070 GOTOR MOVEIT,DMCB,('CORURELQ',QAURL),('CORURL-CORURELD',0)             
         J     EXITY                                                            
                                                                                
BLDOR080 MVI   EORFLAG,C'N'                                                     
         GOTOR BLDACT              BUILD ACTIVITY ELEMENT                       
                                                                                
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOXSPFIL+IO3'                           
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   FULL2,IODA          SAVE DISK ADDRESS                            
                                                                                
         GOTOR LINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTMAP',1)                   
         MVC   DUB(L'CORKORD),CORKORD                                           
         XC    DUB(L'CORKORD),EFFS                                              
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTRAW',1),           +        
               ('LD_LBINQ',DUB),(L'CORKORD,0)                                   
                                                                                
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTRAW',2),           +        
               ('LD_LBINQ',CORKREV),(L'CORKREV,0)                               
*                                                                               
P        USING CORRECD,IOKEY                                                    
         XC    IOKEY,IOKEY                                                      
         MVI   P.CODKTYPE,CODKTYPQ                                              
         MVI   P.CODKSTYP,CODKSTYQ                                              
         MVC   P.CODKAGMD,QMEDX                                                 
         MVC   P.CODKCLT,QCLTX                                                  
         MVC   P.CODKSTA,STAPMKST+2                                             
         MVC   P.CODKCAM,QCAMX                                                  
         XC    P.CODKCAM,EFFS                                                   
         MVC   P.CODKORD,QORDX                                                  
         XC    P.CODKORD,EFFS                                                   
*                                                                               
         GOTOR VDATCON,DMCB,(5,0),(2,P.CODKDATE)                                
         THMS  DDSTIME=YES                                                      
         STCM  R0,15,PACKOF4B                                                   
         ST    R1,FULL                                                          
         AP    PACKOF4B,FULL                                                    
*                                                                               
         CP    PACKOF4B,=P'240000'    PAST MIDNIGHT?                            
         JL    BLDOR090                                                         
         SP    PACKOF4B,=P'240000'    YES, BUMP TO NEXT DAY AND ADJUST          
         GOTOR VDATCON,DMCB,(5,0),(0,DUB)   THE TIME                            
         GOTOR VADDAY,DMCB,DUB,DUB,F'1'                                         
         GOTOR VDATCON,DMCB,(0,DUB),(2,P.CODKDATE)                              
*                                                                               
BLDOR090 TIME  TU                  GET THE TIME IN 38400TH OF A SEC             
         STCM  R0,15,P.CODKTIME                                                 
         XC    P.CODKTIME,EFFS                                                  
*                                                                               
         MVC   IOKEY+36(4),FULL2    DISK ADDRESS                                
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOXSPDIR'                               
         JE    SENDDTTM                                                         
         DC    H'0'                                                             
         DROP  R2,P                                                             
         EJECT                                                                  
***********************************************************************         
* CONTACT RECORD - MAPCODE X'029A'                                    *         
***********************************************************************         
                                                                                
CONREC   LKREQ H,I#COCONT,NEWREC=Y,NEXTREQ=EORREC,ROUTINE=BLDCON                
                                                                                
Action   LKREQ F,1,(D,B#SAVED,QACTION),CHAR,TEXT=SP#ACTN                        
Media    LKREQ F,2,(D,B#WORKD,QMEDX),(U,#VALMED,$VALMED),TEXT=SP#MED            
Contact# LKREQ F,3,(D,B#SAVED,QCONTX),UBIN,TEXT=(*,TCONTNUM)                    
Station  LKREQ F,10,(D,B#SAVED,QDCTSSTA),CHAR,TEXT=SP#STNET                     
EffStart LKREQ F,11,(D,B#SAVED,QDCTSEFF),BDAT,TEXT=(*,TEFFDATE)                 
Office   LKREQ F,12,(D,B#SAVED,QDCTSOFF),CHAR,TEXT=(*,TOFFICE)                  
Client   LKREQ F,13,(D,B#SAVED,QDCTSCLT),CHAR,TEXT=SP#CLI                       
FrstName LKREQ F,14,(D,B#SAVED,QDCTSFNM),CHAR,TEXT=(*,TFNAME),         +        
               LOWERCASE=Y                                                      
LastName LKREQ F,15,(D,B#SAVED,QDCTSLNM),CHAR,TEXT=(*,TLNAME),         +        
               LOWERCASE=Y                                                      
E-mail   LKREQ F,16,(I,B#SAVED,QADATA),VSTR,TEXT=(*,TEMAIL),           +        
               LOWERCASE=Y                                                      
                                                                                
         LKREQ E                                                                
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE AND BUILD CONTACT RECORD                        *         
***********************************************************************         
                                                                                
         USING DCTRECD,R2                                                       
BLDCON   XC    EORADD,EORADD       SET NO RE-ENTRY POINT                        
         CLI   FATALERR,C'Y'                                                    
         JE    EXITY                                                            
                                                                                
         CLI   QACTION,0           TEST ACTION GIVEN                            
         JNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
K        USING DCTKEY,IOKEY        BUILD CONTACT KEY FROM INPUT DATA            
         XC    K.DCTKEY,K.DCTKEY                                                
         MVI   K.DCTKTYPE,DCTKTYPQ                                              
         MVI   K.DCTKSTYP,DCTKSTYQ                                              
         MVC   K.DCTKAGMD,QMEDX                                                 
         NI    K.DCTKAGMD,X'F0'    REGARDLESS OF MEDIA                          
         MVC   K.DCTKCONT,QCONTX                                                
         XC    K.DCTKCONT,EFFS     GET FF COMPLEMENT                            
                                                                                
         CLI   QACTION,QACTADD     TEST ADDING A NEW RECORD                     
         JNE   BLDCON06                                                         
                                                                                
         XC    K.DCTKCONT,K.DCTKCONT                                            
         GOTOR (#IOEXEC,AIOEXEC),'IOHIUPD+IOXSPDIR'                             
                                                                                
         CLC   K.DCTKEY(DCTKCONT-DCTKEY),IOKEYSAV                               
         JE    BLDCON02                                                         
         MVC   K.DCTKEY,IOKEYSAV   RESTORE KEY                                  
         LHI   R0,-1                                                            
         STCM  R0,15,K.DCTKCONT    SET FIRST CONTACT NUMBER ...FFFF             
         J     BLDCON04                                                         
                                                                                
BLDCON02 ICM   R0,15,K.DCTKCONT    GET CONTACT NUMBER                           
         SHI   R0,1                INCREMENT                                    
         STCM  R0,15,K.DCTKCONT                                                 
                                                                                
BLDCON04 STCM  R0,3,QLIN           SAVE IT                                      
         XC    QLIN,EFFS           AND MAKE POSITIVE                            
         GOTOR INIREC,K.DCTKEY     INITIALIZE IO3 FOR NEW RECORD                
         J     BLDCON08                                                         
                                                                                
***********************************************************************         
* READ EXISTING CONTACT                                               *         
***********************************************************************         
                                                                                
BLDCON06 GOTOR (#IOEXEC,AIOEXEC),'IORDUP+IOXSPDIR+IO3'                          
         JNE   EM#NOCON                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOGETRUP+IOXSPFIL+IO3'                        
         JNE   EM#NOCON                                                         
                                                                                
         CLI   QACTION,QACTDEL     TEST DELETE                                  
         JNE   BLDCON08                                                         
         GOTOR DELREC                                                           
         J     EXITY                                                            
                                                                                
***********************************************************************         
* ADD DATA ELEMENTS TO RECORD                                         *         
***********************************************************************         
                                                                                
BLDCON08 GOTOR SAVINI              SAVE F1 AND DELETE ALL ELEMENTS              
                                                                                
         USING DCTDSEL,ELEM                                                     
         XC    DCTDSEL(DCTDSLNQ),DCTDSEL                                        
         MVI   DCTDSEL,DCTDSELQ                                                 
         MVI   DCTDSLEN,DCTDSLNQ                                                
         MVC   DCTDSSTA(QDCTVALL),QDCTVALS                                      
         GOTOR NXTADD                                                           
                                                                                
         GOTOR MOVEIT,DMCB,('DCTEMELQ',QADATA),('DCTEMEML-DCTEMELD',0)          
                                                                                
         GOTOR BLDACT              BUILD ACTIVITY ELEMENT                       
                                                                                
         CLI   QACTION,QACTADD     TEST ADD                                     
         JNE   BLDCON12                                                         
         GOTOR (#IOEXEC,AIOEXEC),'IOADD+IOXSPFIL+IO3'                           
         JE    BLDCON14                                                         
         DC    H'0'                                                             
                                                                                
BLDCON12 GOTOR (#IOEXEC,AIOEXEC),'IOPUT+IOXSPFIL+IO3'                           
         JE    BLDCON14                                                         
         DC    H'0'                                                             
                                                                                
BLDCON14 GOTOR LINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTMAP',1)                   
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTRAW',2),           +        
               ('LD_LBINQ',QLIN),(L'QLIN,0)                                     
         J     SENDDTTM                                                         
         EJECT                                                                  
***********************************************************************         
* EOR RECORD - MAPCODE X'029F'                                        *         
***********************************************************************         
                                                                                
EORREC   LKREQ H,I#COEOR,NEWREC=N,NEXTREQ=ENDREC,ROUTINE=SETEOR                 
                                                                                
ActvtyDt LKREQ F,1,(D,B#SAVED,QACTVDT),BDAT,TEXT=(*,TACTIVDT)                   
ActvtyTm LKREQ F,2,(D,B#SAVED,QACTVTM),HEXD,TEXT=(*,TACTIVTM)                   
                                                                                
         LKREQ E                                                                
                                                                                
SETEOR   OC    EORADD,EORADD       TEST ANY ROUTINE TO CALL                     
         BZR   RE                  NO - EXIT                                    
         CLI   FATALERR,C'Y'                                                    
         BER   RE                                                               
         MVI   EORFLAG,C'Y'        SET UPDATE TRIGGER                           
         GOTOR EORADD              RE-CALL LAST RECORD ROUTINE                  
                                                                                
ENDREC   LKREQ X                                                                
                                                                                
TACTIVDT DC    C'Activity date'                                                 
TACTIVTM DC    C'Activity time'                                                 
TADJCODE DC    C'Adjacency code'                                                
TAVAILID DC    C'Avail id'                                                      
TBOOKTYP DC    C'Book type'                                                     
TBOOK    DC    C'Book'                                                          
TBUYER   DC    C'Buyer'                                                         
TBUYID   DC    C'Buy id'                                                        
TBUYTYPE DC    C'Buy type'                                                      
TCAMNUM  DC    C'Campaign'                                                      
TCOMMENT DC    C'Comment'                                                       
TCONTNUM DC    C'Contact number'                                                
TCOST    DC    C'Cost'                                                          
TDAYS    DC    C'Days'                                                          
TDAYOFWK DC    C'Week start day'                                                
TDEMLIST DC    C'Demo list'                                                     
TDEMLKUP DC    C'Look-up demo vales'                                            
TDEMOEST DC    C'Estimated demo values'                                         
TDEMOVAL DC    C'Demo values'                                                   
TDEMOVER DC    C'Demo overrides'                                                
TDEMREP  DC    C'Rep demo values'                                               
TCAMDESC DC    C'Campaign description'                                          
TDAYPART DC    C'Daypart'                                                       
TDPTMENU DC    C'Daypart menu'                                                  
TEFFDATE DC    C'Effective date'                                                
TEMAIL   DC    C'E-mail address'                                                
TENDDATE DC    C'End date'                                                      
TENDTIME DC    C'End time'                                                      
TESTLST  DC    C'Estimate list'                                                 
TESTOVRD DC    C'Estimate overrides'                                            
TFLAG    DC    C'Flag'                                                          
TFLTEND  DC    C'Flight end date'                                               
TFLTSTRT DC    C'Flight start date'                                             
TFNAME   DC    C'First name'                                                    
TLINE    DC    C'Line'                                                          
TLKUPMKT DC    C'Look-up market'                                                
TLKUPRSV DC    C'Look-up rating service'                                        
TLKUPSTA DC    C'Look-up station'                                               
TLNAME   DC    C'Last name'                                                     
TLOCAL   DC    C'Local station'                                                 
TNAME    DC    C'Name'                                                          
TOFFICE  DC    C'Office'                                                        
TOFFERTY DC    C'Offer type'                                                    
TACTFLG  DC    C'Activity flag'                                                 
TORDCOM  DC    C'Comments'                                                      
TORDDOL  DC    C'Dollars'                                                       
TORDESC  DC    C'Order description'                                             
TORDNUM  DC    C'Order number'                                                  
TORDURL  DC    C'Order URL'                                                     
TORDEDT  DC    C'Expiration date'                                               
TORDETM  DC    C'Expiration time'                                               
TORDSPT  DC    C'Spots'                                                         
TORSTAT  DC    C'Order status'                                                  
TPRD     DC    C'Product'                                                       
TPRD1    DC    C'Product 1'                                                     
TPRD2    DC    C'Product 2'                                                     
TPROGRAM DC    C'Program'                                                       
TPROVCD  DC    C'Province code'                                                 
TRCPTS   DC    C'Recipients'                                                    
TREPCD   DC    C'Rep code'                                                      
TREPCOM  DC    C'Rep comments'                                                  
TREPCOST DC    C'Rep cost'                                                      
TSHOWCD  DC    C'Show code'                                                     
TSLN     DC    C'Spot length'                                                   
TVALSLN  DC    C'Valid spot lengths'                                            
TSPLMKT  DC    C'Spill market'                                                  
TSPTCNTS DC    C'Spot counts'                                                   
TSPTDTS  DC    C'Spot dates'                                                    
TSTAPCT  DC    C'Percentages'                                                   
TSTART   DC    C'Start date'                                                    
TSTATE   DC    C'State'                                                         
TSTATUS  DC    C'Status'                                                        
TSTATYPE DC    C'Station type'                                                  
TSTALIST DC    C'Station list number'                                           
TSTRTIME DC    C'Start time'                                                    
TTOTLEN  DC    C'Total length'                                                  
TTOKEN   DC    C'Token'                                                         
TTOTLNS  DC    C'Total lines'                                                   
TUNAVWK  DC    C'Unavailable weeks'                                             
TWEEKS   DC    C'Weeks'                                                         
TCMPIDCP DC    C'Cnvt Dynamic Prorations'                                       
TCMPIDSP DC    C'Spcl Dynamic Prorations'                                       
TPID     DC    C'PID'                                                           
TTIMESTP DC    C'DateTime Stamp'                                                
         EJECT                                                                  
***********************************************************************         
* MOVE DATA TO ELEMENT AND ADD IT TO RECORD                           *         
*                                                                     *         
* NTRY:- R1 POINTS TO PARAMETER LIST AS FOLLOWS                       *         
*                                                                     *         
*        P1/B0   - ELEMENT CODE                                       *         
*           B1-3 - INDEX VALUE                                        *         
*        P2/B0   - DISPLACEMENT TO DATA IN ELEMENT                    *         
*           B1-3 - NOT DEFINED                                        *         
***********************************************************************         
                                                                                
MOVEIT   NTR1  LABEL=*                                                          
                                                                                
         LR    R2,R1               R2=A(PARAMETER LIST)                         
         ICM   R1,7,1(R2)          GET A(LW_D)                                  
         JZ    EXITN               BAD EXIT IF DATA NOT PRESENT                 
         MVC   ELEM+0(1),0(R2)     SET ELEMENT CODE                             
         MVC   ELEM+1(1),4(R2)     SET INITIAL ELEMENT LENGTH                   
         LLH   RF,LW_LN-LW_D(R1)   ENTRY LENGTH                                 
         SHI   RF,LW_LN1Q          ADJUST TO GET DATA LEN                       
         LHI   R0,LW_DATA1-LW_D    DATA FOR NON-LIST INPUT                      
         TM    LW_TYPE-LW_D(R1),LW_TLSTQ                                        
         JZ    *+12                                                             
         LHI   R0,LW_DATA2-LW_D    DATA FOR LIST INPUT                          
         SHI   RF,LW_LN2Q-LW_LN1Q                                               
         AR    R1,R0               POINT TO DATA                                
                                                                                
         LLC   R0,ELEM+1                                                        
         AR    R0,RF                                                            
         CHI   R0,FF                                                            
         JNH   *+6                                                              
         DC    H'0'                DATA IS TOO LONG FOR ELEMENT                 
         STC   R0,ELEM+1           SET ELEMENT LENGTH                           
                                                                                
         BCTR  RF,0                                                             
         LLC   R4,4(R2)            GET DATA DISPLACEMNT                         
         LA    R4,ELEM(R4)         POINT TO DATA IN ELEMENT                     
         BASR  RE,0                                                             
         MVC   0(0,R4),0(R1)                                                    
         EX    RF,0(RE)            MOVE DATA TO ELEMENT                         
         GOTOR NXTADD              ADD ELEMENT TO RECORD                        
         CR    RE,RE               SET CONDITION CODE TO EQUAL                  
                                                                                
EXITR3   XIT1  REGS=(R3)           EXIT WITH R3 INTACT                          
*                                                                               
INIREC   L     RF,AIO3             INITIALIZE A RECORD                          
         XC    0(256,RF),0(RF)                                                  
         MVC   0(L'CMPKEY,RF),0(R1)                                             
         LHI   R0,CMPFRST+1-CMPKEY                                              
         STCM  R0,3,CMPLEN-CMPRECD(RF)                                          
         MVC   CMPAGY-CMPRECD(,RF),LP_AGY                                       
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ACTIVITY ELEMENT RECEIVED - CHECK AGAINST VALUE IN RECORD           *         
* FOR CHANGES, PC RETURNS DATE AND TIME OF LAST ACTIVITY WHICH GETS   *         
* CHECKED AGAINST VALUE SVACTEL                                       *         
***********************************************************************         
                                                                                
BLDACT   NTR1  LABEL=*                                                          
                                                                                
         GOTOR VDATCON,DMCB,(5,0),(3,DUB)                                       
                                                                                
         LA    R4,DUB+3                                                         
         LA    R5,WORK                                                          
         TIME  BIN,(R4),MF=(E,(R5)),LINKAGE=SYSTEM                              
                                                                                
         USING CDSACELD,ELEM                                                    
         CLI   QACTION,QACTADD     ADD ALWAYS BUILDS A NEW ONE                  
         JE    BLDACT06                                                         
         CLI   QACTION,QACTADR     SO DOES CHANGING A DELETED RECORD            
         JE    BLDACT06                                                         
         OC    SVACTEL,SVACTEL     MUST HAVE SAVED ACTIVITY ELEMENT             
         JZ    BLDACT06            BUT THERE'S NO WAY TO RECOVER                
         MVC   CDSACELD(L'SVACTEL),SVACTEL                                      
                                                                                
BLDACT02 LA    R1,CDSACADT                                                      
         OC    CDSACCDT,CDSACCDT   TEST ANY CHANGE DATE                         
         JZ    *+8                                                              
         LA    R1,CDSACCDT                                                      
         OC    QACTVDT,QACTVDT     TEST NOT SENT                                
         JZ    BLDACT04            YES - IGNORE                                 
         CLC   0(L'CDSACCDT,R1),QACTVDT                                         
         JNE   EM#CKSUM                                                         
         CLC   L'CDSACCDT(L'CDSACCTM,R1),QACTVTM                                
         JNE   EM#CKSUM                                                         
                                                                                
BLDACT04 MVC   CDSACCDT,DUB        SET LAST CHANGE DATE AND TIME                
         MVC   CDSACCTM,DUB+3                                                   
         J     BLDACT08                                                         
                                                                                
BLDACT06 XC    CDSACELD(CDSACLNQ),CDSACELD                                      
         MVI   CDSACEL,CDSACELQ                                                 
         MVI   CDSACLEN,CDSACLNQ                                                
         MVC   CDSACADT,DUB        SET ADD DATE AND TIME                        
         MVC   CDSACATM,DUB+3                                                   
                                                                                
BLDACT08 L     R1,LP_ASECD                                                      
         MVC   CDSACABY,SECOPASS-SECD(R1)                                       
         LLH   R3,CSELEN-CSERECD(R2)                                            
         AR    R3,R2                                                            
         BCTR  R3,0                POINT TO END OF RECORD                       
         GOTOR NXTADD              ADD ACTIVITY ELEMENT                         
         MVC   SVACTEL,ELEM        SAVE IT HERE FOR SENDDTTM                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* SAVE ACTIVITY ELEMENT AND INITIALIZE FOR NEW RECORD BUILDING        *         
***********************************************************************         
                                                                                
SAVINI   NTR1  LABEL=*                                                          
         USING CMPRECD,R2                                                       
         LA    R3,CMPFRST                                                       
         USING CDSACELD,R3                                                      
         XC    SVACTEL,SVACTEL                                                  
         SR    R0,R0                                                            
SAVINI02 CLI   CDSACEL,0           TEST END OF RECORD                           
         JE    SAVINI04                                                         
         CLI   CDSACEL,CDSACELQ                                                 
         JE    *+14                                                             
         IC    R0,CDSACLEN                                                      
         AR    R3,R0                                                            
         J     SAVINI02                                                         
         MVC   SVACTEL,CDSACELD    SAVE CURRENT ACTIVITY ELEMENT                
SAVINI04 LHI   R0,CMPFRST+1-CMPRECD                                             
         STCM  R0,3,CMPLEN         SET INITIAL RECORD LENGTH                    
         XC    CMPFRST(256),CMPFRST                                             
         LA    R3,CMPFRST          POINT R3 TO FIRST ELEMENT                    
         J     EXITR3                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* Validate action code                                                          
***********************************************************************         
                                                                                
VALACTCD LM    R2,R4,LP_AINP                                                    
         CHI   R3,1                Test one byte of input?                      
         JNE   EXITN                                                            
         CLI   0(R2),QACTADD       One of the acceptable value?                 
         JE    VACTCX                                                           
         CLI   0(R2),QACTADR                                                    
         JE    VACTCX                                                           
         CLI   0(R2),QACTCHA                                                    
         JE    VACTCX                                                           
         CLI   0(R2),QACTDEL                                                    
         JNE   EXITN               Nope                                         
                                                                                
VACTCX   MVC   0(1,R4),0(R2)                                                    
         J     EXITY                                                            
***********************************************************************         
* VALIDATE DEMO VALUE - * MEANS NO VALUE SENT                         *         
***********************************************************************         
                                                                                
VALDVL   LM    R2,R4,LP_AINP                                                    
         LHI   R0,-1               PRESET VALUE FOR NO-VALUE                    
         CHI   R3,1                TEST ONE BYTE OF INPUT                       
         JNE   VALDVL02                                                         
         CLI   0(R2),C'*'                                                       
         JE    VALDVLX                                                          
                                                                                
VALDVL02 LR    RE,R2                                                            
         LR    RF,R3                                                            
                                                                                
VALDVL04 CLI   0(RE),C'0'                                                       
         JL    EXITN                                                            
         CLI   0(RE),C'9'                                                       
         JH    EXITN                                                            
         LA    RE,1(RE)                                                         
         JCT   RF,VALDVL04                                                      
                                                                                
         BCTR  R3,0                                                             
         BASR  RE,0                                                             
         EX    R3,8(RE)                                                         
         J     *+10                                                             
         PACK  DUB,0(0,R2)                                                      
         CVB   R0,DUB                                                           
                                                                                
VALDVLX  STCM  R0,7,0(R4)          SET 3-BYTE OUTPUT VALUE                      
         J     EXITY                                                            
                                                                                
***********************************************************************         
* VALIDATE ADJACENCY CODE                                             *         
***********************************************************************         
                                                                                
VALADJ   LM    R2,R4,LP_AINP                                                    
         MVC   0(L'QDPRDADJ,R4),0(R2)                                           
         CHI   R3,1                TEST RECEIVED 2 CHARS                        
         JNH   VALADJX                                                          
         PACK  DUB,0(3,R2)         PACK ONE EXTRA BYTE                          
         MVC   0(L'QDPRDADJ,R4),DUB+6                                           
VALADJX  CR    RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE MARKET FOR BUY DOWNLOAD                         *         
***********************************************************************         
                                                                                
VALMKT   L     R1,LP_AINP                                                       
         L     RF,LP_ILEN                                                       
         SHI   RF,1                                                             
         MVC   DUB,EZEROS                                                       
         BASR  RE,0                                                             
         MVZ   DUB(0),0(R1)                                                     
         EX    RF,0(RE)                                                         
         CLC   DUB,EZEROS                                                       
         JNE   EXITN                                                            
         BASR  RE,0                                                             
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         PACK  DUB,0(0,R1)                                                      
         OI    DUB+L'DUB-1,X'0F'                                                
         LA    R1,IOKEY                                                         
         USING MKTRECD,R1                                                       
         XC    MKTKEY,MKTKEY                                                    
         MVI   MKTKTYPE,MKTKTYPQ                                                
         MVC   MKTKMED,QMEDA                                                    
         UNPK  MKTKMKT,DUB                                                      
         MVC   MKTKAGY,LP_AGY                                                   
         MVC   MKTKFILL,EZEROS                                                  
         GOTOR (#IOEXEC,AIOEXEC),'IORD+IOSTAFIL+IO5'                            
         JNE   EXITN                                                            
         CVB   R0,DUB                                                           
         L     R1,LP_AOUT                                                       
         STCM  R0,3,0(R1)                                                       
         J     EXITY                                                            
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* DELETE DIRECTORY AND FILE RECORDS                                   *         
***********************************************************************         
                                                                                
DELREC   NTR1  LABEL=*                                                          
         OI    IOKEY+(CSEKSTAT-CSERECD),X'80'                                   
         GOTOR (#IOEXEC,AIOEXEC),'IOWRT+IOXSPDIR+IO3'                           
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R1,AIO3                                                          
         OI    CSESTAT-CSERECD(R1),X'80'                                        
         GOTOR (#IOEXEC,AIOEXEC),'IOPUT+IOXSPFIL+IO3'                           
         JE    EXITY                                                            
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* GET PACKED STATION FOR NETWORK                                      *         
* INPUT  IS QLOCAL/QPROVCD                                            *         
* OUTPUT IS SVLOCALX                                                  *         
***********************************************************************         
                                                                                
GOSTAP   NTR1  LABEL=*                                                          
         USING STAPACKD,WORK                                                    
         XC    STAPACKD(STAPACKL),STAPACKD                                      
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,LP_AGY                                                   
         MVI   STAPCTRY,C'C'                                                    
         MVI   STAPMED,C'N'                                                     
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPQMKT,=C'0000'                                                
         MVC   STAPQSTA(5),QLOCAL                                               
         MVC   STAPQNET,QPROVCD                                                 
         GOTOR VSTAPACK,STAPACKD                                                
         CLI   STAPERR,0                                                        
         JE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVLOCALX,STAPMKST+2                                              
         J     EXITY                                                            
         EJECT                                                                  
EM#CKSUM LHI   R1,SE#CKSUM         CHECKSUM MISMATCH                            
         J     VALERRX                                                          
                                                                                
EM#MSGMK LHI   R1,SE#MSGMK         MISSING MARKET                               
         J     VALERRX                                                          
                                                                                
EM#INVST MVI   XTRATEXT+5,C'='                                                  
         MVC   XTRATEXT+6(L'QSTA),QSTA                                          
         LHI   R1,SE#INVST         INVALID STATION/NETWORK                      
         J     VALERRX                                                          
                                                                                
EM#MSGSN LHI   R1,SE#MSGSN         MISSING STATION/NETWORK                      
         J     VALERRX                                                          
                                                                                
EM#NOCAM LHI   R1,SE#NOCAM         CAMPAIGN RECORD NOT FOUND                    
         J     VALERRX                                                          
                                                                                
EM#NOPGM LHI   R1,SE#NOPGM         PROGRAM RECORD NOT FOUND                     
         J     VALERRX                                                          
                                                                                
EM#NOORD LHI   R1,SE#NOORD         ORDER RECORD NOT FOUND                       
         J     VALERRX                                                          
                                                                                
EM#NOCON LHI   R1,SE#NOCON         CONTACT RECORD NOT FOUND                     
         J     VALERRX                                                          
                                                                                
EM#CNTAD LHI   R1,SE#CNTAD         Can't add, duplicate record                  
         J     VALERRM                                                          
                                                                                
EM#NOMMP LHI   R1,SE#NOMMP         Market Mapping record not found              
         J     VALERRM                                                          
                                                                                
EM#NODMV LHI   R1,SE#NODMV         DEMO RECORD NOT FOUND                        
                                                                                
VALERRX  MVI   FATALERR,C'Y'       SET TO SKIP ALL DATA FOR BUY                 
VALERRM  CVD   R1,DUB              Can have error and process rest              
         OI    DUB+7,X'0F'                                                      
         UNPK  XTRATEXT(4),DUB                                                  
         GOTOR PUTERR                                                           
         J     EXITY                                                            
                                                                                
EXITN    SR    RE,RE                                                            
         J     EXITCC                                                           
EXITY    LHI   RE,1                                                             
EXITCC   CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* SEND DATE AND TIME FOR ANY RECORD ADDED/CHANGED                     *         
***********************************************************************         
                                                                                
X        USING CDSACELD,SVACTEL                                                 
SENDDTTM LA    R2,X.CDSACADT                                                    
         OC    X.CDSACCDT,X.CDSACCDT                                            
         JZ    *+8                                                              
         LA    R2,X.CDSACCDT                                                    
         GOTOR LINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTRAW',5),         +        
               ('LD_BDATQ',0(R2)),(L'CDSACCDT,0)                                
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTRAW',6),           +        
               ('LD_HEXDQ',L'CDSACADT(R2)),(L'CDSACATM,0)                       
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTRAW',8),           +        
               ('LD_LBINQ',L'CDSACADT(R2)),(L'CDSACATM,0)                       
         J     EXITY                                                            
         DROP  X                                                                
         EJECT                                                                  
***********************************************************************         
* CALL LINKIO TO BUILD ERROR RETURN                                   *         
***********************************************************************         
                                                                                
PUTERR   NTR1  LABEL=*                                                          
         MVI   FATALERR,C'Y'       SET TO IGNORE ALL FURTHER INPUT              
         STCM  R1,3,WORK           SET ERROR NUMBER IN WORK                     
         GOTOR LINKIO,DMCB,('LIOAPUT',LP_ALIOB),('LIOTMAP',1)                   
         GOTOR (RF),(R1),('LIOAPUT',LP_ALIOB),('LIOTERR',4),           +        
               WORK,(L'XTRATEXT,XTRATEXT)                                       
         MVC   XTRATEXT,SPACES     RESET EXTRA MESSAGE TEXT TO SPACES           
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* ADD ELEMENT TO RECORD                                               *         
***********************************************************************         
                                                                                
NXTADD   CLI   0(R3),0             TEST AT EOR ALREADY                          
         JE    NXTADD02                                                         
         LLC   R0,1(R3)            BUMP TO NEXT AND ADD ELEMENT                 
         AR    R3,R0                                                            
                                                                                
NXTADD02 LR    R0,RE               ADD ELEMENT TO RECORD                        
         L     RF,AIO3             POINT TO RECORD                              
         LLH   RE,CSELEN-CSERECD(RF)                                            
         LLC   R1,ELEM+1           GET LENGTH OF NEW ELEMENT                    
         AR    RE,R1               UPDATE RECORD LENGTH                         
         STCM  RE,3,CSELEN-CSERECD(RF)                                          
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         MVC   0(0,R3),ELEM                                                     
         EX    R1,0(RE)            MOVE ELEMENT TO RECORD                       
         LA    RE,1(R3,R1)                                                      
         MVI   0(RE),0             SET NEW END OF RECORD                        
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
SE#CKSUM EQU   1277                                                             
SE#NOCAM EQU   1293                                                             
SE#NOPGM EQU   1294                                                             
SE#NODMV EQU   1295                                                             
SE#NOORD EQU   1330                                                             
SE#NOCON EQU   1331                                                             
SE#CNTAD EQU   1                   Can't add, key already exists                
SE#NOMMP EQU   2                   Record does not exist                        
                                                                                
GLOBALS  DS    0D                  ** GLOBALLY ADDRESSABLE VALUES **            
         LTORG                                                                  
EZEROS   DC    80C'0'                                                           
                                                                                
M#COSPL  DC    AL2(I#COSPL)        SPILL DEMOS                                  
M#COMKMP DC    AL2(I#COMKMP)       MARKET MAPPING                               
M#COSTMP DC    AL2(I#COSTMP)       STATION MAPPING                              
M#CODMMP DC    AL2(I#CODMMP)       DEMO CAT MAPPING                             
         EJECT                                                                  
SAVED    DSECT                                                                  
                                                                                
LINKIO   DS    A                   A(LINKIO)                                    
RECUP    DS    A                   A(RECUP)                                     
GETFACT  DS    A                   A(GETFACT)                                   
EORADD   DS    A                   A(UPDATE ROUTINE ENTRY POINT)                
                                                                                
FATALERR DS    C                   FATAL ERROR HAS OCCURRED                     
                                                                                
XTRATEXT DS    CL20                EXTRA ERROR MESSAGE TEXT                     
                                                                                
EORFLAG  DS    X                                                                
                                                                                
QVALUES  DS    0F                  ** RECEIVED VALUES **                        
                                                                                
QASLNS   DS    A                                                                
QAFLTS   DS    A                                                                
QADESC   DS    A                                                                
QACOMMS  DS    A                                                                
QARCOM   DS    A                                                                
QAURL    DS    A                                                                
QAAVAIL  DS    A                                                                
QAOFFER  DS    A                                                                
QAWEEKS  DS    A                                                                
QASPOTS  DS    A                                                                
QADEMS   DS    A                                                                
QADATA   DS    A                                                                
QAWHO    DS    A                                                                
*                                                                               
QAMMPTX  DS    0A                                                               
QASMPTX  DS    0A                                                               
QADMPTX  DS    0A                                                               
QANAME   DS    A                                                                
QASTAS   DS    A                                                                
QAPRDS   DS    A                                                                
                                                                                
QACTION  DS    C                   ** ACTION CODE **                            
QACTADD  EQU   C'A'                ADD A NEW RECORD                             
QACTADR  EQU   C'B'                ADDING A PREVIOUSLY DELETED RECORD           
QACTCHA  EQU   C'C'                CHANGE AN EXISTING RECORD                    
QACTDEL  EQU   C'D'                DELETE AN EXISTING RECORD                    
                                                                                
QPROVCD  DS    XL(L'NPCTPROV)                                                   
QSPLMKT  DS    XL(L'DDMDVMKT)                                                   
QCAMX    DS    XL(L'CMPKCAM)                                                    
QORDX    DS    XL(L'CORKORD)                                                    
QCONTX   DS    XL(L'DCTKCONT)                                                   
QACTVDT  DS    XL(L'CDSACADT)                                                   
QACTVTM  DS    XL(L'CDSACATM)                                                   
QLISTNUM DS    XL(L'CSLKLIST)                                                   
QOFFC    DS    CL(L'CORIDOFC)                                                   
QBUYER   DS    CL(L'CORIDBYR)                                                   
QMKT     DS    XL(L'CORIDMKT)                                                   
QDMO     DS    XL(L'DMPDMDMO)                                                   
QLIN     DS    XL(L'DPRKLIN)                                                    
QSTATE   DS    CL(L'DPRPXST)                                                    
QSTA     DS    CL8                                                              
QLOCAL   DS    CL8                                                              
QTOKEN   DS    CL3                                                              
QPID     DS    CL(L'DPRACPID)                                                   
QDATTIM  DS    CL(L'DPRACDTM)                                                   
                                                                                
QTYPSTA  DS    0XL(L'QTYPE+L'QSTA5)                                             
QTYPE    DS    XL(L'CSEKSTTY)                                                   
QSTA5    DS    XL(L'CSEKSTA)                                                    
                                                                                
QMAPTEXT DS    CL(L'DMPDTEXT)      Mapping text                                 
                                                                                
QRECVALS DS    0X                                                               
                                                                                
QCORVALS DS    0X                  ** ORDER VALUES **                           
QCORSTA  DS    XL(L'CORSTST)                                                    
QCORACTF DS    XL(L'CORSTACT)                                                   
QCORSPT  DS    XL(L'CORSTSPT)                                                   
QCORCOS  DS    XL(L'CORSTCOS)                                                   
QCORTEXD DS    XL(L'CORSTEXD)                                                   
QCORTEXT DS    XL(L'CORSTEXT)                                                   
QCORTLNS DS    XL(L'CORSTLNS)                                                   
QCORVALL EQU   *-QCORVALS                                                       
                                                                                
         ORG   QRECVALS                                                         
QDCTVALS DS    0X                  ** CONTACT VALUES **                         
QDCTSSTA DS    XL(L'DCTDSSTA)                                                   
QDCTSEFF DS    XL(L'DCTDSEFF)                                                   
QDCTSOFF DS    XL(L'DCTDSOFF)                                                   
QDCTSCLT DS    XL(L'DCTDSCLT)                                                   
QDCTSFNM DS    XL(L'DCTDSFNM)                                                   
QDCTSLNM DS    XL(L'DCTDSLNM)                                                   
QDCTVALL EQU   *-QDCTVALS                                                       
                                                                                
         ORG   QRECVALS                                                         
QCMPVALS DS    0X                  ** CAMPAIGN VALUES **                        
QCMPDSDT DS    XL(L'CMPIDSDT)                                                   
QCMPDEDT DS    XL(L'CMPIDEDT)                                                   
QCMPDPR1 DS    XL(L'CMPIDPR1)                                                   
QCMPDPR2 DS    XL(L'CMPIDPR2)                                                   
QCMPDSTA DS    XL(L'CMPIDSTA)                                                   
QCMPDTYP DS    XL(L'CMPIDTYP)                                                   
QCMPDDAY DS    XL(L'CMPIDDAY)                                                   
QCMPDDPT DS    XL(L'CMPIDDPT)                                                   
QCMPDSBK DS    XL(L'CMPIDSBK)                                                   
QCMPIDCP DS    XL(L'CMPIDCDP)                                                   
QCMPIDSP DS    XL(L'CMPIDSDP)                                                   
QCMPVALL EQU   *-QCMPVALS                                                       
                                                                                
         ORG   QRECVALS                                                         
QDPRVALS DS    0X                  ** PROGRAM VALUES **                         
QDPRSTOV DS    XL(L'DPRESTOV)                                                   
QDPRDSDT DS    XL(L'DPRPDSDT)                                                   
QDPRDEDT DS    XL(L'DPRPDEDT)                                                   
QDPRDDAY DS    XL(L'DPRPDDAY)                                                   
QDPRDSTM DS    XL(L'DPRPDSTM)                                                   
QDPRDETM DS    XL(L'DPRPDETM)                                                   
QDPRDDPT DS    CL(L'DPRPDDPT)                                                   
QDPRDPRD DS    CL(L'DPRPDPRD)                                                   
QDPRDPR2 DS    CL(L'DPRPDPR2)                                                   
QDPRDSLN DS    XL(L'DPRPDSLN)                                                   
QDPRDSL2 DS    XL(L'DPRPDSL2)                                                   
QDPRDSHO DS    CL(L'DPRPDSHO)                                                   
QDPRDPGM DS    CL(L'DPRPDPGM)                                                   
QDPRDCOS DS    XL(L'DPRPDCOS)                                                   
QDPRDADJ DS    CL(L'DPRPDADJ)                                                   
QDPRDREP DS    CL(L'DPRPDREP)                                                   
QDPRPDID DS    CL(L'DPRPDID)                                                    
QDPRDORD DS    XL(L'DPRPDORD)                                                   
QDPRDRCS DS    XL(L'DPRPDRCS)                                                   
QDPRVALL EQU   *-QDPRVALS                                                       
                                                                                
         ORG   QRECVALS                                                         
QDDMVALS DS    0X                  ** DEMO VALUES **                            
QDDMDBOK DS    XL(L'DDMDDBOK)                                                   
QDDMDBTY DS    CL(L'DDMDDBTY)                                                   
QDDMDPGM DS    CL(L'DDMDDPGM)                                                   
QDDMDMKT DS    XL(L'DDMDDMKT)                                                   
QDDMDSTA DS    CL(L'DDMDDSTA)                                                   
QDDMDRSV DS    CL(L'DDMDDRSV)                                                   
QDDMVALL EQU   *-QDDMVALS                                                       
                                                                                
         ORG                                                                    
                                                                                
SVKEY    DS    XL(L'IOKEY)         SAVED KEY                                    
SVACTEL  DS    XL(CDSACLNQ)        SAVED ACTIVITY ELEMENT                       
SVBSTA   DS    XL(L'DDMKSTA)       SAVED BINARY MARKET/STATION                  
SVLOCALX DS    XL(L'DDMKLCL)       SAVED LOCAL STATION BINARY                   
SVCAMX   DS    XL(L'CSEKCAM)       SAVED CAMPAIGN NUMBER                        
SVLIN    DS    XL(L'QLIN)          SAVED LINE NUMBER                            
SVWEEKS  DS    0X                  SAVED WEEKS                                  
PACKOF4B DS    PL4                 PACKED OF 4 BYTES                            
                                                                                
SAVEL    EQU   *-SAVED                                                          
                                                                                
NPCTD    DSECT                     ** NETWORK PERCENTAGE INPUT ROW **           
NPCTMKT  DS    XL(L'DPRNPMKT)      MARKET                                       
NPCTSTA  DS    CL4                 STATION                                      
NPCTPROV DS    CL2                 PROVINCE                                     
NPCTPCT  DS    XL(L'DPRNPPCT)      PERCENTAGE                                   
NPCTLEN  EQU   *-NPCTD                                                          
                                                                                
* OTHER INCLUDED BOOKS                                                          
         PRINT OFF                                                              
       ++INCLUDE SPGENCDORD                                                     
       ++INCLUDE SPLNKWRK                                                       
       ++INCLUDE SPMSGEQUS                                                      
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE DDLINKIOD                                                      
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010SPLNK24   09/12/17'                                      
         END                                                                    
