*          DATA SET NENAV27    AT LEVEL 034 AS OF 04/04/18                      
*PHASE T31827A                                                                  
NENAV27  TITLE '- MATCHMAKER - UNIT UPLOAD/INVOICE ACTIONS'                     
NENAV27  CSECT                                                                  
*                                                                               
* NOTE: THIS PROGRAM MUST BE REASSMBLED IF THERE ARE                            
*       ANY CHANGES TO BUYUPLDD.                                                
*                                                                               
         PRINT NOGEN                                                            
         NMOD1 0,**NN27**                                                       
         LR    RC,R1                                                            
         USING WORKD,RC            RC=A(GLOBAL WORKING STORAGE)                 
         L     R9,ATWA                                                          
         USING TWAD,R9             R9=A(TWA)                                    
         USING TSARD,TSARBLK       TSAR BLOCK                                   
         USING MMSAVED,OVERSAVE    SAVED STORAGE                                
SAVE     USING SNVKEYD,MMIKEY      SAVED INVOICE KEY                            
                                                                                
         CLI   SVRESUME,0          TEST RETURNING FROM BUY PROGRAM              
         BNE   UNTUPL06                                                         
                                                                                
         GOTOR TSTUPD              TEST FILE IS UPDATABLE                       
                                                                                
         MVC   MMSRECL,TSRECL      SAVE TSAR RECORD LENGTH                      
                                                                                
         OC    MMI2DATE,MMI2DATE   TEST ANY I2 RUNS                             
         BZ    UNTUPL06                                                         
                                                                                
         LA    R2,KEY              ENSURE I2 HASN'T RUN SINCE DOWNLOAD          
         USING SNVKEYD,R2                                                       
         XC    SNVKEY,SNVKEY                                                    
         MVC   SNVKEY(SNVKMINK-SNVKEY),SAVE.SNVKEY                              
         MVC   SNVKMINK,EFFS                                                    
         GOTOR VDATAMGR,DMCB,(X'08',DMREAD),XSPDIR,SNVKEYD,SNVKEYD              
         TM    8(R1),X'02'         TEST RECORD DELETED                          
         BNZ   UNTUPL04            YES - SEND REFRESH MESSAGE                   
         CLI   8(R1),0             TEST FOR OTHER ERRORS                        
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R0,SNVDDA                                                        
         L     R2,AIO1             R2=A(INVOICE RECORD)                         
         GOTOR VDATAMGR,DMCB,GETREC,XSPFIL,(R0),SNVKEYD,DMWORK                  
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R3,SNVELS                                                        
         USING SNVMMELD,R3                                                      
         SR    R0,R0                                                            
UNTUPL02 CLI   SNVMMEL,0           LOCATE MATCHMAKER ELEMENT                    
         BE    UNTUPL04                                                         
         CLI   SNVMMEL,SNVMMELQ                                                 
         BE    *+14                                                             
         IC    R0,SNVMMLEN                                                      
         AR    R3,R0                                                            
         B     UNTUPL02                                                         
                                                                                
         CLC   SNVMMDAT,MMI2DATE   MATCH I2 RUN DATE AND TIME                   
         BNE   UNTUPL04                                                         
         CLC   SNVMMTIM,MMI2TIME                                                
         BE    UNTUPL06                                                         
         DROP  R2,R3                                                            
                                                                                
UNTUPL04 MVC   ERROR,I2RERUN       SEND ERROR I2 RERUN MESSAGE TO PC            
         GOTOR SENDMSG                                                          
                                                                                
UNTUPL06 CLC   SVRCVEL,MAP#DONE    TEST MATCHING COMPLETE                       
         BE    DONE                                                             
         CLC   SVRCVEL,MAP#ERNO    TEST E-MAIL RUN-NOT-ORDERED UNITS            
         BE    ERNO                                                             
                                                                                
         MVC   TSACOM,ACOMFACS     PROCESS UNIT ACTIONS (ADD, CHANGE            
         MVC   TSAREC,ANETBLK      AND DELETE (WITH MATCH/UNMATCH))             
                                                                                
         L     R7,TSAREC                                                        
         USING BUYUPLDD,R7         R7=A(UNIT INTERFACE RECORD)                  
         L     R8,AIO3                                                          
         USING DRAFRECD,R8         R8=A(RETURNED UNIT INFO)                     
*                                                                               
*  RESTORE TSAR BLOCK                                                           
         CLI   SVRSTSAV,C'S'        MUST BE PROCEEDED BY SAVE                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TSACTN,TSARES        RESTORE TSAR                                
         GOTOR VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   SVRSTSAV,C'R'                                                    
*                                                                               
         CLI   SVGLOBSW,C'B'       TEST FIRST TIME THROUGH                      
         BE    UNTUPL13                                                         
         MVI   TSACTN,TSAGET       YES - GET FIRST RECORD FROM TSAR             
         LHI   R0,1                                                             
         STCM  R0,3,TSRNUM                                                      
         GOTOR VTSAR,TSARD                                                      
         BNE   EXIT                EXIT IF NO RECORDS IN BUFFER                 
                                                                                
UNTUPL08 CLC   TSRECL,AFFIDLEN     TEST AFFID DATA PRESENT                      
         BL    UNTUPL10                                                         
         CLI   RUPMFLAG,RUPMFUAQ   TEST 'UNMATCH AFFID FROM UNIT'               
         BNE   UNTUPL10                                                         
         MVI   RUPNATML,1          SET NEW AFFID TIME PRESENT                   
         XC    RUPNATM,RUPNATM     SET NEW TIME TO ZEROES                       
         MVI   RUPOATML,0          SET NO PREVIOUS AFFID TIME                   
                                                                                
UNTUPL10 MVC   MMSRNUM,TSRNUM      SAVE TSAR RECORD NUMBER FOR RETURN           
         CLI   RUPMMIRO,RUPMMIYQ   TEST MIRROR MATCH                            
         BE    UNTUPL13            YES - DON'T GO TO BUY PROGRAM                
                                                                                
         MVI   SVXFROV,THISOLAY    RETURN CONTROL TO THIS OVERLAY               
         MVI   SVGLOBSW,C'B'       SET NOT FIRST TIME                           
                                                                                
         CLI   SVRSTSAV,C'R'        MUST BE PROCEEDED BY RESTORE                
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TSACTN,TSASAV       SAVE TSAR                                    
         GOTOR VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   SVRSTSAV,C'S'                                                    
                                                                                
         MVC   MMSRNUM,TSRNUM      SAVE TSAR RECORD NUMBER FOR RETURN           
         MVC   RUPDEMS,QDEMOS      SET OVERRIDE DEMO CODES                      
                                                                                
*****    ICM   R0,B'1100',=C'L='                                                
*****    ICM   R0,B'0011',TSRECL                                                
*****    GOTOR VDATAMGR,DMCB,DMWRT,TEMPSTR,('PAGE#',0),BUYUPLDD,0,(R0)          
*****    CLI   8(R1),0                                                          
*****    BE    *+6                                                              
*****    DC    H'0'                                                             
*                                                                               
* DO WSSVR CALL TO PASS INFO TO THE BUY SYSTEM                                  
         LA    RE,WSVRBLK                                                       
         USING FAWSSVRD,RE                                                      
         MVC   FAWSTOKN,=CL4'NNAV'                                              
         MVI   FAWSACTN,FAWSUSVE                                                
         MVC   FAWSADR,TSAREC                                                   
         LHI   R0,RUPLEN                                                        
         STCM  R0,3,FAWSLEN                                                     
******   MVC   FAWSLEN,=H'2000'                                                 
         GOTOR VWSSVRS,FAWSSVRD                                                 
         LA    RE,WSVRBLK                                                       
         USING FAWSSVRD,RE                                                      
         CLI   FAWSRTN,FAWSROK                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  RE                                                               
* PASS PAGE AND MONITOR NUMBER THROUGH GLOBBER                                  
         MVC   WORK,=CL4'NNAV'                                                  
****     XC    WORK(4),WORK        PASS TEMPSTR PAGE NUMBER                     
****     MVI   WORK,PAGE#                                                       
         GOTOR VGLOBBER,DMCB,PUTD,WORK,4,GLVBUY1                                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         XC    WORK,WORK                                                        
         LA    R6,WORK             ISSUE GLOBBER TRANSFER CONTROL CALL          
         USING GLVXFRSY,R6                                                      
         MVC   GLVXFRSY,NETSYS     FROM NETPAK/NAVIGATOR                        
         MVC   GLVXFRPR,NAVPGM                                                  
         MVC   GLVXTOSY,NETSYS     TO NETPAK BUY                                
         MVC   GLVXTOPR,BUYPGM                                                  
* SET DIALOGUE PARAMETERS                                                       
         OI    GLVXFLG1,GLV1SEPS+GLV1SEPD                                       
         OC    SVSESSNS,SVSESSNS    FIRST TIME                                  
         BZ    UNTUPL11                                                         
         MVC   GLVXSESR(2),SVSESSNS                                             
         OI    GLVXFLG1,GLV1SIDR+GLV1SIDE                                       
         B     UNTUPL12                                                         
* GET CURRENT SESSION NUMBER                                                    
UNTUPL11 OI    GLVXFLG1,GLV1SIDR                                                
         L     R3,ACOMFACS                                                      
         USING COMFACSD,R3                                                      
         ICM   RF,15,CSWITCH                                                    
         MVC   DMCB(4),=X'FFFFFFFF'                                             
         GOTO1 (RF),DMCB                                                        
         L     R1,DMCB                                                          
         JNZ   *+6                                                              
         DC    H'0'                                                             
         USING UTLD,R1                                                          
         MVC   SVSESSNS(1),TSESSION                                             
         MVC   GLVXSESR(2),SVSESSNS                                             
         DROP  R1,R3                                                            
*                                                                               
UNTUPL12 GOTOR VGLOBBER,DMCB,PUTD,WORK,24,GLVXCTL                               
         CLI   8(R1),0                                                          
         BE    EXIT                GO TO THE NETPAK BUY PROGRAM                 
         DC    H'0'                                                             
         DROP  R6                                                               
                                                                                
UNTUPL13 GOTOR GETHDR,X'31'        GET/SEND ELEMENT HEADER                      
         GOTOR ASETELEM,DMCB,AFABLK,HDRADDR,0,0                                 
         CLC   TSRECL,AFFIDLEN     TEST AFFID DATA PRESENT                      
         BL    *+12                                                             
         CLI   RUPMMIRO,RUPMMIYQ   YES - TEST MIRROR MATCH                      
         BE    UNTUPL22            YES - DIDN'T GO TO BUY PROGRAM               
                                                                                
         CLI   BUYERRSW,C'Y'       TEST BUY PROGRAM POSTED ERROR                
         BNE   UNTUPL14                                                         
         LA    R6,WORK2                                                         
         USING ERRDATA,R6          R6=A(ERROR RETURN VALUES)                    
         LA    R4,ERMSSEQN                                                      
         GOTOR PUTDTA,X'01'                                                     
         LA    R4,ERNUMBR                                                       
         GOTOR PUTDTA,X'39'                                                     
         SR    R0,R0                                                            
         ICM   R0,1,ERNUMBR                                                     
         SR    RF,RF                                                            
         ICM   RF,B'0100',=AL1(7)  OVERRIDE MESSAGE SYSTEM NUMBER TO 7          
         GOTOR VGETTXT,DMCB,(R0),(L'WORK,WORK),('GTMERR',0),0,         *        
               ('GT1OWRK+GT1NOREF',0),(RF)                                      
         LA    R4,WORK                                                          
         GOTOR PUTDTA,X'3A'                                                     
         LA    R4,ERMXTRA                                                       
         GOTOR PUTDTA,X'3B'                                                     
         B     UNTUPL20                                                         
                                                                                
UNTUPL14 LA    R4,DRFSEQN          SEQUENCE NUMBER                              
         GOTOR PUTDTA,X'01'                                                     
                                                                                
         CLC   =C'DEL',DRFNUM      CHECK FOR DELETE                             
         BE    UNTUPL20                                                         
         TM    DRFSTAT,X'80'       TEST FOR REFRESH ACTION                      
         BO    UNTUPL16                                                         
                                                                                
         LA    R4,DRFDATE                                                       
         GOTOR PUTDTA,X'02'        UNIT DATE                                    
         LA    R4,DRFTIME                                                       
         GOTOR PUTDTA,X'04'        START TIME                                   
         LA    R4,DRFTIME+2                                                     
         GOTOR PUTDTA,X'05'        END TIME                                     
         LA    R4,DRFLEN                                                        
         GOTOR PUTDTA,X'06'        UNIT LENGTH                                  
         LA    R4,DRFINT                                                        
         GOTOR PUTDTA,X'07'        INTEGRATION                                  
         LA    R4,DRFPNAM                                                       
         GOTOR PUTDTA,X'08'        PROGRAM NAME                                 
         XC    FULL,FULL                                                        
         MVC   FULL+(L'FULL-L'DRFNTI)(L'DRFNTI),DRFNTI                          
         LA    R4,FULL                                                          
         GOTOR PUTDTA,X'09'        NTI CODE                                     
         LA    R4,DRFBTYP                                                       
         GOTOR PUTDTA,X'0A'        BUY TYPE                                     
         LA    R4,DRFMEDTP                                                      
         GOTOR PUTDTA,X'13'        MEDIA/POSTING TYPES                          
         LA    R4,DRFROT                                                        
         GOTOR PUTDTA,X'14'        ROTATION                                     
         LA    R4,DRFSLN                                                        
         GOTOR PUTDTA,X'0E'        SUB-LINE NUMBER                              
                                                                                
UNTUPL16 LA    R4,DRFHOMSH                                                      
         GOTOR PUTDTA,X'0F'        HOMES SHARE                                  
         LA    R4,DRFHOMHT                                                      
         GOTOR PUTDTA,X'10'        HOMES HUT                                    
         LA    R4,DRFHOMRT                                                      
         GOTOR PUTDTA,X'11'        HOMES RATING                                 
         LA    R4,DRFHOMIM                                                      
         GOTOR PUTDTA,X'12'        HOMES IMPRESSIONS                            
                                                                                
         LA    R6,QDEMOS                                                        
         LA    R4,DRFDEMS                                                       
         LA    R0,25                                                            
UNTUPL18 CLI   1(R6),0                                                          
         BE    UNTUPL20                                                         
         GOTOR PUTDTA,X'0B'        DEMO VPH                                     
         LA    R4,2(R4)                                                         
         GOTOR PUTDTA,X'0C'        DEMO GRP                                     
         LA    R4,2(R4)                                                         
         GOTOR PUTDTA,X'0D'        DEMO IMP                                     
         LA    R4,4(R4)                                                         
         LA    R6,3(R6)                                                         
         BCT   R0,UNTUPL18                                                      
         DROP  R6                                                               
                                                                                
UNTUPL20 MVC   TSRECL,MMSRECL      RESTORE SAVED TSAR RECORD LENGTH             
******   MVI   TSACTN,TSARES       RESTORE TSAR                                 
******   GOTOR VTSAR,TSARD                                                      
******   BE    *+6                                                              
******   DC    H'0'                                                             
         MVC   TSRNUM,MMSRNUM      SET LAST RECORD NUMBER                       
         MVI   TSACTN,TSAGET       GET LAST INTERFACE RECORD                    
         GOTOR VTSAR,TSARD                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   BUYERRSW,C'Y'       TEST BUY PROGRAM POSTED ERROR                
         BE    UNTUPL30                                                         
                                                                                
UNTUPL22 CLC   TSRECL,AFFIDLEN     TEST AFFID DATA PRESENT                      
         BL    UNTUPL30                                                         
         CLI   RUPMFLAG,RUPMFMAQ   TEST 'MATCH AFFID TO UNIT'                   
         BE    *+12                                                             
         CLI   RUPMFLAG,RUPMFUAQ   TEST 'UNMATCH AFFID FROM UNIT'               
         BNE   UNTUPL30                                                         
                                                                                
         GOTOR TSTIRL              TEST IF INVOICE RECORDS ARE LOCKED           
                                                                                
         L     R2,AIO2                                                          
         USING SNVKEYD,R2          R2=A(INVOICE RECORD)                         
         GOTOR VDATAMGR,DMCB,(X'80',GETREC),XSPFIL,RUPMAKEY,SNVKEYD,   *        
               DMWORK                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR TSTSUM              TEST CHECK SUM FOR THIS RECORD               
                                                                                
         SR    R3,R3                                                            
         ICM   R3,3,RUPMAKEY+4                                                  
         LA    R3,SNVKEYD(R3)      R3=A(INVOICE DETAIL ELEMENT)                 
         USING SNVIDELD,R3                                                      
         CLI   SNVIDEL,SNVIDELQ    ENSURE POINTING TO A GOOD ELEMENT            
         BNE   UNTUPL32                                                         
         CLI   SNVIDLEN,SNVIDL2Q                                                
         BL    UNTUPL32                                                         
                                                                                
         CLI   RUPMFLAG,RUPMFUAQ   TEST 'UNMATCH AFFID FROM UNIT'               
         BNE   UNTUPL24                                                         
         XC    SNVIDUPG,SNVIDUPG   CLEAR UNIT MATCH VALUES                      
         XC    SNVIDUDT(SNVIDINT-SNVIDUDT),SNVIDUDT                             
         NI    SNVIDFLG,FF-(SNVIDMIR)                                           
         B     UNTUPL26                                                         
                                                                                
UNTUPL24 MVC   SNVIDUDT,DRFDATE    SEED INVOICE DETAIL WITH UNIT DATE           
         MVC   SNVIDUTM,DRFSQTR    ...START QUARTER HOUR                        
         PACK  DUB,RUPEST                                                       
         CVB   R0,DUB                                                           
         STC   R0,SNVIDUES         ...ESTIMATE NUMBER                           
         MVC   SNVIDUSB,DRFSLN     ...SUB-LINE NUMBER                           
         MVC   SNVIDUDP,DRFDPT     ...DAYPART CODE                              
         MVC   SNVIDUPG,RUPPCODE   ...PROGRAM CODE                              
                                                                                
         CLI   RUPMMIRO,RUPMMIYQ   TEST MIRROR MATCH                            
         BNE   UNTUPL26                                                         
         OI    SNVIDFLG,SNVIDMIR   YES - SET INDICATOR                          
                                                                                
UNTUPL26 NI    SNVIDCTL,FF-(SNVIDICQ+SNVIDITQ+SNVIDIFQ+SNVIDSIQ)                
         NI    SNVIDCT2,FF-(SNVIDISQ+SNVIDIIQ)                                  
                                                                                
         TM    RUPMIGNO,RUPMIGTQ   TEST IGNORE TIME                             
         BZ    *+8                                                              
         OI    SNVIDCTL,SNVIDITQ                                                
         TM    RUPMIGNO,RUPMIGSQ   TEST IGNORE SEPERATION                       
         BZ    *+8                                                              
         OI    SNVIDCTL,SNVIDSIQ                                                
         TM    RUPMIGNO,RUPMIGCQ   TEST IGNORE COST                             
         BZ    *+8                                                              
         OI    SNVIDCTL,SNVIDICQ                                                
         TM    RUPMIGNO,RUPMIGFQ   TEST IGNORE FILM                             
         BZ    *+8                                                              
         OI    SNVIDCTL,SNVIDIFQ                                                
         TM    RUPMIGNO,RUPMIGIQ   TEST IGNORE INTEGRATION                      
         BZ    *+8                                                              
         OI    SNVIDCT2,SNVIDIIQ                                                
         TM    RUPMIGNO,RUPMIGLQ   TEST IGNORE LENGTH                           
         BZ    *+8                                                              
         OI    SNVIDCT2,SNVIDISQ                                                
                                                                                
UNTUPL28 GOTOR VDATAMGR,DMCB,PUTREC,XSPFIL,RUPMAKEY,SNVKEYD,DMWORK              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR SETSUM              SET NEW CHECK SUM FOR THIS RECORD            
                                                                                
UNTUPL30 MVI   BUYERRSW,C'N'       SET NO ERROR                                 
         MVI   TSACTN,TSANXT       READ NEXT TSAR RECORD                        
         GOTOR VTSAR,TSARD                                                      
         BE    UNTUPL08            SEND NEXT REQUEST TO BUY PROGRAM             
                                                                                
         ICM   R1,3,=AL1(STATOWIP,STATNCOM)                                     
         GOTOR UPDINV,(R1)         UPDATE INVOICE HEADERS                       
         B     EXIT                                                             
                                                                                
UNTUPL32 MVC   ERROR,INVCHGD       SEND BACK ERROR MESSAGE IF INVOICE           
         GOTOR SENDMSG             RECORD HAS BEEN CHANGED                      
         DROP  R1,R2,R3,R8                                                      
         EJECT                                                                  
***********************************************************************         
* HANDLE E-MAIL RUN-NOT-ORDERED DETAILS ACTION                        *         
***********************************************************************         
                                                                                
ERNO     L     R4,AIO3             POINT TO INPUT BLOCK                         
         C     R4,DPOINTER         ENSURE THERE IS SOME INPUT DATA              
         BE    EXIT                                                             
                                                                                
         GOTOR TSTIRL              TEST IF INVOICE RECORDS ARE LOCKED           
                                                                                
         L     R2,AIO1                                                          
         USING SNVKEYD,R2          R2=A(INVOICE RECORD)                         
                                                                                
ERNO02   GOTOR VDATAMGR,DMCB,(X'80',GETREC),XSPFIL,(R4),SNVKEYD,DMWORK          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTOR TSTSUM              TEST CHECK SUM FOR THIS RECORD               
                                                                                
         SR    R3,R3                                                            
         ICM   R3,3,4(R4)          R3=DISPLACEMENT TO INVOICE ELEMENT           
         LA    R3,SNVKEYD(R3)      R3=A(ELEMENT)                                
         USING SNVIDELD,R3                                                      
         CLI   SNVIDEL,SNVIDELQ                                                 
         BNE   ERNO04                                                           
         OI    SNVIDCT2,SNVIDEMS   SET E-MAIL SENT FLAG                         
                                                                                
         GOTOR VDATAMGR,DMCB,PUTREC,XSPFIL,(R4),SNVKEY,DMWORK                   
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR SETSUM              SET NEW CHECK SUM FOR THIS RECORD            
                                                                                
         AHI   R4,L'RUPMAKEY       BUMP TO NEXT INVOICE DETAIL                  
         C     R4,DPOINTER                                                      
         BL    ERNO02                                                           
                                                                                
         ICM   R1,3,=AL1(STATOWIP,STATNCOM)                                     
         GOTOR UPDINV,(R1)         UPDATE INVOICE HEADERS                       
         B     EXIT                                                             
                                                                                
ERNO04   MVC   ERROR,INVCHGD       SEND BACK ERROR MESSAGE IF INVOICE           
         GOTOR SENDMSG             RECORD HAS BEEN CHANGED                      
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* HANDLE MATCHING COMPLETE                                            *         
***********************************************************************         
                                                                                
DONE     OC    SAVE.SNVKCLT,SAVE.SNVKCLT                                        
         BZ    EXIT                                                             
         GOTOR TSTIRL              TEST IF INVOICE RECORDS ARE LOCKED           
         ICM   R1,3,=AL1(STATOCOM,STATNWIP)                                     
         GOTOR UPDINV,(R1)         UPDATE INVOICE HEADERS                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TEST IF FILE IS UPDATABLE                                *         
***********************************************************************         
                                                                                
TSTUPD   L     RF,ACOMFACS                                                      
         L     RF,CXTRAINF-COMFACSD(RF)                                         
         USING XTRAINFD,RF                                                      
         TM    XIFLAG2,XICTUEN     TEST CONNECTED WITH U=N OR                   
         JNZ   TSTUPD02            READ-ONLY                                    
         TM    XIFLAG1,XIROSYS+XIROMODE+XIWRONGF                                
         BZR   RE                                                               
         DROP  RF                                                               
                                                                                
TSTUPD02 MVC   ERROR,NOUPDTS       SEND NO UPDATES ERROR                        
         GOTOR SENDMSG                                                          
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO TEST IF INVOICE RECORDS ARE LOCKED                       *         
***********************************************************************         
                                                                                
TSTIRL   NTR1  ,                                                                
         MVC   HALF,SAVE.SNVKCLT                                                
         OC    HALF,HALF                                                        
         BNZ   *+10                                                             
         MVC   HALF,BCLT                                                        
         GOTOR GETCLT,HALF         RESOLVE CLIENT CODE                          
                                                                                
         MVC   FULL,SAVE.SNVKSTA                                                
         OC    FULL,FULL                                                        
         BNZ   TSTIRL02                                                         
         MVC   MMLSTAC,BNET                                                     
         B     TSTIRL04                                                         
                                                                                
TSTIRL02 GOTOR GETSTA,FULL         RESOLVE STATION CODE                         
                                                                                
TSTIRL04 XC    WORK,WORK                                                        
         USING LKKEYD,WORK         USE WORK TO BUILD LOCKET KEY                 
         MVC   LOCKAGY,TWAAGY                                                   
         MVC   LOCKRTY,LOCKNVR                                                  
         MVI   LKNVMED,NETMEDQ                                                  
         MVC   LKNVCLT,MMLCLTC                                                  
         MVC   LKNVSTA,MMLSTAC                                                  
         L     RF,ACOMFACS                                                      
         L     RF,CLOCKET-COMFACSD(RF)                                          
         GOTOR (RF),DMCB,('LKTESTQ',LKKEYD),ACOMFACS                            
         CLI   4(R1),2                                                          
         BE    TSTIRLX                                                          
         CLI   4(R1),0                                                          
         BE    TSTIRLX                                                          
         MVC   ERROR,INVLOCK       SEND INVOICE LOCKED ERROR                    
         GOTOR SENDMSG                                                          
TSTIRLX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO UNPACK CLIENT CODE - ENTRY R1=A(PACKED CLIENT CODE)      *         
***********************************************************************         
                                                                                
GETCLT   CLC   MMLCLT,0(R1)                                                     
         BER   RE                                                               
GETCLTN  NTR1  ,                                                                
         MVC   MMLCLT,0(R1)                                                     
         LA    R2,KEY                                                           
         USING CLTHDRD,R2                                                       
         XC    CKEY,CKEY                                                        
         MVI   CKEYTYPE,CKEYTYPQ                                                
*        MVC   CKEYAM,SAVE.SNVKAM                                               
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,MMLCLT                                                   
         GOTOR VDATAMGR,DMCB,DMREAD,SPTDIR,CKEY,CKEY                            
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R0,CKDA                                                          
         L     R2,AIO2                                                          
         GOTOR VDATAMGR,DMCB,GETREC,SPTFIL,(R0),CKEY,WORK                       
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR VCLUNPK,DMCB,(CPROF+6,CKEYCLT),MMLCLTC                           
GETCLTX  B     EXIT                                                             
         DROP  R2                                                               
                                                                                
***********************************************************************         
* ROUTINE TO UNPACK STATION CODE - ENTRY R1=A(3 BYTE PACKED STATION)  *         
***********************************************************************         
                                                                                
GETSTA   CLC   MMLSTA,0(R1)                                                     
         BER   RE                                                               
GETSTAN  NTR1  ,                                                                
         MVC   MMLSTA,0(R1)                                                     
         XC    WORK,WORK                                                        
         USING STAPACKD,WORK                                                    
         MVI   STAPACT,C'U'                                                     
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPAGY,TWAAGY                                                   
         MVI   STAPMED,NETMEDQ                                                  
         MVC   STAPSTA,MMLSTA                                                   
         GOTOR VSTAPACK,STAPACKD                                                
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   MMLSTAC,STAPQSTA    SET STATION CODE                             
GETSTAX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO UPDATE STATUS IN INVOICE RECORDS AND THEIR PASSIVES      *         
***********************************************************************         
                                                                                
UPDINV   NTR1  ,                                                                
         GOTOR TSTIRL              TEST LOCK                                    
         STCM  R1,3,HALF           SAVE STATUS FLAGS                            
                                                                                
INV      USING SNVKEYD,KEY                                                      
         XC    INV.SNVKEY,INV.SNVKEY                                            
         MVC   INV.SNVKEY(SNVKMINK-SNVKEY),SAVE.SNVKEY                          
         MVC   INV.SNVKMINK,EFFS                                                
         MVC   KEYSAVE,INV.SNVKEY                                               
         L     R2,AIO2                                                          
         USING SNVKEYD,R2          R2 POINTS TO RECORD                          
                                                                                
         GOTOR VDATAMGR,DMCB,(X'80',DMRDHI),XSPDIR,INV.SNVKEY,         *        
               INV.SNVKEY                                                       
         B     UPDINV04                                                         
                                                                                
UPDINV02 GOTOR VDATAMGR,DMCB,(X'80',DMRSEQ),XSPDIR,INV.SNVKEY,         *        
               INV.SNVKEY                                                       
                                                                                
UPDINV04 BNE   UPDINV12                                                         
         CLC   INV.SNVKEY(SNVKINV-SNVKEY),KEYSAVE                               
         BNE   UPDINV12                                                         
         CLC   INV.SNVKMINK,EFFS                                                
         BNE   UPDINV02                                                         
         GOTOR VDATAMGR,DMCB,(X'80',GETREC),XSPFIL,INV.SNVDDA,SNVKEY,  *        
               DMWORK                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         LA    R3,SNVELS                                                        
         USING SNVMMELD,R3                                                      
         SR    R0,R0                                                            
UPDINV06 CLI   SNVMMEL,0                                                        
         BE    UPDINV02                                                         
         CLI   SNVMMEL,SNVMMELQ                                                 
         BE    *+14                                                             
         IC    R0,SNVMMLEN                                                      
         AR    R3,R0                                                            
         B     UPDINV06                                                         
                                                                                
         CLC   SNVMMRP1,MMQPRD     TEST CORRECT PRODUCT                         
         BNE   UPDINV02                                                         
         CLI   SNVMMRE1,0                                                       
         BE    UPDINV08                                                         
         CLC   MMBEST,SNVMMRE1                                                  
         BE    UPDINV08                                                         
         BL    UPDINV02                                                         
         CLC   MMBEST,SNVMMRE2                                                  
         BH    UPDINV02                                                         
                                                                                
UPDINV08 MVC   WORK(1),SNVMMMST                                                 
         NC    WORK(1),HALF+1                                                   
         OC    WORK(1),HALF+0                                                   
         CLC   INV.SNVDSTAT+1(1),WORK                                           
         BE    UPDINV10                                                         
         MVC   INV.SNVDSTAT+1(1),WORK                                           
         GOTOR VDATAMGR,DMCB,DMWRT,XSPDIR,INV.SNVKEY,INV.SNVKEY                 
         BE    UPDINV10                                                         
         DC    H'0'                                                             
                                                                                
UPDINV10 CLC   SNVRSTAT+1(1),WORK                                               
         BNE   *+14                                                             
         CLC   SNVMMMST,WORK                                                    
         BE    UPDINV02                                                         
         MVC   SNVRSTAT+1(1),WORK                                               
         MVC   SNVMMMST,WORK                                                    
         GOTOR VDATAMGR,DMCB,PUTREC,XSPFIL,INV.SNVDDA,SNVKEY,DMWORK             
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTOR SETSUM              SET NEW CHECK SUM FOR THIS RECORD            
         B     UPDINV02                                                         
         DROP  INV,R2                                                           
                                                                                
***********************************************************************         
* UPDATE STATUS IN INVOICE PASSIVE POINTER RECORDS                    *         
***********************************************************************         
                                                                                
PAS      USING SNVNKEY,KEY                                                      
UPDINV12 XC    PAS.SNVNKEY,PAS.SNVNKEY                                          
         MVI   PAS.SNVNTYP,SNVNTYPQ                                             
         MVI   PAS.SNVNSUB,SNVNSUBQ                                             
         MVC   PAS.SNVNAM,SAVE.SNVKAM                                           
         MVC   PAS.SNVNCLT,SAVE.SNVKCLT                                         
         MVC   PAS.SNVNNETW,MMQNET                                              
         MVC   PAS.SNVNMOS,SAVE.SNVKMOS                                         
         MVC   PAS.SNVNPRD,MMQPRD                                               
         MVC   PAS.SNVNEST,MMBEST                                               
         MVC   KEYSAVE,PAS.SNVKEY                                               
                                                                                
         GOTOR VDATAMGR,DMCB,(X'80',DMRDHI),XSPDIR,PAS.SNVKEY,         *        
               PAS.SNVKEY                                                       
         B     UPDINV16                                                         
                                                                                
UPDINV14 GOTOR VDATAMGR,DMCB,(X'80',DMRSEQ),XSPDIR,PAS.SNVKEY,         *        
               PAS.SNVKEY                                                       
                                                                                
UPDINV16 BNE   UPDINVX                                                          
         CLC   PAS.SNVNKEY(SNVNEST2-SNVNKEY),KEYSAVE                            
         BNE   UPDINVX                                                          
         CLI   MMBEST2,0           APPLY ESTIMATE 2 FILTER                      
         BE    *+14                                                             
         CLC   PAS.SNVNEST2,MMBEST2                                             
         BNE   UPDINV14                                                         
         OC    MMQPRD2,MMQPRD2     APPLY PRODUCT 2 FILTER                       
         BZ    *+14                                                             
         CLC   PAS.SNVNPRD2,MMQPRD2                                             
         BNE   UPDINV14                                                         
                                                                                
         MVC   WORK(1),PAS.SNVNSTAT+1                                           
         NC    WORK(1),HALF+1                                                   
         OC    WORK(1),HALF+0                                                   
         CLC   PAS.SNVNSTAT+1(1),WORK                                           
         BE    UPDINV14                                                         
         MVC   PAS.SNVNSTAT+1(1),WORK                                           
         GOTOR VDATAMGR,DMCB,DMWRT,XSPDIR,PAS.SNVNKEY,PAS.SNVNKEY               
         BE    UPDINV14                                                         
         DC    H'0'                                                             
         DROP  PAS                                                              
                                                                                
UPDINVX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* TEST CHECK SUM FOR AN INVOICE RECORD                                *         
***********************************************************************         
                                                                                
TSTSUM   STM   RE,R2,12(RD)                                                     
         LR    R2,R1               R2=A(DMCB)                                   
         L     RF,8(R2)            RF=A(AFFID KEY)                              
         ICM   R1,15,6(RF)         R1=PASSED CHECK SUM                          
                                                                                
         SR    R0,R0                                                            
         ICM   R0,3,MMI#                                                        
         JZ    TSTSUM04                                                         
         LHI   RE,TSARBLK+L'TSARBLK-TWAD                                        
         LA    RE,TWAD(RE)         LOOK-UP CHECK SUM FOR UPDATED RECORD         
TSTSUM02 CLC   0(L'SNVDDA,RE),0(RF)                                             
         JNE   *+12                                                             
         ICM   R1,15,L'SNVDDA(RE)                                               
         J     TSTSUM04                                                         
         AHI   RE,L'SNVDDA+4                                                    
         JCT   R0,TSTSUM02                                                      
                                                                                
TSTSUM04 L     RE,12(R2)           RE=A(RECORD)                                 
         SR    RF,RF                                                            
         ICM   RF,3,SNVRLEN-SNVKEYD(RE)                                         
         SR    R0,R0                                                            
         CKSM  R0,RE                                                            
         JO    *-4                                                              
         CR    R0,R1               TEST CHECK SUM DIFFERENT                     
         JE    TSTSUM06                                                         
         MVC   ERROR,INVCHGD       YES - SEND BACK ERROR MESSAGE                
         GOTOR SENDMSG                                                          
                                                                                
TSTSUM06 LM    RE,R2,12(RD)                                                     
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* CALCULATE NEW CHECK SUM FOR A RECORD AND ADD/UPDATE ENTRY IN        *         
* SAVED WORKING STORAGE TABLE                                         *         
***********************************************************************         
                                                                                
SETSUM   STM   RE,R2,12(RD)                                                     
         LR    R2,R1               R2=A(DMCB)                                   
         L     RE,12(R2)           RE=A(RECORD)                                 
         SR    RF,RF                                                            
         ICM   RF,3,SNVRLEN-SNVKEYD(RE)                                         
         SR    R1,R1                                                            
         CKSM  R1,RE               R1=INVOICE RECORD CHECK SUM                  
         JO    *-4                                                              
                                                                                
         LHI   RE,TSARBLK+L'TSARBLK-TWAD                                        
         LA    RE,TWAD(RE)         LOOK-UP CHECK SUM FOR UPDATED RECORD         
         L     RF,8(R2)                                                         
         SR    R0,R0                                                            
         ICM   R0,3,MMI#                                                        
         JZ    SETSUM06                                                         
SETSUM02 CLC   0(L'SNVDDA,RE),0(RF)                                             
         JE    SETSUM12                                                         
         AHI   RE,L'SNVDDA+4                                                    
         JCT   R0,SETSUM02                                                      
                                                                                
SETSUM04 ICM   R0,3,MMI#           ENTRY NOT FOUND - ADD A NEW ONE              
         CHI   R0,MMI#MAX                                                       
         JNL   SETSUM08                                                         
                                                                                
SETSUM06 AHI   R0,1                                                             
         STCM  R0,3,MMI#                                                        
         J     SETSUM10                                                         
                                                                                
SETSUM08 MVC   ERROR,TABFULL       SEND INVOICE TABLE FULL ERROR                
         GOTOR SENDMSG                                                          
                                                                                
SETSUM10 MVC   0(L'SNVDDA,RE),0(RF)                                             
                                                                                
SETSUM12 STCM  R1,15,L'SNVDDA(RE)                                               
         LM    RE,R2,12(RD)                                                     
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* GET HEADER AND CALL ADDDATA                                         *         
* ON ENTRY R1 CONTAINS DATA ITEM NUMBER WITHIN CURRENT ELEMENT        *         
* R4 POINTS TO DATA VALUE                                             *         
***********************************************************************         
                                                                                
PUTDTA   LR    R0,RE                                                            
         GOTOR GETDATA,(R1)        GET DEFINITION/ADD DATA                      
         GOTOR AADDDATA,DMCB,AFABLK,DATADDR,(R4),0                              
         LR    RE,R0                                                            
         BR    RE                                                               
                                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
FF       EQU   X'FF'                                                            
PAGE#    EQU   1                   TEMPSTR SAVE PAGE NUMBER                     
THISOLAY EQU   X'27'               THIS OVERLAY NUMBER                          
MINSHOUR EQU   60                  MINUTES IN AN HOUR                           
HOURSDAY EQU   24                  HOURS IN A DAY                               
NETMEDQ  EQU   C'N'                NETPAK MEDIA LETTER                          
STATOCOM EQU   SNVMMSCQ            OR MATCHING COMPLETE                         
STATOWIP EQU   SNVMMIPQ            OR WORK IN PROGRESS                          
STATNCOM EQU   FF-SNVMMSCQ         AND MATCHING COMPLETE                        
STATNWIP EQU   FF-SNVMMIPQ         AND WORK IN PROGRESS                         
                                                                                
LOCKNVR  DC    C'NV'               INVOICE RECORD LOCK TYPE                     
MAP#ERNO DC    X'0137'             MAP FOR E-MAIL RNO UNITS                     
MAP#DONE DC    X'0138'             MAP FOR MATCHING COMPLETE                    
INVLOCK  DC    AL2(769)            ERROR - INVOICE RECORDS ARE LOCKED           
I2RERUN  DC    AL2(826)            ERROR - I2 HAS BEEN RERUN                    
INVCHGD  DC    AL2(846)            ERROR - INVOICES CHANGED - REFRESH           
NOUPDTS  DC    AL2(847)            ERROR - CAN'T UPDATE - READ-ONLY             
TABFULL  DC    AL2(848)            ERROR - INVOICE TABLE FULL                   
AFFIDLEN DC    AL2(RUPRMLEN)       L'TSAR RECORD WITH AFFID DATA                
                                                                                
EFFS     DC    X'FFFFFFFFFFFF'                                                  
DMRDHI   DC    C'DMRDHI '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMWRT    DC    C'DMWRT  '                                                       
TEMPSTR  DC    C'TEMPSTR'                                                       
PUTD     DC    C'PUTD'                                                          
NETSYS   DC    C'NET'                                                           
NAVPGM   DC    C'NNA'                                                           
BUYPGM   DC    C'NBU'                                                           
DMREAD   DC    C'DMREAD  '                                                      
GETREC   DC    C'GETREC  '                                                      
PUTREC   DC    C'PUTREC  '                                                      
UNTDIR   DC    C'UNTDIR  '                                                      
UNTFIL   DC    C'UNTFIL  '                                                      
SPTDIR   DC    C'SPTDIR  '                                                      
SPTFIL   DC    C'SPTFIL  '                                                      
XSPDIR   DC    C'XSPDIR  '                                                      
XSPFIL   DC    C'XSPFIL  '                                                      
                                                                                
         LTORG                                                                  
MMI#MAX  EQU   900                                                              
         EJECT                                                                  
* INCLUDED DSECTS                                                               
         PRINT OFF                                                              
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FALOCKUPD                                                      
       ++INCLUDE FAXTRAINF                                                      
       ++INCLUDE NAVDSECTS                                                      
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENSNV                                                       
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE FAWSSVRD                                                       
       ++INCLUDE NENAVWRK                                                       
       ++INCLUDE NENAVMMD                                                       
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
                                                                                
MMSAVED  DSECT                     ** EXTRA SAVED FIELDS **                     
MMSRECL  DS    XL(L'TSRECL)        SAVED TSAR RECORD LENGTH                     
MMSRNUM  DS    XL(L'TSRNUM)        SAVED TSAR RECORD NUMBER                     
MMLCLT   DS    XL(L'SNVKCLT)       LAST CLIENT CODE                             
MMLCLTC  DS    CL3                 ALPHA CLIENT CODE                            
MMLSTA   DS    XL(L'SNVKSTA)       SAVED STATION CODE                           
MMLSTAC  DS    CL8                 ALPHA STATION/MARKET CODE                    
                                                                                
LKKEYD   DSECT                     ** LOCK KEY DEFINITION **                    
         ORG   LOCKKEY                                                          
LKNVMED  DS    CL1                 MEDIA CODE                                   
LKNVCLT  DS    CL3                 CLIENT CODE                                  
LKNVSTA  DS    CL5                 STATION CODE                                 
                                                                                
WORKD    DSECT                                                                  
         ORG   OVWORK               LOCAL WORKING STORAGE                       
WSVRBLK  DS    XL(FAWSSVRL)                                                     
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034NENAV27   04/04/18'                                      
         END                                                                    
