*          DATA SET SEACS21    AT LEVEL 001 AS OF 09/16/13                      
*PHASE TA0D21B                                                                  
*INCLUDE LISTIO                                                                 
         TITLE 'SEACS21 - List Record Maintenance - DATA2'                      
ACS21    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**CS21**,RA,RR=RE                                              
         USING WORKD,R7                  R7=A(GLOBAL W/S)                       
         USING TWAD,R5                   R5=A(TWA)                              
         L     RC,ASYSWORK                                                      
         USING SYSWORK,RC                RC=A(LOCAL W/S)                        
         AHI   R6,SAVAREAX-SAVAREA       R6=A(GLOBAL SAVE AREA)                 
         USING LSAVAREA,R6               R6=A(GLOBAL SAVE AREA)                 
         USING LISTD,LISTBLK                                                    
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         L     RF,=V(LISTIO)                                                    
         AR    RF,RE                                                            
         ST    RF,VLISTIO                                                       
*                                                                               
         OI    ACSSRVH+FHOID,FHOITR+FHOIMO  AVOID NO DATA ENTERED               
*                                                                               
MAIN010  XR    RF,RF                                                            
         IC    RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     VALKEY                    01 - APMVALK                           
         B     VALREC                    02 - APMVALR                           
         B     DISKEY                    03 - APMDISK                           
         B     DISREC                    04 - APMDISR                           
         B     DELREC                    05 - APMDELR                           
         B     RESREC                    06 - APMRESR                           
         B     XIT                       07 - APMVALP                           
         B     XIT                       08 - APMGETS                           
         B     XIT                       09 - APMDISS                           
         B     XIT                       10 - APMVALS                           
         B     XIT                       11 - APMFLST                           
         B     XIT                       12 - APMPROC                           
         B     XIT                       13 - APMFSCR                           
         B     XIT                       14 - APMLSCR                           
         B     XIT                       15 - APMVALQ                           
         B     XIT                       16 - APMREPP                           
         B     XIT                       17 - APMSETT                           
         B     XIT                       18 - APMPUTK                           
         B     VALREC                    19 - APMNEWK                           
         B     XIT                       20 - APMFRP                            
         B     XIT                       21 - APMDISS2                          
*                                                                               
YES      CR    RB,RB                                                            
         J     XIT                                                              
NO       LTR   RB,RB                                                            
XIT      XIT1                                                                   
*                                                                               
EXIT     CLI   APPFKEY,0                 ANY PFKEY PRESSED?                     
         BE    EXITX                                                            
         CLI   APMODE,APMVALK            END OF VALKEY?                         
         BE    EXITX                                                            
         TM    TWASWPST,TWASWAP          SWAP TO NEW RECORD/ACTION?             
         BZ    EXITX                     . NO                                   
         XC    APCURSOR,APCURSOR         DON'T SET CURSOR                       
         MVI   APMODE,APMSWP             SWAP                                   
         MVC   APPARM(1),TWASWPRE        SWAP RECORD                            
         MVC   APPARM+1(1),TWASWPAC      SWAP ACTION                            
*                                                                               
         MVC   SVKEYFLD(L'SCRCOD),SCRCOD                                        
         MVC   SVKEYFLD+L'SCRCOD(L'SCRSYS),SCRSYS                               
         MVC   SVKEYFLD+L'SCRCOD+L'SCRSYS(L'SCRTYP),SCRTYP                      
*                                                                               
EXITX    CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     XIT                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* VALIDATE KEY OF THE LIST RECORD                                               
***********************************************************************         
VALKEY   XC    CURVALS(CURVALNQ),CURVALS                                        
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         NI    LOSIN,X'FF'-LOSINEW       TURN OFF NEW LIST                      
                                                                                
*----------------------------------------                                       
* DATA GROUP                                                                    
*----------------------------------------                                       
         GOTO1 AFVAL,SCRCODH                                                    
         BNE   SAEENK                                                           
         MVC   CURLCOD,FVIFLD            DATA GROUP CODE                        
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING SALAREC,R2                                                       
         MVI   SALATYP,SALATYPQ          C'F'                                   
         MVI   SALASUB,SALASUBQ          X'18'                                  
         MVC   SALAAGY,CUAALF                                                   
         MVC   SALAAGR,FVIFLD            DATA GROUP CODE                        
         GOTO1 AIO,IORD+IOCONFIL+IO3                                            
         BNE   SAERNF                                                           
*                                                                               
         L     R2,AIOAREA3               GET GROUP NAME & # FROM RECORD         
         LA    R3,SALADATA                                                      
VK040    CLI   0(R3),0                                                          
         BE    VK100                                                            
         CLI   0(R3),SALANELQ            DATA GROUP NAME ELEMENT                
         BE    VK080                                                            
         CLI   0(R3),SALASELQ            DATA GROUP SYSTEM ELEMENT              
         BE    VK090                                                            
VK050    LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     VK040                                                            
*                                                                               
         USING SALAND,R3                                                        
VK080    MVC   CURLGRP,SALANNUM                                                 
         MVC   CURLGNM,SPACES                                                   
         LLC   R1,1(R3)                                                         
         AHI   R1,-(SALANLNQ+1)                                                 
         BM    VK050                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CURLGNM(0),SALANNAM                                              
         MVC   SCRNAME(L'CURLGNM),CURLGNM                                       
         OI    SCRNAMEH+6,FVOXMT                                                
         B     VK050                                                            
         DROP  R3                                                               
*                                                                               
         USING SALASD,R3                                                        
VK090    CLI   CURLSYS,0                 DO WE ALREADY HAVE A DEFAULT?          
         BNE   VK050                     . YES                                  
         MVC   CURLSYS,SALASNUM          SAVE FIRST SYSTEM AS DEFAULT           
         B     VK050                                                            
         DROP  R3                                                               
*                                                                               
VK100    OC    CURLGRP,CURLGRP           GROUP NUMBER FOUND?                    
         BNZ   *+6                                                              
         DC    H'0'                      ABSOLUTELY NEED A GROUP #              
*----------------------------------------                                       
* SYSTEM                                                                        
*----------------------------------------                                       
         GOTO1 AFVAL,SCRSYSH                                                    
         BE    VK120                                                            
         CLI   CURLSYS,0                 DO WE ALREADY HAVE A DEFAULT?          
         BE    SAEENK                    . NO                                   
         GOTO1 ADISSYS,CURLSYS                                                  
         MVC   SCRSYS(7),APWORK                                                 
         OI    SCRSYSH+6,FVOXMT                                                 
*                                                                               
VK120    GOTO1 AVALSYS,SCRSYSH                                                  
         BNE   SAEENK                                                           
         L     R1,APPARM                                                        
         CLI   0(R1),X'02'               SPOT                                   
         BE    VK130                                                            
         CLI   0(R1),X'03'               NET                                    
         BE    VK130                                                            
         CLI   0(R1),X'04'               PRINT                                  
         BE    VK130                                                            
         CLI   0(R1),X'06'               ACC                                    
         BNE   SAEIIF                                                           
VK130    MVC   CURLSYS,0(R1)             SET SYSTEM NUMBER                      
         BRAS  RE,DISSSH                 DISPLAY SYSTEM SUMMARY LINE            
                                                                                
*----------------------------------------                                       
* TYPE                                                                          
*----------------------------------------                                       
         XC    LISTBLK,LISTBLK                                                  
         MVC   LISTACOM,ACOM                                                    
         MVC   LISTISYS,CURLSYS          SYSTEM                                 
         MVC   CURLTYP,=AL2(LISTTCLT)    CLIENT IS DEFAULT                      
*                                                                               
         GOTO1 AFVAL,SCRTYPH                                                    
         BE    VK230                                                            
*                                                                               
         MVC   LISTIAGY,CUAALF                                                  
         MVC   LISTIGRP,CURLGRP                                                 
         MVC   LISTISYS,CURLSYS                                                 
         MVI   LISTACTN,LISTANLG         NEXT LIST FOR THIS GROUP               
         GOTO1 VLISTIO,LISTD                                                    
         BNE   VK225                                                            
         CLC   LISTISYS,CURLSYS          FOR THE GIVEN SYSTEM                   
         BNE   VK225                                                            
         MVC   CURLTYP,LISTITYP                                                 
*                                                                               
VK225    MVC   LISTITYP,CURLTYP                                                 
         MVI   LISTACTN,LISTATYP         VALIDATE TYPE                          
         GOTO1 VLISTIO,LISTD                                                    
         BNE   SAEMIF                                                           
         B     VK240                                                            
*                                                                               
VK230    MVC   LISTITNM(L'SCRTYP),SCRTYP                                        
         MVI   LISTACTN,LISTATYP         VALIDATE TYPE                          
         GOTO1 VLISTIO,LISTD                                                    
         BNE   SAEIIF                                                           
         MVC   CURLTYP,LISTITYP                                                 
*                                                                               
VK240    MVC   SCRTYP,LISTITNM                                                  
         OI    SCRTYPH+6,FVOXMT                                                 
                                                                                
*----------------------------------------                                       
* READ RECORD                                                                   
*----------------------------------------                                       
         XC    LISTBLK,LISTBLK                                                  
         MVC   LISTACOM,ACOM                                                    
         MVC   LISTIAGY,CUAALF                                                  
         MVC   LISTIGRP,CURLGRP                                                 
         MVC   LISTISYS,CURLSYS                                                 
         MVC   LISTITYP,CURLTYP                                                 
*                                                                               
         XC    APRECKEY,APRECKEY                                                
         MVC   APRECKEY(LISTIRMX-LISTINFO),LISTINFO                             
*                                                                               
         MVI   LISTACTN,LISTAINF   GET INFO ON LIST                             
         GOTO1 VLISTIO,LISTD                                                    
         BNE   VK250                                                            
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         B     VK260                                                            
VK250    TM    LISTDMER,IOEDEL                                                  
         BZ    VK270                                                            
         MVI   APINDS,APIOKDIS+APIOKRES                                         
VK260    MVC   CURLNUM,LISTINUM                                                 
         MVC   CURDERV,LISTIDER                                                 
         B     VK280                                                            
VK270    MVI   APINDS,APIOKADD+APIOKDIS+APIOKCHA                                
         OI    LOSIN,LOSINEW                                                    
*                                                                               
VK280    CLC   SVKGRP,CURLGRP            SAME KEY DATA GROUP?                   
         BNE   VK300                                                            
         CLC   SVKSYS,CURLSYS            SAME KEY SYSTEM?                       
         BNE   VK300                                                            
         CLC   SVKTYP,CURLTYP            SAVE KEY TYPE?                         
         BNE   VK300                                                            
         B     VKOKX                                                            
*                                                                               
VK300    MVC   SVKGRP,CURLGRP            DATA GROUP                             
         MVC   SVKSYS,CURLSYS            SYSTEM                                 
         MVC   SVKTYP,CURLTYP            TYPE                                   
         XC    SVDERV,SVDERV             DERIVED LIST                           
         XC    SCRDER,SCRDER                                                    
         OI    SCRDERH+6,FVOXMT                                                 
         MVI   PAGENUM,0                                                        
         XC    SVCNT,SVCNT               DATA COUNT                             
         XC    PAGETAB,PAGETAB           CLEAR THE FIRST TWO ENTRIES            
         XC    PAGETAB+L'PAGETAB(L'PAGETAB),PAGETAB+L'PAGETAB                   
         OI    LOIN,LOINKYC              KEY CHANGE                             
         NI    LOSIN,X'FF'-LOSIDON-LOSINOD-LOSIEPF                              
         B     VKOKX                                                            
*                                                                               
VKNOX    MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                                                             
*                                                                               
VKOKX    TM    CUSTAT,CUSDDS                                                    
         BZ    *+8                                                              
         BRAS  RE,INTERNAL                                                      
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* VALIDATE LIST RECORD AND DATA                                                 
***********************************************************************         
VALREC   BRAS  RE,SETSCRN                                                       
*                                                                               
         XC    AERFLD,AERFLD             NO ERRORS, YET                         
         XC    NEWEND,NEWEND                                                    
*                                                                               
         TM    LOSIN,LOSINEW             NEED TO ADD NEW LIST RECORD?           
         BZ    VR010                     . NO, CHANGING                         
         XC    LISTBLK,LISTBLK                                                  
         MVC   LISTACOM,ACOM                                                    
         MVC   LISTIAGY,CUAALF                                                  
         MVC   LISTIGRP,CURLGRP                                                 
         MVC   LISTITYP,CURLTYP                                                 
         MVC   LISTISYS,CURLSYS          SYSTEM                                 
         MVI   LISTACTN,LISTAADD         ADD LIST                               
         GOTO1 VLISTIO,LISTD                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CURLNUM,LISTINUM          SAVE NUMBER                            
*                                                                               
VR010    OC    CURLNUM,CURLNUM           BE SURE WE HAVE A LIST NUMBER          
         BNZ   *+6                       . IF WE DON'T,                         
         DC    H'0'                      . THIS AIN'T GONNA WORK                
                                                                                
*----------------------------------------------------------------------         
* PROCESS DERIVED LISTS                                                         
*----------------------------------------------------------------------         
         MVC   CURDERV,SPACES                                                   
         GOTO1 AFVAL,SCRDERH                                                    
         BNE   VR020                                                            
         CLI   FVILEN,2                                                         
         BNE   SAEIIF                                                           
         CLI   FVIFLD,C'$'               ONLY ALLOW OFFICE LISTS                
         BNE   SAEIIF                                                           
*                                                                               
         LLC   R1,FVILEN                                                        
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CURDERV,FVIFLD                                                   
         OC    CURDERV,SPACES                                                   
*                                                                               
VR020    CLC   CURDERV,SVDERV                                                   
         BE    VR026                                                            
*                                                                               
         XC    LISTBLK,LISTBLK                                                  
         MVC   LISTACOM,ACOM                                                    
         MVC   LISTIAGY,CUAALF                                                  
         MVC   LISTIGRP,CURLGRP                                                 
         MVC   LISTITYP,CURLTYP                                                 
         MVC   LISTISYS,CURLSYS                                                 
         MVC   LISTINUM,CURLNUM                                                 
*                                                                               
         CLC   SVDERV,SPACES                                                    
         BNH   VR022                                                            
*                                                                               
         MVI   LISTACTN,LISTARDL         REMOVE DERIVED LIST                    
         MVC   LISTIDER(L'SVDERV),SVDERV                                        
         GOTO1 VLISTIO,LISTD                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR022    CLC   CURDERV,SPACES                                                   
         BNH   VR024                                                            
*                                                                               
         MVI   LISTACTN,LISTAIDL         INSERT DERIVED LIST                    
         MVC   LISTIDER(L'CURDERV),CURDERV                                      
         GOTO1 VLISTIO,LISTD                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR024    MVC   SVDERV,CURDERV                                                   
                                                                                
*----------------------------------------------------------------------         
* PROCESS SCREEN DATA LINES                                                     
*----------------------------------------------------------------------         
VR026    LA    R8,DISTAB                 R8=A(DISPLAY LINE TABLE)               
         LA    R4,SCRV1F1H               R4=A(DATA VALUE FIELD)                 
         LA    R1,SCRVLF1H                                                      
         ST    R1,ASCRLDF                A(LAST DATA VALUE FIELD)               
*                                                                               
         USING LDTAD,R2                                                         
VR030    C     R4,ASCRLDF                END OF SCREEN?                         
         BH    VR150                     . YES                                  
*                                                                               
         MVI   CURLDIL,0                                                        
         MVI   CURLDATT,0                                                       
         MVC   CURLDVAL,SPACES                                                  
         MVC   CURLDDIS,SPACES                                                  
*                                                                               
         LA    R1,SCRV1F2H-SCRV1F1H(R4)  CLEAR THE MESSAGE FIELD                
         MVC   8(L'LMSG,R1),SPACES                                              
         NI    FHATD(R1),X'FF'-FHATHI                                           
         NI    FHATD(R4),X'FF'-FHATHI                                           
         OI    FHOID(R1),FHOITR                                                 
         OI    FHOID(R4),FHOITR                                                 
*                                                                               
         GOTO1 AFVAL,(R4)                                                       
         BNE   VR040                                                            
*                                                                               
         MVI   CURLDATT,0                                                       
         MVC   CURLDIL,FVILEN            INPUT LENGTH                           
         MVC   CURLDVAL,FVIFLD           DATA VALUE                             
         MVC   CURLDDIS,FVIFLD                                                  
*                                                                               
         CLI   CURLDVAL,C'-'             IS IT A NEGATIVE FILTER?               
         BNE   VR040                                                            
         MVC   CURLDVAL(L'CURLDVAL-1),CURLDVAL+1                                
         MVI   CURLDVAL+L'CURLDVAL-1,C' '                                       
         OI    CURLDATT,LDDANOQ          X'01' NEGATIVE FILTER                  
         LLC   R1,CURLDIL                                                       
         AHI   R1,-1                                                            
         STC   R1,CURLDIL                                                       
*                                                                               
VR040    TM    LOSIN,LOSINEW             IS THIS NEW?                           
         BO    VR050                     . YES, NO NEED FOR REMOVALS            
         CLC   0(L'DISTAB,R8),SPACES     ANYTHING PREVIOUS                      
         BNH   VR050                     . NO                                   
         CLC   0(L'DISTAB,R8),FVIFLD     SEE IF DATA CHANGED                    
         BE    VR090                     . NO, JUST DISPLAY                     
                                                                                
*----------------------------------------                                       
* PROCESS PREVIOUS DATA VALUE, REMOVE DATA                                      
*----------------------------------------                                       
         XC    LISTBLK,LISTBLK                                                  
         MVC   LISTACOM,ACOM                                                    
         MVC   LISTINUM,CURLNUM                                                 
         MVC   LISTITYP,CURLTYP                                                 
         MVC   LISTISYS,CURLSYS                                                 
         MVC   LISTDVAL,0(R8)                                                   
*                                                                               
         CLI   0(R8),C'-'                IS IT A NEGATIVE FILTER?               
         BNE   VR044                                                            
         MVC   LISTDVAL(L'LISTDVAL-1),LISTDVAL+1                                
         MVI   LISTDVAL+L'LISTDVAL-1,C' '                                       
         OI    LISTDATT,LDDANOQ          X'01' NEGATIVE FILTER                  
*                                                                               
VR044    MVI   LISTACTN,LISTAREM         REMOVE DATA FROM LIST                  
         GOTO1 VLISTIO,LISTD                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(L'DISTAB,R8),SPACES     VALUE SUCCESSFULLY REMOVED             
*                                                                               
         GOTOR UPCNT,0                   DECREMENT DATA COUNT                   
*                                                                               
         LA    R1,SCRV1F2H-SCRV1F1H(R4)                                         
         MVC   8(L'LMSG,R1),MICRE        CODE REMOVED                           
                                                                                
*----------------------------------------                                       
* PROCESS CURRENT DATA VALUE, ADD DATA                                          
*----------------------------------------                                       
VR050    CLC   FVIFLD,SPACES                                                    
         BNH   VR100                                                            
*                                                                               
         XC    LISTBLK,LISTBLK           CHECK FOR DUPLICATES                   
         MVC   LISTACOM,ACOM                                                    
         MVC   LISTINUM,CURLNUM                                                 
         MVC   LISTDVAL,CURLDVAL                                                
         MVC   LISTDATT,CURLDATT                                                
         MVI   LISTACTN,LISTAHAS                                                
         GOTO1 VLISTIO,LISTD                                                    
         BE    VR120                     DUPLICATE                              
*                                                                               
         CLC   CURLDIL,LISTIDMN          CHECK DATA MINUMUM LENGTH              
         BL    VR110                                                            
         CLC   CURLDIL,LISTIDMX          CHECK DATA MAXIMUM LENGTH              
         BH    VR110                                                            
*                                                                               
         XC    LISTBLK,LISTBLK                                                  
         MVC   LISTACOM,ACOM                                                    
         MVC   LISTINUM,CURLNUM                                                 
         MVC   LISTITYP,CURLTYP                                                 
         MVC   LISTISYS,CURLSYS                                                 
         MVC   LISTDVAL,CURLDVAL                                                
         MVC   LISTDATT,CURLDATT                                                
         MVI   LISTACTN,LISTAINS         INSERT DATA INTO LIST                  
         GOTO1 VLISTIO,LISTD                                                    
         BNE   VR110                     INVALID                                
*                                                                               
         GOTOR UPCNT,1                   INCREMENT DATA COUNT                   
*                                                                               
         LA    R1,SCRV1F2H-SCRV1F1H(R4)                                         
         CLC   8(L'LMSG,R1),SPACES                                              
         BH    VR088                                                            
         MVC   8(L'LMSG,R1),MICIN        CODE INSERTED                          
         B     VR090                                                            
VR088    MVC   8(L'LMSG,R1),MICCH        CODE CHANGED                           
*                                                                               
VR090    CLC   LINEEND,NEWEND            IF LAST LINE IS STILL THERE            
         BE    VR100                     . THEN KEEP IT                         
         CLC   LINEEND,CURLDVAL                                                 
         BH    VR100                                                            
         MVC   NEWEND,CURLDVAL                                                  
                                                                                
*----------------------------------------                                       
* UPDATE DISPLAY TABLE AND CHECK NEXT                                           
*----------------------------------------                                       
VR100    MVC   0(L'DISTAB,R8),FVIFLD                                            
         B     VR140                                                            
VR110    MVC   APWORK(L'LMSG),MEIID      INVALID INPUT FIELD                    
         B     VR130                                                            
VR120    MVC   APWORK(L'LMSG),MEDUP      DUPLICATE DATA VALUE                   
*                                                                               
VR130    LA    R1,SCRV1F2H-SCRV1F1H(R4)                                         
         MVC   8(L'LMSG,R1),APWORK                                              
         OI    FHATD(R1),FHATHI                                                 
         OI    FHATD(R4),FHATHI                                                 
         OI    FHOID(R1),FHOIHI                                                 
         OI    FHOID(R4),FHOIHI                                                 
         OC    AERFLD,AERFLD                                                    
         BNZ   *+8                                                              
         ST    R4,AERFLD                                                        
*                                                                               
VR140    LA    R8,L'DISTAB(,R8)                                                 
         LA    R4,SCRV2F1H-SCRV1F1H(,R4)                                        
         B     VR030                                                            
*                                                                               
VR150    MVC   LINEEND,NEWEND            SET END LINE                           
         LA    R1,SCRV1F1H               SET CURSOR POSITION                    
         ST    R1,APCURSOR                                                      
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         NI    LOSIN,X'FF'-LOSINEW                                              
         BRAS  RE,SETSCRN                IMMEDIATE DISPLAY ON CHANGE            
         BRAS  RE,DISSSH                 DISPLAY SYSTEM SUMMARY LINE            
         BAS   RE,UPSALA                 UPDATE DATA GROUP RECORD               
         BNE   NO                        . THAT DIDN'T WORK                     
*                                                                               
         OC    AERFLD,AERFLD             ANY DATA ERRORS?                       
         BZ    VRX                                                              
         MVC   APCURSOR,AERFLD                                                  
         MVC   FVADDR,AERFLD                                                    
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                                                             
*                                                                               
VRX      CLI   APPFKEY,PFK07             PAGE UP, DISPLAY PREVIOUS              
         BE    DISREC                                                           
         CLI   APPFKEY,PFK08             PAGE DOWN, DISPLAY NEXT                
         BE    DISREC                                                           
         B     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* DISPLAY KEY OF LIST RECORD                                                    
***********************************************************************         
DISKEY   TM    TWASWPST,TWASWAP    PFKEY TO GET HERE?                           
         BO    DK020               . YES                                        
         LA    R1,APRECKEY                                                      
         USING SALAREC,R1                                                       
         MVC   SVKEYFLD(L'SALAAGR),SALAAGR   PULL THE GROUP                     
         DROP  R1                                                               
*                                                                               
DK020    MVC   SCRCOD,SVKEYFLD     SWAPPING, TRY AND RECALL THE KEY             
         MVC   SCRSYS,SVKEYFLD+L'SCRCOD                                         
         MVC   SCRTYP,SVKEYFLD+L'SCRCOD+L'SCRSYS                                
         OI    SCRCODH+6,FVOXMT                                                 
         OI    SCRSYSH+6,FVOXMT                                                 
         OI    SCRTYPH+6,FVOXMT                                                 
         NI    TWASWPST,X'FF'-TWASWAP                                           
         LA    R1,ACSACTH                                                       
         ST    R1,APCURSOR                                                      
         B     VALKEY              VALKEY WILL DO THE WORK                      
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY LIST DATA RECORDS                                          
***********************************************************************         
DISREC   BRAS  RE,CLRSCR                 CLEAR THE DATA FIELDS                  
         BRAS  RE,SETPAGE                PROCESS PFKEY AND SET PAGE #           
         BRAS  RE,SETSCRN                PROTECT/UNPROTECT FIELDS               
         NI    LOIN,X'FF'-LOINNPG                                               
         NI    LOSIN,X'FF'-LOSINOD                                              
                                                                                
         MVC   SCRDER(L'CURDERV),CURDERV                                        
         OI    SCRDERH+6,FVOXMT                                                 
*                                                                               
         LA    R8,DISTAB                 R8=A(DISPLAY LINE TABLE)               
         LA    R4,SCRV1F1H               R4=A(DATA VALUE FIELD)                 
         LA    R1,SCRVLF1H                                                      
         ST    R1,ASCRLDF                A(LAST DATA VALUE FIELD)               
         XC    LINEEND,LINEEND                                                  
         XC    SVSCNT,SVSCNT             ON SCREEN DATA COUNT                   
*                                                                               
         XC    LISTBLK,LISTBLK           SET UP LISTIO CALL                     
         MVC   LISTACOM,ACOM                                                    
         MVC   LISTINUM,CURLNUM                                                 
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,PAGENUM              IF NOT ON FIRST PAGE                   
         BNZ   DR006                     THEN START WHERE WE LEFT OFF           
*                                                                               
         MVI   LISTACTN,LISTANXT                                                
         GOTO1 VLISTIO,LISTD             DO WE HAVE ANYTHING IN LIST?           
         BE    DR020                     . YES                                  
         OI    LOSIN,LOSINOD             . NO, THEN NO ACCESS                   
         B     DROKX                                                            
*                                                                               
DR006    MHI   RF,L'PAGETAB              FIND FIRST FOR SCREEN                  
         LA    R1,PAGETAB                                                       
         AR    R1,RF                                                            
*                                                                               
         MVC   LISTDVAL,0(R1)                                                   
         MVI   LISTACTN,LISTAGET                                                
         GOTO1 VLISTIO,LISTD                                                    
         BE    DR020                                                            
*                                                                               
DR010    MVI   LISTACTN,LISTANXT                                                
         GOTO1 VLISTIO,LISTD                                                    
         BE    DR020                                                            
         OI    LOSIN,LOSIDON             DONE WITH DATA VALUES                  
         B     DROKX                                                            
*                                                                               
DR020    MVC   CURLDVAL,LISTDVAL                                                
         MVC   CURLDDIS,LISTDVAL                                                
         MVC   CURLDATT,LISTDATT                                                
*                                                                               
         TM    CURLDATT,LDDANOQ          X'01' NEGATIVE FILTER                  
         BZ    DR022                                                            
         MVI   CURLDDIS,C'-'                                                    
         MVC   CURLDDIS+1(L'CURLDDIS-1),CURLDVAL                                
         MVC   CURLDATT,LISTDATT                                                
*                                                                               
DR022    GOTOR SETFLD,APPARM,(R4)                                               
*                                                                               
         MVC   0(L'DISTAB,R8),CURLDDIS   STORE IN DISPLAY TABLE                 
         OC    0(L'DISTAB,R8),SPACES                                            
*                                                                               
         TM    LOIN,LOINNPG              DID WE ADD THIS PAGE TO TABLE?         
         BO    DR030                     . YES                                  
         OI    LOIN,LOINNPG                                                     
*                                                                               
         LLC   RF,PAGENUM                                                       
         MHI   RF,L'PAGETAB                                                     
         LA    R1,PAGETAB                                                       
         AR    R1,RF                                                            
         MVC   0(L'PAGELIN,R1),CURLDVAL                                         
         MVC   L'PAGELIN(L'PAGEPRE,R1),SVCNT                                    
*                                                                               
DR030    MVC   LINEEND,CURLDVAL                                                 
*                                                                               
         LH    R0,SVCNT                  INCREMENT DISPLAYED DATA COUNT         
         AHI   R0,1                                                             
         STH   R0,SVCNT                                                         
*                                                                               
         LH    R0,SVSCNT                 INCREMENT ON SCREEN DATA COUNT         
         AHI   R0,1                                                             
         STH   R0,SVSCNT                                                        
*                                                                               
         LA    R8,L'DISTAB(,R8)                                                 
         LA    R4,SCRV2F1H-SCRV1F1H(,R4)                                        
*                                                                               
         C     R4,ASCRLDF                                                       
         BNH   DR010                                                            
*                                                                               
DROKX    BRAS  RE,SETSCRN                SET SCREEN                             
         NI    LOIN,X'FF'-LOINKYC                                               
*                                                                               
         L     R1,AIOAREA3               SALAREC IN AIO3                        
         MVI   APBYTE,CTFILEQ            CTFILE RECORD                          
         GOTO1 ADISACT                   DISPLAY ACTIVITY DATE                  
*                                                                               
         TM    CUSTAT,CUSDDS                                                    
         BZ    *+8                                                              
         BRAS  RE,INTERNAL                                                      
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO DELETE A DATA ACCESS RECORD                                        
***********************************************************************         
DELREC   XC    LISTBLK,LISTBLK                                                  
         MVC   LISTACOM,ACOM                                                    
         MVC   LISTIAGY,CUAALF                                                  
         MVC   LISTIGRP,CURLGRP                                                 
         MVC   LISTITYP,CURLTYP                                                 
         MVC   LISTISYS,CURLSYS          SYSTEM                                 
*                                                                               
         MVI   LISTACTN,LISTADEL         DELETE LIST                            
         GOTO1 VLISTIO,LISTD                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
DELRECX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO RESTORE A DELETED DATA ACCESS RECORD                               
***********************************************************************         
RESREC   XC    LISTBLK,LISTBLK                                                  
         MVC   LISTACOM,ACOM                                                    
         MVC   LISTIAGY,CUAALF                                                  
         MVC   LISTIGRP,CURLGRP                                                 
         MVC   LISTITYP,CURLTYP                                                 
         MVC   LISTISYS,CURLSYS          SYSTEM                                 
*                                                                               
         MVI   LISTACTN,LISTARES         RESTORE LIST                           
         GOTO1 VLISTIO,LISTD                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
RESRECX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* SET DISPLAY FIELD                                                             
***********************************************************************         
SETFLD   NTR1                                                                   
*                                                                               
         L     R8,0(R1)                                                         
         LA    R9,CURLDDIS                                                      
*                                                                               
         LLC   RE,0(R8)                                                         
         SHI   RE,FHDAD+1          SUBTRACT HEADER LENGTH+1 FOR EXECUTE         
         BNM   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    FHATD(R8),FHATXH    ANY EXTENDED FIELD HEADER?                   
         BZ    SF010               . NO                                         
         SHI   RE,FHDAD            . YES, SUBTRACT FROM LENGTH                  
         BNM   *+6                                                              
         DC    H'0'                                                             
*                                                                               
SF010    EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FHDAD(0,R8),0(R9)   MOVE INTO FIELD DATA                         
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         OC    FHDAD(0,R8),SPACES                                               
*                                                                               
         OI    FHOID(R8),FVOXMT                                                 
         B     XIT                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* CLEAR SCREEN AND DISPLAY TABLE                                                
***********************************************************************         
CLRSCR   NTR1                                                                   
*                                                                               
         LA    R1,SCRV1F1H                                                      
         LA    RF,SCRVLFLH                                                      
CS010    CR    R1,RF                                                            
         BH    CS020                                                            
*                                                                               
         LLC   RE,0(R1)                                                         
         SHI   RE,FHDAD+1          SUBTRACT HEADER LENGTH+1 FOR EXECUTE         
         BNM   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    FHATD(R1),FHATXH    ANY EXTENDED FIELD HEADER?                   
         BZ    CS012               . NO                                         
         SHI   RE,FHDAD            . YES, SUBTRACT FROM LENGTH                  
         BNM   *+6                                                              
         DC    H'0'                                                             
*                                                                               
CS012    EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FHDAD(0,R1),SPACES                                               
         OI    FHOID(R1),FVOXMT                                                 
         NI    FHATD(R1),X'FF'-FHATHI                                           
*                                                                               
         LLC   R0,0(R1)                                                         
         AR    R1,R0                                                            
         B     CS010                                                            
*                                                                               
CS020    LA    RE,DISTAB           CLEAR DISPLAY TABLE                          
         LHI   RF,DISMAX*L'DISTAB                                               
         XR    R0,R0                                                            
         LA    R1,X'40'                                                         
         SLL   R1,24                                                            
         MVCL  RE,R0                                                            
*                                                                               
CSX      B     XIT                                                              
                                                                                
***********************************************************************         
* SET PAGE                                                                      
***********************************************************************         
SETPAGE  NTR1                                                                   
*                                                                               
         TM    LOSIN,LOSINOD             NO DATA                                
         BO    SP010                                                            
*----------------------------------------                                       
* SCROLL UP                                                                     
*----------------------------------------                                       
         CLI   APPFKEY,PFK07             PF07 IS PAGE UP                        
         BNE   SP050                                                            
         CLI   PAGENUM,0                 ALREADY ON FIRST PAGE                  
         BE    SP010                                                            
*                                                                               
         LLC   RF,PAGENUM                DECREMENT PAGE# FOR PAGE UP            
         AHI   RF,-1                                                            
         STC   RF,PAGENUM                                                       
         CLI   PAGENUM,0                 BACKING UP TO THE FIRST PAGE           
         BE    SP010                                                            
*                                                                               
         MHI   RF,L'PAGETAB                                                     
         LA    R1,PAGETAB                                                       
         AR    R1,RF                                                            
         MVC   SVCNT,L'PAGELIN(R1)       NUMBER OF PREVIOUS VALUES              
         NI    LOSIN,X'FF'-LOSIDON-LOSIEPF                                      
         B     SP100                                                            
*                                                                               
SP010    XC    PAGETAB,PAGETAB           CLEAR THE FIRST TWO ENTRIES            
         NI    LOSIN,X'FF'-LOSIDON-LOSINOD-LOSIEPF                              
         B     SP100                                                            
*----------------------------------------                                       
* SCROLL DOWN                                                                   
*----------------------------------------                                       
SP050    CLI   APPFKEY,PFK08                                                    
         BNE   SP100                                                            
         CLI   PAGENUM,PAGEMAX           HIT THE MAXIMUM PAGE COUNT?            
         BE    SP100                                                            
         TM    LOSIN,LOSIDON+LOSIEPF     DONE WITH DATA AND EXTRA PAGE?         
         BO    SP100                                                            
         TM    LOSIN,LOSIDON             DONE WITH DATA VALUES?                 
         BZ    *+8                                                              
         OI    LOSIN,LOSIEPF             ALLOW ONE EXTRA PAGE DOWN              
*                                                                               
         LLC   RF,PAGENUM                BUMP PAGE NUMBER                       
         AHI   RF,1                                                             
         STC   RF,PAGENUM                                                       
         MHI   RF,L'PAGETAB                                                     
         LA    R1,PAGETAB                                                       
         AR    R1,RF                     DISPLY LAST AT TOP OF NEW PAGE         
         MVC   0(L'PAGELIN,R1),LINEEND                                          
         LH    RF,SVCNT                  DECRMNT COUNT FOR REDISPLAYED          
         AHI   RF,-1                      LAST VALUE                            
         STH   RF,SVCNT                                                         
         MVC   L'PAGELIN(L'PAGEPRE,R1),SVCNT                                    
*                                                                               
SP100    LLC   RF,PAGENUM                SET ITEM COUNT AT PAGE START           
         MHI   RF,L'PAGETAB                                                     
         LA    R1,PAGETAB                                                       
         AR    R1,RF                     DISPLY LAST AT TOP OF NEW PAGE         
         MVC   SVCNT,L'PAGELIN(R1)                                              
*                                                                               
         LLC   RF,PAGENUM                CLEAR NEXT ENTRY IN PAGE TABLE         
         MHI   RF,L'PAGETAB                                                     
         LA    R1,PAGETAB                                                       
         AR    R1,RF                                                            
         AHI   R1,L'PAGETAB                                                     
         XC    0(L'PAGETAB,R1),0(R1)                                            
         B     XIT                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* PRINT INTERNAL VALUES - DDS TERMINALS ONLY                                    
***********************************************************************         
INTERNAL NTR1                                                                   
*                                                                               
         MVC   SCRINT,SPACES                                                    
         OI    SCRINTH+6,FVOXMT                                                 
*                                                                               
         CLI   OPTPWD,C'Y'                                                      
         BNE   XIT                                                              
*                                                                               
         LA    R2,APWORK                                                        
         USING LLSTD,R2                                                         
         XC    APWORK,APWORK                                                    
         MVI   LLSTKMAJ,LLSTKMAQ         MAJOR - LISTING SYSSTEM (C'L')         
         MVI   LLSTKMIN,LLSTKMIQ         MINOR - SECURITY SYSTEM (C'S')         
         MVI   LLSTKREC,LLSTKREQ         RECORD- LIST RECORD     (C'L')         
         DROP  R2                                                               
*                                                                               
         GOTO1 VHEXOUT,APPARM,APWORK,SCRINT,3,=C'TOG'                           
         MVI   SCRINT+6,C'/'                                                    
*                                                                               
         USING LLSTKAGY,R2                                                      
         XC    APWORK,APWORK                                                    
         MVC   LLSTKAGY,CUAALF           ALPHA ID                               
         MVC   LLSTKGRP,CURLGRP          GROUP CODE                             
         MVC   LLSTKSYS,CURLSYS          SYSTEM                                 
         MVC   LLSTKTYP,CURLTYP          LIST TYPE                              
         DROP  R2                                                               
*                                                                               
         GOTO1 VHEXOUT,APPARM,APWORK,SCRINT+7,10,=C'TOG'                        
*                                                                               
         GOTO1 VHEXOUT,APPARM,CURLNUM,SCRINT+28,4,=C'TOG'                       
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* SET SCREEN (PROTECT/UNPROTECT/SET HEADING)                                    
***********************************************************************         
SETSCRN  NTR1                                                                   
*                                                                               
         LA    R1,SCRV1F1H                                                      
         LA    RF,SCRVLFLH                                                      
SS010    CR    R1,RF                                                            
         BH    SS020                                                            
*                                                                               
         NI    FHOID(R1),X'FF'-FHOIPR                                           
         NI    FHATD(R1),X'FF'-FHATPR                                           
*                                                                               
         CLI   APACTN,ACTCHA             CHANGING?                              
         BE    SS015                     . ALWAYS UNPROTECT                     
         OI    FHOID(R1),FHOIPR                                                 
         OI    FHATD(R1),FHATPR                                                 
*                                                                               
SS015    OI    FHOID(R1),FVOXMT                                                 
*                                                                               
         LLC   R0,0(R1)                  BUMP                                   
         AR    R1,R0                                                            
         LLC   R0,0(R1)                  & BUMP                                 
         AR    R1,R0                                                            
         B     SS010                                                            
*                                                                               
SS020    MVC   SCRHED1,SPACES                                                   
         OI    SCRHED1H+6,FVOXMT                                                
         MVC   SCRITMN,SPACES                                                   
         OI    SCRITMNH+6,FVOXMT                                                
         MVC   SCRITMS,SPACES                                                   
         OI    SCRITMSH+6,FVOXMT                                                
         MVC   SCRITMT,SPACES                                                   
         OI    SCRITMTH+6,FVOXMT                                                
         MVC   SCRITME,SPACES                                                   
         OI    SCRITMEH+6,FVOXMT                                                
*                                                                               
         TM    LOSIN,LOSINOD             NO ACCESS                              
         BZ    SS040                                                            
         CLI   APACTN,ACTCHA                                                    
         BE    SS040                                                            
         B     SSX                                                              
*                                                                               
SS040    CLC   SCRTYP,SPACES                                                    
         BNH   SSX                                                              
         MVC   SCRHED1(L'SCRTYP),SCRTYP                                         
*                                                                               
         OC    SVCNT,SVCNT                                                      
         BZ    SSX                                                              
*                                                                               
*        MVC   SCRITMN(4),=CL4'Item'     ITEM                                   
*                                                                               
*        LH    RF,SVCNT                  # OF ITEMS DISPLAYED SO FAR            
*        LH    R1,SVSCNT                 # OF ITEMS ON SCREEN                   
*        SR    RF,R1                                                            
*        AHI   RF,1                                                             
*        EDIT  (RF),SCRITMS,0,DUB=APDUB,WRK=APWORK                              
*        MVI   SCRITMT,C'-'              -                                      
*        LH    RF,SVCNT                  END NUMBER                             
*        EDIT  (RF),SCRITME,0,ALIGN=LEFT,DUB=APDUB,WRK=APWORK                   
*                                                                               
SSX      B     XIT                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* UPDATE PAGE COUNTERS                                                          
***********************************************************************         
UPCNT    NTR1                                                                   
*                                                                               
         LHI   RF,-1               DECREMENT BASED ON APWORK VALUE              
         LA    R2,APWORK                                                        
         LTR   R1,R1               ARE WE REMOVING?                             
         BZ    *+12                . YES                                        
         LHI   RF,1                . NO, INCREMENT BASED ON CURLDVAL            
         LA    R2,CURLDVAL                                                      
*                                                                               
         LH    R0,SVCNT                                                         
         AR    R0,RF                                                            
         STH   R0,SVCNT                                                         
*                                                                               
         LH    R0,SVSCNT                                                        
         AR    R0,RF                                                            
         STH   R0,SVSCNT                                                        
*                                                                               
         LA    R3,PAGETAB                                                       
         LHI   R4,PAGEMAX                                                       
*                                                                               
UPC030   CLC   0(L'PAGELIN,R3),SPACES                                           
         BNH   UPCX                                                             
         CLC   0(L'PAGELIN,R3),0(R2)                                            
         BL    UPC040                                                           
         SR    R0,R0                                                            
         ICM   R0,3,L'PAGELIN(R3)                                               
         AR    R0,RF                                                            
         STCM  R0,3,L'PAGELIN(R3)                                               
*                                                                               
UPC040   AHI   R3,L'PAGETAB                                                     
         BCT   R4,UPC030                                                        
*                                                                               
UPCX     B     XIT                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* UPDATE DATA ACCESS GROUP RECORD (SALAREC)                                     
***********************************************************************         
UPSALA   NTR1                                                                   
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING SALAREC,R2                                                       
         MVI   SALATYP,SALATYPQ          C'F'                                   
         MVI   SALASUB,SALASUBQ          X'18'                                  
         MVC   SALAAGY,CUAALF                                                   
         MVC   SALAAGR,CURLCOD           DATA GROUP CODE                        
         GOTO1 AIO,IORD+IOLOCK+IOCONFIL+IO2                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA2                                                      
*                                                                               
         GOTO1 VHELLO,APPARM,(C'D',=C'CTFILE'),('SALASELQ',SALAREC),   +        
               (L'CURLSYS,CURLSYS),0                                            
         CLI   APPARM+12,0                                                      
         BE    UPS010                                                           
         CLI   APPARM+12,6                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPS010   XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING SALASD,R3                                                        
         MVI   SALASEL,SALASELQ          DATA GROUP SYSTEM ELEMENT              
         MVI   SALASLN,SALASLNQ                                                 
         MVC   SALASNUM,CURLSYS          SYSTEM                                 
         MVC   SALASIND,CURLSIND         SYSTEM INDICATOR                       
         OC    SVCNT,SVCNT                                                      
         BNZ   UPS014                                                           
         CLC   SVDERV,SPACES                                                    
         BNH   UPS020                                                           
UPS014   MVC   SALASALL(2),=C'=='        EQUAL SIGN MEANS DATA ACCESS           
         MVC   SALASALL+2(2),CURLGRP                                            
*                                                                               
UPS020   GOTO1 VHELLO,APPARM,(C'P',=C'CTFILE '),SALAREC,SALASD,0                
         CLI   APPARM+12,0                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   APBYTE,CTFILEQ            INDICATE CTFILE                        
         GOTO1 ASETACT,SALAREC                                                  
*                                                                               
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    SALAAGR,SALAAGR           CLEAR GROUP CODE                       
         MVC   SALAAGN,CURLGRP           FILL IN GROUP NUMBER                   
         MVC   IOKEY(L'SALAKEY),SALAKEY  DATA GROUP NUMBER KEY                  
         GOTO1 AIO,IORD+IOLOCK+IOCONFIL+IO3  READ FOR DATA GROUP NUMBER         
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,IOWRITE+IOCONFIL+IO2  WRITE BACK NUMBER SAME AS CODE         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     YES                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
* ROUTINE TO DISPLAY SYSTEMS SUMMARY HEADER                                     
***********************************************************************         
DISSSH   NTR1                                                                   
*                                                                               
         MVC   SYSNAMS,SPACES                                                   
         MVC   APHALF,=Y(CS#SYSAS)                                              
         GOTO1 ADISTXT,APPARM,APHALF                                            
         MVC   SYSNAMS(16),APWORK                                               
*                                                                               
         LA    RE,SYSNAMS+18       SET A(NEXT ENTRY)                            
         ST    RE,ASYSNAMS                                                      
         LA    R1,SYSNAMS                                                       
         SR    RE,R1                                                            
         STC   RE,SYSNAMSL         SET LIST LENGTH                              
         MVI   SYSCNT,0                                                         
*                                                                               
         L     R2,AIOAREA3                                                      
         LA    R3,SALADATA-SALAREC(R2)                                          
*                                                                               
         USING SALASD,R3                                                        
DSSH020  CLI   0(R3),0                                                          
         BE    DSSH080                                                          
         CLI   0(R3),SALASELQ                                                   
         BE    DSSH040                                                          
DSSH030  LLC   R1,1(R3)            NEXT ELEMENT                                 
         AR    R3,R1                                                            
         B     DSSH020                                                          
*                                                                               
DSSH040  L     R4,ASYS             SEARCH SE LIST FOR SE NUM SESYSNUM           
         L     R4,VSELIST-SYSFACD(R4)                                           
         LH    RE,0(R4)                                                         
         L     RF,2(R4)                                                         
         LA    R4,6(R4)            R3=A(SELIST ENTRY)                           
         USING SELISTD,R4                                                       
         CLC   SALASNUM,SEOVSYS                                                 
         BE    DSSH050                                                          
         BXLE  R4,RE,*-10                                                       
         LA    R4,=CL7'XXX    '    SET UNKNOWN SYSTEM NAME                      
*                                                                               
DSSH050  MVI   APBYTE,C' '                                                      
*                                                                               
         XC    LISTIOB(LISTIOBL),LISTIOB                                        
         MVC   LISTACOM,ACOM                                                    
         MVC   LISTIAGY,CUAALF                                                  
         MVC   LISTIGRP,CURLGRP                                                 
         MVC   LISTISYS,SALASNUM                                                
DSSH055  MVI   LISTACTN,LISTANLG   NEXT LIST FOR THIS GROUP                     
         GOTO1 =V(LISTIO),LISTIOB,RR=APRELO                                     
         BNE   DSSH060                                                          
         CLC   LISTISYS,SALASNUM                                                
         BNE   DSSH060                                                          
*                                                                               
         XC    LISTDVAL,LISTDVAL   FIRST ITEM IN LIST                           
         MVI   LISTACTN,LISTANXT   ANY DATA IN LIST                             
         GOTO1 =V(LISTIO),LISTIOB,RR=APRELO                                     
         BNE   DSSH055                                                          
         MVI   APBYTE,C'D'                                                      
         B     DSSH055                                                          
*                                                                               
DSSH060  CLI   APBYTE,C'D'                                                      
         BNE   DSSH030                                                          
*                                                                               
DSSH062  L     RE,ASYSNAMS         MOVE NAME TO LIST                            
         SR    R1,R1                                                            
         ICM   R1,1,SYSCNT         TEST COUNT OF ITEMS IN LIST                  
         BZ    *+12                                                             
         MVI   0(RE),C'/'                                                       
         LA    RE,1(RE)                                                         
         LA    R1,1(R1)                                                         
         STC   R1,SYSCNT           BUMP ITEM COUNT                              
         LA    RF,SYSNAMS+L'SYSNAMS-3                                           
         CR    RE,RF                                                            
         BH    DSSH030                                                          
         MVC   0(3,RE),SENAME      EXTRACT SE NAME                              
         NC    1(2,RE),=8X'BF'     SET TO LOWER CASE                            
         LA    RE,3(RE)                                                         
         ST    RE,ASYSNAMS                                                      
         LA    R1,SYSNAMS                                                       
         SR    RE,R1                                                            
         STC   RE,SYSNAMSL         SET LIST LENGTH                              
         B     DSSH030                                                          
         DROP  R4                                                               
         DROP  R3                                                               
*                                                                               
DSSH080  CLI   SYSCNT,0                                                         
         BNE   DSSH100                                                          
         MVC   SYSNAMS(19),MINOS   NO SYSTEMS ASSIGNED                          
         MVI   SYSNAMSL,19                                                      
*                                                                               
DSSH100  MVI   SCRSYA,C'-'         DISPLAY LIST OF SYSTEMS                      
         MVC   SCRSYA+1(L'SCRSYA-1),SCRSYA                                      
         SR    RF,RF                                                            
         ICM   RF,1,SYSNAMSL       RF=L'SYSTEM NAMES LIST                       
         BZ    DSSH110                                                          
         LA    R1,L'SCRSYA                                                      
         SR    R1,RF                                                            
         BNP   DSSH110                                                          
         SRL   R1,1                                                             
         LA    RE,SCRSYA(R1)                                                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),SYSNAMS                                                  
DSSH110  OI    SCRSYAH+6,X'80'                                                  
*                                                                               
DSSHX    B     XIT                                                              
         EJECT                                                                  
                                                                                
**********************************************************************          
* LITERALS AND CONSTANTS AND ERROR EXITS                                        
**********************************************************************          
SAEIIF   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                      INPUT FIELD NOT VALID                  
SAEENK   MVC   FVMSGNO,=AL2(FVFEKEY)                                            
         B     EXIT                      ENTER KEY                              
SAEMIF   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXIT                      MISSING FIELD                          
SAERNF   MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     EXIT                      RECORD NOT FOUND                       
SAERAE   MVC   FVMSGNO,=AL2(FVFERAE)                                            
         B     EXIT                      ALREADY EXISTS                         
SAERND   MVC   FVMSGNO,=AL2(CE#CRRND)                                           
         B     EXIT                      CAN'T RESTORE-REC NOT DELETED          
SAEMRL   MVC   FVMSGNO,=AL2(CE#LMBE)                                            
         LA    R1,SCRV1F1H                                                      
         ST    R1,FVADDR                                                        
         B     EXIT                      MUST REMOVE LIST ITEMS FIRST           
*                                                                               
FFILL    DC    4X'FF'                                                           
SPACES   DC    CL80' '                                                          
*                                                                               
LMSG     DS    0CL25                                                            
MINOA    DC    CL25'Default to Limited Access'                                  
MINOS    DC    CL25'No Systems Assigned'                                        
MICIN    DC    CL25'Code inserted'                                              
MICRE    DC    CL25'Code removed'                                               
MICCH    DC    CL25'Code changed'                                               
MEIID    DC    CL25'*Error: invalid code'                                       
MEDUP    DC    CL25'*Error: duplicate code'                                     
         LTORG                                                                  
                                                                                
***********************************************************************         
* DSECTS & INCLUDES                                                             
***********************************************************************         
* SEACSWRK                                                                      
* DDLANGEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE SEACSWRK                                                       
       ++INCLUDE DDLANGEQUS                                                     
         PRINT ON                                                               
*                                                                               
* DDLISTD                                                                       
LISTD    DSECT                                                                  
       ++INCLUDE DDLISTD                                                        
         EJECT                                                                  
                                                                                
***********************************************************************         
* SCREEN AND SAVED STORAGE                                                      
***********************************************************************         
TWAD     DSECT                                                                  
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSA6D                                                       
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSA5D                                                       
         EJECT                                                                  
                                                                                
***********************************************************************         
* SAVED AREA DSECT                                                              
***********************************************************************         
LSAVAREA DSECT                                                                  
*                                                                               
LOSIN    DS    X                   LOCAL SAVED INDICATOR                        
LOSINEW  EQU   X'80'               . NEW DATA ACCESS RECORD                     
LOSINOD  EQU   X'20'               . NO DATA ACCESS SET UP                      
LOSIDON  EQU   X'10'               . NO MORE DATA VALUES IN LIST                
LOSIEPF  EQU   X'08'               . EXTRA PAGE DOWN                            
*                                                                               
SVVALS   DS    0X                  CURRENT VALUES                               
SVKNUM   DS    XL4                 . LIST NUMBER                                
SVKGRP   DS    XL2                 . DATA GROUP NUMBER                          
SVKSYS   DS    X                   . DATA SYSTEM                                
SVKTYP   DS    XL2                 . DATA TYPE                                  
SVCNT    DS    H                   . # OF DATA VALUES DISPLAYED SO FAR          
SVSCNT   DS    H                   . # OF DATA VALUES ON SCREEN                 
SVDERV   DS    CL10                . DERIVED LIST                               
SVLNQ    EQU   *-SVVALS                                                         
*                                                                               
DISMAX   EQU   14                                                               
DISTAB   DS    (DISMAX)CL24        DISPLAY TABLE FOR SCREEN LINES               
LINEEND  DS    CL24                LAST DATA LINE ON SCREEN (DISPLAY)           
NEWEND   DS    CL24                LAST DATA LINE ON SCREEN (CHANGE)            
*                                                                               
PAGEMAX  EQU   50                                                               
PAGENUM  DS    X                   CURRENTLY DISPLAYED PAGE                     
PAGEEND  DS    X                   END PAGE THAT WAS DISPLAYED                  
*                                                                               
PAGETAB  DS    0CL28               PAGE TABLE (DISPLAY DATA)                    
PAGELIN  DS    CL24                . LINE VALUE                                 
PAGEPRE  DS    XL2                 . # OF VALUES ON PREVIOUS PAGES              
PAGEDEP  DS    XL2                 . # OF VALUES ON THIS PAGE                   
         ORG   PAGETAB                                                          
         DS    (PAGEMAX)CL(L'PAGETAB)                                           
                                                                                
***********************************************************************         
* LIST SCREEN LINE LAYOUT                                                       
***********************************************************************         
LSCRD    DSECT                                                                  
LSCRSELH DS    XL8                                                              
LSCRSEL  DS    CL3                 ACTION FIELD                                 
LSCRLINH DS    CL8                                                              
LSCRLINX DS    CL8                                                              
LSCRLIN  DS    0CL(L'LSTLIN1)                                                   
         DS    CL2                                                              
LSCRCOD  DS    CL8                 LIST CODE                                    
         DS    CL2                                                              
LSCRSYS  DS    CL8                 SYSTEM                                       
         DS    CL2                                                              
LSCRTYP  DS    CL10                LIST TYPE                                    
         ORG   LSCRLIN+L'LSCRLIN                                                
                                                                                
***********************************************************************         
* LOCAL WORKING STORAGE                                                         
***********************************************************************         
WORKD    DSECT                                                                  
         ORG   APLOCAL                                                          
LPARMS   DS    6A                                                               
VLISTIO  DS    V                   A(LIST IO ROUTINE)                           
ASCRLDF  DS    A                   A(LAST DATA FIELD ON THE SCREEN)             
AERFLD   DS    A                   A(FIRST FIELD IN ERROR)                      
ASYSNAMS DS    A                   A(SYSTEM NAMES POSITION)                     
*                                                                               
LOIN     DS    X                   LOCAL INDICATOR                              
LOINNOD  EQU   X'80'               . DATA VALUES FOUND ON SCREEN                
LOINNPG  EQU   X'40'               . NEW PAGE ADDED TO PAGE TABLE               
LOINKYC  EQU   X'20'               . KEY CHANGE                                 
*                                                                               
CURVALS  DS    0X                  CURRENT VALUES                               
CURLNUM  DS    XL4                 . LIST NUMBER                                
CURLCOD  DS    CL8                 . LIST GROUP CODE                            
CURLGRP  DS    XL2                 . LIST GROUP NUMBER                          
CURLGNM  DS    CL30                . LIST GROUP NAME                            
CURLSYS  DS    X                   . SYSTEM                                     
CURLTYP  DS    XL2                 . LIST TYPE                                  
CURLSIND DS    X                   . SYSTEM INDICATOR                           
CURLDIL  DS    X                   . INPUT LENGTH                               
CURLDDIS DS    CL(L'LISTDVAL)      . INPUT DATA DISPLAY VALUE                   
CURLDVAL DS    CL(L'LISTDVAL)      . INPUT DATA VALUE ABSOLUTE                  
CURLDATT DS    X                   . DATA ATTRIBUTES                            
CURDERV  DS    CL10                . INPUT DATA VALUE ABSOLUTE                  
CURVALNQ EQU   *-CURVALS                                                        
*                                                                               
SELVALS  DS    0X                  SELECT VALUES                                
SELCOD   DS    CL8                 . LIST GROUP CODE                            
SELGRP   DS    XL2                 . LIST GROUP NUMBER                          
SELSYS   DS    X                   . SYSTEM                                     
SELTYP   DS    C                   . DATA TYPE                                  
SELVALNQ EQU   *-SELVALS                                                        
*                                                                               
LISTBLK  DS    CL(LISTIOBL)                                                     
*                                                                               
SYSNAMS  DS    CL78                                                             
SYSNAMSL DS    XL1                                                              
SYSCNT   DS    X                                                                
*                                                                               
LOCALX   EQU   *                                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SEACS21   09/16/13'                                      
         END                                                                    
