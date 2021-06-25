*          DATA SET SEACS0A    AT LEVEL 016 AS OF 11/17/15                      
*PHASE TA0D0AA                                                                  
*INCLUDE SCINKEY                                                                
*INCLUDE DATTIM                                                                 
*INCLUDE LISTIO                                                                 
         TITLE 'SEACS0A -  SYSTEM ACCESS PASSWORD RECORD MAINTENANCE'           
ACS0A    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACSA**,RA,R9,RR=RE                                           
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY                                                         
         USING SA0REC,R2           R2=A(RECORD KEY)                             
         L     RC,ASYSWORK                                                      
         USING SYSWORK,RC          RC=A(SYSTEM + LOCAL W/S)                     
         ST    RE,APRELO                                                        
         L     R1,=V(SCINKEY)                                                   
         AR    R1,RE                                                            
         ST    R1,VSCINKEY                                                      
         L     R1,ACOM                                                          
         L     R1,CXSORT-COMFACSD(R1)                                           
         ST    R1,VXSORT                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         OC    OPTAGY,OPTAGY                                                    
         BZ    *+14                                                             
         MVC   AGENCYID,OPTAGY                                                  
         B     *+10                                                             
         MVC   AGENCYID,CUAALF                                                  
*                                                                               
         MVI   PWDAFLAG,X'FF'      ASSUME NO ACCESS FOR PWD                     
         USING COMFACSD,RE                                                      
         USING XTRAINFD,RF                                                      
         L     RE,ACOM             A(COMFACS)                                   
         ICM   RF,15,CXTRAINF      A(XTRAINFO BLOCK)                            
         BZ    MODE                NO ACCESS FOR PWD                            
*                                                                               
         CLC   AGENCYID,XIAGYPER   AGY = PID AGY?                               
         BNE   MODE                NO - NO ACCESS FOR PWD                       
         DROP  RE,RF                                                            
*                                                                               
         OC    ACASEC,ACASEC                                                    
         BZ    FSTLSTX                                                          
         L     RF,ACASEC           'DDS' PASSWORD                               
         TM    SECINDS-SECD(RF),SECIDDS                                         
         BO    MODE                YES - NO ACCESS FOR PWD                      
         MVI   PWDAFLAG,0          FULL READ/WRITE ACCESS                       
*                                                                               
MODE     EQU   *                                                                
         ZIC   RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     VALKEY              01 - APMVALK                                 
         B     VALREC              02 - APMVALR                                 
         B     DISKEY              03 - APMDISK                                 
         B     DISREC              04 - APMDISR                                 
         B     EXIT                05 - APMDELR                                 
         B     EXIT                06 - APMRESR                                 
         B     VALSEL              07 - APMVALP                                 
         B     GETSEL              08 - APMGETS                                 
         B     DISSEL              09 - APMDISS                                 
         B     EXIT                10 - APMVALS                                 
         B     FSTLST              11 - APMFLST                                 
         B     EXIT                12 - APMPROC                                 
         B     EXIT                13 - APMFSCR                                 
         B     LSTSCR              14 - APMLSCR                                 
         B     VALREQ              15 - APMVALQ                                 
         B     PRTREP              16 - APMREPP                                 
         B     EXIT                17 - APMSETT                                 
         B     PUTKEY              18 - APMPUTK                                 
         B     EXIT                19 - APMNEWK                                 
         B     EXIT                20 - APMFRP                                  
         B     EXIT                21 - APMDISS2                                
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF PASSWORD RECORD                          *         
* USING PERSONAL ID OF ASSOCIATED PERSON RECORD                       *         
***********************************************************************         
VALKEY   MVI   PIDREQD,C'N'                                                     
         GOTO1 AGETAAD,AGENCYID    GET AGENCY ACCESS DETAILS                    
         TM    APWORK,CTAADPRQ     PERSONAL ID REQUIRED WITH PASSWORD?          
         BZ    *+8                                                              
         MVI   PIDREQD,C'Y'                                                     
         MVC   PWDTOUT,APWORK+1    PASSWORD TIMEOUT DAYS                        
         MVC   PWDMINLN,APWORK+2   PASSWORD MINIMUM LENGTH                      
*                                                                               
         LA    R2,IOKEY                                                         
         XC    PWDNUM,PWDNUM       INITIALISE STORAGE                           
         XC    PWDCODE,PWDCODE                                                  
         GOTO1 VDATCON,APPARM,(X'05',0),(X'02',TODAY)                           
         MVC   TODAYC,FFILL        GET TODAYS DATE                              
         XC    TODAYC,TODAY          AND 2S COMPLEMENT                          
*                                  GET CURRENT DATE/TIME INTEGER                
         GOTO1 =V(DATTIM),APPARM,(X'01',DATETIME),0,RR=APRELO                   
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DATETIMC,DATETIME                                                
         XC    DATETIMC,FFILL                                                   
*                                                                               
         XC    DATAGRP,DATAGRP     CLEAR DATA GROUP                             
*                                                                               
         MVI   FVMINL,1            READ PERSONAL ID                             
         GOTO1 AFVAL,SYSPIDH                                                    
         BH    VALKEYEN                                                         
*                                                                               
         USING SAPEREC,R2                                                       
         XC    SAPEKEY,SAPEKEY     READ PERSON RECORD                           
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,AGENCYID                                                 
         MVC   SAPEPID,FVIFLD                                                   
         MVC   SAPEDEF,TODAYC      READ HIGH FOR TODAY                          
         MVC   APRECKEY(L'SAPEKEY),SAPEKEY                                      
         LA    R1,IOHID+IOCONFIL+IO3                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX             I/O ERROR EXIT                               
         L     R2,AIOAREA3                                                      
         CLI   IOERR,0                                                          
         BE    *+12                                                             
         TM    IOERR,IOEDEL                                                     
         BZ    VK050                                                            
         CLC   SAPEKEY(SAPEDEF-SAPEKEY),APRECKEY                                
         BE    *+12                                                             
         MVI   IOERR,IOERNF                                                     
         B     VK050                                                            
         TM    IOERR,IOEDEL                                                     
         BNZ   SAEPED              PERSON ID RECORD FOUND DELETED               
         MVC   APRECKEY(L'SAPEKEY),SAPEKEY  SAVE RECORD KEY                     
*                                                                               
         LA    R3,SAPEDATA         GET ELEMENT DATA                             
         SR    R0,R0                                                            
VK022    CLI   0(R3),0             TEST END OF RECORD                           
         BE    VK030                                                            
         CLI   0(R3),SAPWDELQ                                                   
         BE    VK024                                                            
         CLI   0(R3),SAPERELQ                                                   
         BE    VK025                                                            
         CLI   0(R3),SALACELQ      DATA GROUP ELEMENT                           
         BE    VK026                                                            
VK023    SR    R0,R0                                                            
         IC    R0,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R0                                                            
         B     VK022                                                            
         USING SAPWDD,R3                                                        
VK024    MVC   PWDNUM,SAPWDNUM     SAVE PASSWORD NUMBER                         
         MVC   PWDCODE,SAPWDCOD      AND CODE                                   
         B     VK023                                                            
         DROP  R3                                                               
*                                                                               
         USING SAPERD,R3                                                        
VK025    EQU   *                                                                
*&&UK                                                                           
*----------------------------------                                             
* TEST OFFICE MANAGER ACCESS                                                    
*----------------------------------                                             
         GOTO1 ATSTOMAN,SAPEROFF                                                
         BNE   VALKEYX                                                          
*&&                                                                             
*&&US                                                                           
*----------------------------------                                             
* TEST OFFICE/DEPT MANAGER ACCESS                                               
*----------------------------------                                             
         GOTO1 ATSTDMAN,APPARM,SAPEROFF,SAPERDID                                
         BNE   VALKEYX                                                          
*&&                                                                             
         B     VK023                                                            
         DROP  R3                                                               
*                                                                               
         USING SALACD,R3                                                        
VK026    MVC   DATAGRP,SALACNUM    DATA ACCESS GROUP NUMBER                     
         B     VK023                                                            
         DROP  R3                                                               
*                                                                               
VK030    OC    PWDCODE,PWDCODE     CHECK PASSWORD CODE FOUND OK                 
         BZ    VALKEYER                                                         
         LA    R2,IOKEY            BUILD PASSWORD RECORD KEY                    
         USING SA0REC,R2                                                        
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,AGENCYID                                                 
         CLI   PIDREQD,C'Y'        TEST PERSONAL ID PASSWORD                    
         BE    *+14                                                             
         MVC   SA0KCODE,PWDCODE                                                 
         B     *+10                                                             
         MVC   SA0KNUM,PWDNUM                                                   
         MVC   SA0KSAV(L'SA0KEY),SA0KEY                                         
         LA    R1,IORDD+IOCONFIL+IO1                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX             I/O ERROR EXIT                               
*                                                                               
         CLI   PIDREQD,C'Y'        TEST PERSONAL ID PASSWORD                    
         BE    VK040                                                            
         MVC   IOERRSV,IOERR                                                    
         LA    R2,IOKEY            READ AND LOCK PASSWORD # RECORD              
         USING SA0REC,R2                                                        
         XC    SA0KEY,SA0KEY                                                    
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,AGENCYID                                                 
         MVC   SA0KCODE,PWDNUM                                                  
         LA    R1,IORDD+IOCONFIL+IO2                                            
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                                                              
         BL    VALKEYX             I/O ERROR EXIT                               
         MVC   IOERR,IOERRSV                                                    
*                                                                               
VK040    MVI   FVMINL,1            READ SYSTEM FIELD                            
         GOTO1 AFVAL,SYSSYSH                                                    
         BNE   VK042                                                            
         GOTO1 AVALSYS,SYSSYSH     VALIDATE SYSTEM                              
         BNE   NO                                                               
         MVC   SYSTEM,APWORK       HOLD FOR LATER USE                           
*                                                                               
VK042    CLI   SYSTEM,0            IF NO SYSTEM INPUT                           
         BNE   VK050                                                            
*                                  DEFAULT TO LIST SYSTEM FILTER OR             
         MVC   SYSTEM,SELSYS         FIRST VALID SYSTEM IN RECORD               
         CLI   SYSTEM,0                                                         
         BNE   VK050                                                            
         XC    APELEM,APELEM                                                    
         MVI   APELEM,SASYSELQ                                                  
         L     R2,AIOAREA1                                                      
         LA    R3,SA0DATA                                                       
         USING SASYSD,R3                                                        
VK044    CLI   SASYSEL,0                                                        
         BE    VK050                                                            
         CLI   SASYSEL,SASYSELQ                                                 
         BE    VK048                                                            
VK046    SR    RF,RF                                                            
         IC    RF,SASYSLN                                                       
         AR    R3,RF                                                            
         B     VK044                                                            
VK048    MVC   SYSTEM,SASYSNUM                                                  
         DROP  R3                                                               
         GOTO1 ADISSYS,SYSTEM      REDISPLAY SYSTEM                             
         MVC   SYSSYS,APWORK                                                    
         OI    SYSSYSH+(FVOIND-FVIHDR),FVOXMT                                   
         GOTO1 AVALSYS,SYSSYSH     VALIDATE SYSTEM                              
         BNE   VK046                                                            
*                                                                               
VK050    CLI   SYSTEM,0                                                         
         BE    VK060                                                            
         GOTO1 ASETSEL,SYSTEM                                                   
         BNE   NO                                                               
*                                                                               
VK060    CLI   IOERR,0             SET IO INDICATOR FLAGS                       
         BNE   *+12                                                             
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         B     VALKEYY1                                                         
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    VALKEYY2                                                         
         MVI   APINDS,APIOKDIS+APIOKRES                                         
         B     VALKEYY1                                                         
*                                                                               
         USING SAPEREC,R1                                                       
VALKEYY1 LA    R1,APRECKEY         DISPLAY KEY DATA                             
         MVC   SYSPID(L'SAPEPID),SAPEPID                                        
         OI    SYSPIDH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
VALKEYY2 MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     VALKEYX                                                          
VALKEYER MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALKEYX                                                          
VALKEYEN MVC   FVMSGNO,=AL2(FVFEKEY)                                            
VALKEYX  B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO CHANGE SYSTEM ACCESS DATA FOR PASSWORD RECORD            *         
***********************************************************************         
VALREC   L     R2,AIOAREA1                                                      
         MVC   SA0KEY,SA0KSAV                                                   
         CLI   SYSTEM,0                                                         
         BE    DISREC                                                           
         CLC   SYSTEM,SAVSYS                                                    
         BE    VR002                                                            
         B     DISREC                                                           
*                                                                               
VR002    CLI   APACTN,ACTADD       ADD FUNCTION ?                               
         BNE   VR010                 OR CHANGE                                  
         B     VALRECX             ADD NOT VALID FOR THIS PHASE                 
*                                                                               
VR010    LA    R3,SA0DATA          CHANGE RECORD                                
         MVC   SVSYSALL,NAUTH                                                   
VR012    CLI   0(R3),0             TEST E-O-R                                   
         BE    VR020                                                            
         CLI   0(R3),SAACVELQ      X'01' ACTIVITY ELEMENT                       
         BE    VR014                                                            
         CLI   0(R3),SASYSELQ      X'21' SYSTEM ELEMENT                         
         BE    VR018                                                            
         CLI   0(R3),SACLAELQ      X'E1' CLIENT LIMIT ACCESS ELEMENT            
         BE    VR019                                                            
         CLI   PIDREQD,C'Y'        TEST PERSONAL ID PASSWORD                    
         BE    *+12                YES: SAVE OLD PASSWD POINTER ELEM            
         CLI   0(R3),SAPASELQ      X'03' POINTER ELEMENT                        
         BE    VR014                                                            
         B     VR016                                                            
*                                                                               
VR014    SR    R0,R0               DELETE ELEMENT                               
         ICM   R0,1,0(R3)                                                       
         MVC   APWORK(1),2(R3)                                                  
         GOTO1 VHELLO,APPARM,(C'D',CTFILE),((R0),SA0REC),(1,APWORK)             
         CLI   APPARM+12,0                                                      
         BE    *+6                                                              
         DC    H'00'                                                            
         B     VR012                                                            
*                                  BUMP TO NEXT ELEMENT                         
VR016    SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     VR012                                                            
*                                  DELETE SYSTEM ELEMENT IF ENTERED             
         USING SASYSD,R3                                                        
VR018    CLI   SYSTEM,0                                                         
         BE    VR016                                                            
         CLC   SYSTEM,SASYSNUM                                                  
         BNE   VR016                                                            
         MVC   SVSYSALL,SASYSALL                                                
         B     VR014                                                            
*                                                                               
         USING SACLAD,R3                                                        
VR019    CLI   SYSTEM,0                                                         
         BE    VR016                                                            
         CLC   SYSTEM,SACLASYS                                                  
         BNE   VR016                                                            
         B     VR014                                                            
         DROP  R3                                                               
*                                                                               
VR020    OC    DATAGRP,DATAGRP     ANY DATA GROUP FOR PERSON?                   
         BNZ   VR040               . YES                                        
*&&US                                                                           
*----------------------------------                                             
* CLIENT LIST LIMIT ACCESS DEFAULT                                              
* PRE-VALIDATE LIMIT ACCESS FIELD                                               
* TO EXTRACT ANY LIST OF CLIENT CODES                                           
*----------------------------------                                             
         XC    CLSTACC,CLSTACC                                                  
         CLI   SYSTEM,X'02'        SPOT                                         
         BE    VR030                                                            
         CLI   SYSTEM,X'0D'        SPOT TRAFFIC                                 
         BE    VR030                                                            
         CLI   SYSTEM,X'03'        NET                                          
         BE    VR030                                                            
         CLI   SYSTEM,X'04'        PRINT                                        
         BE    VR030                                                            
         B     VR040                                                            
*                                                                               
VR030    BRAS  RE,VALCLA                                                        
         BNE   VALRECX             EXIT ERROR                                   
*&&                                                                             
VR040    XC    APELEM,APELEM       INITIALISE SYSTEM ELEMENT                    
         LA    R3,APELEM                                                        
         USING SASYSD,R3                                                        
         MVI   SASYSEL,SASYSELQ                                                 
         MVI   SASYSLN,SASYSLNQ                                                 
         MVC   SASYSNUM,SYSTEM                                                  
         MVC   SASYSALL,XAUTH      PRESET ALL & PROGRAM VALUES                  
         MVC   SASYSPGM,XAUTH                                                   
         OI    SASYSEL+5,X'80'                                                  
         MVC   SASYSPGM+2(126),SASYSPGM                                         
*                                                                               
         OC    DATAGRP,DATAGRP     ANY DATA GROUP FOR PERSON?                   
         BNZ   VR100               . YES                                        
*&&UK                                                                           
         CLI   SYSTEM,X'0D'        CLEAR LIMIT ACCESS IF STRAFFIC               
         BNE   *+14                                                             
         XC    SASYSLMT,SASYSLMT                                                
         B     VR100                                                            
*&&                                                                             
         LA    R1,SYSLACCH                                                      
         GOTO1 AFVAL                                                            
         BNE   VR100                                                            
         CLI   SYSTEM,0            SYSTEM MUST BE SPECIFIED                     
         BE    SAEIIF                                                           
*&&US                                                                           
         OC    CLSTACC,CLSTACC     TEST CLIENT LIST LACC DEFAULT                
         BZ    VR050                                                            
         MVC   SASYSLMT,CLSTACC                                                 
         B     VR100                                                            
*&&                                                                             
VR050    GOTO1 AVALLACC,APPARM,(SYSTEM,SASYSLMT)                                
         BNE   VALRECX             EXIT ERROR                                   
*                                                                               
*----------------------------------                                             
* VALIDATE PROGRAM ACCESS CODE TABLE                                            
*----------------------------------                                             
VR100    GOTO1 AFVAL,SYSALLH       READ DEFAULT ACCESS CODE                     
         BE    VR110                                                            
         MVC   SASYSALL,NAUTH                                                   
         B     VR130                                                            
*                                  TEST FOR DELETE CURRENT SYSTEM               
VR110    CLI   FVILEN,1                                                         
         BNE   VR120                                                            
         CLC   FVIFLD(1),CT@DEL                                                 
         BNE   VR120                                                            
         CLI   APACTN,ACTADD       INVALID FOR ADD ACTION                       
         BE    SAEIIF                                                           
         B     VR300               IF OK DO NOT REPLACE ELEMENT                 
*                                  VALIDATE DEFAULT ACCESS CODE                 
VR120    BAS   RE,VALPAVAL           FOR ALL PROGRAMS                           
         BNE   EXIT                                                             
         MVC   SASYSALL,PACCVAL                                                 
*                                                                               
VR130    BAS   RE,GETASF           GET AGENCY SYSTEM SECURITY FLAGS             
         BAS   RE,VALPACC          VALIDATE ACCESS CODES                        
         BNE   EXIT                  FOR EACH PROGRAM IN TABLE                  
*                                                                               
VR200    GOTO1 AADDELS,SA0REC      REPLACE UPDATED SYSTEM EL                    
         B     VR300                                                            
         DROP  R3                                                               
*                                                                               
VR300    GOTO1 ASETACT,SA0REC                                                   
*                                  UPDATE PASSWORD CODE RECORD                  
VRUPDCOD MVC   IOKEY(L'SA0KEY),SA0KEY                                           
         CLI   PIDREQD,C'Y'        TEST PERSONAL ID PASSWORD                    
         BE    VRUPD010                                                         
         MVC   APELEM(2),=X'0304'  BUILD POINTER ELEMENTS                       
         MVC   APELEM+2(L'PWDNUM),PWDNUM                                        
         GOTO1 AADDELS,SA0REC                                                   
*                                                                               
VRUPD010 CLI   APACTN,ACTCHA                                                    
         BE    VRCHACOD                                                         
*                                  ADD RECORD                                   
VRADDCOD GOTO1 AIO,IOADD+IOCONFIL+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         B     VRUPDNUM                                                         
VRCHACOD L     R2,AIOAREA2         CHANGE RECORD                                
         GOTO1 AIO,IORDUPD+IOCONFIL+IO2                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   PIDREQD,C'Y'        TEST PERSONAL ID PASSWORD                    
         BE    VALRECOK                                                         
         B     VRUPDNUM                                                         
*                                  UPDATE PASSWORD NUMBER RECORD                
VRUPDNUM XC    SA0KEYS,SA0KEYS                                                  
         MVC   SA0KNUM,PWDNUM                                                   
         MVC   IOKEY(L'SA0KEY),SA0KEY                                           
         MVI   APELEM,SAPEFELQ     REMOVE EFFECTIVE DATES ELEMENT               
         MVI   APELEM+1,0                                                       
         GOTO1 ADELELS,SA0REC                                                   
         MVI   APELEM,X'03'        REMOVE OLD ELEMENT                           
         MVI   APELEM+1,0                                                       
         GOTO1 ADELELS,SA0REC                                                   
         MVC   APELEM(2),=X'030C'  BUILD NEW POINTER ELEMENT                    
         MVC   APELEM+2(L'PWDCODE),PWDCODE                                      
         GOTO1 AADDELS,SA0REC                                                   
         CLI   APACTN,ACTCHA                                                    
         BE    VRCHANUM                                                         
*                                  ADD RECORD                                   
VRADDNUM GOTO1 AIO,IOADD+IOCONFIL+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         B     VALRECOK                                                         
*                                                                               
VRCHANUM L     R2,AIOAREA2         CHANGE RECORD                                
         GOTO1 AIO,IORDUPD+IOCONFIL+IO2                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIOAREA1                                                      
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     VALRECOK                                                         
*                                                                               
VALRECOK MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     DISREC                                                           
*                                                                               
VALRECX  B     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF PASSWORD RECORD                           *         
***********************************************************************         
         USING SAPEREC,R1                                                       
DISKEY   LA    R1,APRECKEY         POINT TO PERSON RECORD KEY SAVE              
         MVC   SYSPID(L'SAPEPID),SAPEPID  DISPLAY ID                            
         OI    SYSPIDH+(FVOIND-FVIHDR),FVOXMT                                   
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         CLI   SYSTEM,0                                                         
         BE    DISKEYX                                                          
         GOTO1 ADISSYS,SYSTEM      DISPLAY SYSTEM                               
         MVC   SYSSYS,APWORK                                                    
         OI    SYSSYSH+(FVOIND-FVIHDR),FVOXMT                                   
*                                                                               
DISKEYX  B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY PASSWORD RECORD                                  *         
***********************************************************************         
DISREC   L     R2,AIOAREA1                                                      
*                                                                               
         TWAXC SYSLACCH                                                         
         TWAXC SYSPN01H,PROT=Y                                                  
*                                                                               
         BAS   RE,REENTER          HANDLE DELETE/RESTORE PROMPT                 
*&&US*&& BAS   RE,DISDHD           DISPLAY LIMIT ACCESS/DATA GROUP              
         BAS   RE,DISSHD           DISPLAY SYSTEMS SUMMARY HEADER               
*                                                                               
         CLI   SYSTEM,0                                                         
         BE    DISRECX                                                          
*                                                                               
         GOTO1 ADISSYS,SYSTEM      REDISPLAY SYSTEM                             
         MVC   SYSSYS,APWORK                                                    
         OI    SYSSYSH+(FVOIND-FVIHDR),FVOXMT                                   
         LA    R3,SA0DATA          GET PASSWORD RECORD ELEMENT DATA             
         XC    ASYSEL,ASYSEL                                                    
*                                                                               
DR020    CLI   0(R3),0             TEST END OF RECORD                           
         BE    DR100                                                            
         CLI   0(R3),SASYSELQ      TEST SYSTEM ELEMENT                          
         BNE   DR040                                                            
         USING SASYSD,R3                                                        
         CLC   SASYSNUM,SYSTEM                                                  
         BNE   DR040                                                            
         ST    R3,ASYSEL                                                        
         B     DR100                                                            
*                                                                               
DR040    ZIC   RF,1(R3)            GET NEXT ELEMENT                             
         AR    R3,RF                                                            
         B     DR020                                                            
*                                  PROCESS SYSTEM ELEMENT                       
DR100    BAS   RE,GETASF           GET AGENCY SYSTEM SECURITY FLAGS             
         BAS   RE,INITPGML         INITIALISE PROGRAM ACCESS CODE LIST          
         OC    ASYSEL,ASYSEL                                                    
         BZ    DISRECX                                                          
*                                  PROCESS SYSTEM ELEMENT                       
         OC    DATAGRP,DATAGRP     ANY DATA GROUP FOR PERSON?                   
         BNZ   DR110               . YES                                        
*&&US                                                                           
         XC    CLSTACC,CLSTACC     CLIENT LIST LIMIT ACCESS SET                 
         CLI   SYSTEM,X'07'                                                     
         BE    DR110                                                            
         CLI   SYSTEM,X'08'                                                     
         BE    DR110                                                            
         CLI   SYSTEM,X'0A'                                                     
         BE    DR110                                                            
         CLI   SYSTEM,X'0C'                                                     
         BE    DR110                                                            
         BRAS  RE,DISCLA                                                        
*                                                                               
         OC    CLSTACC,CLSTACC     CLIENT LIST LIMIT ACCESS SET                 
         BNZ   DR110                                                            
*&&                                                                             
         GOTO1 ADISLACC,APPARM,(SASYSNUM,SASYSLMT)                              
         MVC   SYSLACC,APWORK                                                   
*                                                                               
DR110    MVC   PACCVAL,SASYSALL    DISPLAY VALUE                                
         BAS   RE,DISPAVAL                                                      
         MVC   SYSALL,APWORK                                                    
         BAS   RE,DISPACC          DISPLAY PROGRAM ACCESS CODES                 
*                                                                               
DISRECX  MVC   SAVSYS,SYSTEM       SAVE LAST DISPLAYED SYSTEM                   
         B     EXIT                                                             
*                                                                               
REENTER  CLI   APACTN,ACTDEL       HANDLE DELETE REENTER PROMPT                 
         BE    *+12                                                             
         CLI   APACTN,ACTRES       HANDLE RESTORE REENTER PROMPT                
         BNE   *+8                                                              
         OI    ACSSRVH+FHOID,FHOITR+FHOIMO AVOID NO DATA ENTERED MSG            
         BR    RE                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY SYSTEMS SUMMARY HEADER                           *         
***********************************************************************         
         SPACE 1                                                                
         USING SA0REC,R2                                                        
DISSHD   NTR1                                                                   
         MVI   SYSNAMS,C' '                                                     
         MVC   SYSNAMS+1(L'SYSNAMS-1),SYSNAMS                                   
*                                                                               
         MVC   APHALF,=Y(CS#SYSAS)                                              
         GOTO1 ADISTXT,APPARM,APHALF                                            
         MVC   SYSNAMS(16),APWORK                                               
*                                                                               
         LA    RE,SYSNAMS+18       SET A(NEXT ENTRY)                            
         ST    RE,ASYSNAMS                                                      
         LA    R1,SYSNAMS                                                       
         SR    RE,R1                                                            
         STC   RE,SYSNAMSL         SET LIST LENGTH                              
         LA    R3,SA0DATA          GET PASSWORD RECORD ELEMENT DATA             
*                                                                               
DSHD10   CLI   0(R3),0             TEST END OF RECORD                           
         BE    DSHD100                                                          
         CLI   0(R3),SASYSELQ      TEST SYSTEM ELEMENT                          
         BE    DSHD30                                                           
DSHD20   ZIC   RF,1(R3)            GET NEXT ELEMENT                             
         AR    R3,RF                                                            
         B     DSHD10                                                           
*                                                                               
         USING SASYSD,R3                                                        
DSHD30   L     R4,ASYS             SEARCH SE LIST FOR SE NUM SESYSNUM           
         L     R4,VSELIST-SYSFACD(R4)                                           
         LH    RE,0(R4)                                                         
         L     RF,2(R4)                                                         
         LA    R4,6(R4)            R3=A(SELIST ENTRY)                           
         USING SELISTD,R4                                                       
         CLC   SASYSNUM,SEOVSYS                                                 
         BE    DSHD40                                                           
         BXLE  R4,RE,*-10                                                       
         LA    R4,=CL7'XXX    '    SET UNKNOWN SYSTEM NAME                      
*                                                                               
DSHD40   L     RE,ASYSNAMS         MOVE NAME TO LIST                            
         SR    R1,R1                                                            
         ICM   R1,1,SYCNT          TEST COUNT OF ITEMS IN LIST                  
         BZ    *+12                                                             
         MVI   0(RE),C'/'                                                       
         LA    RE,1(RE)                                                         
         LA    R1,1(R1)                                                         
         STC   R1,SYCNT            BUMP ITEM COUNT                              
         LA    RF,SYSNAMS+L'SYSNAMS-3                                           
         CR    RE,RF                                                            
         BH    DSHD20                                                           
         MVC   0(3,RE),SENAME      EXTRACT SE NAME                              
         NC    1(2,RE),=8X'BF'     SET TO LOWER CASE                            
         LA    RE,3(RE)                                                         
         ST    RE,ASYSNAMS                                                      
         LA    R1,SYSNAMS                                                       
         SR    RE,R1                                                            
         STC   RE,SYSNAMSL         SET LIST LENGTH                              
         B     DSHD20                                                           
         DROP  R4                                                               
*                                                                               
DSHD100  MVI   SYSSHD,C'-'         DISPLAY LIST OF SYSTEMS                      
         MVC   SYSSHD+1(L'SYSSHD-1),SYSSHD                                      
         SR    RF,RF               DISPLAY LIST OF SYSTEMS                      
         ICM   RF,1,SYSNAMSL       RF=L'SYSTEM NAMES LIST                       
         BZ    DSHD110                                                          
         LA    R1,L'SYSSHD                                                      
         SR    R1,RF                                                            
         BNP   DSHD110                                                          
         SRL   R1,1                                                             
         LA    RE,SYSSHD(R1)                                                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),SYSNAMS                                                  
DSHD110  OI    SYSSHDH+6,X'80'                                                  
*                                                                               
DSHDX    B     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO DETERMINE DATA ACCESS SETUP                                        
***********************************************************************         
         USING SA0REC,R2                                                        
DISDHD   NTR1                                                                   
*                                                                               
         NI    SYSLAHH+FHATD,X'FF'-FHATLO                                       
         OI    SYSLAHH+FHOID,FHOITR                                             
         MVC   SYSLACC,SPACES                                                   
         NI    SYSLACCH+FHATD,X'FF'-FHATPR                                      
         OI    SYSLACCH+FHOID,FHOITR                                            
         MVC   SYSLAC2,SPACES                                                   
         NI    SYSLAC2H+FHATD,X'FF'-FHATPR                                      
         OI    SYSLAC2H+FHOID,FHOITR                                            
         MVC   SYSLAC3,SPACES                                                   
         NI    SYSLAC3H+FHATD,X'FF'-FHATPR                                      
         OI    SYSLAC3H+FHOID,FHOITR                                            
*                                                                               
         CLI   SYSTEM,X'07'        TALENT SYSTEM                                
         BE    DSD010                                                           
         CLI   SYSTEM,X'08'        REP SYSTEM                                   
         BE    DSD010                                                           
         CLI   SYSTEM,X'0A'        CONTROL SYSTEM                               
         BE    DSD010                                                           
         CLI   SYSTEM,X'0C'        DEMO SYSTEM                                  
         BE    DSD010                                                           
         B     DSD020                                                           
DSD010   OI    SYSLACCH+FHATD,FHATPR                                            
         OI    SYSLAC2H+FHATD,FHATPR                                            
         OI    SYSLAC3H+FHATD,FHATPR                                            
         B     DSDHDX                                                           
*                                                                               
DSD020   OC    DATAGRP,DATAGRP     ANY DATA GROUP FOR PERSON?                   
         BZ    DSDHDX              . NO, LEAVE LIMITED ACCESS                   
*                                                                               
         NI    SYSLAHH+FHATD,X'FF'-FHATLO                                       
         OI    SYSLACCH+FHATD,FHATPR                                            
         OI    SYSLAC2H+FHATD,FHATPR                                            
         OI    SYSLAC3H+FHATD,FHATPR                                            
*                                                                               
         LA    R2,IOKEY                                                         
         USING SALAREC,R2                                                       
         XC    SALAKEY,SALAKEY     CHECK DATA GROUP RECORD EXISTS               
         MVI   SALATYP,SALATYPQ                                                 
         MVI   SALASUB,SALASUBQ                                                 
         MVC   SALAAGY,AGENCYID                                                 
         MVC   SALAAGN,DATAGRP                                                  
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BL    SAEIIO                                                           
         BH    SAERNF                                                           
         L     R2,AIOAREA2                                                      
*                                                                               
         GOTO1 AGETLNAM,SALAREC                                                 
         MVC   SYSLACC(12),=CL12'Data Group:'                                   
         MVC   SYSLACC+12(L'SALANCOD),APWORK                                    
         MVI   SYSLACC+12+L'SALANCOD+1,C'-'                                     
         MVC   SYSLACC+12+L'SALANCOD+3(L'SALANNAM),APWORK+L'SALANCOD            
*                                                                               
         MVI   APBYTE,C' '                                                      
         XC    APELEM,APELEM                                                    
         MVI   APELEM,SALASELQ     FIND SYSTEM ELEMENTS                         
         GOTO1 AGETELS,SALAREC                                                  
         ICM   R3,15,APPARM        APPARM =A(ELEMENT) IF FOUND                  
         BZ    DSD060                                                           
*                                                                               
         USING SALASD,R3                                                        
DSD030   CLI   0(R3),0                                                          
         BE    DSD060                                                           
         CLI   0(R3),SALASELQ                                                   
         BNE   DSD060                                                           
         CLC   SALASNUM,SYSTEM                                                  
         BE    DSD038                                                           
         LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DSD030                                                           
*                                                                               
DSD038   LA    R4,APELEM                                                        
         USING LISTIOD,R4                                                       
         XC    LISTIOB(LISTIOBL),LISTIOB                                        
         MVC   LISTACOM,ACOM                                                    
         MVC   LISTIAGY,CUAALF                                                  
         MVC   LISTIGRP,DATAGRP                                                 
         MVC   LISTISYS,SYSTEM                                                  
DSD040   MVI   LISTACTN,LISTANLG         NEXT LIST FOR THIS GROUP               
         GOTO1 =V(LISTIO),LISTIOB,RR=APRELO                                     
         BNE   DSD060                                                           
         CLC   LISTISYS,SYSTEM                                                  
         BNE   DSD060                                                           
*                                                                               
         MVI   LISTACTN,LISTATYP         GET LIST TYPE                          
         GOTO1 =V(LISTIO),LISTIOB,RR=APRELO                                     
         BNE   DSD040                                                           
         MVC   APWORK(L'LISTITNM),LISTITNM                                      
*                                                                               
         XC    LISTDVAL,LISTDVAL         FIRST ITEM IN LIST                     
         MVI   LISTACTN,LISTANXT         ANY DATA IN LIST                       
         GOTO1 =V(LISTIO),LISTIOB,RR=APRELO                                     
         BNE   DSD040                                                           
         MVI   APBYTE,C'D'                                                      
         MVC   SYSLAC2(12),=CL12'Data Type :'                                   
         MVC   SYSLAC2+12(L'SALANCOD),APWORK                                    
         B     DSD040                                                           
         DROP  R4                                                               
*                                                                               
DSD060   CLI   APBYTE,C'D'                                                      
         BE    DSDHDX                                                           
         MVC   SYSLAC2,SPACES                                                   
         MVC   SYSLAC2(12),=CL12'Data Type :'                                   
         MVC   SYSLAC2+12(12),=CL12'<Company ID>'                               
*                                                                               
DSDHDX   B     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO HANDLE FIRST FOR SCREEN LIST (SET SCREEN TO MODIFIED)              
***********************************************************************         
FSTLST   OI    ACSSRVH+FHOID,FHOIMO                                             
*                                  CHECK PASSWORD FIELD SECURITY                
         MVI   PWDAFLAG,0                                                       
         OC    ACASEC,ACASEC                                                    
         BZ    FSTLSTX                                                          
         CLI   ASONOFF,ASOFF                                                    
         BE    FSTLSTX                                                          
         MVI   APBYTE,FLDPWD                                                    
         LA    RF,APBYTE                                                        
         GOTO1 VSECRET,SCPARM,('SECPFLDP',ACASEC),(RF)                          
         BL    FLST010                                                          
         BH    FLST020                                                          
         B     FSTLSTX             FULL READ/WRITE ACCESS                       
*                                  NO ACCESS                                    
FLST010  OI    LSTPWHH+(FVATRB-FVIHDR),X'FF'                                    
         OI    LSTPWDH+(FVATRB-FVIHDR),X'FF'                                    
         XC    LSTSH1+8(11),LSTSH1+8                                            
         XC    LSTSH2+8(11),LSTSH2+8                                            
         OI    LSTSH1H+(FVOIND-FVIHDR),FVOXMT                                   
         OI    LSTSH2H+(FVOIND-FVIHDR),FVOXMT                                   
         MVI   PWDAFLAG,X'FF'                                                   
         B     FSTLSTX                                                          
*                                  READ ACCESS ONLY                             
FLST020  OI    LSTPWDH+(FVATRB-FVIHDR),FVAPROT                                  
         OI    LSTPWDH+(FVOIND-FVIHDR),FVOXMT                                   
         B     FSTLSTX                                                          
*                                                                               
FSTLSTX  B     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS                                         
***********************************************************************         
         USING SAPEREC,R2                                                       
VALSEL   MVI   PIDREQD,C'N'                                                     
         GOTO1 AGETAAD,AGENCYID                                                 
         TM    APWORK,CTAADPRQ     PERSONAL ID REQUIRED WITH PASSWORD?          
         BZ    *+8                                                              
         MVI   PIDREQD,C'Y'                                                     
         MVC   PWDTOUT,APWORK+1    PASSWORD TIMEOUT DAYS                        
         MVC   PWDMINLN,APWORK+2   PASSWORD MINIMUM LENGTH                      
*                                                                               
         LA    R2,APRECKEY         BUILD FIRST PERSON RECORD KEY                
         MVI   GETSEQF,0                                                        
         OI    ACSSRVH+FHOID,FHOIMO                                             
         XC    SAPEKEY,SAPEKEY                                                  
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,AGENCYID                                                 
         MVC   SAVPARM(32),APPARM                                               
         GOTO1 VDATCON,APPARM,(X'05',0),(X'02',TODAY)                           
         MVC   TODAYC,FFILL        GET TODAYS DATE AND COMPLEMENT               
         XC    TODAYC,TODAY                                                     
         MVC   SAPEDEF,TODAYC                                                   
         XC    SAVPID,SAVPID                                                    
         XC    SELOPT(SELOPTL),SELOPT                                           
*                                                                               
VSPID    GOTO1 AFVAL,LSTPIDH       VALIDATE PERSON ID FILTER                    
         BNE   VSPIDX                                                           
         ZIC   R1,FVILEN                                                        
         L     RE,=F'-1'                                                        
         LA    RF,FVIFLD                                                        
VSPID1   CLI   0(RF),C'A'          FIND LENGTH TO 1ST SP CHAR                   
         BL    VSPID2              FOR KEY COMPARE IN GETSEL                    
         LA    RE,1(,RE)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R1,VSPID1                                                        
VSPID2   STC   RE,SELKEYCL                                                      
         MVC   SELPID,FVIFLD                                                    
         MVC   SELPIDL,FVILEN                                                   
         MVC   SELPIDSP,0(RF)                                                   
VSPIDX   EQU   *                                                                
*                                                                               
         MVC   SAPEPID,FVIFLD                                                   
VS002    GOTO1 AFVAL,LSTUSRH       GET USER ID FILTER                           
         BNE   VS010                                                            
         GOTO1 AVALUID,LSTUSRH                                                  
         BNE   VALSELX                                                          
         MVC   SELUSR,FVIFLD                                                    
*                                                                               
VS010    GOTO1 AFVAL,LSTPWDH       GET PASSWORD FILTER                          
         BNE   VS020                                                            
         CLI   PWDAFLAG,0          PWD FILTER ALLOWED?                          
         BNE   *+12                NO                                           
         CLI   PIDREQD,C'Y'        TEST PESONAL ID PASSWORD                     
         BNE   VS015               NO - OKAY FOR PWD FILTER                     
*                                                                               
         XC    LSTPWD,LSTPWD       CLEAR THE FILTER                             
         B     VS020                                                            
*                                                                               
VS015    MVC   SELPWD,FVIFLD                                                    
         MVC   SELPWDL,FVILEN                                                   
*                                                                               
VS020    GOTO1 AFVAL,LSTDEFH       GET EFFECTIVE DATE FILTER                    
         BNE   VS030                                                            
         MVC   SELDEFC,FFILL                                                    
         ZIC   R0,FVILEN                                                        
         MVC   APBYTE,CULANG                                                    
         OI    APBYTE,PVINSGLO+PVINSGLS                                         
         GOTO1 VPERVAL,APPARM,((R0),FVIFLD),(APBYTE,APWORK)                     
         CLI   4(R1),PVRCONE                                                    
         BE    VS026                                                            
         SR    RE,RE                                                            
         ZIC   RF,FVXLEN                                                        
         EX    RF,FLDALL           'ALL' KEYWORD                                
         BNE   SAEIIF                                                           
         LA    R1,LSTDEFH                                                       
         BAS   RE,ECHOALL                                                       
         XC    SELDEFC,FFILL                                                    
         B     VS028                                                            
VS026    XC    SELDEFC,APWORK+PVALCSTA-PERVALD                                  
VS028    MVC   SAPEDEF,SELDEFC                                                  
         MVI   SELDEFF,1           FLAG DATE FILTER SET                         
*                                                                               
VS030    GOTO1 AFVAL,LSTAGRH       VALIDATE ACCESS GROUP FILTER                 
         BNE   VS040                                                            
         LA    R2,IOKEY                                                         
         USING SAAGREC,R2                                                       
         XC    SAAGKEY,SAAGKEY     CHECK ACCESS GROUP RECORD EXISTS             
         MVI   SAAGTYP,SAAGTYPQ                                                 
         MVI   SAAGSUB,SAAGSUBQ                                                 
         MVC   SAAGAGY,AGENCYID                                                 
         MVC   SAAGAGR,FVIFLD                                                   
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BL    SAEIIO                                                           
         BH    SAERNF                                                           
         MVC   SELAGR,FVIFLD                                                    
*                                                                               
VS040    GOTO1 AFVAL,LSTOFFH       VALIDATE OFFICE CODE FILTER                  
         BNE   VS050                                                            
         LA    R2,IOKEY                                                         
         USING SAOFREC,R2                                                       
         XC    SAOFKEY,SAOFKEY     CHECK OFFICE RECORD EXISTS                   
         MVI   SAOFTYP,SAOFTYPQ                                                 
         MVI   SAOFSUB,SAOFSUBQ                                                 
         MVC   SAOFAGY,AGENCYID                                                 
         MVC   SAOFOID,FVIFLD                                                   
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BL    SAEIIO                                                           
         BH    SAERNF                                                           
         MVC   SELOFF,FVIFLD                                                    
*                                                                               
VS050    GOTO1 AFVAL,LSTDIDH       VALIDATE DEPARTMENT CODE FILTER              
         BNE   VS060                                                            
         OC    SELOFF,SELOFF                                                    
         BZ    SAEODF                                                           
         LA    R2,IOKEY                                                         
         USING SADPREC,R2                                                       
         XC    SADPKEY,SADPKEY     CHECK DEPARTMENT RECORD EXISTS               
         MVI   SADPTYP,SADPTYPQ                                                 
         MVI   SADPSUB,SADPSUBQ                                                 
         MVC   SADPAGY,AGENCYID                                                 
         MVC   SADPOID,SELOFF                                                   
         MVC   SADPDID,FVIFLD                                                   
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BL    SAEIIO                                                           
         BH    SAERNF                                                           
         MVC   SELDID,FVIFLD                                                    
*                                                                               
VS060    GOTO1 AFVAL,LSTSYSH       VALIDATE SYSTEM NAME FILTER                  
         BNE   VS070                                                            
         GOTO1 AVALSYS,LSTSYSH     VALIDATE SYSTEM                              
         BNE   NO                                                               
         MVC   SELSYS,APWORK                                                    
*                                                                               
VS070    GOTO1 AFVAL,LSTPGMH       VALIDATE PROGRAM NAME FILTER                 
         BNE   VS080                                                            
         OC    SELSYS,SELSYS                                                    
         BE    SAEPFS                                                           
         GOTO1 AVALPGM,APPARM,(SELSYS,LSTPGMH)  VALIDATE PROGRAM                
         BNE   SAEPGM                                                           
         MVC   SELPGM,APWORK                                                    
*                                                                               
VS080    GOTO1 AFVAL,LSTAPCH       VALIDATE APPROVER GROUP CODE FILTER          
         BNE   VS090                                                            
         LA    R2,IOKEY                                                         
         USING SAAPREC,R2                                                       
         XC    SAAPKEY,SAAPKEY     CHECK APPROVER GROUP RDCORD EXISTS           
         MVI   SAAPTYP,SAAPTYPQ                                                 
         MVI   SAAPSUB,SAAPSUBQ                                                 
         MVC   SAAPAGY,AGENCYID                                                 
         MVC   SAAPAGR,FVIFLD                                                   
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BL    SAEIIO                                                           
         BH    SAERNF                                                           
         MVC   SELAPC,FVIFLD                                                    
*                                                                               
VS090    EQU   *                                                                
*                                                                               
VALSELY  MVC   APPARM(32),SAVPARM                                               
         LA    R0,LSTACT1H         SET ADDRESS OF FIRST LIST LINE               
         ST    R0,APPARM+0                                                      
         LA    R1,LSTACT2H         SET LIST LINE LENGTH                         
         SR    R1,R0                                                            
         STH   R1,APPARM+6                                                      
         MVI   APPARM+4,(LSTFOOTH-LSTACT1H)/(LSTACT2H-LSTACT1H)                 
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST/SELECT RECORD                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING SAPEREC,R2                                                       
GETSEL   LA    R2,IOKEY            READ NEXT RECORD FROM                        
         MVC   SAPEKEY,APRECKEY      LAST KEY SAVE                              
         TM    GETSEQF,APILRERD    TEST GETSEL SEQUENCE BROKEN                  
         BZ    GS002                                                            
         NI    GETSEQF,X'FF'-APILRERD                                           
         B     GS004                                                            
GS002    TM    APINDS,APILRERD     TEST LIST READ SEQUENCE BROKEN               
         BZ    GS010                                                            
         NI    APINDS,X'FF'-APILRERD                                            
GS004    GOTO1 AIO,IOCONFIL+IORD+IO1                                            
         BE    GS020                                                            
         B     GSEND                                                            
GS010    TM    APINDS,APILNSEQ     TEST FIRST LINE IN LIST SEQUENCE             
         BNZ   GS020                                                            
         OI    APINDS,APILNSEQ                                                  
         LA    R1,IOCONFIL+IOHI+IO1                                             
         B     *+8                                                              
GS020    LA    R1,IOCONFIL+IOSQ+IO1  ELSE NEXT LIST LINE                        
         GOTO1 AIO                                                              
         BNE   GSEND                                                            
*                                                                               
         GOTO1 VGETFACT,APPARM,0   GET A(SYSTEM INFO BLOCK)                     
         L     R1,APPARM                                                        
         USING FACTSD,R1                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,3,FATMAXIO       MAXIMUM ALLOWABLE IOS                        
         MH    RF,=H'9'                                                         
         D     RE,=F'10'           90 PERCENT OF MAX IOS IN RF                  
         CLM   RF,3,FATIOCNT       TEST RUNNING OUT OF IOS                      
         BH    GS021                                                            
         DROP  R1                                                               
         MVC   FVMSGNO,=AL2(GI$IOLIM)                                           
         MVI   FVOSYS,GTGENSYS     GENERAL MESSAGE                              
         MVI   FVOMTYP,C'I'                                                     
         OI    ACLSMIND,ACLSMIMP   END OF LIST MESSAGE PROVIDED                 
         L     R1,ALSM                                                          
         USING LSMD,R1                                                          
         OI    LSMINDS,LSMIEOS     END OF SCREEN                                
         DROP  R1                                                               
         B     GETSELX                                                          
*                                                                               
GS021    L     R2,AIOAREA1                                                      
         MVC   APRECKEY(L'SAPEKEY),SAPEKEY  SAVE LAST KEY READ                  
*                                  CHECK PERSON RECORD TYPE                     
         CLI   SAPETYP,SAPETYPQ                                                 
         BNE   GSEND                 IF NOT EXIT                                
         CLI   SAPESUB,SAPESUBQ                                                 
         BNE   GSEND                                                            
         CLC   SAPEAGY,AGENCYID                                                 
         BNE   GSEND                                                            
         CLI   SELDEFF,0           FILTER ON EFFECTIVE DATE IN KEY              
         BZ    GS022                                                            
         CLC   SAPEDEF,SELDEFC                                                  
         BL    GS020                                                            
         B     GS026                                                            
*                                                                               
GS022    CLC   SAPEDEF,TODAYC      CHECK CURRENT EFFECTIVE DATE                 
         BL    GS020                                                            
         CLC   SAPEPID,SAVPID                                                   
         BE    GS020               GET NEXT PERSON ID                           
         MVC   SAVPID,SAPEPID                                                   
         MVI   CURRPID,C'Y'                                                     
         MVI   EXPFLAG,C'N'        INITIALISE EXPIRED FLAG                      
         B     GSPID                                                            
*                                                                               
GS026    MVI   CURRPID,C'N'        SET CUREENT PERSONAL ID FLAG                 
         CLC   SAPEDEF,TODAYC      CHECK CURRENT EFFECTIVE DATE                 
         BL    GSPID                                                            
         CLC   SAPEPID,SAVPID                                                   
         BE    GSPID                                                            
         MVC   SAVPID,SAPEPID                                                   
         MVI   CURRPID,C'Y'                                                     
         MVI   EXPFLAG,C'N'        INITIALISE EXPIRED FLAG                      
         B     GSPID                                                            
*                                 FILTER ON PERSONAL ID                         
GSPID    CLI   SELPIDSP,C' '       OFFICE CODE - FILTER ONLY IF IT              
         BNH   GSPIDX                CONTAINS SPECIAL (WILD) CHARS.             
         XR    R1,R1                                                            
         ICM   R1,1,SELKEYCL                                                    
         BM    GSPID1                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SAPEPID(0),SELPID                                                
         BH    GSEND               (NO MORE RELEVENT RECORDS)                   
GSPID1   GOTO1 ATXTFLT,APPARM,(SELPIDL,SELPID),(L'SAPEPID,SAPEPID)              
         BNE   GS020               READ NEXT RECORD                             
GSPIDX   EQU   *                                                                
*                                                                               
GS030    LA    R3,SAPEDATA         FILTER ON DATA IN ELEMENTS                   
         MVI   SELAGRF,0                                                        
         MVI   SELPWDF,0                                                        
         MVI   SELAPCF,0                                                        
*                                                                               
GS032    CLI   0(R3),0             E-O-R                                        
         BE    GS036                                                            
         CLI   0(R3),SAPERELQ                                                   
         BE    GSPER                                                            
         CLI   0(R3),SAAGCELQ                                                   
         BE    GSAGC                                                            
         CLI   0(R3),SAAPCELQ                                                   
         BE    GSAPC                                                            
         CLI   0(R3),SAPWDELQ                                                   
         BE    GSPWD                                                            
GS034    SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GS032                                                            
*                                                                               
         USING SAPERD,R3                                                        
GSPER    EQU   *                   FILTER ON PERSONNEL DETAILS                  
         CLI   CURRPID,C'Y'        TEST IF CURRENT PID                          
         BNE   GSPE010                                                          
         MVI   EXPFLAG,C'N'        SET EXPIRED FLAG                             
         OC    SAPERDTE,SAPERDTE                                                
         BZ    GSPE010                                                          
         CLC   SAPERDTE,TODAY                                                   
         BNL   GSPE010                                                          
         MVI   EXPFLAG,C'Y'                                                     
*                                                                               
GSPE010  CLI   OPTTER,C'Y'         OPTION TERM=Y                                
         BE    GSPE100                                                          
         CLI   OPTTER,C'O'         OPTION TERM=O                                
         BE    GSPE020                                                          
*                                  DEFAULT TERM=N                               
         CLI   EXPFLAG,C'N'                                                     
         BE    GSPE100                                                          
         B     GETSEL                                                           
*                                                                               
GSPE020  CLI   EXPFLAG,C'Y'                                                     
         BE    GSPE100                                                          
         B     GETSEL                                                           
*                                  TEST OFFICE MANAGER ACCESS                   
GSPE100  EQU   *                                                                
*&&UK                                                                           
*                                  TEST OFFICE MANAGER ACCESS                   
         GOTO1 ATSTOMAN,SAPEROFF                                                
         BNE   GSPE104                                                          
         B     GSPE106                                                          
*&&                                                                             
*&&US                                                                           
*                                  TEST OFFICE/DEPT MANAGER ACCESS              
         GOTO1 ATSTDMAN,APPARM,SAPEROFF,SAPERDID                                
         BE    GSPE106                                                          
*&&                                                                             
*                                                                               
GSPE104  MVC   GETSEQF,APINDS      SAVE APINDS FROM ROUTS                       
         MVC   FVMSGNO,=AL2(FVFOK) CLEAR ERROR MESSAGE TO CONTINUE LIST         
         B     GETSEL                                                           
*                                                                               
GSPE106  EQU   *                                                                
         MVC   GETSEQF,APINDS      SAVE APINDS FROM ROUTS                       
         OC    SELOFF,SELOFF                                                    
         BZ    GSPE110                                                          
         CLC   SELOFF,SAPEROFF                                                  
         BNE   GETSEL                                                           
GSPE110  OC    SELDID,SELDID                                                    
         BZ    GSPE120                                                          
         CLC   SELDID,SAPERDID                                                  
         BNE   GETSEL                                                           
GSPE120  CLI   SAPERLN,SAPERLNQ                                                 
         BL    GETSEL                                                           
         B     GS034                                                            
*                                                                               
         USING SAPWDD,R3                                                        
GSPWD    CLI   SAPWDLN,SAPWDLNQ                                                 
         BL    GETSEL                                                           
         OC    SELPWD,SELPWD       FILTER ON PASSWORD CODE                      
         BZ    GSPWD010                                                         
         SR    R1,R1                                                            
         IC    R1,SELPWDL                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SELPWD(0),SAPWDCOD                                               
         BNE   GETSEL                                                           
*                                                                               
GSPWD010 MVI   SELPWDF,1           SAVE PASSWORD CODE/NUMBER                    
         MVC   SAVPWD,SAPWDCOD                                                  
         MVC   SAVPNUM,SAPWDNUM                                                 
         B     GS034                                                            
*                                                                               
         USING SAAGCD,R3                                                        
GSAGC    MVI   SELAGRF,1                                                        
         OC    SELAGR,SELAGR       FILTER ON ACCESS GROUP CODE                  
         BZ    GS034                                                            
         CLI   SAAGCLN,SAAGCLNQ                                                 
         BL    GETSEL                                                           
         CLC   SELAGR,SAAGCCOD                                                  
         BNE   GETSEL                                                           
         B     GS034                                                            
*                                                                               
         USING SAAPCD,R3                                                        
GSAPC    MVI   SELAPCF,1                                                        
         OC    SELAPC,SELAPC       FILTER ON APPROVER GROUP CODE                
         BZ    GS034                                                            
         CLI   SAAPCLN,SAAPCLNQ                                                 
         BL    GETSEL                                                           
         CLC   SELAPC,SAAPCCOD                                                  
         BNE   GETSEL                                                           
         B     GS034                                                            
*                                                                               
*                                                                               
GS036    CLI   SELAGRF,0           CHECK ACCESS GROUP FOUND                     
         BNE   *+14                                                             
         OC    SELAGR,SELAGR                                                    
         BNZ   GETSEL                                                           
         CLI   SELAPCF,0           CHECK APPROVER GROUP FOUND                   
         BNE   *+14                                                             
         OC    SELAPC,SELAPC                                                    
         BNZ   GETSEL                                                           
         CLI   SELPWDF,0           CHECK PASSWORD RECORD FOUND                  
         BE    GETSEL                                                           
         EJECT                                                                  
***********************************************************************         
* CONTINUE GETSEL, FILTER ON DATA FROM PASSWORD RECORD                *         
***********************************************************************         
         SPACE 1                                                                
GS040    LA    R2,IOKEY                                                         
         USING SA0REC,R2                                                        
         XC    SA0KEY,SA0KEY       READ PASSWORD RECORD                         
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,AGENCYID                                                 
         CLI   PIDREQD,C'Y'        TEST PERSONAL ID PASSWORD                    
         BE    *+14                                                             
         MVC   SA0KCODE,SAVPWD                                                  
         B     *+10                                                             
         MVC   SA0KNUM,SAVPNUM                                                  
         OI    GETSEQF,APILRERD    FLAG GETSEL READ SEQUENCE BROKEN             
         GOTO1 AIO,IORD+IOCONFIL+IO3                                            
         BNL   *+6                                                              
         DC    H'00'                                                            
         BH    GETSEL              IGNORE IF PASSWORD RECORD REMOVED            
         OC    SELSYS,SELSYS       CHECK FILTERS REQUIRED                       
         BNZ   *+14                                                             
         OC    SELUSR,SELUSR                                                    
         BZ    GS050                                                            
         L     R2,AIOAREA3                                                      
         LA    R3,SA0DATA                                                       
         MVI   SELSYSF,0           FLAG FOR SYSTEM FILTER                       
*                                                                               
GS042    CLI   0(R3),0             E-O-R                                        
         BE    GS046                                                            
         CLI   0(R3),SASYSELQ      FIND SYSTEM ELEMENT                          
         BE    GSSYS                                                            
GS044    SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GS042                                                            
*                                                                               
         USING SASYSD,R3                                                        
GSSYS    OC    SELSYS,SELSYS       FILTER ON SYSTEM CODE                        
         BZ    GS044                                                            
         CLC   SASYSNUM,SELSYS                                                  
         BNE   GS044                                                            
         MVI   SELSYSF,1           FLAG SYSTEM FOUND                            
*                                  FILTER PROGRAM CODE                          
         OC    SELPGM,SELPGM                                                    
         BZ    GS044                                                            
         MVC   PROGRAM,SELPGM                                                   
         ST    R3,ASYSEL                                                        
         BAS   RE,GETPAVAL         FIND PROGRAM IN SYSTEM ELEMENT               
         BNE   GSSYS010                                                         
         CLC   PACCVAL,NAUTH       IF PROGRAM NOT AUTHORISED                    
         BE    GETSEL                GET NEXT RECORD                            
         B     GS044                 ELSE OK                                    
GSSYS010 CLC   SASYSALL,NAUTH      IF PROGRAM NOT FOUND                         
         BE    GETSEL                AND DEFAULT NOT AUTH, GET NEXT             
         B     GS044                 ELSE OK                                    
*                                  FILTER PROGRAM CODE                          
GS046    OC    SELSYS,SELSYS       FILTER ON SYSTEM CODE                        
         BZ    GS048                                                            
         CLI   SELSYSF,0           CHECK SYSTEM FOUND                           
         BE    GETSEL                ELSE NEXT RECORD RESTART SEQUENCE          
*                                                                               
GS048    L     R2,AIOAREA3                                                      
         OC    SELUSR,SELUSR                                                    
         BZ    GS050                                                            
         MVC   APWORK,SELUSR                                                    
         BAS   RE,FILTUSER         FILTER USER ID COMPATIBLE                    
         BE    GS050                 OK                                         
         B     GETSEL                ELSE NEXT RECORD RESTART SEQUENCE          
*                                                                               
GS050    MVC   FVMSGNO,=AL2(FVFOK) EXIT OK                                      
         B     GETSELX                                                          
*                                                                               
GSEND    MVI   APMODE,APMEOFS      SET NO MORE RECORDS TO COME                  
*                                                                               
GETSELX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY LIST/SELECT LINE                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING SAPEREC,R2                                                       
DISSEL   L     R2,AIOAREA1                                                      
         MVC   SAVPARM(32),APPARM                                               
         L     R4,APPARM                                                        
         USING LISTD,R4            R4=A(LIST/SELECT LINE)                       
*                                                                               
         MVC   LISTPID,SAPEPID     DISPLAY PERSONAL-ID                          
         MVC   APHALF,FFILL                                                     
         XC    APHALF,SAPEDEF                                                   
         XC    APWORK,APWORK                                                    
         GOTO1 VDATCON,APPARM,(2,APHALF),(X'51',APWORK)                         
         MVC   LISTDEF,APWORK      DISPLAY EFFECTIVE DATE                       
         CLI   EXPFLAG,C'Y'                                                     
         BNE   *+8                                                              
         MVI   LISTTERM,C'*'                                                    
         MVI   APFLAG,0                                                         
         LA    R3,SAPEDATA         GET ELEMENT DATA                             
DSLP1    CLI   0(R3),0             E-O-R                                        
         BE    DS300                                                            
         CLI   0(R3),SAPWDELQ                                                   
         BE    DSPWD                                                            
*                                                                               
DSLP1A   SR    R0,R0               BUMP TO NEXT ELEMENT                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DSLP1                                                            
*                                  PASSWORD POINTER ELEMENT                     
         USING SAPWDD,R3                                                        
DSPWD    CLI   SAPWDLN,SAPWDLNQ    DISPLAY PASSWORD DETAILS                     
         BL    DSLP1A                                                           
*                                                                               
         CLI   PWDAFLAG,0                                                       
         BNE   *+18                                                             
         CLI   PIDREQD,C'Y'        TEST PESONAL ID PASSWORD                     
         BE    *+10                                                             
         MVC   LISTPWD,SAPWDCOD                                                 
*                                                                               
DS200    LA    R2,IOKEY                                                         
         USING SA0REC,R2                                                        
         XC    SA0KEY,SA0KEY       READ PASSWORD RECORD                         
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,AGENCYID                                                 
         CLI   PIDREQD,C'Y'        TEST PESONAL ID PASSWORD                     
         BE    *+14                                                             
         MVC   SA0KCODE,SAPWDCOD                                                
         B     *+10                                                             
         MVC   SA0KNUM,SAPWDNUM                                                 
         GOTO1 AIO,IORD+IOCONFIL+IO3                                            
         BE    *+6                                                              
         DC    H'00'                                                            
         OI    APINDS,APILRERD     FLAG LIST READ SEQUENCE BROKEN               
         MVI   APFLAG,1            FLAG PASSWORD RECORD FOUND                   
         B     DSLP1A                                                           
*                                                                               
DS300    CLI   APFLAG,0            DISPLAY PASSWORD RECORD DATA                 
         BE    DISSELX               IF FOUND OK                                
         L     R2,AIOAREA3                                                      
         MVI   APBYTE,C'L'         GET LONG SYSTEM NAMES FIRST                  
DS340    LA    R8,WORK                                                          
         MVC   WORK,SPACES                                                      
         LA    R3,SA0DATA          GET SYSTEM ELEMENTS                          
DS350    CLI   0(R3),0             TEST END OF RECORD                           
         BE    DS500                                                            
         CLI   0(R3),SASYSELQ                                                   
         BE    DS370                                                            
         CLI   0(R3),SAPEFELQ                                                   
         BE    DSPEF                                                            
DS360    ZIC   R0,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R0                                                            
         B     DS350                                                            
*                                  SYSTEM ELEMENT                               
         USING SASYSD,R3                                                        
DS370    GOTO1 ADISSYS,SASYSNUM                                                 
         CLI   APBYTE,C'S'         TEST SHORT NAMES REQUIRED                    
         BNE   DS380                                                            
         L     R1,APPARM                                                        
         MVC   APWORK(7),SPACES    MOVE SHORT NAME TO APWORK                    
         MVC   APWORK(3),9(R1)                                                  
DS380    MVC   0(7,R8),APWORK      INSERT NAME                                  
         LA    R8,6(R8)                                                         
         CLI   0(R8),C' '          SQUASH OUT SPACES                            
         BNE   *+8                                                              
         BCT   R8,*-8                                                           
         LA    R8,2(R8)                                                         
         B     DS360                                                            
*                                                                               
         USING SAPEFD,R3                                                        
DSPEF    B     DS360               !! IGNORE THIS CODE FOR NOW                  
         CLC   SAPEFSTA,FFILL                                                   
         BE    DSPEF10                                                          
         GOTO1 VDATCON,APPARM,(2,SAPEFSTA),(X'51',APWORK)                       
         MVC   LISTDEF1,APWORK                                                  
DSPEF10  CLC   SAPEFEND,FFILL                                                   
         BE    DS360                                                            
         GOTO1 VDATCON,APPARM,(2,SAPEFEND),(X'51',APWORK)                       
         MVC   LISTDEF2,APWORK                                                  
         B     DS360                                                            
*                                                                               
DS500    LA    R1,WORK+L'LISTSYST  SEE IF LIST IS TOO LONG                      
         CR    R8,R1                                                            
         BH    *+14                YES SO RERUN FOR SHORT NAMES                 
         MVC   LISTSYST,WORK                                                    
         B     DISSELX                                                          
         CLI   APBYTE,C'S'                                                      
         BE    *+12                                                             
         MVI   APBYTE,C'S'                                                      
         B     DS340                                                            
         MVC   LISTSYST,WORK                                                    
         CLI   LISTSYST+L'LISTSYST-1,C' '                                       
         BE    DISSELX                                                          
         MVI   LISTSYST+L'LISTSYST-1,C'>'                                       
         B     DISSELX                                                          
*                                                                               
DISSELX  MVC   APPARM(32),SAVPARM                                               
         B     EXIT                                                             
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PROGRAM FUNCTION KEYS)    *         
***********************************************************************         
         SPACE 1                                                                
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE REPORT REQUEST SCREEN                           *         
***********************************************************************         
         SPACE 1                                                                
         USING SAPEREC,R2                                                       
VALREQ   L     R4,AREP                                                          
         USING REPD,R4             R4=A(REPORT WORK AREA)                       
         XC    APRECKEY,APRECKEY                                                
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,REPREQH       VALIDATE REQUESTOR                           
         BNE   VALREQX                                                          
         MVC   INUSER,FVIFLD       SET REQUESTOR                                
*                                                                               
         GOTO1 AVALWHEN,REPWHENH   VALIDATE WHEN                                
         BNE   VALREQX                                                          
*                                                                               
         GOTO1 AVALDEST,REPDESTH   VALIDATE DESTINATION ID                      
         BNE   VALREQX                                                          
*                                                                               
         GOTO1 AVALOTYP,REPOTYPH   VALIDATE OUTPUT TYPE                         
         BNE   VALREQX                                                          
*                                                                               
         GOTO1 AFVAL,REPREQPH      VALIDATE REQUEST PASSWORD                    
         BNE   *+10                                                             
         MVC   REPPSWD,FVIFLD                                                   
*                                                                               
         LA    R2,APRECKEY         BUILD KEY OF PERSON RECORD                   
         XC    SAPEKEY,SAPEKEY                                                  
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,AGENCYID                                                 
*                                                                               
         GOTO1 VDATCON,APPARM,(X'05',0),(X'02',TODAY)                           
         MVC   TODAYC,FFILL        GET TODAYS DATE AND COMPLEMENT               
         XC    TODAYC,TODAY                                                     
         MVC   SAPEDEF,TODAYC                                                   
         XC    SAVPID,SAVPID                                                    
         GOTO1 AFVAL,REPPIDH       READ FIRST PERSONAL ID                       
         BNE   *+10                                                             
         MVC   SAPEPID,FVIFLD                                                   
         XC    SELOPT(SELOPTL),SELOPT                                           
*                                                                               
VQ002    GOTO1 AFVAL,REPUSRH       GET USER ID FILTER                           
         BNE   VQ010                                                            
         MVC   FVMSGNO,=AL2(CE#NONOW)                                           
         CLI   INWHEN,MIXIOKN      TEST FOR NOW REPORT                          
         BE    VALREQX             'NOW' REPORT CAN'T USE THIS FILTER           
         GOTO1 AVALUID,REPUSRH                                                  
         BNE   VALREQX                                                          
         MVC   SELUSR,FVIFLD                                                    
*                                                                               
VQ010    GOTO1 AFVAL,REPPWDH       GET PASSWORD FILTER                          
         BNE   VQ020                                                            
         MVC   SELPWD,FVIFLD                                                    
         MVC   SELPWDL,FVILEN                                                   
*                                                                               
VQ020    GOTO1 AFVAL,REPDEFH       GET EFFECTIVE DATE FILTER                    
         BNE   VQ030                                                            
         MVC   SELDEFC,FFILL                                                    
         ZIC   R0,FVILEN                                                        
         MVC   APBYTE,CULANG                                                    
         OI    APBYTE,PVINSGLO+PVINSGLS                                         
         GOTO1 VPERVAL,APPARM,((R0),FVIFLD),(APBYTE,APWORK)                     
         CLI   4(R1),PVRCONE                                                    
         BE    VQ026                                                            
         SR    RE,RE                                                            
         ZIC   RF,FVXLEN                                                        
         EX    RF,FLDALL           'ALL' KEYWORD                                
         BNE   SAEIIF                                                           
         LA    R1,REPDEFH                                                       
         BAS   RE,ECHOALL                                                       
         XC    SELDEFC,FFILL                                                    
         B     VQ028                                                            
VQ026    XC    SELDEFC,APWORK+PVALCSTA-PERVALD                                  
VQ028    MVC   SAPEDEF,SELDEFC                                                  
         MVI   SELDEFF,1           FLAG DATE FILTER SET                         
*                                                                               
VQ030    GOTO1 AFVAL,REPAGRH       VALIDATE ACCESS GROUP FILTER                 
         BNE   VQ040                                                            
         LA    R2,IOKEY                                                         
         USING SAAGREC,R2                                                       
         XC    SAAGKEY,SAAGKEY     CHECK ACCESS GROUP RECORD EXISTS             
         MVI   SAAGTYP,SAAGTYPQ                                                 
         MVI   SAAGSUB,SAAGSUBQ                                                 
         MVC   SAAGAGY,AGENCYID                                                 
         MVC   SAAGAGR,FVIFLD                                                   
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BL    SAEIIO                                                           
         BH    SAERNF                                                           
         MVC   SELAGR,FVIFLD                                                    
*                                                                               
VQ040    GOTO1 AFVAL,REPOFFH       VALIDATE OFFICE CODE FILTER                  
         BNE   VQ050                                                            
         LA    R2,IOKEY                                                         
         USING SAOFREC,R2                                                       
         XC    SAOFKEY,SAOFKEY     CHECK OFFICE RECORD EXISTS                   
         MVI   SAOFTYP,SAOFTYPQ                                                 
         MVI   SAOFSUB,SAOFSUBQ                                                 
         MVC   SAOFAGY,AGENCYID                                                 
         MVC   SAOFOID,FVIFLD                                                   
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BL    SAEIIO                                                           
         BH    SAERNF                                                           
         MVC   SELOFF,FVIFLD                                                    
*                                                                               
VQ050    GOTO1 AFVAL,REPDIDH       VALIDATE DEPARTMENT CODE FILTER              
         BNE   VQ060                                                            
         OC    SELOFF,SELOFF                                                    
         BZ    SAEODF                                                           
         LA    R2,IOKEY                                                         
         USING SADPREC,R2                                                       
         XC    SADPKEY,SADPKEY     CHECK DEPARTMENT RECORD EXISTS               
         MVI   SADPTYP,SADPTYPQ                                                 
         MVI   SADPSUB,SADPSUBQ                                                 
         MVC   SADPAGY,AGENCYID                                                 
         MVC   SADPOID,SELOFF                                                   
         MVC   SADPDID,FVIFLD                                                   
         GOTO1 AIO,IORD+IOCONFIL+IO2                                            
         BL    SAEIIO                                                           
         BH    SAERNF                                                           
         MVC   SELDID,FVIFLD                                                    
*                                                                               
VQ060    GOTO1 AFVAL,REPSYSH       VALIDATE SYSTEM NAME FILTER                  
         BNE   VQ070                                                            
         GOTO1 AVALSYS,REPSYSH     VALIDATE SYSTEM                              
         BNE   NO                                                               
         MVC   SELSYS,APWORK                                                    
*                                                                               
VQ070    GOTO1 AFVAL,REPPGMH       VALIDATE PROGRAM NAME FILTER                 
         BNE   VQ080                                                            
         OC    SELSYS,SELSYS                                                    
         BE    SAEPFS                                                           
         GOTO1 AVALPGM,APPARM,(SELSYS,REPPGMH)  VALIDATE PROGRAM                
         BNE   SAEPGM                                                           
         MVC   SELPGM,APWORK                                                    
*                                                                               
VQ080    EQU   *                                                                
*                                                                               
VQ090    MVI   RPTFMT,0                                                         
         GOTO1 AFVAL,REPFSYSH      SEE IF SYSTEM REPORT REQUIRED                
         BNE   VQ100                                                            
         ZIC   RF,FVXLEN                                                        
         EX    RF,FLDALL                                                        
         BNE   VQ092                                                            
         LA    R1,REPFSYSH                                                      
         BAS   RE,ECHOALL                                                       
         MVI   RPTFMT,X'FF'        FLAG SYSTEM ALL                              
         B     VQ100                                                            
VQ092    GOTO1 AVALSYS,REPFSYSH    VALIDATE SYSTEM                              
         BNE   NO                                                               
         MVC   RPTFMT,APWORK       SET SYSTEM SELECT                            
*                                                                               
VQ100    MVCDD REPDESC,CT#SYSL     SET REPORT DESCRIPTION                       
         GOTO1 VDICTAT,APPARM,C'SL  ',REPDESC                                   
         MVI   REPHEADI,REPHSPAC+REPHCLRA                                       
         MVI   REPFOOTN,0                                                       
         MVI   REPMIDSI,REPMCLRA                                                
         LA    R0,REPSPEC1                                                      
         CLI   RPTFMT,0                                                         
         BNE   VQ110                                                            
         MVI   REPMIDSI,REPMSPAC+REPMCLRA                                       
         LA    R0,REPSPEC0                                                      
VQ110    ST    R0,REPAPHS                                                       
         OI    REPIND2,REPILOW                                                  
*                                  CHECK PASSWORD FIELD SECURITY                
         MVI   PWDAFLAG,0                                                       
         OC    ACASEC,ACASEC                                                    
         BZ    VQ300                                                            
         CLI   ASONOFF,ASOFF                                                    
         BE    VQ300                                                            
         MVI   APBYTE,FLDPWD                                                    
         LA    RF,APBYTE                                                        
         GOTO1 VSECRET,SCPARM,('SECPFLDP',ACASEC),(RF)                          
         BNL   VQ300               FULL READ/WRITE ACCESS                       
*                                    OR NO ACCESS                               
VQ200    MVI   PWDAFLAG,X'FF'                                                   
*                                                                               
VQ300    MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALREQX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GENERATE REPORT                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING SAPEREC,R2                                                       
PRTREP   L     R4,AREP                                                          
         LA    R2,IOKEY                                                         
         MVC   SAPEKEY(L'SAPEKEY),APRECKEY SET INITIAL KEY VALUE                
         LA    R1,IOHI+IOCONFIL+IO1                                             
         B     PR014               GET FIRST RECORD                             
*                                  GET NEXT RECORD (SEQUENCE BROKEN)            
PR010    LA    R2,IOKEY                                                         
         MVC   SAPEKEY(L'SAPEKEY),APRECKEY                                      
         GOTO1 AIO,IORD+IOCONFIL+IO1                                            
         BNE   PRTREPX                                                          
*                                  GET NEXT RECORD (IN SEQUENCE)                
PR012    LA    R1,IOSQ+IOCONFIL+IO1                                             
PR014    GOTO1 AIO                                                              
         BNE   PRTREPX                                                          
*                                                                               
PR020    L     R2,AIOAREA1         CHECK PERSON RECORD KEY TYPE                 
         CLI   SAPETYP,SAPETYPQ                                                 
         BNE   PRTREPX             IF NOT EXIT REPORT                           
         CLI   SAPESUB,SAPESUBQ                                                 
         BNE   PRTREPX                                                          
         CLC   SAPEAGY,AGENCYID                                                 
         BNE   PRTREPX                                                          
         CLI   SELDEFF,0           FILTER ON EFFECTIVE DATE                     
         BZ    PR022                                                            
         CLC   SAPEDEF,SELDEFC                                                  
         BL    PR012                                                            
         B     PR024                                                            
PR022    CLC   SAPEDEF,TODAYC                                                   
         BL    PR012                                                            
         CLC   SAPEPID,SAVPID                                                   
         BE    PR012                                                            
         MVC   SAVPID,SAPEPID                                                   
*                                                                               
PR024    MVC   APRECKEY(L'SAPEKEY),SAPEKEY  SAVE LAST RECORD KEY                
         MVC   RPTPID,SAPEPID     DISPLAY PERSONAL-ID                           
         MVC   APHALF,FFILL                                                     
         XC    APHALF,SAPEDEF                                                   
         XC    APWORK,APWORK                                                    
         GOTO1 VDATCON,APPARM,(2,APHALF),(X'51',APWORK)                         
         MVC   RPTDEF1,APWORK      DISPLAY EFFECTIVE DATE                       
         MVI   APFLAG,0                                                         
         MVI   SELAGRF,0                                                        
         LA    R3,SAPEDATA         GET ELEMENT DATA                             
*                                                                               
PR030    CLI   0(R3),0             E-O-R                                        
         BE    PR100                                                            
         CLI   0(R3),SAPERELQ                                                   
         BE    PRPER                                                            
         CLI   0(R3),SAAGCELQ                                                   
         BE    PRAGC                                                            
         CLI   0(R3),SAACVELQ                                                   
         BE    PRACV                                                            
         CLI   0(R3),SAPWDELQ                                                   
         BE    PRPWD                                                            
*                                                                               
PR040    ZIC   R0,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R0                                                            
         B     PR030                                                            
*                                  ACTIVITY ELEMENT                             
         USING SAACVD,R3                                                        
PRACV    GOTO1 VDATCON,APPARM,(3,SAACVDT),(8,RPTDATE)                           
         B     PR040                                                            
*                                                                               
         USING SAPERD,R3           PERSONNEL DETAILS ELEMENT                    
PRPER    EQU   *                                                                
*&&UK                                                                           
*                                  CHECK OFFICE MANAGER ACCESS                  
         GOTO1 ATSTOMAN,SAPEROFF                                                
         BNE   PRPE004                                                          
         B     PRPE006                                                          
*&&                                                                             
*&&US                                                                           
*                                  CHECK OFFICE/DEPT MANAGER ACCESS             
         GOTO1 ATSTDMAN,APPARM,SAPEROFF,SAPERDID                                
         BE    PRPE006                                                          
*&&                                                                             
PRPE004  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     PR010                                                            
PRPE006  OC    SELOFF,SELOFF       FILTER ON PERSONNEL DETAILS                  
         BZ    PRPE010                                                          
         CLC   SELOFF,SAPEROFF                                                  
         BNE   PR010                                                            
PRPE010  OC    SELDID,SELDID                                                    
         BZ    PRPE020                                                          
         CLC   SELDID,SAPERDID                                                  
         BNE   PR010                                                            
PRPE020  CLI   SAPERLN,SAPERLNQ                                                 
         BL    PR010                                                            
         B     PR040                                                            
*                                                                               
         USING SAAGCD,R3                                                        
PRAGC    MVI   SELAGRF,1                                                        
         OC    SELAGR,SELAGR       FILTER ON ACCESS GROUP CODE                  
         BZ    PR040                                                            
         CLI   SAAGCLN,SAAGCLNQ                                                 
         BL    PR010                                                            
         CLC   SELAGR,SAAGCCOD                                                  
         BNE   PR010                                                            
         B     PR040                                                            
*                                  PASSWORD POINTER ELEMENT                     
         USING SAPWDD,R3                                                        
PRPWD    XC    RPTCODE,RPTCODE                                                  
         OC    SELPWD,SELPWD       FILTER ON PASSWORD                           
         BZ    PRPWD10                                                          
         CLI   SAPWDLN,SAPWDLNQ                                                 
         BL    PR010                                                            
         SR    R1,R1                                                            
         IC    R1,SELPWDL                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SELPWD(0),SAPWDCOD                                               
         BNE   PR010                                                            
PRPWD10  MVC   RPTCODE,SAPWDCOD    SAVE PASSWORD CODE                           
         LA    R2,IOKEY                                                         
         USING SA0REC,R2                                                        
         XC    SA0KEY,SA0KEY       READ PASSWORD RECORD                         
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,AGENCYID                                                 
         MVC   SA0KCODE,RPTCODE                                                 
         OI    APINDS,APILRERD     FLAG READ SEQUENCE BROKEN                    
         GOTO1 AIO,IORD+IOCONFIL+IO3                                            
         BNL   *+6                                                              
         DC    H'00'                                                            
         BNE   PR040               IGNORE IF PASSWORD RECORD REMOVED            
         MVI   APFLAG,1            FLAG PASSWORD RECORD FOUND                   
         B     PR040                                                            
*                                                                               
PR100    CLI   APFLAG,0            DISPLAY PASSWORD RECORD DATA                 
         BE    PR400                 IF FOUND OK AND FILTER ON DETAILS          
         L     R2,AIOAREA3                                                      
         XC    COUNT,COUNT         COUNT OF SYSTEMS                             
         LA    R8,WORK                                                          
         MVC   WORK,SPACES                                                      
         MVI   SELSYSF,0           FLAG FOR SYSTEM FILTER                       
         LA    R3,SA0DATA          GET ELEMENT DATA                             
*                                                                               
PR130    CLI   0(R3),0             E-O-R                                        
         BE    PR200                                                            
         CLI   0(R3),SASYSELQ                                                   
         BE    PRSYS                                                            
PR140    ZIC   R0,1(R3)            BUMP TO NEXT ELEMENT                         
         AR    R3,R0                                                            
         B     PR130                                                            
*                                  SYSTEM ELEMENT                               
         USING SASYSD,R3                                                        
PRSYS    OC    SELSYS,SELSYS                                                    
         BZ    PRSYS100                                                         
         CLC   SASYSNUM,SELSYS                                                  
         BNE   PRSYS100                                                         
         MVI   SELSYSF,1           FLAG SYSTEM FOUND                            
*                                  FILTER PROGRAM CODE                          
         OC    SELPGM,SELPGM                                                    
         BZ    PRSYS100                                                         
         MVC   PROGRAM,SELPGM                                                   
         ST    R3,ASYSEL                                                        
         BAS   RE,GETPAVAL         FIND PROGRAM IN SYSTEM ELEMENT               
         BNE   PRSYS010                                                         
         CLC   PACCVAL,NAUTH       IF PROGRAM NOT AUTHORISED                    
         BE    PR010                 GET NEXT RECORD                            
         B     PRSYS100              ELSE OK                                    
PRSYS010 CLC   SASYSALL,NAUTH      IF PROGRAM NOT FOUND                         
         BE    PR010                 AND DEFAULT NOT AUTH, GET NEXT             
         B     PRSYS100              ELSE OK                                    
PRSYS100 ZIC   RF,COUNT                                                         
         LA    RF,1(RF)                                                         
         STC   RF,COUNT                                                         
         GOTO1 ADISSYS,SASYSNUM                                                 
         MVC   0(7,R8),APWORK      INSERT NAME                                  
         LA    R8,6(R8)                                                         
         CLI   0(R8),C' '          SQUASH OUT SPACES                            
         BNE   *+8                                                              
         BCT   R8,*-8                                                           
         LA    R8,2(R8)                                                         
         B     PR140                                                            
*                                  FILTER PROGRAM CODE                          
PR200    OC    SELSYS,SELSYS       FILTER ON SYSTEM CODE                        
         BZ    PR210                                                            
         CLI   SELSYSF,0           CHECK SYSTEM FOUND                           
         BE    PR010                 ELSE NEXT RECORD RESTART SEQUENCE          
*                                  FILTER ON COMPATIBLE USERID                  
PR210    L     R2,AIOAREA3                                                      
         OC    SELUSR,SELUSR                                                    
         BZ    PR220                                                            
         MVC   APWORK,SELUSR                                                    
         BAS   RE,FILTUSER         FILTER USER ID COMPATIBLE                    
         BE    PR220                 OK                                         
         B     PR010                 ELSE NEXT RECORD RESTART SEQUENCE          
*                                                                               
PR220    CLI   SELAGRF,0           CHECK ACCESS GROUP FOUND                     
         BNE   *+14                                                             
         OC    SELAGR,SELAGR                                                    
         BNZ   PR010                                                            
         CLI   APFLAG,0            CHECK PASSWORD RECORD FOUND                  
         BE    PR010                 AVOIDING OLD STYLE RECORDS                 
         MVC   RPTSYS,WORK                                                      
         CLI   RPTFMT,0            TEST SYSTEM LIST REQUIRED                    
         BNE   PRT010                                                           
*                                                                               
         MVC   PRTPID,RPTPID       MOVE FIELDS INTO LIST LINE                   
         CLI   PWDAFLAG,0                                                       
         BNE   *+18                                                             
         CLI   PIDREQD,C'Y'        TEST PESONAL ID PASSWORD                     
         BE    *+10                                                             
         MVC   PRTCODE,RPTCODE                                                  
         MVC   PRTDEF1,RPTDEF1                                                  
         MVC   PRTDATE,RPTDATE                                                  
         MVC   PRTSYS,RPTSYS                                                    
         B     PR400              AND PRINT IT                                  
*                                                                               
PRT010   CLI   RPTFMT,X'FF'       TEST ALL SYSTEMS                              
         BE    PRT012                                                           
         LA    RF,1               SET TO 1 SYSTEM                               
         B     PRT014                                                           
PRT012   ZIC   RF,COUNT                                                         
*                                                                               
PRT014   SLL   RF,2               CALCULATE SIZE OF ENTRY                       
         LA    RF,7(RF)                                                         
         SR    R1,R1                                                            
         IC    R1,REPLINE         ADD TO CURRENT LINE POS                       
         AR    RF,R1                                                            
         CLM   RF,1,REPMAXL       WILL IT FIT ON THE PAGE                       
         BNH   *+8                                                              
         OI    REPHEADI,REPHFRCE  NO SO FORCE NEW PAGE                          
*                                                                               
         MVC   PRLPID,RPTPID                                                    
         CLI   PWDAFLAG,0                                                       
         BNE   *+18                                                             
         CLI   PIDREQD,C'Y'        TEST PESONAL ID PASSWORD                     
         BE    *+10                                                             
         MVC   PRLCODE,RPTCODE                                                  
         MVC   PRLDATE,RPTDATE                                                  
*                                                                               
         MVCDD PRLPIDD,CT#PID     MOVE IN DICTIONARY LABLES                     
         MVCDD PRLCODED,CT#PSWD                                                 
         MVCDD PRLDATED,CT#LCHNG                                                
         MVCDD PRLUSRD,CT#USRIS                                                 
*                                                                               
         MVC   PRLLIN2,SPACES      PAD WITH BLANK LINES                         
         MVC   PRLLIN3,SPACES                                                   
*                                                                               
PRT020   BAS   RE,DISIDS           DISPLAY USERID LINE                          
         LA    RE,BLOCK                                                         
         LA    RF,PRLUSR                                                        
         SR    R1,R1                                                            
         ICM   R1,1,BLKCNT                                                      
         BZ    PRT030                                                           
PRT025   MVC   0(10,RF),0(RE)      INSERT USERID                                
         LA    RF,9(RF)                                                         
         CLI   0(RF),C' '          SQUASH OUT SPACES                            
         BNE   *+10                                                             
         BCTR  RF,0                                                             
         B     *-10                                                             
         MVI   1(RF),C','          INSERT , BETWEEN IDS                         
         LA    RF,2(RF)                                                         
         LA    RE,10(RE)                                                        
         BCT   R1,PRT025           GET NEXT USERID                              
         BCTR  RF,0                                                             
         MVI   0(RF),C' '          REMOVE , AFTER LAST ID                       
*                                                                               
PRT030   GOTO1 VREPORT,REPD        PRINT THIS BLOCK                             
*                                                                               
         LA    R8,PRLLIN1          SET UP FOR SYSTEM ACCESS LIST                
         USING PRLLINS,R8                                                       
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'21'        GET SYSTEM ELEMENTS                          
         GOTO1 AGETELS,SA0REC                                                   
         ICM   R3,15,APPARM        WAS EL FOUND                                 
         BZ    PR300                                                            
         USING SASYSD,R3                                                        
PRT032   CLI   RPTFMT,X'FF'        IS SYSTEM ALL                                
         BE    *+14                                                             
         CLC   RPTFMT,SASYSNUM     IS THIS SELECTED SYSTEM                      
         BNE   PRT036                                                           
*                                                                               
         MVI   PRLSYS1,C' '        START WITH BLANK LINE                        
         GOTO1 ADISSYS,SASYSNUM                                                 
         MVC   PRLSYS(7),APWORK    SYSTEM NAME                                  
         MVCDD PRLSYSD,CT#SYS                                                   
*                                  BUILD LIMIT ACCESS                           
         GOTO1 ADISLACC,APPARM,(SASYSNUM,SASYSLMT)                              
         MVC   PRLLIM,APWORK                                                    
         OC    PRLLIM,PRLLIM       TEST IF ANY THERE                            
         BZ    *+10                                                             
         MVCDD PRLLIMD,CT#LIMAC    ONLY PRINT HEADER IF NEEDED                  
*                                                                               
         MVC   PRLSYS2,SPACES      ANOTHER BLANK LINE                           
         BAS   RE,BLCKACC          BUILD PROGRAM ACCESS BLOCK                   
         LA    RE,BLOCK                                                         
         LA    RF,PRLSYS3                                                       
         LA    R0,PRLEND                                                        
         SH    R0,=H'10'                                                        
         SR    R1,R1                                                            
         IC    R1,BLKCNT                                                        
PRT035   MVC   0(10,RF),0(RE)      INSERT PROG= ENTRY                           
         LA    RF,9(RF)                                                         
         CLI   0(RF),C' '          SQUASH OUT SPACES                            
         BNE   *+10                                                             
         BCTR  RF,0                                                             
         B     *-10                                                             
         MVI   1(RF),C','          INSERT , BETWEEN ENTRYS                      
         LA    RF,2(RF)                                                         
         LA    RE,10(RE)                                                        
         CR    RF,R0                                                            
         BNL   PRT035A                                                          
         BCT   R1,PRT035           GET NEXT PROG= ENTRY                         
         BCTR  RF,0                                                             
         MVI   0(RF),C' '          REMOVE , AFTER LAST                          
PRT035A  GOTO1 VREPORT,REPD        PRINT THIS ONE                               
         LA    R8,PRLLIN1          RESET FOR NEXT                               
*                                                                               
PRT036   BAS   RE,NEXTEL           GET NEXT SYSTEM ELEMENT                      
         BNE   PR300                                                            
         B     PRT032                                                           
*                                                                               
PR300    MVC   PRLSYS1,ASTS        BUILD LINE OF ASTS                           
PR400    GOTO1 VREPORT,REPD                                                     
         B     PR010               READ SEQUENCE BROKEN                         
*                                                                               
PRTREPX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PUT KEY OF RECORD TO SAVED KEY TABLE                     *         
***********************************************************************         
         SPACE 1                                                                
         USING SAPEREC,R2                                                       
PUTKEY   LA    R2,APRECKEY                                                      
         LA    R3,APELEM                                                        
         SR    R0,R0                                                            
*                                                                               
         MVI   0(R3),KEYPID                                                     
         MVI   1(R3),10                                                         
         MVC   2(8,R3),SAPEPID                                                  
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
*                                                                               
         MVI   0(R3),0                                                          
         GOTO1 APUTKEY                                                          
*                                                                               
PUTKEYX  B     EXIT                                                             
*                                                                               
         EJECT                                                                  
********************************************************************            
*   BUILD PROGRAM ACCESS LIST INTO BLOCK                           *            
*   R3 = SYS ELEMENT                                               *            
********************************************************************            
BLCKACC  NTR1                                                                   
         USING SASYSD,R3                                                        
         ST    R3,ASYSEL                                                        
         MVC   SYSTEM,SASYSNUM                                                  
         GOTO1 ASETSEL,SYSTEM                                                   
         MVC   SYSALL,SASYSALL     SAVE ALL= VALUE                              
         MVI   BLKCNT,0                                                         
         LA    R4,BLOCK            SET R4 TO START OF TABLE                     
         ZIC   R8,SASYSLN                                                       
         CLI   SASYSLN,16         ALL=VALUE ONLY                                
         BE    BLACX                                                            
         LA    R3,SASYSPGM                                                      
         DROP  R3                                                               
*                                                                               
BLAC1    CH    R8,=H'16'                                                        
         BNH   BLACX                                                            
         MVC   PROGRAM,0(R3)                                                    
         MVC   APBYTE,0(R3)                                                     
         LA    R3,1(R3)                                                         
         GOTO1 ATSTPGM,APBYTE      CHECK USER PROGRAM AUTH<>NO                  
         BE    BLAC102                                                          
         MVC   FVMSGNO,=AL2(FVFOK) CLEAR RETURN MESSAGE                         
*&&UK*&& B     BLAC3                                                            
         BAS   RE,GETPAVAL           IN SYSTEM ELEMENT                          
         BNE   BLAC101                                                          
         CLC   PACCVAL,NAUTH                                                    
         BNE   BLAC102             IF AUTH<>N DISPLAY PROGRAM                   
         B     BLAC3                                                            
BLAC101  L     RE,ASYSEL           CHECK AUTH TAKEN FROM DEFAULT                
         USING SASYSD,RE                                                        
         CLC   SASYSALL,NAUTH                                                   
         BNE   BLAC102             IF AUTH<>N DISPLAY PROGRAM                   
         B     BLAC3                                                            
         DROP  RE                                                               
*                                                                               
BLAC102  GOTO1 ADISPGM,APPARM,(SYSTEM,PROGRAM)                                  
         ICM   R1,15,APPARM                                                     
         BZ    BLAC3               NO PROGRAM SO SKIP                           
         GOTO1 TSTAGYLA,(R1)       TEST RESTRICTED AGENCY ACCESS LIST           
         BNE   BLAC3                                                            
         CLC   SYSALL,0(R3)                                                     
         BE    BLAC3               ACCESS SAME AS ALL= SO SKIP                  
         MVC   0(10,R4),SPACES                                                  
         MVC   0(4,R4),APWORK                                                   
         LR    R0,R4               SAVE R4 VALUE                                
BLACALL  LA    R4,3(R4)                                                         
         CLI   0(R4),C' '                                                       
         BNE   *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C'='                                                       
         CLC   YAUTH,0(R3)                                                      
         BNE   BLAC1A                                                           
         MVC   2(1,R4),CT@YES     PROG=Y                                        
         B     BLAC2                                                            
*                                                                               
BLAC1A   CLC   NAUTH,0(R3)                                                      
         BNE   BLAC1B                                                           
         MVC   2(1,R4),CT@NO      PROG=N                                        
         B     BLAC2                                                            
*                                  PROG=XXXX                                    
BLAC1B   GOTO1 VHEXOUT,APPARM,(R3),2(R4),2,=C'TOG'                              
*                                                                               
BLAC2    LR    R4,R0               RESTORE R4                                   
         LA    R4,10(R4)           AND INDEX TO NEXT                            
         SR    R1,R1                                                            
         IC    R1,BLKCNT                                                        
         LA    R1,1(R1)            BUMP PROG COUNT                              
         STC   R1,BLKCNT                                                        
BLAC3    CLI   PROGRAM,0                                                        
         BE    BLACX1              ALL= VALUE OUTPUT                            
         LA    R3,2(R3)            NEXT PROGRAM                                 
         SH    R8,=H'3'                                                         
         B     BLAC1                                                            
*                                                                               
BLACX    LA    R3,SYSALL                                                        
         MVI   PROGRAM,0           SET TO OUTPUT ALL=                           
         MVC   0(10,R4),SPACES                                                  
         MVC   0(4,R4),=C'ALL '                                                 
         B     BLACALL                                                          
*                                                                               
BLACX1   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*   BUILD ID LIST INTO BLOCK                                          *         
*   R2 = RECORD                                                       *         
***********************************************************************         
         USING SA0REC,R2                                                        
DISIDS   NTR1                                                                   
         MVI   BLKCNT,0                                                         
         XC    APELEM,APELEM                                                    
         MVI   APELEM,X'20'        GET ID ELEMENT                               
         GOTO1 AGETELS,SA0REC                                                   
         ICM   R3,15,APPARM        WAS EL FOUND                                 
         BZ    DISIDX              NOT FOUND END                                
         LA    R4,BLOCK                                                         
DISID01  MVC   0(10,R4),SAID-SAIDD(R3)                                          
         OC    0(2,R4),0(R4)       TEST ID LIST                                 
         BNZ   *+10                                                             
         MVC   0(2,R4),=C'L='                                                   
         LA    R4,10(R4)                                                        
         SR    R1,R1                                                            
         IC    R1,BLKCNT                                                        
         LA    R1,1(R1)            BUMP COUNTER                                 
         STC   R1,BLKCNT                                                        
         BAS   RE,NEXTEL           GET NEXT ID                                  
         BE    DISID01                                                          
*                                                                               
DISIDX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET AGENCY SYSTEM SECURITY FLAGS AND SAVE IN SAVASF                 *         
***********************************************************************         
GETASF   NTR1  ,                                                                
         XC    SAVASF(L'SAVASF),SAVASF                                          
         L     R4,AIOAREA2         READ ID RECORD FROM CONTROL FILE             
         USING CT5REC,R4                                                        
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,AGENCYID                                                
         MVC   IOKEY(L'CT5KEY),CT5KEY                                           
         GOTO1 AIO,IOREAD+IOCONFIL+IO2                                          
         BNL   *+6                                                              
         DC    H'00'                                                            
         BH    GASFX                                                            
*                                                                               
         LA    R3,CT5DATA          EXTRACT INFO FROM ELEMENTS                   
         SR    R0,R0                                                            
         USING CTSYSEL,R3                                                       
GASF010  CLI   0(R3),0                                                          
         BE    GASFX                                                            
         CLI   CTSYSEL,CTSYSELQ                                                 
         BE    GASF030                                                          
*                                                                               
GASF020  IC    R0,1(R3)            DO NEXT ELEMENT                              
         AR    R3,R0                                                            
         B     GASF010                                                          
*                                                                               
GASF030  CLC   SYSTEM,CTSYSNUM                                                  
         BNE   GASF020                                                          
         CLI   CTSYSLEN,X'18'                                                   
         BNE   GASFX                                                            
         MVC   SAVASF,CTSYSPGM                                                  
         B     GASFX                                                            
*                                                                               
GASFX    MVC   FVMSGNO,=AL2(FVFOK) IGNORE IO ERROR MESSAGE                      
         B     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* TEST IF PROGRAM AUTH IS TO BE DISPLAYED AS 'C' CONVERTED TO NEW SEC.*         
***********************************************************************         
TSTASF   NTR1  ,                                                                
*&&US*&& B     TASFNO              UK ONLY                                      
         CLI   SYSTEM,X'04'        MEDIA/MPL/FEE/ACC SYSTEM ONLY                
         BE    TASF008                                                          
         CLI   SYSTEM,X'05'                                                     
         BE    TASF008                                                          
         CLI   SYSTEM,X'07'                                                     
         BE    TASF008                                                          
         CLI   SYSTEM,X'06'                                                     
         BE    TASF008                                                          
         B     TASFNO                                                           
         USING PGMLSTD,R1          R1=A(PROGRAMS LIST)                          
TASF008  ICM   R1,15,APGMLST                                                    
         BZ    TASFNO                                                           
TASF010  CLC   PROGRAM,PGMNUM                                                   
         BE    TASF020                                                          
         SR    RF,RF                                                            
         ICM   RF,3,PLSTLEN                                                     
         LA    R1,0(RF,R1)                                                      
         CLM   R1,15,APGMLSTX      GET A(END PGMLST)                            
         BL    TASF010                                                          
         DC    H'0'                                                             
*                                                                               
TASF020  TM    PGMIND2,PGMISECA    TEST PROGRAM SUPPORTS NEW SECURITY           
         BZ    TASFNO                                                           
         TM    PGMIND2,PGMISECB    TEST PROGRAM SUPPORTS OLD SECURITY           
         BZ    TASFOK              MUST BE CONVERTED                            
         DROP  R1                                                               
*                                                                               
         LA    RE,1                TEST AGENCY SYSTEM SECURITY FLAGS            
         SLL   RE,31               FOR PROGRAM IN 64 BIT MASK                   
         SR    RF,RF                                                            
         SR    R1,R1                                                            
         IC    R1,PROGRAM                                                       
         BCTR  R1,0                                                             
         SRDL  RE,0(R1)                                                         
         LTR   RE,RE                                                            
         BZ    TASF030                                                          
         ICM   RF,15,SAVASF        FIRST GROUP OF FOUR BYTES                    
         NR    RE,RF                                                            
         BZ    TASFNO                                                           
         B     TASFOK                                                           
*                                                                               
TASF030  ICM   RE,15,SAVASF+4      SECOND GROUP OF FOUR BYTES                   
         NR    RF,RE                                                            
         BZ    TASFNO                                                           
         B     TASFOK                                                           
*                                                                               
TASFNO   B     NO                  EXIT NO NOT 'C' TYPE                         
TASFOK   B     YES                 EXIT OK 'C' TYPE                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD PROGRAM NAME LIST AND CODE SAVE TABLE              *         
***********************************************************************         
         SPACE 1                                                                
INITPGML NTR1                                                                   
         BAS   RE,GSELST           GET SELIST ENTRY FOR SYSTEM                  
         L     R1,ASE                ADDRESS IN ASE                             
         L     R1,SEPGMS-SELISTD(R1)                                            
         LH    RE,0(R1)                                                         
         STCM  RE,3,PLSTLEN        SAVE LENGTH PGMLST ENTRY                     
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         ST    R1,APGMLST                                                       
         USING PGMLSTD,R1          R1=A(PROGRAMS LIST)                          
         LA    R4,WORKD            SORT PROGRAM NAMES ALPHA                     
         AHI   R4,XSORTBLK-WORKD                                                
         SR    R8,R8               COUNT NUMBER OF PROGRAMS                     
         XC    PROGRAM,PROGRAM                                                  
*                                  SORT PROGRAM NAMES ALPHABETICALLY            
IPL010   EQU   *                                                                
*&&UK                                                                           
*        TM    PGMIND3,PGMIPC      IGNORE PC PROGRAM                            
*        BO    IPL011                                                           
*&&                                                                             
         TM    CUSTAT,CUSDDS                                                    
         BO    *+12                IF NOT DDS TERMINAL                          
         TM    PGMIND,PGMINOP        IGNORE NOP PROGRAM                         
         BO    IPL011                                                           
*                                                                               
         TM    CUSTAT,CUSDDS                                                    
         BO    *+12                IF NOT DDS TERMINAL                          
         TM    PGMIND,PGMIACC        IGNORE RESTRICTED ACCESS PROGRAM           
         BO    IPL011                                                           
*                                                                               
         TM    CUSTAT,CUSDDS                                                    
         NOP   *+12                IF NOT DDS TERMINAL                          
         TM    PGMIND4,PGMINAUT      IGNORE NO AUTH REQUIRED PROGRAM            
         BO    IPL011                                                           
*&&UK                                                                           
         CLI   PGMCTRY,0           TEST COUNTRY CODE (NOT IN US)                
         BE    *+14                  KEEP ENGLISH DEFAULT                       
         CLC   PGMCTRY,CUCTRY        AND CONNECT COUNTRY                        
         BNE   IPL011                                                           
*&&                                                                             
         MVC   L'PGMNAME(1,R4),PGMNUM                                           
         MVC   0(L'PGMNAME,R4),PGMNAME                                          
         LA    R4,L'PGMNAME+L'PGMNUM(R4)                                        
         LA    R8,1(R8)                                                         
IPL011   BXLE  R1,RE,IPL012                                                     
         B     IPL016                                                           
IPL012   MVC   PROGRAM,PGMNUM                                                   
         MVC   PROGCTRY,PGMCTRY                                                 
         LR    R0,RF                                                            
         LR    RF,R1                                                            
         SR    RF,RE                                                            
         L     R3,APGMLST                                                       
IPL013   EQU   *                                                                
*&&UK                                                                           
         CLC   CUCTRY,PGMCTRY-PGMLSTD(R3)                                       
         BE    *+14                NO SYNONYM CHECK ACROSS COUNTRIES            
         CLC   PROGCTRY,PGMCTRY-PGMLSTD(R3)                                     
         BNE   IPL014                                                           
*&&                                                                             
         CLC   PROGRAM,PGMNUM-PGMLSTD(R3)                                       
         BNE   IPL014              AVOID SYNONOMOUS PROGRAMS                    
         LR    RF,R0                                                            
         B     IPL011                                                           
IPL014   BXLE  R3,RE,IPL013                                                     
         LR    RF,R0                                                            
         B     IPL010                                                           
*                                                                               
IPL016   ST    R1,APGMLSTX                                                      
         LA    R4,WORKD                                                         
         AHI   R4,XSORTBLK-WORKD                                                
         LA    R0,L'PGMNAME                                                     
         LA    R3,L'PGMNAME+L'PGMNUM                                            
         GOTO1 VXSORT,APPARM,(X'00',(R4)),(R8),(R3),(R0),0                      
*                                                                               
         TWAXC SYSPN01H,PROT=Y                                                  
         LA    R3,PSAVTAB          POINT TO SAVE TABLE                          
         LTR   R8,R8                                                            
         BZ    IPLX                EXIT IF NO PROGRAMS IN LIST                  
         LA    R2,SYSPN01H         BUILD PROGRAM CODE SAVE TABLE                
         ST    R2,LINEADR            AND DISPLAY PROGRAM NAMES                  
         MVI   LINEFLD,4                                                        
         USING PGMLD,R2                                                         
IPL020   LA    RE,SYSTENDH         SAVE A(DISPLAY TABLE END)                    
         CR    R2,RE               DIE IF TABLE OVERFLOW                        
         BNL   IPLX                                                             
*                                                                               
         MVC   0(1,R3),L'PGMNAME(R4)  SAVE PROGRAM CODE                         
         XC    1(4,R3),1(R3)       CLEAR DISPLAY ADDRESS                        
         MVC   5(2,R3),NAUTH       DEFAULT TO NO AUTH                           
*                                                                               
         ICM   R1,15,APGMLST       GET A(PGMLST ENTRY)                          
IPL0200  CLC   L'PGMNAME(1,R4),PGMNUM-PGMLSTD(R1)                               
         BE    IPL0201                                                          
         SR    RF,RF                                                            
         ICM   RF,3,PLSTLEN                                                     
         LA    R1,0(RF,R1)                                                      
         CLM   R1,15,APGMLSTX      GET A(END PGMLST)                            
         BL    IPL0200                                                          
         DC    H'0'                                                             
         B     IPL024                                                           
*                                                                               
IPL0201  GOTO1 TSTAGYLA,(R1)       TEST RESTRICTED AGENCY ACCESS LIST           
         BNE   IPL024                                                           
*                                                                               
         MVC   APBYTE,0(R3)                                                     
         GOTO1 ATSTPGM,APBYTE      CHECK USER PROGRAM AUTH<>NO                  
         BE    IPL021                                                           
         MVC   FVMSGNO,=AL2(FVFOK) CLEAR RETURN MESSAGE                         
*                                  BRANCH OUT HERE IN UK ONLY                   
*&&UK*&& B     IPL024                                                           
*                                  DO THIS BIT IN US ONLY                       
         OC    ASYSEL,ASYSEL                                                    
         BZ    IPL020B                                                          
         MVC   PROGRAM,APBYTE      CHECK AUTH VALUE ALREADY PRESENT             
         BAS   RE,GETPAVAL           IN SYSTEM ELEMENT                          
         BNE   IPL020A                                                          
         CLC   PACCVAL,NAUTH                                                    
         BNE   IPL021              IF AUTH<>N DISPLAY PROGRAM                   
         B     IPL020B                                                          
IPL020A  L     RE,ASYSEL           CHECK AUTH TAKEN FROM DEFAULT                
         USING SASYSD,RE                                                        
         CLC   SASYSALL,NAUTH                                                   
         BNE   IPL021              IF AUTH<>N DISPLAY PROGRAM                   
         DROP  RE                                                               
IPL020B  MVC   1(4,R3),=XL4'FF'    FLAG AUTH=NO OVERRIDE                        
         B     IPL024                                                           
*                                                                               
IPL021   S     R2,APRELO           SAVE DISPLAY ADDRESS                         
         ST    R2,1(R3)              IN RELOCATABLE FORM                        
         A     R2,APRELO                                                        
*                                  DISPLAY PROGRAM NAME                         
         MVC   PGMLNAM(L'PGMLNAM),0(R4)                                         
         OI    PGMLNAMH+(FVOIND-FVIHDR),FVOXMT                                  
         NI    PGMLNAMH+(FVATRB-FVIHDR),X'FF'-FVAHIGH                           
         XC    PGMLVAL,PGMLVAL                                                  
         OI    PGMLVALH+(FVOIND-FVIHDR),FVOXMT                                  
         OI    PGMLVALH+(FVATRB-FVIHDR),FVAHIGH                                 
         ZIC   R0,LINEFLD                                                       
         BCT   R0,IPL022                                                        
         L     RF,LINEADR                                                       
         LA    R2,PGMLINE(RF)                                                   
         ST    R2,LINEADR                                                       
         MVI   LINEFLD,4                                                        
         B     IPL024                                                           
IPL022   STC   R0,LINEFLD                                                       
         LA    R2,PGMLLEN(R2)                                                   
IPL024   LA    R4,L'PGMNAME+L'PGMNUM(R4)                                        
         LA    R3,L'PSAVTAB(R3)    BUMP SAVE TABLE POINTER                      
         BCT   R8,IPL020                                                        
*                                                                               
IPLX     XC    0(L'PSAVTAB,R3),0(R3)                                            
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LOCATE SELIST ENTRY FOR SYSTEM AND SAVE AT ASE                      *         
***********************************************************************         
GSELST   NTR1                                                                   
         L     R3,ASYS                                                          
         L     R3,VSELIST-SYSFACD(R3)                                           
         LH    RE,0(R3)                                                         
         L     RF,2(R3)                                                         
         LA    R3,6(R3)                                                         
         USING SELISTD,R3                                                       
*                                                                               
         CLC   SYSTEM,SEOVSYS                                                   
         BE    *+10                                                             
         BXLE  R3,RE,*-10                                                       
         DC    H'0'                DIE IF N/F                                   
*                                                                               
         ST    R3,ASE              SAVE A(SELIST ENTRY)                         
         MVC   APGM,SEPGMS         AND A(SEPGMS)                                
         XIT1                                                                   
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* TEST RESTRICTED AGENCY ACCESS LIST IN PGMAGYLA                      *         
***********************************************************************         
         SPACE 1                                                                
TSTAGYLA NTR1                                                                   
         USING PGMLSTD,R1          R1=A(PROGRAMS LIST)                          
         SR    RF,RF                                                            
         ICM   RF,7,PGMAGYLA                                                    
         BZ    TALAOK              ALL AGENCIES VALID                           
*                                                                               
TALA010  CLC   0(2,RF),SPACES      END OF LIST, AGENCY INVALID                  
         BE    TALANO                                                           
         CLC   0(2,RF),AGENCYID    AGENCY FOUND IN LIST                         
         BE    TALAOK                                                           
         LA    RF,2(RF)                                                         
         B     TALA010                                                          
*                                                                               
TALANO   B     NO                                                               
TALAOK   B     YES                                                              
         DROP  R1                                                               
         EJECT                                                                  
********************************************************************            
*   DISPLAY PROGRAM ACCESS CODES                                   *            
*   R3 = SYS ELEMENT                                               *            
********************************************************************            
         USING SASYSD,R3                                                        
DISPACC  NTR1                                                                   
         LA    R4,PSAVTAB          POINT TO PROGRAM CODE SAVE TABLE             
*                                                                               
         DROP  R2                                                               
         USING PGMLD,R8                                                         
DPAC010  OC    0(L'PSAVTAB,R4),0(R4)                                            
         BZ    DPACX               EXIT AT END OF TABLE                         
         MVC   PROGRAM,0(R4)       SAVE PROGRAM CODE                            
         CLC   1(4,R4),=XL4'FF'    CHECK AUTH=NO OVERIDE                        
         BNE   DPAC011                                                          
         MVC   PACCVAL,NAUTH                                                    
         B     DPAC040                                                          
DPAC011  OC    1(4,R4),1(R4)                                                    
         BNZ   DPAC012                                                          
         BAS   RE,GETPAVAL         GET ACCESS CODE FROM SYSTEM ELEMENT          
         BE    DPAC040                                                          
         MVC   PACCVAL,SASYSALL    DEFAULT TO ALL VALUE                         
         B     DPAC040                                                          
*                                                                               
DPAC012  L     R8,1(R4)                                                         
         A     R8,APRELO                                                        
         BAS   RE,GETPAVAL         GET ACCESS CODE FROM SYSTEM ELEMENT          
         BNE   DPAC020             NOT FOUND USE DEFAULT                        
         OI    PGMLNAMH+(FVATRB-FVIHDR),FVAHIGH                                 
         NI    PGMLVALH+(FVATRB-FVIHDR),X'FF'-FVAHIGH                           
         B     DPAC030                                                          
*                                                                               
DPAC020  MVC   PACCVAL,SASYSALL    DEFAULT TO ALL VALUE                         
         NI    PGMLNAMH+(FVATRB-FVIHDR),X'FF'-FVAHIGH                           
         OI    PGMLVALH+(FVATRB-FVIHDR),FVAHIGH                                 
*                                                                               
DPAC030  EQU   *                                                                
         MVI   ACCASF,C'N'                                                      
         BAS   RE,TSTASF           TEST FOR 'C' CONVERTED PROGRAM               
         BNE   DPAC032                                                          
         CLI   SYSTEM,X'06'        SPECIAL PROCESSING FOR ACC                   
         BNE   *+12                                                             
         MVI   ACCASF,C'Y'                                                      
         B     DPAC032                                                          
         OI    PGMLNAMH+(FVATRB-FVIHDR),FVAHIGH                                 
         NI    PGMLVALH+(FVATRB-FVIHDR),X'FF'-FVAHIGH                           
         MVI   PGMLVAL,C'C'                                                     
         B     DPAC040                                                          
DPAC032  BAS   RE,DISPAVAL         DISPLAY ACCESS CODE VALUE IN TABLE           
         MVC   PGMLVAL,APWORK                                                   
*                                                                               
DPAC040  MVC   5(2,R4),PACCVAL                                                  
         LA    R4,L'PSAVTAB(R4)                                                 
         B     DPAC010             DO NEXT TABLE ENTRY                          
*                                                                               
DPACX    B     EXIT                                                             
         DROP  R3                                                               
         SPACE 2                                                                
********************************************************************            
*   FORMAT PROGRAM ACCESS CODE VALUE FOR DISPLAY                   *            
*   PACCVAL 2 BYTE INPUT CODE, OUTPUT TEXT IN APWORK               *            
********************************************************************            
         SPACE 1                                                                
DISPAVAL NTR1                                                                   
         XC    APWORK,APWORK                                                    
         CLI   ACCASF,C'Y'         TEST FOR CONVERTED ACC PROGRAM               
         BE    DPAV030                                                          
         CLC   YAUTH,PACCVAL                                                    
         BNE   DPAV010                                                          
         MVC   APWORK(1),CT@YES    PROG=Y                                       
         B     DPAVX                                                            
*                                                                               
DPAV010  EQU   *                                                                
         CLC   NAUTH,PACCVAL                                                    
         BNE   DPAV020                                                          
         MVC   APWORK(1),CT@NO     PROG=N                                       
         B     DPAVX                                                            
*                                                                               
DPAV020  EQU   *                                                                
*                                  OUTPUT HEX 4 CHAR VALUE                      
         GOTO1 VHEXOUT,APPARM,PACCVAL,APWORK,2,=C'TOG'                          
         B     DPAVX                                                            
*                                                                               
DPAV030  EQU   *                                                                
         CLC   YAUTH,PACCVAL                                                    
         BNE   DPAV032                                                          
         MVI   APWORK,C'C'                                                      
         B     DPAVX                                                            
*                                                                               
DPAV032  EQU   *                                                                
         CLC   NAUTH,PACCVAL                                                    
         BNE   DPAV040                                                          
         MVI   APWORK,C'N'                                                      
         B     DPAVX                                                            
*                                  OUTPUT CONVERTED + HEX VALUE                 
DPAV040  EQU   *                                                                
         MVI   APWORK,C'C'                                                      
         GOTO1 VHEXOUT,APPARM,PACCVAL+1,APWORK+1,1,=C'TOG'                      
*                                                                               
DPAVX    B     EXIT                                                             
         SPACE 2                                                                
********************************************************************            
*   GET PROGRAM ACCESS CODE VALUE FROM SYSTEM ELEMENT              *            
*   ON INPUT PROGRAM 1 BYTE PROGRAM CODE, R3 POINTS TO SYSTEM ELEM.*            
*   ON OUTPUT PACCVAL 2 BYTE ACCESS CODE IF FOUND ELSE CC .NE.     *            
********************************************************************            
         SPACE 1                                                                
         USING SASYSD,R3                                                        
GETPAVAL NTR1                                                                   
         L     R3,ASYSEL                                                        
         XC    PACCVAL,PACCVAL                                                  
         LA    R1,SASYSPGM         POINT TO SYSTEM ELEMENT                      
         ZIC   RE,SASYSLN                                                       
*                                  FIND PROGRAM IN ELEMENT                      
GPAV010  CH    RE,=Y(SASYSLNQ)                                                  
         BNH   GPAVNO              END OF ELEMENT                               
         CLC   PROGRAM,0(R1)                                                    
         BE    GPAVYES             PROGRAM FOUND                                
         LA    R1,L'SASYSPGM(R1)   GET NEXT PROGRAM                             
         SH    RE,=Y(L'SASYSPGM)                                                
         B     GPAV010                                                          
*                                                                               
GPAVNO   B     NO                  PROGRAM NOT FOUND                            
*                                                                               
GPAVYES  MVC   PACCVAL,1(R1)       SAVE ACCESS CODE VALUE                       
         B     YES                                                              
         DROP  R3                                                               
         EJECT                                                                  
********************************************************************            
*   VALIDATE PROGRAM ACCESS CODES                                  *            
*   R3 = SYS ELEMENT                                               *            
********************************************************************            
         USING SASYSD,R3                                                        
VALPACC  NTR1                                                                   
         LA    R2,PSAVTAB          POINT TO PROGRAM CODE SAVE TABLE             
         LA    R4,WORKD                                                         
         AHI   R4,XSORTBLK-WORKD                                                
         SR    R8,R8                                                            
*                                                                               
VPAC010  OC    0(L'PSAVTAB,R2),0(R2)                                            
         BZ    VPAC100             END OF TABLE                                 
         MVC   PROGRAM,0(R2)       SAVE PROGRAM CODE                            
         CLC   1(4,R2),=XL4'FF'    CHECK FOR OVERIDE                            
         BE    VPAC011                                                          
         OC    1(4,R2),1(R2)       CHECK NOT SET                                
         BZ    VPAC014                                                          
*&&UK*&& B     VPAC012                                                          
         GOTO1 ATSTPGM,0(R2)       CHECK USER PROGRAM AUTH<>NO IN US            
         BE    VPAC012               ELSE AUTO SET AUTH=N                       
         MVC   FVMSGNO,=AL2(FVFOK) CLEAR RETURN MESSAGE                         
         MVC   5(2,R2),NAUTH                                                    
*                                                                               
VPAC011  MVC   PACCVAL,5(R2)       USE TABLE VALUE                              
         CLC   SASYSALL,PACCVAL    CHECK IF SAME AS DEFAULT                     
         BNE   VPAC016             IGNORE IF SAME AS DEFAULT                    
         B     VPAC020                                                          
*                                                                               
VPAC012  EQU   *                                                                
         MVI   ACCASF,C'N'                                                      
         BAS   RE,TSTASF           TEST FOR 'C' CONVERTED PROGRAM               
         BNE   VPAC013                                                          
         CLI   SYSTEM,X'06'        SPECIAL PROCESSING FOR ACC                   
         BNE   VPAC020             IGNORE IF NOT CONVERTED OR ACC               
         MVI   ACCASF,C'Y'                                                      
*                                                                               
VPAC013  EQU   *                                                                
         L     R1,1(R2)            GET DISPLAY ADDRESS                          
         A     R1,APRELO                                                        
         LA    R1,PGMLVALH-PGMLD(R1)                                            
         GOTO1 AFVAL               VALIDATE ACCESS CODE FIELD                   
         BNE   VPAC020                                                          
         BAS   RE,VALPAVAL         VALIDATE CODE RETURN IN PACCVAL              
         BNE   VPACNO                                                           
*                                                                               
VPAC014  CLC   SVSYSALL,PACCVAL    CHECK IF SAME AS OLD DEFAULT                 
         BE    VPAC020               IF SO IGNORE                               
         CLC   SASYSALL,PACCVAL    CHECK IF SAME AS NEW DEFAULT                 
         BE    VPAC020               IF SO IGNORE                               
*                                                                               
VPAC016  MVC   0(1,R4),0(R2)       ELSE ADD VALUE TO SORT BLOCK                 
         MVC   1(L'PACCVAL,R4),PACCVAL                                          
         LA    R4,L'PGMNUM+L'PACCVAL(R4)                                        
         LA    R8,1(R8)            BUMP COUNT                                   
VPAC020  LA    R2,L'PSAVTAB(R2)                                                 
         B     VPAC010             GET NEXT PROGRAM                             
*                                                                               
VPAC100  LA    R1,SASYSLNQ                                                      
         LTR   R8,R8                                                            
         BZ    VPAC120             EXIT IF NO PROGRAM CODES TO SORT             
         LA    R4,WORKD            ELSE SORT PROGRAM CODES                      
         AHI   R4,XSORTBLK-WORKD                                                
         LA    R0,L'PGMNUM+L'PACCVAL                                            
         LA    RF,L'PGMNUM                                                      
         GOTO1 VXSORT,APPARM,(X'00',(R4)),(R8),(R0),(RF),0                      
         LA    R1,SASYSLNQ                                                      
         LA    RE,SASYSPGM                                                      
*                                                                               
VPAC110  MVC   0(L'PGMNUM,RE),0(R4)      BUILD PROGRAM ACCESS CODES             
         MVC   1(L'PACCVAL,RE),1(R4)     INTO SYSTEM ELEMENT                    
         LA    R1,L'PGMNUM+L'PACCVAL(R1)                                        
         LA    RE,L'PGMNUM+L'PACCVAL(RE)                                        
         LA    R4,L'PGMNUM+L'PACCVAL(R4)                                        
         BCT   R8,VPAC110                                                       
*                                                                               
VPAC120  STC   R1,SASYSLN          SAVE NEW LENGTH OF SYSTEM ELEMENT            
         B     VPACYES                                                          
*                                                                               
VPACNO   B     NO                                                               
*                                                                               
VPACYES  B     YES                                                              
         DROP  R3                                                               
         EJECT                                                                  
********************************************************************            
*   VALIDATE PROGRAM ACCESS CODE VALUE                             *            
*   VALUE IN FVIFLD AFTER AFVAL CALL                               *            
*   RETURN INTERNAL CODE IN PACCVAL                                *            
********************************************************************            
         SPACE 1                                                                
         USING SASYSD,R3                                                        
VALPAVAL NTR1                                                                   
         CLI   ACCASF,C'Y'                                                      
         BE    VPAV030                                                          
         CLI   FVILEN,1                                                         
         BE    VPAV020                                                          
         CLI   FVILEN,4            MAX 4 CHARACTERS                             
         BH    SAEFTL                                                           
         BE    VPAV010             4 CHARS IS HEX CODE                          
         B     SAEIIF                                                           
*                                  IF NOT Y OR N MUST BE VALID HEX              
VPAV010  EQU   *                                                                
         GOTO1 VHEXIN,APPARM,FVIFLD,PACCVAL,4                                   
         OC    APPARM+12(4),APPARM+12                                           
         BZ    SAEFNH                                                           
         B     VPAVYES                                                          
*                                                                               
VPAV020  EQU   *                                                                
         MVC   PACCVAL,YAUTH       ONE CHR INPUT S/B Y OR N                     
         CLC   FVIFLD(1),CT@YES                                                 
         BE    VPAVYES                                                          
         MVC   PACCVAL,NAUTH                                                    
         CLC   FVIFLD(1),CT@NO                                                  
         BE    VPAVYES                                                          
         B     SAEIIF                                                           
*                                  HERE IF CONVERTED ACC PROGRAM                
VPAV030  EQU   *                                                                
         CLI   FVILEN,1                                                         
         BNE   VPAV040                                                          
         MVC   PACCVAL,YAUTH       ONE CHR INPUT S/B Y OR N OR C                
         CLC   FVIFLD(1),CT@YES                                                 
         BE    VPAVYES                                                          
         MVC   PACCVAL,NAUTH                                                    
         CLC   FVIFLD(1),CT@NO                                                  
         BE    VPAVYES                                                          
         CLI   FVIFLD,C'C'                                                      
         BNE   SAEIIF                                                           
         MVC   PACCVAL,YAUTH                                                    
         B     VPAVYES                                                          
*                                                                               
VPAV040  EQU   *                                                                
         MVI   PACCVAL,X'FF'                                                    
         CLI   FVILEN,2                                                         
         BNE   VPAV050                                                          
         MVI   PACCVAL,X'FF'                                                    
         GOTO1 VHEXIN,APPARM,FVIFLD,PACCVAL+1,2                                 
         OC    APPARM+12(4),APPARM+12                                           
         BZ    SAEFNH                                                           
         MVI   PACCVAL,X'FF'                                                    
         B     VPAVYES                                                          
*                                                                               
VPAV050  EQU   *                                                                
         CLI   FVILEN,3                                                         
         BH    SAEFTL                                                           
         CLI   FVIFLD,C'C'                                                      
         BNE   SAEIIF                                                           
         GOTO1 VHEXIN,APPARM,FVIFLD+1,PACCVAL+1,2                               
         OC    APPARM+12(4),APPARM+12                                           
         BZ    SAEFNH                                                           
         MVI   PACCVAL,X'FF'                                                    
         B     VPAVYES                                                          
*                                                                               
VPAVNO   B     NO                                                               
*                                                                               
VPAVYES  B     YES                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* FIND NEXT ELEMENT OF SAME CODE WITHIN RECORD                        *         
* NTRY R3=A(CURRENT ELEMENT)                                          *         
*      APELEM = ELCODE TO FIND                                        *         
* EXIT CC EQ - FOUND - R3=A(NEW ELEMENT)                              *         
*      CC NE - NOT FOUND                                              *         
***********************************************************************         
NEXTEL   ZIC   RF,1(R3)            L'ELEMENT                                    
         AR    R3,RF               A(NEXT ELEMNT)                               
         ICM   RF,1,1(R3)          L'ELEMENT                                    
         BNZ   *+8                                                              
         LTR   RB,RB               FORCE CC NON ZERO                            
         BR    RE                                                               
         CLC   0(1,R3),APELEM                                                   
         BR    RE                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO FIND IF USER ID IN APWORK IS COMPATIBLE WITH RECORD      *         
* R2 = A(RECORD)                                                      *         
***********************************************************************         
FILTUSER NTR1                      BUILD COMPATIBLE ID TABLE                    
         GOTO1 VGETIDS,APPARM,(C'C',(R2)),ATIA,VDMGR                            
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'00'               IO ERROR                                     
         CLI   0(R1),0                                                          
         BE    YES                 NULL TABLE SO CAN ACCESS ANY                 
         L     R1,4(R1)                                                         
FUSER010 CLI   0(R1),X'FF'         TEST E-O-L                                   
         BE    NO                                                               
         CLC   0(10,R1),=CL10'ALL'                                              
         BNE   FUSER020                                                         
         TM    CUSTAT,CUSDDS       ONLY VALID FOR DDS TERMINALS                 
         BZ    FUSER030                                                         
         B     YES                                                              
FUSER020 CLC   0(10,R1),APWORK     MATCH ID WITH TABLE                          
         BE    YES                                                              
FUSER030 LA    R1,12(R1)           GET NEXT TABLE ENETRY                        
         B     FUSER010                                                         
         EJECT                                                                  
*                                  GETTXT MESSAGE # ERROR EXITS                 
SAEIIF   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     NO                  INPUT FIELD ERROR                            
SAEFTL   MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     NO                  INPUT FIELD TOO LONG                         
SAEFNN   MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     NO                  INPUT FIELD NOT NUMERIC                      
SAEFTS   MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     NO                  INPUT FIELD TOO SHORT                        
SAEFNH   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     NO                  INPUT FIELD ERROR                            
SAEMIF   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     NO                  MISSING FIELD                                
SAEIIO   MVC   FVMSGNO,=AL2(FVFIOER)                                            
         B     NO                  I/O ERROR                                    
SAERNF   MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     NO                  RECORD NOT FOUND                             
SAEDIF   MVC   FVMSGNO,=AL2(FVFDUPE)                                            
         B     NO                  DUPLICATE                                    
SAERAE   MVC   FVMSGNO,=AL2(FVFERAE)                                            
         B     NO                  ALREADY EXISTS                               
SAESYS   MVC   FVMSGNO,=AL2(FVFESYS)                                            
         B     NO                  SYSTEM NAME ERROR                            
SAEPGM   MVC   FVMSGNO,=AL2(FVFEPGM)                                            
         B     NO                  SYSTEM NAME ERROR                            
SAEPFS   MVC   FVMSGNO,=AL2(CE#PFNVS)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  PROGRAM FILTER INVALID WOUT SYS              
SAEODF   MVC   FVMSGNO,=AL2(CE#OFFDP)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  MUST ENTER OFFICE CODE FOR DEPT.             
SAEFTB   MVC   FVMSGNO,=AL2(CE#FVMAX)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  FIELD VALUE EXCEEDS MAXIMUM                  
SAEPED   MVC   FVMSGNO,=AL2(CE#PEDEL)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  PERSONAL ID RECORD IS DELETED                
SAEGI0   MVC   FVMSGNO,=AL2(CE#GID00)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  GETIDS RETURN CODE 00                        
SAEGIF   MVC   FVMSGNO,=AL2(CE#GIDFF)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     NO                  GETID RETURN CODE FF                         
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         B     EXIT                                                             
*                                                                               
ECHOALL  MVC   FVIFLD-FVIHDR(L'CT@ALL,R1),CT@ALL                                
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         BR    RE                                                               
*                                                                               
FLDALL   CLC   FVIFLD(0),CT@ALL                                                 
*                                                                               
         LTORG                                                                  
*                                                                               
CTFILE   DC    C'CTFILE '                                                       
SPACES   DC    132C' '                                                          
FFILL    DC    32X'FF'                                                          
ASTS     DC    C'+',130C'-',C'+'                                                
*                                                                               
YAUTH    DC    X'000F'                                                          
NAUTH    DC    X'0000'                                                          
XAUTH    DC    X'FFFF'                                                          
*                                                                               
REPSPEC0 DS    0X                  REPORT HEADINGS SPECIFICATIONS               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,57,CT#SYSL,18,L                                               
         SPEC  H2,57,CT#SYSL,18,LU                                              
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  M1,4,CT#PID,10,L                                                 
         SPEC  M2,4,CT#PID,10,LU                                                
         SPEC  M1,15,CT#PSWD,10,L                                               
         SPEC  M2,15,CT#PSWD,10,LU                                              
         SPEC  M1,29,CT#EFFD,18,L                                               
         SPEC  M2,29,CT#EFFD,18,LU                                              
         SPEC  M1,48,CT#LCHNG,12,L                                              
         SPEC  M2,48,CT#LCHNG,12,LU                                             
         SPEC  M1,63,CT#VALS,20,L                                               
         SPEC  M2,63,CT#VALS,20,LU                                              
         SPEC  END                                                              
         SPACE 2                                                                
REPSPEC1 DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,57,CT#SYSL,18,L                                               
         SPEC  H2,57,CT#SYSL,18,LU                                              
         SPEC  M1,1,132C'*'                                                     
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  END                                                              
* SEACSDICT                                                                     
       ++INCLUDE SEACSDICT                                                      
         EJECT                                                                  
                                                                                
***********************************************************************         
* FOLLOWING TABLES/SUBROUTINES ARE ONLY ADDRESSABLE VIA =A(.....)     *         
***********************************************************************         
         DROP  RB,RA                                                            
         EJECT                                                                  
*&&US                                                                           
***********************************************************************         
* ROUTINE TO VALIDATE CLIENT CODE LIST IN SYSTEM LIMIT ACCESS FIELD   *         
***********************************************************************         
         USING SA0REC,R2           R2=A(RECORD KEY)                             
VALCLA   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 AFVAL,SYSLACCH                                                   
         BE    VCLA010                                                          
         CLI   SYSLAC2H+5,0                                                     
         BNE   *+12                                                             
         CLI   SYSLAC3H+5,0                                                     
         BE    VCLAOK              NOTHING ON ALL LACC FIELDS, OK               
         MVC   FVMSGNO,=AL2(FVFNONE)   MISSING INPUT FROM 1ST FIELD             
         B     VCLANO                                                           
*                                                                               
VCLA010  MVI   FVINDX,0            SCAN 1ST INPUT FIELD LIST                    
         GOTO1 VSCANNER,APPARM,FVIHDR,(20,BLOCK)                                
         CLI   4(R1),0                                                          
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFTOOM)                                            
         B     VCLANO                                                           
         MVC   FLDCNT,4(R1)        SAVE # FIELDS IN SCAN (BLOCK)                
*                                                                               
         GOTO1 AFVAL,SYSLAC2H                                                   
         BE    *+12                                                             
         MVI   FLDCNT2,0           NOTHING IN 2ND FIELD                         
         B     VCLA015                                                          
*                                                                               
         MVI   FVINDX,0            SCAN 2ND INPUT FIELD LIST                    
         GOTO1 VSCANNER,APPARM,FVIHDR,(20,BLOCK2)                               
         CLI   4(R1),0                                                          
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFTOOM)                                            
         B     VCLANO                                                           
         MVC   FLDCNT2,4(R1)        SAVE # FIELDS IN SCAN (BLOCK2)              
*                                                                               
VCLA015  GOTO1 AFVAL,SYSLAC3H                                                   
         BE    *+12                                                             
         MVI   FLDCNT3,0           NOTHING IN 3RD FIELD                         
         B     VCLA020                                                          
*                                                                               
         MVI   FVINDX,0            SCAN 3RD INPUT FIELD LIST                    
         GOTO1 VSCANNER,APPARM,FVIHDR,(20,BLOCK3)                               
         CLI   4(R1),0                                                          
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFTOOM)                                            
         B     VCLANO                                                           
         MVC   FLDCNT3,4(R1)       SAVE # FIELDS IN SCAN (BLOCK2)               
*                                                                               
VCLA020  ZIC   RE,FLDCNT           # CLIENT CODE IN 1ST FIELD                   
         ZIC   RF,FLDCNT2          # CLIENT CODE IN 2ND FIELD                   
         ZIC   R1,FLDCNT3          # CLIENT CODE IN 3RD FIELD                   
         AR    R1,RE                                                            
         AR    R1,RF                                                            
*                                                                               
*        FASECRET READS THE CLIENT CODE LIST INTO THE AREA "SECCLAL",           
*        SO MAX # OF CLIENT CODES IS LIMITED BY THAT. (MAX = 40)                
*                                                                               
         CHI   R1,40                                                            
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFTOOM)                                            
         B     VCLANO                                                           
*                                                                               
         CHI   R1,1                ONLY 1 CLIENT CODE?                          
         BE    VCLAOK              LEFT IT FOR LATER CHECK IN VALLACC           
         STC   R1,COUNT            TOTAL # OF CLIENT CODES                      
*                                                                               
*        MERGE BLOCK,BLOCK2,BLOCK3 INTO CONTINUOUS AREA OF BLOCK+BLOCK1         
*                                                                               
         ZIC   R1,FLDCNT           # CLIENT CODE IN 1ST FIELD                   
         MHI   R1,L'BLOCK                                                       
         LA    R9,BLOCK(R1)        PT TO THE END OF 1ST LIST                    
*                                                                               
         ZIC   RF,FLDCNT2          # CLIENT CODE IN 2ND FIELD                   
         LTR   RF,RF                                                            
         BZ    VCLA030             NO NEED TO MERGE                             
*                                                                               
         MHI   RF,L'BLOCK2                                                      
         LA    RE,BLOCK2           RE=A(BLK2),RF=L'BLK2                         
         LR    R1,RF                                                            
         LR    R0,R9                                                            
         MVCL  R0,RE               R0=A(BLK),R1=L'BLK=L'BLK2                    
*                                                                               
VCLA030  ZIC   R1,FLDCNT2          # CLIENT CODE IN 2ND FIELD                   
         MHI   R1,L'BLOCK                                                       
         AR    R9,R1               PT TO THE END OF 1ST + 2ND LIST              
*                                                                               
         ZIC   RF,FLDCNT3          # CLIENT CODE IN 3RD FIELD                   
         LTR   RF,RF                                                            
         BZ    VCLA035             NO NEED TO MERGE                             
*                                                                               
         MHI   RF,L'BLOCK3                                                      
         LA    RE,BLOCK3           RE=A(BLK3),RF=L'BLK3                         
         LR    R1,RF                                                            
         LR    R0,R9                                                            
         MVCL  R0,RE               R0=A(BLK),R1=L'BLK=L'BLK3                    
*                                                                               
VCLA035  LA    R9,BLOCK                                                         
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM                                                        
         USING SACLAD,R3                                                        
         MVI   SACLAEL,SACLAELQ                                                 
         MVI   SACLALN,SACLALNQ                                                 
         MVC   SACLASYS,SYSTEM                                                  
         LA    R3,SACLACOD                                                      
         DROP  R3                                                               
         MVI   FVINDX,1            INITIALISE SCAN FIELD INDEX                  
*                                                                               
VCLA040  CLC   FVINDX,COUNT        PROCESS EACH FIELD IN SCAN LIST              
         BH    VCLA100                                                          
         CLI   1(R9),0             VALID SINGLE FIELD                           
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VCLA200                                                          
         CLI   0(R9),2             VALID FIELD LENGTH                           
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     VCLA200                                                          
         CLI   0(R9),3                                                          
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     VCLA200                                                          
         SR    RF,RF                                                            
         IC    RF,0(R9)                                                         
         GOTO1 VALAN,APPARM,12(R9),(RF)                                         
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VCLA200                                                          
*                                  TEST DUPE                                    
         LA    R4,BLOCK                                                         
VCLA045  CR    R4,R9                                                            
         BNL   VCLA050                                                          
         CLC   0(1,R4),0(R9)                                                    
         BNE   VCLA048                                                          
         SR    RF,RF                                                            
         IC    RF,0(R9)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R4),12(R9)                                                  
         BNE   VCLA048                                                          
         LLC   RF,COUNT            THIS IS A DUPE, REMOVE FROM COUNT            
         AHI   RF,-1                                                            
         STC   RF,COUNT                                                         
         B     VCLA085                                                          
VCLA048  LA    R4,L'BLOCK(R4)                                                   
         B     VCLA045                                                          
*                                                                               
VCLA050  CLI   SYSTEM,X'04'        PRINT                                        
         BNE   VCLA060                                                          
         MVC   0(3,R3),=CL3'  '                                                 
         SR    RF,RF                                                            
         IC    RF,0(R9)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),12(R9)                                                   
         B     VCLA070                                                          
*                                                                               
VCLA060  MVC   APWORK(3),=CL3'  '                                               
         SR    RF,RF                                                            
         IC    RF,0(R9)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   APWORK(0),12(R9)                                                 
         MVC   APPARM+4(4),=X'D9000A14' CLPACK                                  
         L     RF,VCOLY                                                         
         GOTO1 (RF),APPARM,0                                                    
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),APWORK,(R3)                                            
         CLI   0(R1),0                                                          
         BE    VCLA070                                                          
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VCLA200                                                          
VCLA070  OC    CLSTACC,CLSTACC                                                  
         BNZ   VCLA080                                                          
         MVC   CLSTACC(3),0(R3)                                                 
*                                                                               
VCLA080  LA    R3,L'SACLACOD(,R3)  BUMP TO NEXT POSITION IN ELEMENT             
         ZIC   RF,FVINDX           DO NEXT FIELD IN SCAN LIST                   
         LA    RF,1(RF)                                                         
         STC   RF,FVINDX                                                        
VCLA085  LA    R9,L'BLOCK(R9)                                                   
         B     VCLA040                                                          
*                                                                               
VCLA100  MVI   FVINDX,0                                                         
         LLC   R8,COUNT            TOTAL # OF CLIENT CODES                      
         MHI   R8,L'SACLACOD       LENGTH OF CLIENT                             
         AHI   R8,-L'SACLACOD      ONE IS INCLUDED IN FIXED PORTION             
         AHI   R8,SACLALNQ         ADD LENGTH OF FIXED PORTION                  
         STC   R8,APELEM+1                                                      
         LA    R3,APELEM+(SACLACOD-SACLAEL)                                     
         LLC   R8,COUNT            TOTAL # OF CLIENT CODES                      
         GOTO1 VXSORT,APPARM,(R3),(R8),L'SACLACOD,L'SACLACOD,0                  
         GOTO1 AADDELS,SA0REC      ADD CLIENT LIMIT ACCESS ELEMENT              
         B     VCLAOK                                                           
*                                                                               
VCLA200  LA    R1,SYSLACCH                                                      
         CLC   FVINDX,FLDCNT                                                    
         BNH   VCLA210             WITHIN 1ST FIELD                             
*                                                                               
         LA    R1,SYSLAC2H                                                      
         ZIC   RE,FVINDX                                                        
         ZIC   RF,FLDCNT                                                        
         SR    RE,RF                                                            
         STC   RE,FVINDX                                                        
         CLC   FVINDX,FLDCNT2                                                   
         BNH   VCLA210             WITHIN 2ND FIELD                             
*                                                                               
         LA    R1,SYSLAC3H                                                      
         ZIC   RF,FLDCNT2                                                       
         SR    RE,RF                                                            
         STC   RE,FVINDX           MUST WITHIN 3RD FIELD                        
*                                                                               
VCLA210  ST    R1,FVADDR                                                        
         B     VCLANO                                                           
*                                                                               
VCLAOK   SR    RC,RC                                                            
VCLANO   LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R2                                                               
                                                                                
***********************************************************************         
* VALIDATE ALPHANUMERIC FIELD                                         *         
*                                                                     *         
* NTRY: 0(R1) = FIELD                                                 *         
*       4(R1) = LENGTH                                                *         
*                                                                     *         
* EXIT: CC=ZERO IF OK                                                 *         
***********************************************************************         
VALAN    NTR1                                                                   
         ICM   RE,15,0(R1)                                                      
         SR    RF,RF                                                            
         IC    RF,7(R1)                                                         
         LTR   RF,RF                                                            
         BZ    VAANOK                                                           
VAAN010  CLI   0(RE),C'A'                                                       
         BL    VAAN020                                                          
         CLI   0(RE),C'Z'                                                       
         BH    VAAN020                                                          
         B     VAAN030                                                          
VAAN020  CLI   0(RE),C'0'                                                       
         BL    VAANNO                                                           
         CLI   0(RE),C'9'                                                       
         BH    VAANNO                                                           
VAAN030  LA    RE,1(RE)                                                         
         BCT   RF,VAAN010                                                       
         B     VAANOK                                                           
*                                                                               
VAANOK   SR    RC,RC                                                            
VAANNO   LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY CLIENT CODE LIST IN SYSTEM LIMIT ACCESS FIELD    *         
***********************************************************************         
         USING SA0REC,R2           R2=A(RECORD KEY)                             
DISCLA   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,BLOCK                                                         
         MVI   0(R4),C' '                                                       
         MVC   1(L'SYSLACC+L'SYSLAC2+L'SYSLAC3+1,R4),0(R4)                      
         LA    R3,SA0DATA                                                       
*                                                                               
DCLA010  CLI   0(R3),0             TEST END OF RECORD                           
         BE    DCLA100                                                          
         CLI   0(R3),SACLAELQ      X'E1' ONE CLIENT PER ELEMENT                 
         BE    DCLA030                                                          
DCLA020  LLC   RF,1(R3)            GET NEXT ELEMENT                             
         AR    R3,RF                                                            
         B     DCLA010                                                          
*                                                                               
         USING SACLAD,R3                                                        
DCLA030  CLC   SACLASYS,SYSTEM                                                  
         BNE   DCLA020                                                          
         LA    R8,SACLACOD         POINT TO FIRST CLIENT CODE                   
         B     DCLA050                                                          
*                                                                               
DCLA040  LA    R8,L'SACLACOD(,R8)  BUMP TO NEXT CLIENT IN ELEMENT               
         LLC   RF,1(R3)                                                         
         AR    RF,R3                                                            
         CR    RF,R8               ARE WE FINISHED WITH CLIENTS?                
         BNH   DCLA020             YES                                          
*                                                                               
DCLA050  OC    CLSTACC,CLSTACC                                                  
         BNZ   DCLA060                                                          
         MVC   CLSTACC(3),0(R8)                                                 
*                                                                               
DCLA060  MVC   APWORK(4),SPACES                                                 
         CLI   SYSTEM,X'04'        PRINT                                        
         BNE   DCLA070                                                          
         MVC   APWORK(3),0(R8)                                                  
         B     DCLA080                                                          
*                                                                               
DCLA070  MVC   APPARM+4(4),=X'D9000A15' CLUNPK                                  
         L     RF,VCOLY                                                         
         GOTO1 (RF),APPARM,0                                                    
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),(R8),APWORK                                            
*                                                                               
DCLA080  MVC   0(3,R4),APWORK                                                   
         CLI   2(R4),C' '                                                       
         BE    DCLA090                                                          
         MVI   3(R4),C','                                                       
         LA    R4,4(R4)                                                         
         B     DCLA040                                                          
*                                                                               
DCLA090  MVI   2(R4),C','                                                       
         LA    R4,3(R4)                                                         
         B     DCLA040                                                          
*                                                                               
DCLA100  OC    CLSTACC,CLSTACC                                                  
         BZ    DCLA110                                                          
         BCTR  R4,0                                                             
         MVI   0(R4),C' '                                                       
*                                                                               
         LA    RE,BLOCK+L'SYSLACC                                               
         CLI   0(RE),C' '                                                       
         BNE   *+14                                                             
         MVC   SYSLACC,BLOCK       ONLY 1 LINE OF CLIENT CODES                  
         B     DCLA110                                                          
*                                  FIND THE LAST "," IN 1ST LINE                
         CLI   0(RE),C','                                                       
         BE    *+8                                                              
         BCT   RE,*-8                                                           
*                                  MVC IN 1ST LINE UP TO THIS ","               
         LR    RF,RE                                                            
         LA    R1,BLOCK                                                         
         SR    RF,R1                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SYSLACC(0),BLOCK                                                 
*                                  MVC IN 2ND LINE AFTER THIS ","               
         LA    R1,1(RE)                                                         
         LA    RE,L'SYSLAC2(R1)                                                 
         CLI   0(RE),C' '                                                       
         BNE   *+14                                                             
         MVC   SYSLAC2,0(R1)       DON'T HAVE A 3RD LINE                        
         B     DCLA110                                                          
*                                  FIND THE LAST "," IN 2ND LINE                
         CLI   0(RE),C','                                                       
         BE    *+8                                                              
         BCT   RE,*-8                                                           
*                                  MVC IN 2ND LINE UP TO THIS ","               
         LR    RF,RE                                                            
         SR    RF,R1                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SYSLAC2(0),0(R1)                                                 
*                                  MVC IN 3RD LINE AFTER THIS ","               
         MVC   SYSLAC3,1(RE)                                                    
*                                                                               
DCLA110  XIT1                                                                   
         LTORG                                                                  
         DROP  R2,R3                                                            
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* SEACSWRK                                                                      
       ++INCLUDE SEACSWRK                                                       
         EJECT                                                                  
* DDLISTD                                                                       
LISTIOD  DSECT                                                                  
       ++INCLUDE DDLISTD                                                        
         EJECT                                                                  
* FAFACTS                                                                       
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
* FAXTRAINF                                                                     
       ++INCLUDE FAXTRAINF                                                      
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSF5D                                                       
         EJECT                                                                  
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSD5D                                                       
         ORG                                                                    
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSB5D                                                       
         ORG                                                                    
         EJECT                                                                  
*                                  WORKING STORAGE SAVED IN TWA                 
         ORG   SAVOVER                                                          
SAVSYS   DS    XL1                 SAVE CURRENT SYSTEM                          
SAVASF   DS    XL8                 SAVE AGENCY SYSTEM SECURITY FLAGS            
PLSTLEN  DS    XL2                                                              
APGMLST  DS    A                                                                
APGMLSTX DS    A                                                                
ASE      DS    A                                                                
APGM     DS    A                                                                
*                                                                               
* TABLE OF SYSTEM PROGRAM CODES AND CORRESPONDING DISPLAY ADDRESSES             
* EACH ENTRY IS 1 BYTE PROGRAM CODE, 4 BYTES DISPLAY ADDRESS (-RELO)            
*               4 BYTES DISPLAY ADDRESS (-RELO) OR 0 IF USER AUTH=NO            
*               2 BYTES PROGRAM AUTH CODE                                       
PSAVTAB  DS    64XL7               TABLE ENTRIES                                
PWDAFLAG DS    XL1                 PASSWORD FIELD ACCESS FLAG                   
*                                                                               
LISTD    DSECT                     ** LIST/SELECT LINE LAYOUT **                
LISTSELH DS    XL8                                                              
LISTSEL  DS    CL3                 ACTION FIELD                                 
LISTLINH DS    CL8                                                              
LISTLINX DS    CL8                                                              
LISTLIN  DS    0CL(L'LSTLIN1)                                                   
LISTPID  DS    CL8                 PERSONAL ID                                  
         DS    CL1                                                              
LISTPWD  DS    CL10                PASSWORD CODE                                
LISTTERM DS    CL1                 EXPIRED FLAG                                 
LISTDEF  DS    CL10                EFFECTIVE DATE                               
         DS    CL1                                                              
LISTSYST DS    CL30                VALID SYSTEM NAME LIST                       
         ORG   LISTDEF                                                          
LISTDEF1 DS    CL7                                                              
         DS    CL1                                                              
LISTDEF2 DS    CL7                                                              
         DS    CL1                                                              
         ORG   LISTLIN+L'LISTLIN                                                
         EJECT                                                                  
REPD     DSECT                                                                  
         ORG   REPP1                                                            
PRTLIN   DS    0CL(L'REPP1)        DSECT FOR SHORT REPORT                       
         DS    CL3                                                              
PRTPID   DS    CL8                 PERSONAL ID                                  
         DS    CL3                                                              
PRTCODE  DS    CL10                PASSWORD CODE                                
         DS    CL4                                                              
PRTDEF1  DS    CL8                 EFFECTIVE DATE                               
         DS    CL11                                                             
PRTDATE  DS    CL8                 ACTIVITY DATE                                
         DS    CL7                                                              
PRTSYS   DS    CL40                VALID SYSTEM NAME LIST                       
*                                                                               
         ORG   REPP1                                                            
PRLLIN1  DS    0CL(L'REPP1)        DSECT FOR FULL REPORT                        
PRLPIDD  DS    CL9                 PERSONAL ID DESCRIPTOR                       
         DS    CL1                                                              
PRLPID   DS    CL8                 PERSONAL ID                                  
         DS    CL3                                                              
PRLCODED DS    CL8                 PASSWORD CODE DESCRIPTOR                     
         DS    CL1                                                              
PRLCODE  DS    CL10                PASSWORD CODE                                
         DS    CL3                                                              
PRLDATED DS    CL12                EFFECTIVE DATE DESCRIPTOR                    
         DS    CL1                                                              
PRLDATE  DS    CL8                 EFFECTIVE DATE                               
         ORG   REPP2                                                            
PRLLIN2  DS    0CL(L'REPP2)                                                     
         DS    CL132                                                            
         ORG   REPP3                                                            
PRLLIN3  DS    0CL(L'REPP3)                                                     
         DS    CL132                                                            
         ORG   REPP4                                                            
PRLLIN4  DS    0CL(L'REPP4)                                                     
PRLUSRD  DS    CL9                 USER ID DESCRIPTOR                           
         DS    CL1                                                              
PRLUSR   DS    CL120               USER ID LIST                                 
         ORG   REPP5                                                            
PRLLIN5  DS    0CL(L'REPP5)                                                     
         DS    CL132                                                            
*                                                                               
PRLLINS  DSECT                                                                  
PRLSYS1  DS    CL132                                                            
PRLSYSD  DS    CL9                 SYSTEM DESCRIPTOR                            
         DS    CL1                                                              
PRLSYS   DS    CL8                 SYSTEM NAME                                  
         DS    CL4                                                              
PRLLIMD  DS    CL12                LIMIT ACCESS DESCRIPTOR                      
         DS    CL1                                                              
PRLLIM   DS    CL12                LIMIT ACCESS CODE                            
         DS    CL85                                                             
PRLSYS2  DS    CL132               SYSTEM PROGRAM ACCESS CODE LISTS             
PRLSYS3  DS    CL132                                                            
PRLSYSN  DS    CL132                                                            
PRLEND   DS    0C                                                               
*                                                                               
PGMLD    DSECT                     ** PROGRAM LIST DISPLAY DSECT **             
PGMLNAMH DS    CL8                                                              
PGMLNAM  DS    CL(L'SYSPN01)                                                    
PGMLVALH DS    CL8                                                              
PGMLVAL  DS    CL(L'SYSPV01)                                                    
PGMLLEN  EQU   *-PGMLD                                                          
PGMLINE  EQU   SYSPN05H-SYSPN01H                                                
         EJECT                                                                  
WORKD    DSECT                                                                  
         ORG   APLOCAL             ** DSECT TO COVER LOCAL W/S **               
*                                                                               
DUB      DS    D                                                                
SAVPARM  DS    8F                                                               
VSCINKEY DS    A                                                                
VXSORT   DS    A                                                                
FIELD    DS    F                                                                
RETURN   DS    A                                                                
WORK     DS    CL80                                                             
EXPFLAG  DS    CL1                 EXPIRED PERSON FLAG FOR GETSEL               
CURRPID  DS    CL1                 CURRENT PERSONAL ID FLAG FOR GETSEL          
ACCASF   DS    CL1                 ACC PROGRAM CONVERTED FLAG                   
IOERRSV  DS    XL(L'IOERR)         IOERR SAVE                                   
*                                                                               
SAVPID   DS    CL(L'SAPEPID)       SAVE LAST PID FOR LIST FILTER                
SAVPWD   DS    CL(L'SA0KCODE)      SAVE LAST PWD FOR LIST FILTER                
SAVPNUM  DS    CL(L'SA0KNUM)       SAVE LAST PWD NUMBER FOR LIST FILTER         
*                                                                               
PWDCODE  DS    CL10                PASSWORD CODE SAVE                           
PWDNUM   DS    XL2                 PASSWORD NUMBER SAVE                         
PIDREQD  DS    XL1                 PERSONID REQUIRED WITH PASSWD=Y              
PWDTOUT  DS    XL1                 PASSWORD TIMEOUT IN DAYS                     
PWDMINLN DS    XL1                 PASSWORD MINIMUM LENGTH                      
*                                                                               
TODAY    DS    XL2                 TODAYS DATE COMPRESSED BINARY                
TODAYC   DS    XL2                 COMPLEMENT OF IT                             
DATETIME DS    XL4                 DATE/TIME VALUE FROM DATTIM                  
DATETIMC DS    XL4                 DATE/TIME VALUE 1'S COMPLEMENTED             
CLSTACC  DS    XL4                 CLIENT LIST LIMIT ACCESS VALUE               
DATAGRP  DS    XL2                 DATA ACCESS GROUP NUMBER                     
*                                                                               
AGENCYID DS    XL2                 AGENCY ALPHA ID                              
*                                                                               
SA0KSAV  DS    XL(L'SA0KEY)        PASSWORD KEY SAVE                            
*                                                                               
SELOPT   DS    0X                  SELECT OPTIONS                               
SELPWD   DS    CL10                PASSWORD CODE FILTER                         
SELPWDL  DS    XL1                 PASSWORD CODE FILTER LENGTH                  
SELPWDF  DS    XL1                 PASSWORD FILTER FLAG                         
SELPID   DS    CL8                 PERSONAL-ID CODE FILTER                      
SELPIDL  DS    XL1                 PERSONAL-ID CODE FILTER LENGTH               
SELPIDSP DS    XL1                 PID CODE SPECIAL CHARACTER                   
SELKEYCL DS    XL1                 PID CODE LENGTH                              
SELDEFC  DS    XL2                 EFFECTIVE DATE COMPLEMENT FILTER             
SELDEFF  DS    XL1                 EFFECTIVE DATE PRESENT FLAG                  
SELUSR   DS    XL(L'SAID)          USER ID FILTER                               
SELAGR   DS    XL(L'SAAGAGR)       ACCESS GROUP FILTER                          
SELAGRF  DS    XL1                 ACCESS GROUP FILTER FLAG                     
SELOFF   DS    CL(L'SAOFOID)       OFFICE CODE FILTER                           
SELDID   DS    CL(L'SADPDID)       DEPARTMENT CODE FILTER                       
SELSYS   DS    XL1                 SYSTEM SELECT                                
SELSYSF  DS    XL1                 SYSTEM FILTER FOUND FLAG                     
SELPGM   DS    XL1                 PROGRAM SELECT                               
SELSRT   DS    CL1                 SORT SEQUENCE:                               
SELAPC   DS    CL8                 APPROVER GROUP FILTER                        
SELAPCF  DS    XL1                 APPROVER GROUP FILTER FLAG                   
*                                  C'1' - PERSONAL ID WITHIN DEPT               
*                                  C'2' - FIRST NAME                            
*                                  C'3' - FIRST NAME WITHIN DEPT                
*                                  C'4' - LAST NAME                             
*                                  C'5' - LAST NAME WITHIN DEPT                 
SELOPTL  EQU   *-SELOPT                                                         
*                                                                               
RPTWORK  DS    0CL85               REPORT WORK                                  
RPTPID   DS    CL22                PERSONAL ID                                  
RPTCODE  DS    CL10                PASSWORD CODE                                
RPTDEF1  DS    CL8                 EFFECTIVE DATE                               
RPTDEF2  DS    CL8                                                              
RPTDATE  DS    CL8                 ACTIVITY DATE                                
RPTSYS   DS    CL40                VALID SYSTEM NAMES LIST                      
RPTFMT   DS    CL1                 LONG/SHORT REPORT FORMAT                     
*                                                                               
GETSEQF  DS    XL1                 GETSEL READ SEQUENCE BROKEN FLAG             
FLDCNT   DS    XL1                 DISPLAY FIELD COUNT                          
FLDCNT2  DS    XL1                 DISPLAY FIELD COUNT                          
FLDCNT3  DS    XL1                 DISPLAY FIELD COUNT                          
COUNT    DS    XL1                 COUNTER                                      
PROGRAM  DS    CL1                 PROGRAM CODE                                 
PROGCTRY DS    XL1                 PROGRAM COUNTRY CODE                         
SVSYSALL DS    XL(L'SASYSALL)      SAVE OLD PROGRAM DEFAULT ACCESS CODE         
PACCVAL  DS    XL(L'SASYSALL)      PROGRAM ACCESS CODE                          
PGNAME   DS    CL8                 PROGRAM NAME SAVE                            
SYSTEM   DS    CL1                 SYSTEM SE NUMBER SAVE                        
SYCNT    DS    CL1                                                              
ASYSNAMS DS    A                                                                
SYSNAMS  DS    CL78                                                             
SYSNAMSL DS    XL1                                                              
SYSNUMS  DS    XL1                                                              
LINEADR  DS    A                   DISPLAY PROGRAM LIST LINE ADDRESS            
LINEFLD  DS    XL1                 DISPLAY PROGRAM LIST FIELD COUNT             
ASYSEL   DS    A                                                                
*                                                                               
BLKCNT   DS    XL1                                                              
BLOCK    DS    20CL32              SCANNER BLOCKS                               
BLOCK1   DS    20CL32                                                           
BLOCK2   DS    20CL32                                                           
BLOCK3   DS    20CL32                                                           
*                                                                               
XSORTBLK DS    64XL(10)                                                         
*                                                                               
LOCALX   EQU   *                                                                
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016SEACS0A   11/17/15'                                      
         END                                                                    
