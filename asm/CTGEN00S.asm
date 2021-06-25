*          DATA SET CTGEN00S   AT LEVEL 011 AS OF 05/01/02                      
*PHASE TA0B00A                                                                  
*                                                                               
*                                                                               
         TITLE 'CTGEN00 - FILE MAINTENANCE - CONTROLLER'                        
GEN00    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,**GEN0**,RA,R9,R8,CLEAR=YES,RR=RE                    
         LR    R7,RC                                                            
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         MVC   ASYSFACS,0(R1)      SAVE SYSFACS ADDRESS                         
*                                                                               
         ST    R1,ACFULL           SET BASE ADDRESSES                           
         ST    RE,ACRELO                                                        
         ST    RB,ACBASE1                                                       
         ST    RA,ACBASE2                                                       
         ST    R9,ACBASE3                                                       
         ST    R8,ACBASE4                                                       
         ST    RD,ACWORKA                                                       
*                                                                               
         L     RF,ASYSFACS                                                      
         L     RF,VCALLOV-SYSFACD(RF)                                           
         GOTO1 (RF),ACPARM,(1,0),0,0         LOAD TABLE PHASE (01)              
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                CAN'T LOAD CONTROLLER TABLES                 
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,1(R1)          A(LOAD POINT)                                
         LA    R0,(ACSELTAB-ACRECTAB)/4+1 N'TABLES                              
         SR    RE,RE                                                            
*                                                                               
GEN1     L     R1,0(RE,RF)                                                      
         AR    R1,RF                                                            
         ST    R1,ACRECTAB(RE)                                                  
         LA    RE,4(RE)                                                         
         BCT   R0,GEN1                                                          
*                                                                               
         LA    R1,CONADDRS         SET HOOK ADDRESSES                           
         SR    RE,RE                                                            
         LA    R0,CONADDRN                                                      
GEN2     L     RF,CONADDRS(RE)                                                  
         LTR   RF,RF                                                            
         BZ    *+12                                                             
         A     RF,ACRELO                                                        
         ST    RF,ACPHSLST(RE)                                                  
         LA    RE,4(RE)                                                         
         BCT   R0,GEN2                                                          
*                                                                               
         LA    R0,WORKD            SET I/O VARIABLES                            
         AH    R0,=Y(IODA1-WORKD)                                               
         ST    R0,ACIOADD                                                       
         LH    R0,=Y(IODA2-IODA1)                                               
         STH   R0,ACIOLEN                                                       
         MVI   ACIONUM,3                                                        
         MVI   ACIOIND,ACIOIDA+ACIOIWK                                          
*                                                                               
         MVC   ACSYSPGM,=X'0A0B'                                                
         MVI   ACHLPSCR,X'FE'      SET TWA VARIABLES                            
         MVI   ACTWAREC,1                                                       
         MVI   ACTWAACT,2                                                       
         MVI   ACTWAKEY,3                                                       
         MVI   ACTWAOPT,4                                                       
*                                                                               
*&&UK*&& MVI   SCDELIM,C'/'        OVERIDE NORMAL KEY-MERGE CHARACTER           
*                                  SET HOOK INDICATORS                          
         MVI   ACACTIND,ACHKAFT    ACTION HOOK AFTER                            
*                                                                               
         MVI   ACLSMIND,ACLSMISK   SET SELTAB WEXTENSION FLAG                   
*                                                                               
         LH    R0,=Y(GENTABH-TWAD)                                              
         STCM  R0,3,ACENDTWA                                                    
         LH    R1,=Y(IOAREA1-WORKD)                                             
         LA    R1,WORKD(R1)                                                     
         ST    R1,AIOAREA1                                                      
         LH    R1,=Y(IOAREA2-WORKD)                                             
         LA    R1,WORKD(R1)                                                     
         ST    R1,AIOAREA2                                                      
         LH    R1,=Y(IOAREA3-WORKD)                                             
         LA    R1,WORKD(R1)                                                     
         ST    R1,AIOAREA3                                                      
         LH    R1,=Y(APLOCAL-WORKD)                                             
         LA    R1,WORKD(R1)                                                     
         ST    R1,AAPLOCAL                                                      
*                                                                               
         L     R1,ACFULL           DUMMY-UP REGULAR FACPAK PLIST                
         MVC   ACPARM+00(4),28(R1)                                              
         MVC   ACPARM+04(4),20(R1)                                              
         MVC   ACPARM+08(4),00(R1)                                              
         MVC   ACPARM+12(4),04(R1)                                              
         MVC   ACPARM+16(4),12(R1)                                              
         LA    R1,ACPARM                                                        
         ST    R1,ACPARMA          SET A(REG PLIST) IN ACPARMA                  
*                                                                               
         L     RF,ACPARM+16        CALL GENERAL CONTROLLER                      
         L     RF,CGENERAL-COMFACSD(RF)                                         
         LA    R1,WORKD                                                         
         BASR  RE,RF                                                            
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* HOOK ROUTINES CALLED WITH ACMODE SETTINGS                           *         
*                                                                     *         
* NTRY - ACMODE=MODE FOR HOOK                                         *         
*                                                                     *         
* EXIT - FVMSGNO SET TO ERROR NUMBER WITH CC=NEQ ON ERROR             *         
*        FVMSGNO SET TO FVFOK WITH CC=EQ IF OK                        *         
***********************************************************************         
         SPACE 1                                                                
HOOK     NTR1  BASE=ACBASE1                                                     
         L     RA,ACBASE2                                                       
         L     R9,ACBASE3                                                       
         L     R8,ACBASE4                                                       
         USING TWAD,R5                                                          
         L     R5,ATWA                                                          
         L     RE,4(RD)                                                         
         MVC   0(4,RE),=C'+HOO'                                                 
*                                                                               
         CLI   TWAOMODE,TWAOLOAD   IGNORE SPOOF LOAD SCREEN MODE                
         BE    HOOKOKEX                                                         
*                                                                               
         LA    RE,GENRECH          SET CURSOR ON ERROR                          
         ST    RE,FVADDR                                                        
         ZIC   RF,ACMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
         B     HOOKX               ACMFRST - FIRST TIME FOR TRANSACTION         
         B     HOOKX               ACMRECR - PROCESS RECORD TYPE                
         B     HKVALACT            ACMACTR - PROCESS ACTION                     
         B     HOOKX               ACMKEYR - PROCESS KEY                        
         B     HOOKX               ACMOPTR - PROCESS OPTIONS                    
         B     HOOKX               ACMLFMR - PROCESS FILE MAINTENANCE           
         B     HOOKX               ACMLSMR - PROCESS LIST/SELECT SCREEN         
         B     HOOKX               ACMREPR - PROCESS REPORT                     
         B     HOOKX               ACMOTHR - PROCESS OTHER                      
         B     HOOKX               ACMRECA - PROCESS REC/ACTION (LIST)          
         B     HOOKX                                                            
         B     HOOKX                                                            
         B     HOOKX                                                            
         B     HOOKX                                                            
         B     HOOKX                                                            
         B     HOOKX               ACMLAST - LAST TIME MODE                     
*                                                                               
HOOKOKEX MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     HOOKX                                                            
HOOKBADX MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     HOOKX                                                            
HOOKX    CLC   FVMSGNO,=AL2(FVFOK) SET CONDITION CODE FOR CALLER                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ACTION HOOK - AFTER GENERAL'S RECORD/ACTION CHECKING                *         
***********************************************************************         
         SPACE 1                                                                
* CHECK FOR DDS RETRICTED ACCESS RECORD-ACTIONS                                 
*                                                                               
HKVALACT EQU   *                                                                
*&&US                                                                           
         ICM   RE,15,AMIXNTRY      GET A(MIXTABLE ENTRY)                        
         BNZ   *+6                                                              
         DC    H'00'                                                            
         USING MIXTABD,RE                                                       
         TM    MIXINDS,MIXIREP     IGNORE IN REPORT MODE                        
         BO    HKACT100                                                         
         TM    MIXUSER,RESTRCTQ    CHECK FOR RESTRICTED ACCESS                  
         BZ    HKACT100                                                         
         GOTO1 VGETFACT,APPARM,0                                                
         L     R2,0(R1)            R1 = A(SYSTEM DEFINITION BLOCK)              
         USING FACTSD,R2                                                        
         CLI   FASYSID,1                                                        
         BE    HKACT100            IGNORE IN TEST SYSTEM                        
         TM    FATFLAG,X'08'       CHECK PERSONAL PASSWORD                      
         BZ    HKACT010              IF NOT ACCESS NOT ALLOWED                  
         CLC   CUAALF,=C'**'       TEST SPECIAL AGENCY ALPHA                    
         BE    HKACT100              IF SO ACCESS ALLOWED                       
*                                    ELSE ERROR MESSAGE                         
HKACT010 MVC   FVMSGNO,=AL2(CE#SECLO)                                           
         B     HOOKX                                                            
         DROP  RE,R2                                                            
*&&                                                                             
HKACT100 EQU   *                                                                
*                                                                               
HKACTX   B     HOOKOKEX                                                         
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* COMMON ROUTINES AVAILABLE TO CONTROLLER AND OVERLAYS                *         
*                                                                     *         
* NTRY - R1=A(PARAMETER LIST)                                         *         
*        RF=ROUTINE NUMBER (HIGH ORDER BYTE)                          *         
*                                                                     *         
* EXIT - FVMSGNO SET TO ERROR NUMBER WITH CC=NEQ ON ERROR             *         
*        FVMSGNO SET TO FVFOK WITH CC=EQ IF OK                        *         
***********************************************************************         
         SPACE 1                                                                
ROUTS    NTR1  BASE=ACBASE1,WORK=(RC,RWRKX-RWRKD),LABEL=NO                      
         USING RWRKD,RC            RC=A(LOCAL W/S)                              
         L     RA,ACBASE2                                                       
         L     R9,ACBASE3                                                       
         L     R8,ACBASE4                                                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVC   RIOSAVE,IOAREA      SAVE I/O AREA                                
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     ROUTSBR(RF)                                                      
*                                                                               
ROUTSX   CLC   RIOSAVE,IOAREA      TEST ANY I/O EXECUTED                        
         BE    *+14                                                             
         OI    APINDS,APILRERD     YES - SET APPLICATION FLAG                   
         MVC   IOAREA(L'RIOSAVE),RIOSAVE                                        
         CLC   FVMSGNO,=AL2(FVFOK) SET CONDITION CODE FOR CALLER                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* TABLE OF BRANCH ADDRESSES TO ROUTS ROUTINES                         *         
***********************************************************************         
         SPACE 1                                                                
ROUTSBR  B     ADDELS                                                           
         B     DELELS                                                           
         B     GETELS                                                           
         B     SETACT                                                           
         B     DISACT                                                           
         B     GETACT                                                           
         B     ROUTSX              N/D                                          
         B     ROUTSX              N/D                                          
         B     ROUTSX              N/D                                          
         B     VALSYS                                                           
         B     DISSYS                                                           
         B     VALSE                                                            
         B     DISSE                                                            
         B     VALPGM                                                           
         B     DISPGM                                                           
         B     VALTXT                                                           
         B     DISTXT                                                           
         B     VALLNG                                                           
         B     DISLNG                                                           
         B     TXTFLT                                                           
         B     DISLACC                                                          
         B     VALLACC                                                          
         B     DISPSHP                                                          
         B     BLDSHP                                                           
         B     ROUTSX              N/D                                          
         B     ROUTSX              N/D                                          
         B     ROUTSX              N/D                                          
         B     ROUTSX              N/D                                          
         B     ROUTSX              N/D                                          
         B     ROUTSX              N/D                                          
         B     ROUTSX              N/D                                          
         B     ROUTSX              N/D                                          
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD AN ELEMENT TO A RECORD                               *         
*                                                                     *         
* NTRY - R1=A(RECORD TO ADD ELEMENT TO)                               *         
*        APELEM CONTAINS ELEMENT                                      *         
***********************************************************************         
         SPACE 1                                                                
ADDELS   LR    R0,R1                                                            
         L     RE,ARECNTRY                                                      
         LA    RF,CTFILE                                                        
         CLI   RECUSER-RECTABD(RE),CTFILEQ                                      
         BE    *+8                                                              
         LA    RF,GENFIL                                                        
         GOTO1 VHELLO,RPARM,(C'P',(RF)),(R0),APELEM,0                           
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
ADDELSX  B     ROUTSX                                                           
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO DELETE AN ELEMENT FROM A RECORD                          *         
*                                                                     *         
* NTRY - R1=A(RECORD TO DELETE ELEMENT FROM)                          *         
*        APELEM CONTAINS ELEMENT CODE OF ELEMENT TO BE DELETED        *         
***********************************************************************         
         SPACE 1                                                                
DELELS   LR    R0,R1                                                            
         L     RE,ARECNTRY                                                      
         LA    RF,CTFILE                                                        
         CLI   RECUSER-RECTABD(RE),CTFILEQ                                      
         BE    *+8                                                              
         LA    RF,GENFIL                                                        
         GOTO1 VHELLO,RPARM,(C'D',(RF)),(APELEM,(R0)),                 *        
               (APELEM+1,APELEM+2)                                              
         CLI   12(R1),6            TEST ELEMENT NOT FOUND                       
         BE    DELELSX                                                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
DELELSX  B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET AN ELEMENT IN A RECORD                               *         
*                                                                     *         
* NTRY - R1=A(RECORD TO GET ELEMENT FROM)                             *         
*        APELEM CONTAINS ELEMENT CODE AND DATA TO SEARCH FOR          *         
* EXIT - APPARM CONTAINS ADDRESS OF ELEMENT OR ZEROES IF NOT FOUND    *         
***********************************************************************         
         SPACE 1                                                                
GETELS   LR    R0,R1                                                            
         L     RE,ARECNTRY                                                      
         LA    RF,CTFILE                                                        
         CLI   RECUSER-RECTABD(RE),CTFILEQ                                      
         BE    *+8                                                              
         LA    RF,GENFIL                                                        
         GOTO1 VHELLO,RPARM,(C'G',(RF)),(APELEM,(R0)),                 *        
               (APELEM+1,APELEM+2)                                              
         XC    APPARM(4),APPARM                                                 
         CLI   12(R1),0                                                         
         BNE   *+10                                                             
         MVC   APPARM(4),12(R1)                                                 
GETELSX  B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD AN ACTIVITY ELEMENT                                  *         
*                                                                     *         
* NTRY - R1=A(RECORD TO ADD ELEMENT)                                  *         
***********************************************************************         
         SPACE 1                                                                
SETACT   OC    ACTEL,ACTEL         TEST ACTVITY ELEMENT FOUND                   
         BNZ   SETACT1                                                          
         GOTO1 AGETACT             NO - EXTRACT IT                              
*                                                                               
SETACT1  MVC   ACTEL,APELEM                                                     
         LA    R2,ACTEL                                                         
         CLI   ACTEL,1             TEST CTFILE/GENFIL                           
         BNE   SETACT4                                                          
         USING CTACTD,R2                                                        
         MVI   CTACTEL,CTACTELQ                                                 
         MVI   CTACTLEN,5                                                       
         MVC   CTACTDT,ASBDAT                                                   
         B     SETACT6                                                          
*                                                                               
         USING GACTELD,R2                                                       
SETACT4  CLI   ACTEL,GACTELQ       TEST CTFILE/GENFIL                           
         BNE   SETACTX             UNKNOWN                                      
         MVI   GACTLN,GACTLNQ                                                   
         OC    GACTADT,GACTADT                                                  
         BNZ   SETACT5                                                          
         MVC   GACTADT,ASBDAT                                                   
         MVC   GACTAAG,CUAALF                                                   
         TM    CUSTAT,CUSPER                                                    
         BZ    SETACT5                                                          
         MVC   GACTAPW,CUPASS                                                   
SETACT5  MVC   GACTCDT,ASBDAT                                                   
         MVC   GACTCAG,CUAALF                                                   
         TM    CUSTAT,CUSPER                                                    
         BZ    *+10                                                             
         MVC   GACTCPW,CUPASS                                                   
         MVI   ACTEL,GACTELQ                                                    
         DROP  R2                                                               
*                                                                               
SETACT6  XC    APELEM,APELEM                                                    
         MVC   APELEM(1),ACTEL     SET ACTIVITY ELEMENT CODE                    
         GOTO1 ADELELS             DELETE AND RE-ADD ELEMENT                    
         MVC   APELEM(L'ACTEL),ACTEL                                            
         GOTO1 AADDELS                                                          
         XC    ACTEL,ACTEL         CLEAR ACTIVITY ELEMENT                       
*                                                                               
SETACTX  B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY ACTIVITY DATE                                    *         
*                                                                     *         
* NTRY - R1=A(RECORD)                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISACT   GOTO1 AGETACT             EXTRACT ACTIVITY ELEMENT                     
         BNE   DISACTX                                                          
         LA    R2,ACTEL            POINT TO ACTIVITY ELEMENT                    
         CLI   ACTEL,1                                                          
         BNE   DISACT2                                                          
         USING CTACTD,R2                                                        
         MVC   FVXTRA(L'ACTMSG1),ACTMSG1                                        
         LA    R3,FVXTRA+L'ACTMSG1+1                                            
         GOTO1 VDATCON,RPARM,(3,CTACTDT),(8,(R3))                               
*&&UK*&& OI    0(R3),X'F0'                                                      
         B     DISACTX                                                          
*                                                                               
DISACT2  CLI   ACTEL,GACTELQ                                                    
         BNE   DISACTX                                                          
         USING GACTELD,R2                                                       
         MVC   FVXTRA(L'ACTMSG1),ACTMSG1                                        
         LA    R3,FVXTRA+L'ACTMSG1+1                                            
         GOTO1 VDATCON,RPARM,(3,GACTCDT),(8,(R3))                               
*&&UK*&& OI    0(R3),X'F0'                                                      
         OC    GACTCAG,GACTCAG     TEST FOR AGENCY CODE                         
         BZ    DISACTX                                                          
         MVI   8(R3),C'('                                                       
         MVI   11(R3),C')'                                                      
         MVC   9(L'GACTCAG,R3),GACTCAG                                          
         DROP  R2                                                               
*                                                                               
DISACTX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET ACTIVITY ELEMENT INTO ACTEL                          *         
*                                                                     *         
* NTRY - R1=A(RECORD)                                                 *         
* EXIT - ACTEL CONTAINS ACTIVITY ELEMENT OR BINARY ZEROES             *         
*        APELEM CONTAINS ACTIVITY ELEMENT CODE                        *         
*        FVMSGNO=ZERO IF ACTIVITY ELEMENT NOT FOUND                   *         
*        CC=EQUAL IF ELEMENT FOUND, NOT EQUAL IF NOT FOUND            *         
***********************************************************************         
         SPACE 1                                                                
GETACT   XC    APELEM,APELEM                                                    
         MVI   APELEM,1                                                         
         XC    ACTEL,ACTEL                                                      
         L     RE,ARECNTRY                                                      
         CLI   RECUSER-RECTABD(RE),CTFILEQ                                      
         BE    *+8                                                              
         MVI   APELEM,GACTELQ                                                   
         GOTO1 AGETELS                                                          
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         ICM   R1,15,APPARM                                                     
         BNZ   *+14                                                             
         XC    FVMSGNO,FVMSGNO     SET IF NO ACTIVITY ELEMENT                   
         B     GETACTX                                                          
         MVC   ACTEL,0(R1)         SAVE ELEMENT IN WORKING STORAGE              
GETACTX  B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SYSTEM NAME                                     *         
*                                                                     *         
* NTRY - R1=A(FIELD HEADER OF SYSTEM FIELD)                           *         
* EXIT - APWORK+0(1)=NATIVE SYSTEM NUMBER                             *         
*        APWORK+1(1)=EQUATED SYSTEM NUMBER                            *         
*        APPARM(4)=A(SYSTEM LIST TABLE ENTRY)                         *         
*        CC=EQUAL IF SYSTEM NAME IS VALID, NOT EQUAL IF INVALID       *         
* NOTE - IF SHORTENED NAME WAS INPUT, FULL NAME WILL BE OUTPUT        *         
***********************************************************************         
         SPACE 1                                                                
VALSYS   MVI   FVMAXL,L'SYSLNAME                                                
         GOTO1 AFVAL                                                            
         BNE   VALSYSX                                                          
         LA    RE,SYSGEN           DUMMY SYSLIST ENTRY FOR GENERAL              
         USING SYSLSTD,RE                                                       
         ZIC   RF,FVXLEN           RF=L'INPUT-1                                 
         EX    RF,*+12                                                          
         BE    VALSYS3                                                          
         B     *+10                                                             
         CLC   SYSLNAME(0),FVIFLD  CLC C'GENERAL',FVIFLD                        
*                                                                               
         L     RE,ASYSLST                                                       
         LA    RE,6(RE)            RE=A(SYSTEM LIST)                            
*                                                                               
VALSYS2  CLI   0(RE),0             TEST E-O-T                                   
         BE    VALSYS4                                                          
         EX    RF,*+8              MATCH INPUT TO TABLE                         
         B     *+10                                                             
         CLC   SYSLNAME(0),FVIFLD                                               
         BE    *+12                                                             
         LA    RE,SYSLLEN(RE)      NO - BUMP TO NEXT TABLE ENTRY                
         B     VALSYS2                                                          
VALSYS3  MVC   APWORK(2),SYSLNUM   RETURN SYSTEM NUMBERS                        
         ST    RE,APPARM           RETURN A(SYSLST ENTRY)                       
         CLC   SYSLNAME,FVIFLD     TEST FULL NAME DISPLAYED                     
         BE    VALSYSX                                                          
         L     R1,FVADDR                                                        
         MVC   L'FVIHDR(L'SYSLNAME,R1),SYSLNAME                                 
         OI    FVOIND-FVIHDR(R1),FVOXMT                                         
         B     VALSYSX                                                          
*                                                                               
VALSYS4  MVC   FVMSGNO,=AL2(FVFESYS)         INVALID SYSTEM                     
*                                                                               
VALSYSX  B     ROUTSX                                                           
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY SYSTEM NAME                                      *         
*                                                                     *         
* NTRY - R1=A(SYSTEM NUMBER)                                          *         
* EXIT - APWORK+0(7)=SYSTEM NAME                                      *         
*      - APPARM+0(4)=A(SYSLIST ENTRY) - 0 = UNKNOWN                   *         
***********************************************************************         
         SPACE 1                                                                
DISSYS   SR    RF,RF                                                            
         ICM   RF,1,0(R1)          RF=SYSTEM NUMBER                             
         BNZ   DISSYS1                                                          
         LA    RE,SYSGEN           DUMMY SYSLIST ENTRY FOR GENERAL              
         MVC   APWORK(L'SYSLNAME),SYSLNAME-SYSLSTD(RE)                          
         STCM  RE,15,APPARM                                                     
         B     DISSYSX                                                          
DISSYS1  L     RE,ASYSLST                                                       
         LA    RE,6(RE)                                                         
         USING SYSLSTD,RE          RE=A(SYSTEM LIST)                            
DISSYS2  CLI   0(RE),0             TEST E-O-T                                   
         BE    DISSYS4                                                          
         CLM   RF,1,SYSLNUM        MATCH ON SYSTEM NUMBER                       
         BE    *+12                                                             
         LA    RE,SYSLLEN(RE)      NO - BUMP TO NEXT TABLE ENTRY                
         B     DISSYS2                                                          
         MVC   APWORK(L'SYSLNAME),SYSLNAME                                      
         STCM  RE,15,APPARM        RETURN A(SYSLIST ENTRY)                      
         B     DISSYSX                                                          
*                                                                               
DISSYS4  MVI   APWORK,C' '         OUTPUT SYSTEM NUMBER                         
         MVC   APWORK+1(L'SYSLNAME-1),APWORK                                    
         XC    APPARM(4),APPARM    NO A(SYSLIST ENTRY)                          
         CVD   RF,RDUB                                                          
         OI    RDUB+7,X'0F'                                                     
         UNPK  RWORK(3),RDUB                                                    
         CLI   RWORK,C'0'                                                       
         BE    *+14                                                             
         MVC   APWORK(3),RWORK                                                  
         B     DISSYSX                                                          
         CLI   RWORK+1,C'0'                                                     
         BE    *+14                                                             
         MVC   APWORK(2),RWORK+1                                                
         B     DISSYSX                                                          
         MVC   APWORK(1),RWORK+2                                                
*                                                                               
DISSYSX  B     ROUTSX                                                           
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE SE NAME                                         *         
*                                                                     *         
* NTRY - R1=A(FIELD HEADER OF SYSTEM FIELD)                           *         
* EXIT - APWORK+0(1)=SE NUMBER                                        *         
*        APWORK+1(1)=CALL OVERLAY SYSTEM NUMBER                       *         
*        APPARM(4)=A(SELIST ENTRY)                                    *         
*        CC=EQUAL IF SE NAME IS VALID, NOT EQUAL IF INVALID           *         
***********************************************************************         
         SPACE 1                                                                
VALSE    MVI   FVMAXL,L'SENAME                                                  
         GOTO1 AFVAL                                                            
         BNE   VALSEX                                                           
         L     R1,ASYSFACS                                                      
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1                                                       
         CLC   SENAME,FVIFLD       MATCH ON SE NAME                             
         BE    VALSE2                                                           
         BXLE  R1,RE,*-10                                                       
         MVC   FVMSGNO,=AL2(FVFESYS)         INVALID SYSTEM                     
         B     VALSEX                                                           
*                                                                               
VALSE2   MVC   APWORK(2),SESYS                                                  
         ST    R1,APPARM                                                        
*                                                                               
VALSEX   B     ROUTSX                                                           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY SE NAME                                          *         
*                                                                     *         
* NTRY - R1=A(SE NUMBER)                                              *         
* EXIT - APWORK+0(7)=SE NAME OR 'SYS=XX' IF NOT FOUND                 *         
*        APPARM(4)=A(SELIST ENTRY) OR ZEROES IF NOT FOUND             *         
***********************************************************************         
         SPACE 1                                                                
DISSE    MVC   RWORK(1),0(R1)                                                   
         L     R1,ASYSFACS                                                      
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1                                                       
         CLC   SESYS,RWORK         MATCH ON SE NUMBER                           
         BE    DISSE2                                                           
         BXLE  R1,RE,*-10                                                       
         XC    APPARM(4),APPARM                                                 
         MVI   APWORK,C' '                                                      
         MVC   APWORK+1(L'SENAME-1),APWORK                                      
         MVC   APWORK(4),=C'SYS='                                               
         XOUT  RWORK,APWORK+4,1                                                 
         B     DISSEX                                                           
*                                                                               
DISSE2   MVC   APWORK(L'SENAME),SENAME                                          
         ST    R1,APPARM                                                        
*                                                                               
DISSEX   B     ROUTSX                                                           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE PROGRAM NAME                                    *         
*                                                                     *         
* NTRY - R1=A(PARAMETER LIST) - P1/B0=SYSTEM OVERLAY NUMBER           *         
*                               P1/B1-3=A(PROGRAM FIELD HEADER)       *         
* EXIT - APWORK+0(1)=PROGRAM NUMBER                                   *         
*        APPARM(4)=A(PROGRAM LIST ENTRY)                              *         
*        CC=EQUAL IF PROGRAM NAME IS VALID, NOT EQUAL IF INVALID      *         
* NOTE - IF SHORTENED NAME WAS INPUT, FULL NAME WILL BE OUTPUT        *         
***********************************************************************         
         SPACE 1                                                                
VALPGM   MVC   RWORK(1),0(R1)      SAVE SYSTEM NUMBER                           
         L     R1,0(R1)            POINT TO INPUT FIELD HEADER                  
         MVI   FVMAXL,L'PGMNAME                                                 
         GOTO1 AFVAL               TEST FOR INPUT                               
         BNE   VALPGMX                                                          
         L     R1,ASYSFACS                                                      
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1          FIND SELIST ENTRY                            
         CLC   SEOVSYS,RWORK                                                    
         BE    VALPGM2                                                          
         BXLE  R1,RE,*-10                                                       
         MVC   FVMSGNO,=AL2(FVFESYS)         INVALID SYSTEM                     
         B     VALPGMX                                                          
*                                                                               
VALPGM2  L     R1,SEPGMS                                                        
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING PGMLSTD,R1          R1=A(PROGRAMS LIST)                          
         ZIC   R2,FVXLEN           R2=INPUT LENGTH-1                            
VALPGM4  EX    R2,*+8                                                           
         B     *+10                                                             
         CLC   PGMNAME(0),FVIFLD   MATCH INPUT TO PROGRAM NAME                  
         BE    VALPGM6                                                          
         BXLE  R1,RE,VALPGM4                                                    
         MVC   FVMSGNO,=AL2(FVFEPGM)         INVALID PROGRAM                    
         B     VALPGMX                                                          
*                                                                               
VALPGM6  ST    R1,APPARM           SET A(PGMLST ENTRY)                          
         MVC   APWORK(1),PGMNUM                                                 
         CLC   PGMNAME,FVIFLD      TEST FULL NAME DISPLAYED                     
         BE    VALPGMX                                                          
         L     RE,FVADDR           NO - DISPLAY FULL NAME                       
         MVC   L'FVIHDR(L'PGMNAME,RE),PGMNAME                                   
         OI    FVOIND-FVIHDR(RE),FVOXMT                                         
VALPGMX  B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY PROGRAM NAME                                     *         
*                                                                     *         
* NTRY - R1=A(PARAMETER LIST) - P1/B0=SYSTEM OVERLAY NUMBER           *         
*                               P1/B1-3=A(PROGRAM NUMBER)             *         
* EXIT - APWORK(7)=PROGRAM NAME                                       *         
*        APPARM(4)=A(PROGRAM LIST ENTRY) OR ZERO IF NOT FOUND         *         
***********************************************************************         
         SPACE 1                                                                
DISPGM   MVC   RWORK(1),0(R1)      SAVE SYSTEM NUMBER                           
         L     R1,0(R1)            POINT TO INPUT FIELD HEADER                  
         MVC   RWORK+1(1),0(R1)                                                 
         L     R1,ASYSFACS                                                      
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1          FIND SELIST ENTRY                            
         CLC   SEOVSYS,RWORK                                                    
         BE    *+12                                                             
         BXLE  R1,RE,*-10                                                       
         B     DISPGM2                                                          
         L     R1,SEPGMS                                                        
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING PGMLSTD,R1          FIND PGMLST ENTRY                            
         CLC   PGMNUM,RWORK+1                                                   
         BE    *+12                                                             
         BXLE  R1,RE,*-10                                                       
         B     DISPGM2                                                          
         ST    R1,APPARM                                                        
         MVC   APWORK(L'PGMNAME),PGMNAME                                        
         B     DISPGMX                                                          
*                                                                               
DISPGM2  MVI   APWORK,C' '                                                      
         MVC   APWORK+1(L'PGMNAME-1),APWORK                                     
         MVC   APWORK(4),=C'PGM='                                               
         XOUT  RWORK+1,APWORK+4,1                                               
         XC    APPARM(4),APPARM                                                 
*                                                                               
DISPGMX  B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE A SYSTEMS TEXT NUMBER                           *         
*                                                                     *         
* NTRY - R1=A(PARAMETER LIST) - P1/B0=SYSTEM OVERLAY NUMBER           *         
*                               P1/B1-3=A(FIELD HEADER OF NUMBER)     *         
* EXIT - APHALF=TEXT NUMBER                                           *         
*        APWORK=TEXT                                                  *         
***********************************************************************         
         SPACE 1                                                                
VALTXT   MVC   RWORK+0(1),0(R1)                                                 
         L     R1,0(R1)                                                         
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL                                                            
         BNE   VALTXTX                                                          
         TM    FVIIND,FVINUM                                                    
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     VALTXTX                                                          
         MVC   APHALF,SCFULL+2                                                  
         LA    R1,ACPARM                                                        
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTMSGNO,APHALF                                                   
         MVI   GTMAXL,L'APWORK                                                  
         LA    R0,APWORK                                                        
         STCM  R0,7,GTAOUT                                                      
         MVI   GTMTYP,GTMSCR                                                    
         OI    GT1INDS,GT1OWRK                                                  
         MVC   GTMSYS,RWORK                                                     
         GOTO1 VGETTXT                                                          
         TM    GT1INDS,GT1NOMSG                                                 
         BZ    *+10                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
VALTXTX  B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY SYSTEMS TEXT                                     *         
*                                                                     *         
* NTRY - R1=A(PARAMETER LIST) - P1/B0=SYSTEM OVERLAY NUMBER           *         
*                               P1/B1-3=A(TEXT NUMBER)                *         
* EXIT - APWORK=TEXT                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISTXT   MVC   RWORK+0(1),0(R1)                                                 
         L     R1,0(R1)                                                         
         MVC   RWORK+1(2),0(R1)                                                 
         LA    R1,ACPARM                                                        
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTMSGNO,RWORK+1                                                  
         MVI   GTMAXL,L'APWORK                                                  
         LA    R0,APWORK                                                        
         STCM  R0,7,GTAOUT                                                      
         MVI   GTMTYP,GTMSCR                                                    
         OI    GT1INDS,GT1OWRK                                                  
         MVC   GTMSYS,RWORK+0                                                   
         GOTO1 VGETTXT                                                          
         TM    GT1INDS,GT1NOMSG                                                 
         BZ    DISTXTX                                                          
         MVI   APWORK,C'?'                                                      
         MVC   APWORK+1(L'APWORK-1),APWORK                                      
DISTXTX  B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE LANGUAGE                                        *         
*                                                                     *         
* NTRY - R1=A(FIELD HEADER OF LANGUAGE FIELD)                         *         
* EXIT - APWORK+0(1)=LANGUAGE NUMBER                                  *         
*        APPARM(4)=A(LANGUAGE TABLE ENTRY)                            *         
*        CC=EQUAL IF LANGUAGE NAME IS VALID, NOT EQUAL IF INVALID     *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
VALLNG   MVI   FVMAXL,L'LANGFUL                                                 
         GOTO1 AFVAL                                                            
         BNE   VALLNGX                                                          
         L     R1,ALANG            A(LANGUAGE TABLE)                            
         LH    RE,0(R1)            L'TABLE ENTRIES                              
         L     RF,2(R1)            A(LANGUAGE TABLE END)                        
         LA    R1,6(R1)            A(FIRST TABLE ENTRY)                         
         USING LANGTABD,R1         R1=A(LANGUAGE TABLE)                         
         ZIC   R2,FVXLEN           R2=L'INPUT-1                                 
VALLNG2  CLI   FVILEN,L'LANGSHR    TEST IF LONGER THAN SHORT NAME               
         BH    VALLNG4                                                          
         EX    R2,*+8              MATCH INPUT TO SHORT NAME                    
         B     *+10                                                             
         CLC   LANGSHR(0),FVIFLD   ENGLISH SHORT NAME                           
         BE    VALLNG6                                                          
*                                                                               
VALLNG4  EX    R2,*+8              MATCH INPUT TO FULL NAME                     
         B     *+10                                                             
         CLC   LANGFUL(0),FVIFLD   ENGLISH FULL NAME                            
         BE    *+12                                                             
         BXLE  R1,RE,VALLNG2       NO - BUMP TO NEXT TABLE ENTRY                
         B     VALLNG8                                                          
*                                                                               
VALLNG6  MVC   APWORK(L'LANGCODE),LANGCODE  RETURN LANGUAGE CODE                
         ST    R1,APPARM           RETURN A(LANGUAGE TABLE ENTRY)               
         B     VALLNGX                                                          
*                                                                               
VALLNG8  MVC   FVMSGNO,=AL2(FVFELANG)        INVALID LANGUAGE                   
*                                                                               
VALLNGX  B     ROUTSX                                                           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY LANGUAGE NAME                                    *         
*                                                                     *         
* NTRY - R1=A(LANGUAGE CODE) - MUST BE VALID                          *         
* EXIT - APWORK(3)=LANGUAGE SHORT NAME                                *         
*        APPARM(4)=A(LANGUAGE TABLE ENTRY)                            *         
***********************************************************************         
         SPACE 1                                                                
DISLNG   MVC   RWORK(1),0(R1)      SAVE LANGUAGE CODE                           
         L     R1,ALANG                                                         
         LH    RF,0(R1)            L'TABLE ENTRIES                              
         ZIC   R0,RWORK            ENTRY NUMBER IN TABLE                        
         MR    RE,R0               INDEX INTO TABLE                             
         L     RE,2(R1)            A(LANGUAGE TABLE END)                        
         LA    R1,6(RF,R1)         A(TABLE ENTRY)                               
         USING LANGTABD,R1         R1=A(LANGUAGE TABLE)                         
         CR    R1,RE               CHECK IN TABLE                               
         BL    *+6                                                              
         DC    H'0'                INVALID LANGUAGE CODE                        
         CLC   LANGCODE,RWORK      CHECK WE GOT THE CORRECT ENTRY               
         BE    *+6                                                              
         DC    H'0'                INDEX INTO TABLE WRONG                       
         ST    R1,APPARM                                                        
         MVI   APWORK,C' '                                                      
         MVC   APWORK+1(L'APWORK-1),APWORK                                      
         MVC   APWORK(L'LANGSHR),LANGSHR                                        
*                                                                               
DISLNGX  B     ROUTSX                                                           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FILTER ON A TEXT STRING                                  *         
* NTRY -                                                              *         
* PARAM1 BYTE0    - L'FILTER TEXT                                     *         
*        BYTE1-3  - A(FILTER TEXT)                                    *         
* PARAM2 BYTE0    - L'TEXT TO FILTER ON                               *         
*        BYTE1-3  - A(TEXT TO FILTER ON)                              *         
* TEXT FORMAT  ABC A*C (ABC  '*' = WILDCARD '(' = SCAN FOR TEXT       *         
*                                                                     *         
* EXIT - CC  =  IF TEXT FOUND                                         *         
*        CC  <> IF TEXT NOT FOUND                                     *         
***********************************************************************         
         SPACE 1                                                                
TXTFLT   L     R4,0(R1)            R4=A(TEXT TO FILTER WITH)                    
         L     R3,4(R1)            R3=A(TEXT TO FILTER ON)                      
         SR    RF,RF                                                            
         ICM   RF,1,0(R1)          RF=L'FILTER TEXT                             
         BZ    TXTFLTOK            NO FILTER                                    
         LR    R1,R3               GET L'FIELD TO FILTER ON                     
         SRL   R1,24                                                            
         CLM   R1,1,=AL1(L'ACWORK) PREVENT OVERFLOW                             
         BNH   *+8                                                              
         LA    R1,L'ACWORK                                                      
*                                                                               
         MVI   ACWORK,C' '         CONVERT FIELD TO FILTER ON TO U/C            
         MVC   ACWORK+1(L'ACWORK-1),ACWORK                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    ACWORK(0),0(R3)                                                  
         LA    R3,ACWORK                                                        
         LA    R1,1(R1)            SET BACK TO FULL LENGTH                      
         CLI   0(R4),C'('          CONVENTION TO SCAN FOR FILTER                
         BE    TXTF020                                                          
         CR    R1,RF               TEST FIELD LONG ENOUGH TO BOTHER             
         BL    TXTFLTNE                                                         
         LR    RE,R4               RE=A(FILTER TEXT)                            
         SPACE 1                                                                
TXTF010  CLI   0(RE),C'*'          IGNORE WILDCARD CHARACTERS                   
         BE    *+14                                                             
         CLC   0(1,RE),0(R3)                                                    
         BNE   TXTFLTNE                                                         
*                                                                               
         LA    R3,1(R3)            BUMP TO NEXT CHAR IN FILTER AND TEXT         
         LA    RE,1(RE)                                                         
         BCT   RF,TXTF010                                                       
         B     TXTFLTOK            FILTER MATCHES DATA                          
         SPACE 1                                                                
TXTF020  SH    RF,=Y(2)            DROP THE '(' AND GET EX L'FILTER             
         BM    TXTFLTNE                                                         
         LR    RE,R1                                                            
         SR    RE,RF               RE=NUMBER OF COMPARES REQUIRED               
         BNP   TXTFLTNE            TEXT NOT LONG ENOUGH TO BOTHER               
         SPACE 1                                                                
TXTF030  EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),1(R4)                                                    
         BE    TXTFLTOK                                                         
         LA    R3,1(R3)                                                         
         BCT   RE,TXTF030                                                       
         SPACE 1                                                                
TXTFLTNE MVC   FVMSGNO,=AL2(FVFNOTV) FORCE CC NOT EQUAL (NOT FOUND)             
*                                                                               
TXTFLTOK B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
*   BUILD LIMIT ACCESS IN APWORK                                      *         
*                                                                     *         
* NTRY - R1=A(PARAMETER LIST) - P1/B0=SYSTEM OVERLAY NUMBER           *         
*                               P1/B1-3=A(LIMIT ACCESS INTERNAL CODE) *         
* EXIT - APWORK+0(6)=LIMIT ACCESS CODE                                *         
***********************************************************************         
         SPACE 1                                                                
DISLACC  MVC   RWORK(1),0(R1)      SAVE SYSTEM NUMBER                           
         L     R3,0(R1)            POINT TO LIMIT ACCESS CODE                   
         XC    APWORK,APWORK                                                    
         OC    0(L'CTSYSLMT,R3),0(R3)                                           
         BZ    DLACCX                                                           
         MVC   APWORK(L'CTSYSLMT),0(R3)                                         
*                                                                               
         CLI   RWORK,9             MEDIABASE                                    
         BNE   DLACC02                                                          
         GOTO1 VHEXOUT,APPARM,(R3),APWORK,4,=C'N'                               
*                                                                               
DLACC02  EQU   *                                                                
*                                                                               
*                                                                               
*&&UK                                                                           
         CLI   RWORK,4             TEST UK/MEDIA                                
         BNE   DLACC30                                                          
         LA    RE,APWORK                                                        
         LR    R4,R3                                                            
         LA    R2,4                                                             
DLACC10  SR    R0,R0                255,255,99,99 MAX VALUES                    
         IC    R0,0(R4)                                                         
         EDIT  (R0),(3,0(RE)),ALIGN=LEFT,ZERO=NOBLANK,                 *        
               WRK=RWORK,DUB=RDUB                                               
         AR    RE,R0     '                                                      
         MVI   0(RE),C','                                                       
         LA    RE,1(RE)                                                         
         LA    R4,1(R4)                                                         
         BCT   R2,DLACC10                                                       
         BCTR  RE,0                                                             
         MVI   0(RE),C' '                                                       
         B     DLACCX                                                           
*&&                                                                             
*&&US                                                                           
         CLI   RWORK,3             TEST NETWORK                                 
         BE    *+12                                                             
         CLI   RWORK,2             TEST SPOT                                    
         BNE   DLACC20                                                          
*                                                                               
         MVC   APPARM+4(4),=X'D9000A15' CLUNPK                                  
         L     RF,VCOLY                                                         
         GOTO1 (RF),APPARM,0                                                    
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),(R3),APWORK                                            
*                                                                               
DLACC20  EQU   *                                                                
*&&                                                                             
DLACC30  CLI   RWORK,14            TEST PERSONNEL                               
         BNE   DLACCX                                                           
         XC    APWORK,APWORK                                                    
         OC    2(2,R3),2(R3)                                                    
         BZ    DLACCX                                                           
         LA    RE,APWORK           FORMAT N(NN)-N(NN)                           
         ZIC   R0,2(R3)                                                         
         EDIT  (R0),(3,0(RE)),ALIGN=LEFT,WRK=RWORK,DUB=RDUB                     
         AR    RE,R0                                                            
         MVI   0(RE),C'-'                                                       
         ZIC   R0,3(R3)                                                         
         EDIT  (R0),(3,1(RE)),ALIGN=LEFT,WRK=RWORK,DUB=RDUB                     
*                                                                               
DLACCX   B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
*   VALIDATE LIMIT ACCESS FIELD IN FVAREA                             *         
*                                                                     *         
* NTRY - R1=A(PARAMETER LIST) - P1/B0=SYSTEM OVERLAY NUMBER           *         
*                               P1/B1-3=A(LIMIT ACCESS INTERNAL CODE) *         
* EXIT - CC=EQUAL IF LIMIT ACCESS VALID, NOT EQUAL IF INVALID         *         
***********************************************************************         
         SPACE 1                                                                
VALLACC  MVC   RWORK(1),0(R1)      SAVE SYSTEM NUMBER                           
         L     R3,0(R1)            POINT TO LIMIT ACCESS CODE                   
         MVC   0(L'CTSYSLMT,R3),FVIFLD                                          
*                                                                               
         CLI   RWORK,9             MEDIABASE SYSTEM                             
         BNE   VLAC02                                                           
         XC    0(L'CTSYSLMT,R3),0(R3)                                           
         CLI   FVILEN,8            MUST BE LESS THAN 8                          
         BH    VAEFTL                                                           
         TM    FVILEN,X'01'        AND MUST BE EVEN                             
         BNZ   VAEIIF                                                           
         ZIC   R0,FVILEN                                                        
         XC    APDUB,APDUB                                                      
         GOTO1 VHEXIN,APPARM,FVIFLD,APDUB,(R0),0                                
         OC    APPARM+12(4),APPARM+12                                           
         BZ    VAEIIF                                                           
         MVC   0(4,R3),APDUB                                                    
*                                                                               
VLAC02   EQU   *                                                                
*                                                                               
         CLI   RWORK,14            TEST PERSONNEL SYSTEM                        
         BNE   VLAC10                                                           
         XC    0(L'CTSYSLMT,R3),0(R3)                                           
         GOTO1 VSCANNER,APPARM,FVIHDR,(2,RBLOCK),C',=*-'                        
         XC    8(4,R1),8(R1)       CLEAR SPECIAL SCAN CHARACTERS                
         CLI   4(R1),0                                                          
         BE    VAEIIF                                                           
         CLI   4(R1),1             FORMAT=N(NN)-N(NN)                           
         BNE   VAEIIF                                                           
         CLI   RBLOCK,0            VALIDATE LOWER LIMIT                         
         BE    VAEIIF                                                           
         TM    RBLOCK+2,X'80'                                                   
         BZ    VAEFNN                                                           
         OC    RBLOCK+4(4),RBLOCK+4                                             
         BZ    VAEIIF                                                           
         OC    RBLOCK+4(3),RBLOCK+4                                             
         BNZ   VAEIIF                                                           
         MVC   2(1,R3),RBLOCK+7                                                 
         MVC   3(1,R3),RBLOCK+7                                                 
         CLI   RBLOCK+1,0          TEST UPPER LIMIT GIVEN                       
         BE    VLACX                                                            
         MVI   FVINDX,2                                                         
         TM    RBLOCK+3,X'80'      VALIDATE UPPER LIMIT                         
         BZ    VAEFNN                                                           
         OC    RBLOCK+8(4),RBLOCK+8                                             
         BZ    VAEIIF                                                           
         OC    RBLOCK+8(3),RBLOCK+8                                             
         BNZ   VAEIIF                                                           
         MVC   3(1,R3),RBLOCK+11                                                
         CLC   3(1,R3),2(R3)                                                    
         BL    VAEIIF                                                           
         B     VLACX                                                            
VLAC10   DS    0H                                                               
*&&UK                                                                           
         CLI   RWORK,4             TEST UK/MEDIA                                
         BNE   VLACX                                                            
         GOTO1 VSCANNER,APPARM,FVIHDR,(5,RBLOCK),X'6B5E6BFF'                    
         CLI   4(R1),4             FORMAT=N(NN),N(NN),N(N),N(N)                 
         BNE   VAEIIF                                                           
         LA    RF,RBLOCK                                                        
         LR    R1,R3                                                            
         LA    R0,4                                                             
         LA    RE,1                                                             
VLAC20   STC   RE,FVINDX                                                        
         CLI   0(RF),0                                                          
         BE    VAEMIF                                                           
         CLI   0(RF),3                                                          
         BH    VAEFTL                                                           
         TM    2(RF),X'80'                                                      
         BZ    VAEFNN                                                           
         CLI   FVINDX,2            MAXVALUES=255,255,99,99                      
         BH    VLAC22                                                           
         CLC   6(2,RF),=H'255'                                                  
         BH    VAEFTB                                                           
         B     VLAC24                                                           
VLAC22   CLC   6(2,RF),=H'99'                                                   
         BH    VAEFTB                                                           
VLAC24   MVC   0(1,R1),7(RF)                                                    
         LA    RF,L'RBLOCK(RF)                                                  
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,VLAC20                                                        
         MVI   FVINDX,0                                                         
         B     VLACX                                                            
*&&                                                                             
*&&US                                                                           
         CLI   RWORK,3             TEST NETWORK                                 
         BE    *+12                                                             
         CLI   RWORK,2             TEST SPOT                                    
         BNE   VLAC30                                                           
*                                                                               
         MVC   APPARM+4(4),=X'D9000A14' CLPACK                                  
         L     RF,VCOLY                                                         
         GOTO1 (RF),APPARM,0                                                    
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),FVIFLD,(R3)                                            
         CLI   0(R1),0                                                          
         BNE   VAEIIF                                                           
*                                                                               
VLAC30   EQU   *                                                                
*&&                                                                             
*                                                                               
VLACX    B     ROUTSX                                                           
*                                  GETTXT MESSAGE # ERROR EXITS                 
VAEIIF   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     ROUTSX              INPUT FIELD ERROR                            
VAEFTL   MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     ROUTSX              INPUT FIELD TOO LONG                         
VAEFNN   MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     ROUTSX              INPUT FIELD NOT NUMERIC                      
VAEFTS   MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     ROUTSX              INPUT FIELD TOO SHORT                        
VAEFNH   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     ROUTSX              INPUT FIELD ERROR                            
VAEMIF   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     ROUTSX              MISSING FIELD                                
VAEFTB   MVC   FVMSGNO,=AL2(CE#FVMAX)                                           
         MVC   FVOSYS,ASSYSE                                                    
         B     ROUTSX              FIELD VALUE EXCEEDS MAXIMUM                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY SHIPPING INFORMATION THAT STARTS WITH NUMERIC TO SCREEN FLD *         
*                                                                     *         
*    P1 = R2 = ADDRESS OF SCREEN FIELD DATA                           *         
*    P2 = R3 = ADDRESS OF ELEMENT DATA                                *         
*    P3 = R4 = LENGTH OF ELEMENT DATA                                 *         
*         BYTE 0 = RETURNED LENGTH OF SCREEN FIELD DATA               *         
***********************************************************************         
         SPACE 1                                                                
DISPSHP  EQU   *                                                                
*&&US                                                                           
         LM    R2,R4,0(R1)                                                      
         EDIT  (2,(R3)),(4,(R2)),ALIGN=LEFT,ZERO=NOBLANK,              *        
               WRK=APWORK,DUB=APDUB                                             
         LA    RE,2                                                             
         AR    R2,R0               BUMP R2 PAST DISPLAYED NUMERIC               
         LR    RF,R4                                                            
         SR    RF,RE                                                            
         AR    RF,R0               RF = FINAL LENGTH OF SCREEN DATA             
         SR    R4,RE                                                            
         C     R4,=F'0'                                                         
         BE    DISPSHP1                                                         
         BCTR  R4,0                                                             
         EX    R4,*+8              MOVE IN REST OF INFO                         
         B     *+10                                                             
         MVC   0(0,R2),2(R3)                                                    
DISPSHP1 STC   RF,8(R1)            RETURN FINAL LENGTH                          
*&&                                                                             
         B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* LOCAL SUBROUITNE                                                    *         
* BUILD SHIPPING INFORMATION THAT STARTS WITH NUMERIC FROM SCREEN FLD *         
*                                                                     *         
*    P1 = ADDRESS OF SCREEN FIELD DATA                                *         
*    P2 = ADDRESS OF ELEMENT DATA                                     *         
*    P3 = LENGTH OF SCREEN FIELD DATA                                 *         
*    BYTE 0 = RETURNED LENGTH OF ELEMENT DATA                         *         
***********************************************************************         
         SPACE 1                                                                
BLDSHP   EQU   *                                                                
*&&US                                                                           
         L     R2,0(R1)                                                         
         L     R4,8(R1)                                                         
         LR    R3,R2               SCAN UNTIL NON NUMERIC                       
BLDSHP1  CLI   0(R3),C'0'                                                       
         BL    BLDSHP2                                                          
         CLI   0(R3),C'9'                                                       
         BH    BLDSHP2                                                          
         LA    R3,1(R3)                                                         
         B     BLDSHP1                                                          
BLDSHP2  SR    R3,R2               R3 = LENGTH OF NUMERIC                       
         LR    RF,R4                                                            
         SR    RF,R3               RF = LENGTH OF REST OF INFO                  
         LA    R4,2(RF)            R4 = FINAL LENGTH OF ELEMENT DATA            
         LA    RE,0(R2,R3)         ELEMENT DATA                                 
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  APDUB,0(0,R2)                                                    
         CVB   R0,APDUB            R0 = VALUE OF NUMERIC                        
         MVI   8(R1),0                                                          
         C     R0,=F'9999'                                                      
         BH    BLDSHPX             ERROR - FIELD TO BIG                         
         STC   R4,8(R1)            OTHERWISE RETURN FINAL DATA LENGTH           
         L     R3,4(R1)                                                         
         STCM  R0,3,0(R3)                                                       
         C     RF,=F'0'                                                         
         BE    BLDSHPX                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8              MOVE IN REST OF INFO                         
         B     *+10                                                             
         MVC   2(0,R3),0(RE)                                                    
*&&                                                                             
BLDSHPX  B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ENTRY POINT FOR OPTION ROUTINES                                     *         
***********************************************************************         
         SPACE 1                                                                
OBASE    NTR1  BASE=ACBASE1,LABEL=NO                                            
         L     RA,ACBASE2                                                       
         L     R9,ACBASE3                                                       
         L     RE,4(RD)                                                         
         MVC   0(4,RE),=C'+OBA'                                                 
         SRL   RF,24               DETERMINE OPTION NUMBER (1 BASED)            
         SLL   RF,2                                                             
         CH    RF,=AL2(OPTMAX)                                                  
         BNH   *+2(RF)                                                          
         DC    H'0'                OPTION NUMBER OUSIDE RANGE                   
         SPACE 2                                                                
OBRANCH1 B     VALSCAN                                                          
         B     VALPROG                                                          
         B     VALSUB                                                           
         B     VALUSER                                                          
         B     VALSYSO                                                          
         B     VALPER                                                           
OPTMAX   EQU   *-OBRANCH1                                                       
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE OPTION SCAN=                                    *         
*                                                                     *         
* EXIT - SCAN STRINGS IN STANDARD SCANNER FORMAT IN APSCANB           *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
*                                                                               
VALSCAN  GOTO1 VSCANNER,ACPARM,FVIHDR,(5,AIOAREA2),X'6B7E4EFF'                  
         SR    R0,R0                                                            
         ICM   R0,1,4(R1)                                                       
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALSCANX                                                         
         ZIC   R0,4(R1)                                                         
         L     R1,AIOAREA2                                                      
         LA    RE,SCWORK                                                        
*                                                                               
VALSCAN2 SR    RF,RF                                                            
         ICM   RF,1,0(R1)          RF=L'TEXT STRING                             
         STC   RF,0(RE)                                                         
         BZ    VALSCAN4                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RE),12(R1)      MOVE TEXT STRING                             
         LA    RE,1(RE,RF)                                                      
VALSCAN4 LA    R1,32(R1)                                                        
         BCT   R0,VALSCAN2                                                      
         MVI   0(RE),0                                                          
*                                                                               
VALSCANX B     EXIT                                                             
*                                                                               
VALPROG  SR    RF,RF               MOVE WHOLE FIELD                             
         IC    RF,FVTLEN           PROG NAME VALIDATED                          
         BCTR  RF,0                LATER WHEN SYSTEM KNOWN                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SCWORK(0),FVIHDR                                                 
*                                                                               
VALPROGX B     EXIT                                                             
*                                                                               
VALSUB   SR    RF,RF               JUST MOVE DATA                               
         IC    RF,FVXLEN           NO VALIDATION REQUIRED                       
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SCWORK(0),FVIFLD                                                 
*                                                                               
VALSUBX  B     EXIT                                                             
*                                                                               
VALUSER  SR    RF,RF               JUST MOVE DATA                               
         IC    RF,FVXLEN           NO VALIDATION REQUIRED                       
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SCWORK(0),FVIFLD                                                 
*                                                                               
VALUSERX B     EXIT                                                             
*                                                                               
VALSYSO  L     RE,ASYSLST                                                       
         LA    RE,6(RE)            RE=A(SYSTEM LIST)                            
         USING SYSLSTD,RE                                                       
         ZIC   RF,FVXLEN           RF=L'INPUT-1                                 
*                                                                               
VSYSO1   CLI   0(RE),0             TEST E-O-T                                   
         BE    VSYSO2                                                           
         EX    RF,*+8              MATCH INPUT TO TABLE                         
         B     *+10                                                             
         CLC   SYSLNAME(0),FVIFLD                                               
         BE    *+12                                                             
         LA    RE,SYSLLEN(RE)      NO - BUMP TO NEXT TABLE ENTRY                
         B     VSYSO1                                                           
*                                                                               
         MVC   SCWORK(1),SYSLNUM   RETURN SYSTEM NUMBER                         
         B     VSYSOX                                                           
*                                                                               
VSYSO2   MVC   FVMSGNO,=AL2(FVFESYS)         INVALID SYSTEM                     
*                                                                               
VSYSOX   B     EXIT                                                             
*                                                                               
VALPER   GOTO1 VPERVAL,SCPARM,(FVILEN,FVIFLD),(X'20',SCWORK)                    
         NI    4(R1),(X'FF'-X'04')           ONE DATE IS OK                     
         CLI   4(R1),X'00'                                                      
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFINVDT)        INVALID DATE                       
         B     VALPERX                                                          
*                                                                               
         MVC   SCWORK(6),SCWORK+28                                              
*                                                                               
VALPERX  B     EXIT                                                             
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
ACTMSG1  DC    C'. Updated'                                                     
         SPACE 1                                                                
SYSGEN   DC    X'0000',C'GENERALGEN',C'  ',X'0000' DUMMY SYSLIST ENTRY          
         SPACE 1                                                                
CTFILE   DC    C'CTFILE '                                                       
GENFIL   DC    C'GENFIL '                                                       
         SPACE 1                                                                
CONADDRS DS    0F                  ** ADDRESS OF ROUTINES ETC. **               
         DC    A(PHASES)                                                        
         DC    A(HOOK)                                                          
         DC    A(OBASE)                                                         
         DC    A(ROUTS)                                                         
CONADDRN EQU   (*-CONADDRS)/L'CONADDRS                                          
         SPACE 1                                                                
PHASES   DS    0X                  ** LOADED CORERES PHASES **                  
         DC    X'FF'                                                            
         EJECT                                                                  
* CTGENWRK                                                                      
       ++INCLUDE CTGENWRK                                                       
         EJECT                                                                  
***********************************************************************         
* ROUTS S/R LOCAL W/S                                                 *         
***********************************************************************         
         SPACE 1                                                                
RWRKD    DSECT                                                                  
RIOSAVE  DS    XL(IOAREAX-IOAREA)                                               
RDUB     DS    D                                                                
RWORK    DS    XL64                                                             
RPARM    DS    8F                                                               
RHALF    DS    H                                                                
RBYTE    DS    XL1                                                              
RFLAG    DS    XL1                                                              
RBLOCK   DS    20CL32                                                           
RIO      DS    2000C                                                            
RWRKX    EQU   *                                                                
         SPACE 1                                                                
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011CTGEN00S  05/01/02'                                      
         END                                                                    
