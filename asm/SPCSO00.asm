*          DATA SET SPCSO00    AT LEVEL 045 AS OF 08/06/03                      
*PHASE T21800A,+0                                                               
*INCLUDE MEDGET                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE BINSRCH2                                                               
*INCLUDE DPTRD                                                                  
*INCLUDE EQVRD                                                                  
*INCLUDE KHDUMMY                                                                
         TITLE 'T21800 - CHILD SPOT OPERATION CONTROLLER'                       
T21800   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LENWORK,T21800,R7,RR=R2,CLEAR=YES                                
         ST    R2,RELO                                                          
         LR    R9,R1                                                            
         LR    R8,RC                                                            
         USING SPOOLD,R8                                                        
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
         LA    RC,SPOOLEND                                                      
         USING GEND,RC                                                          
         ST    R9,SYSPARMS                                                      
         LA    R9,IO                                                            
         AH    R9,=Y(LENIOAS)      GRABBING 3 I/O AREAS PLUS LABELS             
         USING SYSD,R9                                                          
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RD,SYSRD                                                         
         ST    R7,BASER7                                                        
         GOTO1 GENCON,DMCB,(R8)                                                 
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* INITIALIZE SYSTEM DEPENDENT VALUES *                                          
         SPACE 1                                                                
SYSINIT  NTR1                                                                   
         LA    R2,SYSV                                                          
         LA    R3,SYSVCON                                                       
         LA    R4,NVTYPES                                                       
         SPACE 1                                                                
SYS2     L     R1,0(R3)            RELOCATE SYSTEM VTYPES                       
         A     R1,RELO                                                          
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,SYS2                                                          
         SPACE 1                                                                
         LA    R2,VCOMMON          SET UP COMMON ENTRIES                        
         SR    R3,R3                                                            
         LA    R4,SYSCOMM                                                       
         LA    R5,VCOUNT                                                        
         SPACE 1                                                                
SYS4     ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,SYS4                                                          
         SPACE 1                                                                
         XC    DMCB,DMCB           OBTAIN CORE-RESIDENT ADDRESSES               
         LA    R2,CORETAB                                                       
         LA    R0,CORES            COUNTER                                      
         LA    R4,COREFACS         POINT TO ADDRESS AREA                        
         L     R1,SYSPARMS                                                      
         L     R1,8(R1)                                                         
         USING COMFACSD,R1                                                      
         L     RF,CCALLOV                                                       
         DROP  R1                                                               
         LA    R1,DMCB                                                          
         MVC   DMCB+4(3),=X'D9000A'                                             
         SPACE 1                                                                
SYS6     MVC   DMCB+7(1),0(R2)                                                  
         GOTO1 (RF),(R1),0                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   0(4,R4),DMCB        SAVE MODULE ADDRESS                          
         LA    R2,1(R2)            NEXT MODULE NUMBER                           
         LA    R4,4(R4)            NEXT ADDRESS                                 
         BCT   R0,SYS6                                                          
*                                                                               
         MVC   MSPACK,GOMSPACK     SET A(MSPACK) AND A(MSUNPK)                  
         MVC   MSUNPK,GOMSUNPK                                                  
         SPACE 1                                                                
* SET SYSTEM DEPENDENT VALUES *                                                 
         SPACE 1                                                                
         MVI   SYSTEM,C'F'         FILE MAINT                                   
         MVI   MAXIOS,NIOS         USES 3 I/O AREAS                             
         MVC   SIZEIO,=AL4(LIOS)   EACH I/O IS 2000 BYTES                       
         MVC   SYSDUMMY,VDUMMY     END OF SYSTEM BASE                           
         MVC   GETUSER,GETAGY      ROUTINE TO GET USER NAME AND ADDRESS         
         MVC   LKEY,=H'13'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'     USUALLY SPOTFILE                             
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         MVI   GETMSYS,23          USES GETMSG FOR SYSTEM 23                    
         MVC   LWORK,=Y(LENWORK)   SET WORK AREA LENGTH                         
         MVC   RCPROG(2),=C'CS'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D9021800'    PRESET FOR SYSTEM CALLOVS               
         LA    R1,RECACT           RECORD/ACTION DIRECTORY                      
         ST    R1,ARECACT                                                       
         LA    R1,SVSTART          SET SAVED STORAGE START                      
         ST    R1,ASTARTSV                                                      
         MVI   NTWA,X'81'          SAVE ONE LARGE TWA                           
         OI    GENSTAT1,USKYMRG    SET MERGE KEY FIELDS LIST/DISPLAY            
         B     XIT                                                              
         EJECT                                                                  
* SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY *                              
         SPACE 1                                                                
         DS    0H                                                               
VCOMMON  NTR1  BASE=SYSRB,LABEL=*                                               
         ST    RD,COMMRD                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     RA,ATWA                                                          
         L     R8,ASPOOLD                                                       
         L     R7,BASER7                                                        
         SRL   RF,24                                                            
         B     VBRANCH(RF)                                                      
         SPACE 1                                                                
VBRANCH  B     VUSER                                                            
         B     VMED                                                             
         B     VCLI                                                             
         B     VMKT                                                             
         B     VSTAT                                                            
         B     VMEST                                                            
         B     VSEST                                                            
         B     VREF                                                             
         B     VCLEARF                                                          
         B     VCLRACC                                                          
         B     VBLDCAL                                                          
         B     VCMPGOL                                                          
         B     VCMPCGOL                                                         
         B     VTOTFULL                                                         
         B     VFINDOVR                                                         
         B     VCMPNTP                                                          
         B     VCMPMGOL                                                         
         B     VMSPACK                                                          
         B     VMSUNPK                                                          
         SPACE 1                                                                
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         SPACE 1                                                                
BASER7   DC    A(0)                                                             
         EJECT                                                                  
* GET AGENCY NAME/ADDRESS FROM CONTROL FILE ID RECORD *                         
         SPACE 1                                                                
VUSER    L     R3,ATWA                                                          
         CLI   29(R3),0            TEST FIRST TIME                              
         BE    *+14                YES - READ DATA                              
         MVC   USERNAME(66),SVUSER ELSE MOVED SAVED DATA                        
         B     VUSER10                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4           BUILD ID RECORD KEY                          
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),10(R3)  FROM TWA                                     
         MVC   AIO,AIO1                                                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD '),=C'CTFILE ',KEY,AIO                 
*                                                                               
         L     R4,AIO                                                           
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'36'        ORIGIN DETAILS                               
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
         USING CTORGD,R6                                                        
         MVC   USERNAME,CTORGNAM                                                
         MVC   USERADDR,CTORGADD                                                
         MVC   SVUSER(66),USERNAME SAVE FOR FUTURE REF                          
         DROP  R6                                                               
*                                                                               
VUSER10  L     RF,SYSPARMS         DETERMINE NUMBER OF PFKEY PRESSED            
         L     RF,0(RF)                                                         
         USING TIOBD,RF                                                         
         ZIC   R0,TIOBAID                                                       
         C     R0,=F'12'                                                        
         BNH   *+8                                                              
         S     R0,=F'12'                                                        
         STC   R0,PFKEY                                                         
*                                                                               
         XC    NTPPERC,NTPPERC     INIT NTP PERCENTAGE TO NULL                  
*                                                                               
         B     XIT                                                              
         SPACE 1                                                                
* VALIDATE MEDIA CODE *                                                         
         SPACE 1                                                                
VMED     GOTO1 ANY                                                              
         XC    KEY,KEY             GET AGENCY RECORD                            
         LA    R4,KEY                                                           
         USING AGYKEY,R4                                                        
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,AGENCY                                                   
         DROP  R4                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         USING AGYKEY,R6                                                        
         MVC   SVAPROF,AGYPROF     SAVE AGENCY PROFILE                          
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
VMED2    BAS   RE,NEXTEL                                                        
         BNE   VMED4                                                            
         CLC   2(1,R6),8(R2)                                                    
         BNE   VMED2                                                            
         MVC   QMED,8(R2)          SAVE INPUT MEDIA CODE                        
         MVC   BAGYMD,3(R6)        DIG OUT AGENCY/MEDIA                         
         MVC   MEDNM,4(R6)         MEDIA NAME                                   
         MVC   MEDCAPT,14(R6)      AND CAPTION                                  
         B     XIT                                                              
         SPACE 1                                                                
VMED4    MVI   ERROR,INVMED                                                     
         B     TRAPERR                                                          
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE CLIENT - ON EXIT QCLT AND BCLT CONTAIN VALUES                        
         SPACE 1                                                                
VCLI     GOTO1 ANY                 CLIENT                                       
*                                                                               
         MVI   ERROR,INVCLI                                                     
         MVC   QCLT(3),WORK                                                     
         CLI   5(R2),3                                                          
         BH    TRAPERR                                                          
         CLI   5(R2),2                                                          
         BL    TRAPERR                                                          
*                                                                               
         GOTO1 CLPACK,DMCB,QCLT,BCLT                                            
         CLI   0(R1),0                                                          
         BNE   TRAPERR                                                          
         SPACE 1                                                                
* READ CLIENT HEADER *                                                          
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         USING CLTHDRD,R6                                                       
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         SPACE 1                                                                
* SAVE CLIENT PRODUCT LIST *                                                    
         SPACE 1                                                                
         LA    R4,CLIST                                                         
         LA    R5,880                                                           
         LA    RE,SVCLIST                                                       
         LA    RF,880                                                           
         MVCL  RE,R4                                                            
*                                                                               
         MVC   SVCPROF,CPROF       SAVE CLIENT PROFILES                         
         MVC   SVCXTRA,CEXTRA                                                   
         MVC   CLTNM,CNAME         AND CLIENT NAME                              
*                                                                               
         L     RA,ATWA                                                          
         USING T218FFD,RA                                                       
*                                                                               
*****                                                                           
         MVI   ERROR,SECLOCK                                                    
         OC    T218FFD+6(2),T218FFD+6  TEST ANY SECURITY LIMIT                  
         BZ    XIT                                                              
         CLI   T218FFD+6,C'*'      TEST OFFICE LOCKOUT                          
         BE    CLT20               YES                                          
         CLI   T218FFD+6,C'+'      TEST MKT LOCKOUT                             
         BE    CLT20               YES                                          
         CLI   T218FFD+6,C'$'      TEST OFFICE LIST                             
         BE    CLT20               YES                                          
*****                                                                           
CLT10    CLC   T218FFD+6(2),BCLT                                                
         BNE   TRAPERR                                                          
CLT20    CLI   T218FFD+6,C'$'                                                   
         BE    CLT30                                                            
         CLI   T218FFD+6,C'*'                                                   
         BNE   *+14                                                             
         CLC   T218FFD+7(1),COFFICE                                             
         BNE   TRAPERR                                                          
         B     XIT                                                              
*****                                                                           
CLT30    CLI   T218FFD+6,C'$'      TEST OFFICE LIST                             
         BNE   XIT                                                              
         LA    R1,DUB                                                           
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,T218FFD+6                                                
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,COFFICE                                                   
         DROP  R1                                                               
*                                                                               
         GOTO1 OFFICER,DMCB,DUB,ACOMFACS                                        
         CLI   0(R1),0                                                          
         BNE   TRAPERR                                                          
         B     XIT                                                              
         DROP  R6,RA                                                            
         EJECT                                                                  
* VALIDATE STATION CALL LETTERS - ON EXIT QSTA QMKT BMKTSTA                     
*                                         AND STAPRINT ARE SET                  
         SPACE 1                                                                
VSTAT    DS    0H                                                               
         LA    R4,BLOCK                                                         
         USING STABLKD,R4                                                       
         XC    QCBLNET,QCBLNET                                                  
         XC    0(STBLNQ,R4),0(R4)  CLEAR INTERFACE BLOCK                        
         MVC   STBMED,QMED         SET MEDIA                                    
         ST    R2,STBADDR          SET A(STATION FIELD)                         
         MVI   STBCTRY,C'U'                                                     
         MVC   STBACOM,ACOMFACS                                                 
* GET STAVAL ADDRESS                                                            
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A68'                                           
         L     R1,SYSPARMS                                                      
         L     R1,8(R1)                                                         
         L     RF,CCALLOV-COMFACSD(R1)                                          
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(R4)                                                   
*                                                                               
         MVI   ERROR,NETNOTON                                                   
         CLI   STBERR,STBNOTON                                                  
         BE    TRAPERR                                                          
*                                                                               
         MVI   ERROR,INVSTAT                                                    
         CLI   STBERR,0                                                         
         BNE   TRAPERR                                                          
*                                                                               
VSTA10   MVC   QSTA,STBSTA         SAVE STATION                                 
         CLI   QSTA+4,C' '                                                      
         BNE   *+8                                                              
         MVI   QSTA+4,C'T'                                                      
         MVC   QCBLNET,STBNET      SAVE CBLNET                                  
*                                                                               
         MVC   STAPRNT,SPACES                                                   
         MVC   STAPRNT(4),QSTA                                                  
         LA    RE,STAPRNT+3                                                     
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'-'                                                       
         MVC   2(1,RE),QSTA+4      MOVE SUB-MEDIA                               
         MVI   3(RE),C'V'          ASSUME TV                                    
*                                                                               
         MVC   BIGSTA(5),QSTA      COPY TO 8 BYTE STATION                       
         CLI   QCBLNET,C' '                                                     
         BNH   VSTA12                                                           
         MVI   BIGSTA+4,C'/'                                                    
         MVC   BIGSTA+5(3),QCBLNET                                              
         MVC   STAPRNT+4(3),QCBLNET AND DO AS WELL AS WE CAN HERE               
         SPACE 1                                                                
* READ STATION MASTER RECORD *                                                  
         SPACE 1                                                                
VSTA12   DS    0H                                                               
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(5),QSTA                                                    
         MVC   KEY+7(2),AGENCY                                                  
         MVC   KEY+9(3),QCLT                                                    
         MVC   AIO,AIO1                                                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'STATION',KEY,AIO                  
         CLI   8(R1),0                                                          
         BNE   TRAPERR                                                          
*                                                                               
         L     R6,AIO                                                           
         USING STARECD,R6                                                       
         MVC   QMKT,SMKT                                                        
         GOTO1 MSPACK,DMCB,QMKT,QSTA,BMKTSTA                                    
         MVC   SVTAX,SNEWTAX                                                    
         EJECT                                                                  
* READ MARKET RECORD TO IO1+400                                                 
         SPACE 1                                                                
         LA    R6,400(R6)                                                       
*                                                                               
VSTA14   ST    R6,AIO                                                           
         USING MKTRECD,R6                                                       
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),QMKT                                                    
         MVC   KEY+6(2),AGENCY                                                  
*                                                                               
         MVI   ERROR,INVMKT                                                     
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'STATION',KEY,AIO                  
*                                                                               
         MVC   AIO,AIO1            RESTORE AIO                                  
*                                                                               
         CLC   KEY(15),0(R6)                                                    
         BNE   TRAPERR                                                          
*                                                                               
         L     RA,ATWA                                                          
         USING T218FFD,RA                                                       
*                                                                               
         CLI   T218FFD+6,C'+'      TEST MARKET LOCKOUT                          
         BNE   VSTA20                                                           
         LA    R0,3                                                             
         LA    R1,MKTLTACC                                                      
         CLC   T218FFD+7(1),0(R1)                                               
         BE    VSTA20                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,*-14                                                          
         MVI   ERROR,NOMKTACC                                                   
         B     TRAPERR                                                          
         DROP  RA                                                               
*                                                                               
VSTA20   MVC   MKTNM,MKTNAME       RETURN MARKET NAME TO USER                   
         MVI   BYTE,C'0'           FIND RATING SERVICE MARKET                   
         CLI   BKVALSRC,C'N'                                                    
         BE    *+8                                                              
         MVI   BYTE,C'1'                                                        
         CLC   MKTRS1,BYTE                                                      
         BNE   *+10                                                             
         MVC   MKTRS,MKTRSM1                                                    
         CLC   MKTRS2,BYTE                                                      
         BNE   *+10                                                             
         MVC   MKTRS,MKTRSM2                                                    
*                                                                               
         MVC   SVSWPCLS,MKTCLAS1                                                
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE MARKET CODE *                                                        
         SPACE 1                                                                
VMKT     XC    BSTA,BSTA                                                        
         TM    4(R2),X'08'         TEST NUMERIC                                 
         BO    *+12                                                             
         MVI   ERROR,INVMKT                                                     
         B     TRAPERR                                                          
*                                                                               
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2) *EXECUTED*                                           
*                                                                               
         CVB   R0,DUB                                                           
         STCM  R0,3,BMKT                                                        
*                                                                               
         OI    DUB+7,X'0F'                                                      
         UNPK  QMKT,DUB            SET FOR USER TOO                             
*                                                                               
         L     R6,AIO1                                                          
         B     VSTA14                                                           
         EJECT                                                                  
* VALIDATE MASTER ESTIMATE NUMBER *                                             
         SPACE 1                                                                
VMEST    MVI   ERROR,INVEST                                                     
         TM    4(R2),X'08'         TEST NUMERIC                                 
         BZ    TRAPERR                                                          
*                                                                               
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2) *EXECUTED*                                           
*                                                                               
         CVB   R0,DUB                                                           
         STC   R0,BMEST                                                         
         MVC   QMEST,8(R2)         SAVE EBCDIC REPRESENTATION                   
*                                                                               
         LA    R6,KEY              READ ESTIMATE HEADER RECORD                  
         USING ESTHDRD,R6                                                       
         XC    KEY,KEY                                                          
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,=C'POL'                                                  
         MVC   EKEYEST,BMEST                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   TRAPERR                                                          
         L     R6,AIO                                                           
         GOTO1 GETREC                                                           
         CLI   EMSTRIND,C'M'       CHECK THAT IT IS MASTER ESTIMATE             
         BNE   TRAPERR                                                          
         MVC   QSTART,ESTART       SAVE INFORMATION FOR LATER                   
         MVC   QEND,EEND                                                        
         MVC   QDAYMENU,EDAYMENU                                                
         MVC   QDEMOS,EDEMOS                                                    
         MVC   QDESC,EDESC                                                      
*                                                                               
         LA    RE,QDEMOS           END DEMO LIST WITH A X'FF'                   
         CLI   1(RE),0                                                          
         BE    *+12                                                             
         LA    RE,3(RE)                                                         
         B     *-12                                                             
         MVI   0(RE),X'FF'                                                      
         EJECT                                                                  
* READ MASTER ESTIMATE LIST AND VALIDATE GIVEN MASTER ESTIMATE AS               
* CASH/TRADE OR TRADE ONLY                                                      
*                                                                               
VMEST2   LA    R6,KEY              READ MASTER ESTIMATE LIST RECORD             
         USING MASKEY,R6                                                        
         XC    KEY,KEY                                                          
         MVI   MASKTYPE,MASKTYPQ                                                
         MVI   MASKSTYP,MASKSTPQ                                                
         MVC   MASKAM,BAGYMD                                                    
         MVC   MASKCLT,BCLT                                                     
         MVC   MASKDATE,QSTART                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   TRAPERR             ERROR NOT FOUND                              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO              EXTRACT MASTER ESTIMATE LIST                 
         MVI   ELCODE,MELCODEQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING MELELEM,R6                                                       
         MVC   MESTLST,MELLIST                                                  
         DROP  R6                                                               
*                                                                               
         L     R6,AIO              EXTRACT DATE TO SPLIT BUYLINES               
         MVI   ELCODE,MSDCODEQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING MSDELEM,R6                                                       
         GOTO1 DATCON,DMCB,(0,MSDDATE),(2,BUYSPLIT)                             
         DROP  R6                                                               
*                                                                               
         LA    R4,MESTLST          POINT TO MASTER ESTIMATE LIST                
         USING MESTLSTD,R4                                                      
*                                                                               
VMEST3   CLC   MESTNUM,BMEST       IF MATCH WITH GIVEN EST THEN DONE            
         BE    VMEST4                                                           
*                                                                               
         CLI   MESTTYPE,C'C'       IF CASH ESTIMATE                             
         BNE   *+10                                                             
         MVC   ASCSUBS,MESTSUBS    SAVE SUBESTS FOR TRADE ONLY                  
*                                                                               
         LA    R4,MESTLSTL(R4)     BUMP TO NEXT TABLE ENTRY                     
         CLI   0(R4),0                                                          
         BNE   VMEST3              IF END OF LIST THEN ERROR                    
         B     TRAPERR                                                          
*                                                                               
VMEST4   MVC   TRADONLY,MESTTYPE   SAVE ESTIMATE TYPE                           
         MVC   MASTSPLN,MESTSPLN   AND SPOTLEN                                  
         MVC   SVSUBS,MESTSUBS     AND SUB ESTIMATE LIST                        
         LA    RF,MESTLST          SAVE DISPLACEMENT TO MESTLST ENTRY           
         LR    RE,R4                                                            
         SR    RE,RF                                                            
         ST    RE,DMEST                                                         
*                                                                               
         CLI   TRADONLY,C'T'       TRADE ONLY ESTIMATES NOT VALID FOR..         
         BNE   VMEST5                                                           
         CLI   MYOVNUM,X'02'       STATION MAINTENANCE                          
         BE    TRDERR                                                           
         CLI   MYOVNUM,X'05'       ALLOC GENERATE                               
         BE    TRDERR                                                           
*                                                                               
VMEST5   LA    R5,MESTSUBS         POINT TO SUB ESTIMATE LIST                   
         B     VMEST10                                                          
         DROP  R4                                                               
         EJECT                                                                  
* BUILD EXPANDED SUB ESTIMATE LIST FROM INFO IN SUB ESTIMATE RECORDS            
*                                                                               
VMEST10  MVI   QBOOKS,X'FF'        RESET DEMO BOOK LIST TO EMPTY                
*                                                                               
         LA    R4,SVSUBEST         POINT TO EXPANDED SUB ESTIMATE LIST          
         USING SESTLSTD,R4                                                      
*                                                                               
VMEST20  LA    R6,KEY              READ ESTIMATE HEADER RECORD FOR              
         USING ESTHDRD,R6              EACH SUB ESTIMATE                        
         XC    KEY,KEY                                                          
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,=C'POL'                                                  
         MVC   EKEYEST,0(R5)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   TRAPERR                                                          
         L     R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                  ADD ENTRY TO EXPANDED LIST                   
         MVC   SNUM,EKEYEST        EST NUM                                      
         GOTO1 DATCON,DMCB,(0,ESTART),(2,SSTART)     EST START DATE             
         GOTO1 DATCON,DMCB,(0,EEND),(2,SEND)         EST END DATE               
         MVC   SBOOK,EBOOK         EST BOOK                                     
         MVC   SHUTADJ,EHUTADJ     EST ADJACENCY BITS                           
*                                                                               
         LA    R4,SESTLSTL(R4)     BUMP EXPANDED LIST POINTER                   
*                                                                               
         LA    RE,QBOOKS           ADD BOOK TO DEMO BOOK LIST                   
*                                                                               
VMEST25  CLI   0(RE),X'FF'         IF END OF LIST REACHED                       
         BE    VMEST26                                                          
         CLC   0(2,RE),EBOOK       BEFORE DUPLICATE FOUND                       
         BE    VMEST27                                                          
         LA    RE,2(RE)                                                         
         B     VMEST25                                                          
*                                                                               
VMEST26  MVC   0(2,RE),EBOOK       ADD BOOK                                     
         MVI   2(RE),X'FF'         AND SET NEW END OF LIST MARKER               
         DROP  R6                                                               
*                                                                               
VMEST27  LA    R5,1(R5)            BUMP TO NEXT SUB ESTIMATE                    
         CLI   0(R5),0                                                          
         BNE   VMEST20             GO AGAIN IF END OF LIST NOT REACHED          
*                                                                               
VMEST30  MVI   0(R4),0             SET END OF EXPANDED LIST INDICATOR           
         B     VMEST40                                                          
         DROP  R4                                                               
         EJECT                                                                  
* BUILD LIST OF VALID DAYPARTS                                                  
*                                                                               
VMEST40  LA    R1,DMCB                                                          
         MVC   0(2,R1),AGENCY                                                   
         MVC   2(1,R1),QMED                                                     
         MVC   3(1,R1),QDAYMENU                                                 
         GOTO1 =V(DPTRD),(R1),,AIO2,DATAMGR,RR=RELO                             
*                                                                               
         CLI   8(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         TM    8(R1),X'08'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
* SAVE VALID CODES                                                              
         XC    SVMENU,SVMENU                                                    
         LA    R0,L'SVMENU-1       R0=COUNTER                                   
         L     R1,AIO2                                                          
         LA    R4,SVMENU                                                        
*                                                                               
VMEST50  CLI   0(R1),0             TEST FOR EOT                                 
         BE    VMEST60                                                          
         MVC   0(1,R4),0(R1)                                                    
         LA    R4,1(R4)                                                         
         LA    R1,5(R1)                                                         
         BCT   R0,VMEST50                                                       
         EJECT                                                                  
* BUILD QUARTER TABLE DATES, WEEKS PER MONTH TABLE, MONTH NAMES                 
*                                                                               
VMEST60  XC    WORK,WORK           BUILD BLOCK OF BROADMON START DATES          
         MVC   WORK(4),GETBROAD                                                 
         MVC   WORK+4(4),ADDAY                                                  
         MVC   WORK+8(4),GETDAY                                                 
         MVC   WORK+12(4),DATCON                                                
         GOTO1 MOBILE,DMCB,(12,QSTART),BLOCK,WORK,0                             
*                                                                               
         SR    R2,R2               FIX UP QUARTERS TO BE 13 WEEKS               
         LA    R4,BLOCK                                                         
         MVC   THISDATE,QSTART                                                  
*                                                                               
VMEST70  C     R2,=F'13'           TEST BEGIN OF QUARTER                        
         BE    VMEST80                                                          
         C     R2,=F'26'                                                        
         BE    VMEST80                                                          
         C     R2,=F'39'                                                        
         BE    VMEST80                                                          
*                                                                               
         C     R2,=F'12'           TEST END OF QUARTER                          
         BE    VMEST90                                                          
         C     R2,=F'25'                                                        
         BE    VMEST90                                                          
         C     R2,=F'38'                                                        
         BE    VMEST90                                                          
         B     VMEST100                                                         
*                                  SET BEGIN OF QUARTER DATE                    
VMEST80  GOTO1 DATCON,DMCB,(0,THISDATE),(2,0(R4))                               
         B     VMEST100                                                         
*                                  SET END OF QUARTER DATE                      
VMEST90  GOTO1 ADDAY,DMCB,THISDATE,NEXTDATE,F'6'                                
         GOTO1 DATCON,DMCB,(0,NEXTDATE),(2,10(R4))                              
         LA    R4,12(R4)                                                        
*                                  BUMP TO NEXT WEEK                            
VMEST100 GOTO1 ADDAY,DMCB,THISDATE,THISDATE,F'7'                                
         LA    R2,1(R2)                                                         
         CLC   THISDATE,QEND       REPEAT UNTIL END OF YEAR                     
         BL    VMEST70                                                          
*                                                                               
         LA    R6,QUARTAB          SET QUARTER TABLE START/END DATES            
         USING QUARTABD,R6                                                      
         LA    R4,BLOCK                                                         
         LA    R3,4                                                             
*                                  SAVE START/END FOR THIS QUARTER              
VMEST110 GOTO1 DATCON,DMCB,(2,0(R4)),(0,QUARSTRT)                               
         GOTO1 DATCON,DMCB,(2,10(R4)),(0,QUAREND)                               
         LA    R6,QUARTABL(R6)                                                  
         LA    R4,12(R4)           BUMP 3 MONTHS TO NEXT QUARTER                
         BCT   R3,VMEST110                                                      
         DROP  R6                                                               
*                                                                               
         LA    R4,BLOCK            COMPUTE WEEKS PER MONTH TO WPMTAB            
         LA    R3,WPMTAB                                                        
         MVC   THISDATE,QSTART                                                  
*                                                                               
VMEST150 SR    R2,R2               FOR EACH MONTH RESET COUNTER/ENDDATE         
         GOTO1 DATCON,DMCB,(2,2(R4)),(0,NEXTDATE)                               
*                                                                               
VMEST160 CLC   THISDATE,NEXTDATE   WHILE NOT END OF MONTH                       
         BNL   VMEST170                                                         
         LA    R2,1(R2)            BUMP COUNT AND DATE                          
         GOTO1 ADDAY,DMCB,THISDATE,THISDATE,F'7'                                
         B     VMEST160                                                         
*                                                                               
VMEST170 STC   R2,0(R3)            SAVE COUNT IN WPMTAB                         
         LA    R3,1(R3)            BUMP WPMTAB AND BLOCK POINTERS               
         LA    R4,4(R4)                                                         
         CLC   THISDATE,QEND       REPEAT UNTIL END OF YEAR                     
         BL    VMEST150                                                         
*                                  SET MONTH NAMES                              
         MVC   MONAMES,=C'JUNJULAUGSEPOCTNOVDECJANFEBMARAPRMAY'                 
*                                                                               
VMESTX   B     XIT                                                              
         EJECT                                                                  
* VALIDATE SUB ESTIMATE NUMBER *                                                
         SPACE 1                                                                
VSEST    MVI   ERROR,INVEST                                                     
         TM    4(R2),X'08'         TEST NUMERIC                                 
         BZ    TRAPERR                                                          
*                                                                               
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2) *EXECUTED*                                           
*                                                                               
         CVB   R0,DUB                                                           
         STC   R0,BSEST                                                         
         MVC   QSEST,8(R2)                                                      
*                                                                               
         LA    R6,KEY              READ ESTIMATE HEADER RECORD                  
         USING ESTHDRD,R6                                                       
         XC    KEY,KEY                                                          
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,=C'POL'                                                  
         MVC   EKEYEST,BSEST                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   TRAPERR                                                          
         L     R6,AIO                                                           
         GOTO1 GETREC                                                           
         CLI   EMSTRIND,C'M'       MAKE SURE IT ISN'T A MASTER ESTIMATE         
         BE    TRAPERR                                                          
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE REFERENCE NUMBER *                                                   
         SPACE 1                                                                
VREF     MVI   ERROR,INVREF                                                     
         TM    4(R2),X'08'         TEST NUMERIC                                 
         BZ    TRAPERR                                                          
*                                                                               
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2) *EXECUTED*                                           
*                                                                               
         CVB   R0,DUB                                                           
         STC   R0,BREF                                                          
         B     XIT                                                              
         EJECT                                                                  
* VCLEARF - CLEAR AND FOUT FIELDS                                               
*                                                                               
* ON ENTRY                                                                      
*        P1    BYTE 0    = 0 UNPROTECTED FIELDS                                 
*                        = 1 PROTECTED FIELDS                                   
*              BYTES 1-3 = A(START FIELD HEADER)                                
*        P2    BYTES 1-3 = A(END FIELD HEADER)                                  
*                                                                               
VCLEARF  LM    R2,R3,0(R1)                                                      
         SR    RE,RE                                                            
         LA    R4,X'10'            BRANCH CONDITION                             
         LA    R5,MOVESPA          CLEAR FIELD INSTRUCTION                      
         CLI   0(R1),0             TEST FOR UNPROTECTED FIELDS                  
         BE    *+12                YES                                          
         LA    R4,X'80'            SET BRANCH CONDITION AND CLEAR               
         LA    R5,ZEROFLD          INSTRUCTION FOR PROTECTED FIELDS             
         SPACE 1                                                                
VCLEARF2 IC    RE,0(R2)            LENGTH OF FIELD PLUS HEADER                  
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         EX    R4,TSTBRAN          BRANCH ACCORDINGLY                           
         LR    R1,RE                                                            
         SH    R1,=H'9'            SET EXECUTE LENGTH FOR FIELD DATA            
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    *+8                                                              
         SH    R1,=H'8'            LESS 8 MORE FOR EXTENDED FIELD               
         EX    R1,0(R5)            CLEAR FIELD                                  
         OI    6(R2),X'80'         SET TO TRANSMIT                              
         SPACE 1                                                                
VCLEARF4 LA    R2,0(RE,R2)                                                      
         CR    R2,R3               TEST IF END FIELD REACHED                    
         BL    VCLEARF2            NO-CONTINUE                                  
         B     XIT                 YES-ALL DONE                                 
         SPACE 1                                                                
MOVESPA  MVC   8(0,R2),SPACES                                                   
ZEROFLD  XC    8(0,R2),8(R2)                                                    
TSTBRAN  BC    0,VCLEARF4                                                       
         EJECT                                                                  
* INITIALIZE WEEKLY AND QUARTERLY ACCUMULATORS                                  
*                                                                               
VCLRACC  LA    R6,ACCTAB           INITIALIZE ACCUMULATOR TABLE FROM            
         USING ACCTABD,R6              MASTER EST. START & END DATES            
         MVC   THISDATE,QSTART                                                  
         SR    R3,R3               COUNT NUMBER OF WEEKS                        
*                                                                               
VCA10    GOTO1 DATCON,DMCB,(0,THISDATE),(2,ACCDATE)                             
         XC    ACCONE,ACCONE                                                    
         XC    ACCTWO,ACCTWO                                                    
         LA    R6,ACCTABL(R6)      BUMP TO NEXT ACCTAB ENTRY                    
         LA    R3,1(R3)                                                         
         GOTO1 ADDAY,DMCB,THISDATE,THISDATE,F'7'                                
         CLC   THISDATE,QEND                                                    
         BNH   VCA10                                                            
         ST    R3,YRWEEKS          SAVE NUMBER OF WEEKS                         
         DROP  R6                                                               
*                                                                               
         LA    R6,QUARTAB          INITIALIZE QUARTER TABLE                     
         USING QUARTABD,R6                                                      
         LA    R3,4                                                             
*                                                                               
VCA20    XC    QUARACC,QUARACC     CLEAR ACCUMULATOR                            
         LA    R6,QUARTABL(R6)                                                  
         BCT   R3,VCA20                                                         
         DROP  R6                                                               
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* BUILD CALENDER AND DISPLAY ACCUMULATOR VALUES ROUTINE *                       
         SPACE 1                                                                
VBLDCAL  MVC   THISDATE,QSTART     INITIALIZE DATE                              
         LA    R8,MONAMES          POINT TO MONTH NAMES                         
         LA    RA,ACCTAB           POINT TO ACCUMULATOR TABLE                   
         USING ACCTABD,RA                                                       
         SR    R4,R4               INITIALIZE MONTH COUNTER                     
*                                                                               
VBC110   SR    R0,R0               CALCULATE SCREEN POSITION FOR THIS           
         LR    R1,R4                   QUARTER                                  
         D     R0,=F'3'                                                         
         M     R0,=F'19'                                                        
         L     R2,CALSPTR                                                       
         LA    R3,8(R2,R1)                                                      
         MVC   7(4,R3),CALHEADS    DISPLAY HEADINGS                             
         MVC   13(4,R3),CALHEADS+4                                              
         OI    6(R2),X'80'         TRANSMIT AND BUMP TO NEXT FIELD              
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         AR    R3,R0                                                            
*                                                                               
VBC120   ZIC   R5,WPMTAB(R4)       R5 = NUMBER OF WEEKS THIS MONTH              
*                                  DISPLAY DATE,ACCUMULATOR VALUES              
VBC130   GOTO1 DATCON,DMCB,(0,THISDATE),(4,0(R3))                               
         EDIT  (4,ACCONE),(6,6(R3)),ZERO=NOBLANK,MINUS=YES                      
         EDIT  (4,ACCTWO),(6,12(R3)),ZERO=NOBLANK,MINUS=YES                     
         LA    RA,ACCTABL(RA)                                                   
         GOTO1 ADDAY,DMCB,THISDATE,THISDATE,F'7'                                
         OI    6(R2),X'80'         TRANSMIT AND BUMP TO NEXT FIELD              
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         AR    R3,R0                                                            
         BCT   R5,VBC130           REPEAT UNTIL END OF MONTH                    
*                                                                               
         LA    R4,1(R4)                                                         
         SR    R0,R0                                                            
         LR    R1,R4                                                            
         D     R0,=F'3'                                                         
         LTR   R0,R0                                                            
         BNE   VBC120              REPEAT UNTIL END OF QUARTER                  
*                                                                               
         C     R4,=F'12'                                                        
         BL    VBC110              REPEAT UNTIL END OF YEAR                     
         B     XIT                                                              
         DROP  RA                                                               
         EJECT                                                                  
* COMPUTE GOAL DOLLARS FOR THE GIVEN MARKET OR SPECIFIC STATION, AND IF         
* A STATION IS SPECIFIED, APPLY THE STATION PERCENTAGE                          
*                                                                               
VCMPGOL  DS    0H                                                               
         BAS   RE,COMPGOAL         COMPUTE GOAL DOLLARS                         
         BAS   RE,STAPERC          APPLY STATION PERCENTAGES                    
         B     XIT                                                              
         SPACE 2                                                                
* COMPUTE GOAL DOLLARS FOR THE GIVEN MARKET.  DON'T APPLY STATION               
* PERCENTAGES EVEN IF STATION IS SPECIFIED (USED FOR ASSIGNING                  
* TRADE SPOTS BASED ON GOAL DISTRIBUTION).                                      
*                                                                               
VCMPMGOL DS    0H                                                               
         BAS   RE,COMPGOAL         COMPUTE GOAL DOLLARS                         
         B     XIT                                                              
         SPACE 2                                                                
* COMPUTE CASH GOAL DOLALRS BY FIRST COMPUTING THE GOAL DOLLARS, THEN           
* SUBTRACTING OUT THE TRADE DOLLARS, AND THEN APPLYING THE STATION              
* PERCENTAGES                                                                   
*                                                                               
VCMPCGOL DS    0H                                                               
         BAS   RE,COMPGOAL         FIRST COMPUTE GOAL DOLLARS                   
         BAS   RE,SUBTRADE         SUBTRACT OUT TRADE DOLLARS                   
         BAS   RE,STAPERC          APPLY STATION PERCENTAGES                    
         B     XIT                                                              
         EJECT                                                                  
* COMPUTE GOAL DOLLARS FOR THE GIVEN MARKET                                     
*                                                                               
COMPGOAL NTR1                                                                   
         CLI   TRADONLY,C'T'       TRADE ONLY ESTIMATES NOT VALID               
         BE    TRDERR                                                           
*                                                                               
         XC    KEY,KEY             READ GOAL RECORDS                            
         LA    R4,KEY                                                           
         USING GOALRECD,R4                                                      
         MVI   GKEYTYPE,X'02'      FILL KEY WITH AGYMD/CLIENT                   
         MVC   GKEYAM,BAGYMD                                                    
         MVC   GKEYCLT,BCLT                                                     
*                                                                               
CG10     MVI   RDUPDATE,C'N'       GET NEXT GOAL RECORD                         
         GOTO1 HIGH                                                             
*                                                                               
CG20     CLC   KEY(4),KEYSAVE      TEST DONE FOR THIS AGYMD/CLT                 
         BNE   CGX                                                              
*                                                                               
CG30     CLC   GKEYMKT,BMKT        TEST CORRECT MARKET                          
         BE    CG60                                                             
         BL    CG50                                                             
*                                                                               
CG40     MVI   GKEYMKT,X'FF'       FORCE NEXT PRODUCT                           
         B     CG10                                                             
*                                                                               
CG50     XC    GKEYMKT(8),GKEYMKT  FORCE CORRECT MARKET                         
         MVC   GKEYMKT,BMKT                                                     
         B     CG10                                                             
*                                                                               
CG60     LA    R5,SVSUBS           SEARCH SUB-ESTIMATE LIST FOR MATCH           
*                                                                               
CG70     CLC   GKEYEST,0(R5)       COMPARE GOAL ESTIMATE WITH LIST              
         BE    CG100                                                            
         BL    CG90                                                             
*                                                                               
CG80     LA    R5,1(R5)            BUMP TO NEXT LIST ENTRY                      
         CLI   0(R5),0             TEST END OF LIST                             
         BE    CG40                                                             
         B     CG70                TRY NEXT SUB-ESTIMATE NUMBER                 
*                                                                               
CG90     XC    GKEYEST(6),GKEYEST  FORCE CORRECT ESTIMATE                       
         MVC   GKEYEST,0(R5)                                                    
         B     CG10                                                             
         EJECT                                                                  
CG100    MVI   RDUPDATE,C'N'       READ IN RECORD                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'21'        FIND FIRST WEEK ELEMENT                      
         BAS   RE,GETEL                                                         
         BNE   CG120                                                            
         USING GLEMENT,R6                                                       
*                                                                               
CG110    L     R3,YRWEEKS          FIND ACCUMULATOR FOR THIS WEEK               
         GOTO1 BINSRCH,DMCB,(X'02',GLWEEK),ACCTAB,(R3),10,(0,2)                 
         CLI   0(R1),X'01'                                                      
         BNE   *+6                                                              
         DC    H'0'                DIE IF CAN'T FIND WEEK                       
*                                                                               
         L     R3,ACCNUM           POINT R3 TO ACCUMULATOR                      
         SLL   R3,2                                                             
         A     R3,0(R1)                                                         
         LA    R3,2(R3)                                                         
*                                                                               
         SR    R0,R0               ADD GOAL DOLLARS TO ACCUMULATOR              
         ICM   R1,15,GLBUDGET                                                   
         D     R0,=F'100'                                                       
         ICM   RF,15,0(R3)                                                      
         AR    RF,R1                                                            
         STCM  RF,15,0(R3)                                                      
*                                                                               
         L     RF,GOLALL           BUMP TOTAL FOR ALL PRODUCTS                  
         AR    RF,R1                                                            
         ST    RF,GOLALL                                                        
         CLI   GKEYPRD,X'3F'       IF GROUP 1 PRODUCT                           
         BH    CG112                                                            
         L     RF,GOLGRP1          THEN BUMP TOTAL FOR GROUP 1                  
         AR    RF,R1                                                            
         ST    RF,GOLGRP1                                                       
         B     CG118                                                            
CG112    CLI   GKEYPRD,X'5F'       ELSE IF GROUP 2 PRODUCT                      
         BH    CG114                                                            
         L     RF,GOLGRP2          THEN BUMP TOTAL FOR GROUP 2                  
         AR    RF,R1                                                            
         ST    RF,GOLGRP2                                                       
         B     CG118                                                            
CG114    L     RF,GOLGRP3          ELSE BUMP TOTAL FOR GROUP 3                  
         AR    RF,R1                                                            
         ST    RF,GOLGRP3                                                       
CG118    DS    0H                                                               
*                                                                               
         BAS   RE,NEXTEL           BUMP TO NEXT WEEK ELEMENT                    
         BE    CG110                                                            
*                                                                               
CG120    MVI   RDUPDATE,C'N'       PROCESS NEXT RECORD                          
         GOTO1 SEQ                                                              
         B     CG20                                                             
*                                                                               
CGX      B     XIT                                                              
         EJECT                                                                  
* SUBTRACT FROM THE GOAL DOLLARS FOUND IN THE ACCUMULATORS ALL TRADE            
* DOLLARS FROM THE CASH ESTIMATE AND FROM ALL ASSOCIATED TRADE-ONLY             
* ESTIMATES, LEAVING THE CASH GOAL (AVAILABLE CASH) IN THE ACCUMULATORS         
*                                                                               
SUBTRADE NTR1                                                                   
         LA    R4,KEY              READ PROGRAM RECORDS                         
         USING CSOKEY,R4                                                        
         XC    KEY,KEY                                                          
         MVI   CSOKTYPE,CSOKTYPQ                                                
         MVI   CSOKSTYP,CSOKSTPQ                                                
         MVC   CSOKAM,BAGYMD       FILL WITH AGYMD/CLT/MKT                      
         MVC   CSOKCLT,BCLT                                                     
         MVC   CSOKMKT,BMKT                                                     
         MVC   CSOKSTA,=X'000001'  SKIP STATION PERCENTAGE RECORD               
*                                                                               
ST10     MVI   RDUPDATE,C'N'       READ NEXT PROGRAM RECORD                     
         GOTO1 HIGH                                                             
*                                                                               
ST20     CLC   KEY(7),KEYSAVE      TEST DONE FOR THIS AGYMD/CLT/MKT             
         BNE   STX                                                              
*                                                                               
ST30     L     R3,DMEST            SEARCH MESTLST FOR MATCHING ESTIMATE         
         LA    R3,MESTLST(R3)                                                   
         USING MESTLSTD,R3                                                      
*                                                                               
ST40     CLC   CSOKEST,MESTNUM     FIRST TEST CASH/TRADE ESTIMATE               
         BE    ST100                                                            
*                                                                               
         LA    R3,MESTLSTL(R3)     BUMP TO FIRST TRADE-ONLY ESTIMATE            
*                                                                               
ST50     CLI   0(R3),0             WHILE NOT END OF LIST                        
         BE    ST90                                                             
         CLI   MESTTYPE,C'T'       OR END OF TRADE-ONLYS                        
         BNE   ST90                                                             
*                                                                               
         CLC   CSOKEST,MESTNUM     TEST MATCHING ESTIMATE                       
         BE    ST100                                                            
*                                                                               
         LA    R3,MESTLSTL(R3)     TRY NEXT ESTIMATE IN LIST                    
         B     ST50                                                             
         DROP  R3                                                               
*                                                                               
ST90     MVI   CSOKREF,X'FF'       FORCE NEXT ESTIMATE                          
         B     ST10                                                             
         EJECT                                                                  
ST100    MVI   RDUPDATE,C'N'       READ IN RECORD                               
         GOTO1 GETREC                                                           
         L     R6,AIO              POINT TO FIRST WEEKLY ELEMENT                
         MVI   ELCODE,WKCODEQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSOWKEL,R6                                                       
*                                                                               
         LA    R5,ACCTAB           LOOP THROUGH ACCUMULATOR TABLE AND           
         USING ACCTABD,R5              SUBTRACT OUT TRADE DOLLARS               
         L     RF,ACCNUM                                                        
         SLL   RF,2                                                             
         LA    R5,ACCONE(RF)       POINT TO CORRECT ACCUMULATOR                 
*                                                                               
ST110    SR    RE,RE               TRADE DOLLARS = TRADE SPOTS * RATE           
         ICM   RE,3,WKTSPOTS                     * NTP %                        
         ICM   R1,15,WKCOST                                                     
         MR    R0,RE                                                            
         M     R0,NTPPERC                                                       
         A     R1,=F'5000'                                                      
         D     R0,=A(10*1000)                                                   
         ICM   RF,15,0(R5)         SUBTRACT OUT TRADE DOLLARS                   
         SR    RF,R1                                                            
         STCM  RF,15,0(R5)                                                      
*                                                                               
         LA    R5,ACCTABL(R5)      BUMP TO NEXT ACCUMULATOR                     
         BAS   RE,NEXTEL                                                        
         BE    ST110               REPEAT UNTIL NO MORE WEEKS                   
*                                                                               
         MVI   RDUPDATE,C'N'       DO NEXT RECORD                               
         GOTO1 SEQ                                                              
         B     ST20                                                             
*                                                                               
STX      B     XIT                                                              
         EJECT                                                                  
* APPLY STATION PERCENTAGES TO GOAL DOLLARS IF STATION IS SPECIFIED             
*                                                                               
STAPERC  NTR1                                                                   
         OC    BSTA,BSTA           IF STATION IS SPECIFIED                      
         BZ    XIT                                                              
*                                                                               
         XC    KEY,KEY             ADJUST GOAL DOLLARS BY APPLYING              
         LA    R4,KEY                  STATION PERCENTAGES                      
         USING CSORECD,R4                                                       
         MVI   CSOKTYPE,CSOKTYPQ                                                
         MVI   CSOKSTYP,CSOKSTPQ                                                
         MVC   CSOKAM,BAGYMD                                                    
         MVC   CSOKCLT,BCLT                                                     
         MVC   CSOKMKT,BMKT                                                     
         MVC   CSOKEST,BMEST                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                READ STATION PERCENTAGE RECORD               
         CLC   KEY(10),KEYSAVE                                                  
         BNE   ERRSNF              ERROR STATION RECORD NOT FOUND               
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO              POINT R6 TO FIRST STATION ELEMENT            
         MVI   ELCODE,STACODEQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSOSTAEL,R6                                                      
*                                                                               
SP10     CLC   STANAME(4),QSTA     LOOP UNTIL STATION NAME MATCHES              
         BNE   SP15                                                             
         CLI   STANAME+4,C' '                                                   
         BNH   SP20                                                             
         CLC   STANAME+4(1),QSTA+4                                              
         BE    SP20                                                             
*                                                                               
SP15     BAS   RE,NEXTEL                                                        
         BE    SP10                                                             
         B     ERRSNF              ERROR STATION NOT FOUND                      
*                                                                               
SP20     XC    SCANOUT,SCANOUT     MOVE DATES AND PERCENTS INTO SCANOUT         
         ZIC   RF,STALEN                                                        
         SH    RF,=H'8'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SCANOUT(0),STADPTAB                                              
*                                                                               
         LA    R5,SCANOUT          POINT R5 TO DATES AND PERCENTAGES            
         USING DPTABD,R5                                                        
*                                                                               
         LA    R3,ACCTAB           INITIALIZE POINTER TO ACCTAB                 
         USING ACCTABD,R3                                                       
         MVC   THISDATE,QSTART     INITIALIZE DATE COUNTER                      
*                                                                               
SP30     GOTO1 DATCON,DMCB,(0,THISDATE),(3,NEXTDATE)                            
*                                                                               
         OC    DPDATE,DPDATE       IF NEXT DATE IN TABLE HAS BEEN               
         BZ    SP50                    REACHED                                  
         CLC   NEXTDATE,DPDATE                                                  
         BL    SP50                                                             
         MVC   STATPCT,DPPCT       USE NEW STATION PERCENTAGE                   
         LA    R5,DPTABL(R5)       BUMP DATE PERCENTAGE TABLE                   
*                                                                               
SP50     L     R1,STATPCT          LOAD R1 WITH STATION PERCENTAGE              
*                                                                               
         L     R4,ACCNUM           APPLY PERCENTAGE TO ACCUMULATOR              
         SLL   R4,2                                                             
         LA    R4,ACCONE(R4)                                                    
         ICM   RF,15,0(R4)                                                      
         MR    R0,RF                                                            
         D     R0,=A(100*1000)                                                  
         STCM  R1,15,0(R4)                                                      
*                                                                               
         LA    R3,ACCTABL(R3)      BUMP TO NEXT ACCUMULATOR                     
         GOTO1 ADDAY,DMCB,THISDATE,THISDATE,F'7'                                
         CLC   THISDATE,QEND                                                    
         BNH   SP30                REPEAT UNTIL END OF YEAR                     
*                                                                               
SPX      B     XIT                                                              
         EJECT                                                                  
* ROUTINE TO TOTAL UP THE ACCUMULATORS IN ACCTAB, SAVE THE TOTALS               
*     FOR EACH QUARTER IN QUARTAB, AND RETURN THE TOTAL FOR THE YEAR            
*                                                                               
* PARAMETER 1 - POINTS TO THE FIRST FULL WORD                                   
*               RETURNS TOTAL FOR THE YEAR                                      
* PARAMETER 2 - SEPARATION BETWEEN EACH FULL WORD                               
*                                                                               
VTOTFULL SR    R2,R2               INITIALIZE QUARTER TOTAL                     
         SR    R6,R6               INITIALIZE YEAR TOTAL                        
         L     R3,0(R1)            POINT R3 TO FIRST FULL WORD                  
         L     R4,4(R1)            R4 CONTAINS TABLE ELEMENT LENGTH             
         LA    R5,QUARTAB          R5 = A(FIRST QUARTER IN QUARTAB)             
         USING QUARTABD,R5                                                      
         MVC   THISDATE,QSTART     THISDATE = START DATE                        
*                                                                               
VTOT10   ICM   RF,15,0(R3)         ADD THIS FULL WORD TO BOTH TOTALS            
         AR    R2,RF                                                            
         AR    R6,RF                                                            
*                                                                               
         AR    R3,R4               BUMP TO NEXT WEEK                            
         GOTO1 ADDAY,DMCB,THISDATE,THISDATE,F'7'                                
*                                                                               
         CLC   THISDATE,QUAREND    IF REACHED THE NEXT QUARTER                  
         BNH   VTOT10                                                           
         STCM  R2,15,QUARACC       THEN SAVE QUARTER TOTAL                      
         SR    R2,R2               AND CLEAR IT                                 
         LA    R5,QUARTABL(R5)     BUMP TO NEXT QUARTER                         
*                                                                               
         CLC   THISDATE,QEND       REPEAT UNTIL END OF WEEKLY TABLE             
         BNH   VTOT10                                                           
         DROP  R5                                                               
*                                                                               
VTOT50   ST    R6,0(R1)            STORE YEAR TOTAL BACK INTO PARM 1            
         B     XIT                                                              
         EJECT                                                                  
* SUBROUTINE TO LOOK IN AIO FOR THE DEMO OVERRIDE VALUE FOR DEMO                
* POINTED TO BY P1 AND DEMO BOOK POINTED TO BY P2. VALUE WILL BE                
* RETURNED IN P1                                                                
*                                                                               
VFINDOVR L     R2,0(R1)            R2 = A(DEMO)                                 
         L     R3,4(R1)            R3 = A(DEMO BOOK)                            
         MVC   0(4,R1),=X'0000FFFF'   SET VALUE NOT FOUND                       
         L     R6,AIO                                                           
         MVI   ELCODE,BOKCODEQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING DEMBOKEL,R6         R6 = A(FIRST DEMO BOOK ELEMENT)              
*                                                                               
VFO10    CLC   BOKBOOK,0(R3)       TEST MATCHING DEMO BOOK                      
         BE    VFO20                                                            
         BAS   RE,NEXTEL           BUMP TO NEXT ELEMENT                         
         BNE   VFOX                                                             
         B     VFO10                                                            
*                                                                               
VFO20    ZIC   R4,1(R6)            POINT R4 PASSED LAST DEMO                    
         AR    R4,R6                                                            
         LA    R5,BOKOVLST         POINT R5 TO FIRST DEMO                       
         USING OVLSTD,R5                                                        
*                                                                               
VFO30    CR    R5,R4               TEST PASSED LAST DEMO                        
         BNL   VFOX                                                             
         CLC   OVDEMO,0(R2)        TEST MATCHING DEMO                           
         BE    VFO40                                                            
         LA    R5,OVLSTL(R5)       BUMP TO NEXT DEMO                            
         B     VFO30                                                            
*                                                                               
VFO40    MVC   2(2,R1),OVVAL       RETURN DEMO VALUE                            
*                                                                               
VFOX     B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE COMPUTES THE NTP PERCENTAGE BY WEIGHTING EACH PRODUCT            
* GROUP BY THE NUMBER OF GOAL DOLLARS ALLOCATED FOR IT.                         
*                                                                               
VCMPNTP  DS    0H                                                               
         CLC   QSTART,=C'960000'   USE 2.25% FOR ESTIMATE BEFORE 1996           
         BH    VCN10                                                            
         MVC   NTPPERC,=F'225'                                                  
         B     VCNX                                                             
*                                                                               
VCN10    XC    GOLALL(16),GOLALL   CLEAR GOAL ACCUMS FOR EACH GROUP             
         GOTO1 CLRACC                                                           
         XC    ACCNUM,ACCNUM                                                    
*                                                                               
         CLI   TRADONLY,C'C'       IF CASH ESTIMATE                             
         BNE   VCN50                                                            
         BAS   RE,COMPGOAL         THEN COMPUTE GOAL VALUES                     
         B     VCN90                                                            
*                                                                               
VCN50    MVC   DUB,SVSUBS          ELSE PRETEND TO BE ASSOCIATED CASH           
         MVC   SVSUBS,ASCSUBS          ESTIMATE TO COMPUTE GOALS                
         MVI   TRADONLY,C'C'                                                    
         BAS   RE,COMPGOAL                                                      
*                                                                               
         MVC   SVSUBS,DUB          GO BACK TO REAL ESTIMATE                     
         MVI   TRADONLY,C'T'                                                    
*                                                                               
VCN90    XC    NTPPERC,NTPPERC                                                  
         OC    GOLALL,GOLALL                                                    
         BNZ   VCN92                                                            
         MVC   NTPPERC,=F'570'     ASSUME NTP GRP 1                             
         B     VCNX                                                             
*                                                                               
VCN92    L     R1,GOLGRP1          NTPPERC = (GRP1 * 5.70)/TOTAL +              
         M     R0,=F'570'                    (GRP2 * 15.0)/TOTAL +              
         D     R0,GOLALL                     (GRP3 * 15.0)/TOTAL                
         LR    RF,R1                                                            
         L     R1,GOLGRP2                                                       
         M     R0,=F'1500'                                                      
         D     R0,GOLALL                                                        
         AR    RF,R1                                                            
         L     R1,GOLGRP3                                                       
         M     R0,=F'1500'                                                      
         D     R0,GOLALL                                                        
         AR    RF,R1                                                            
         ST    RF,NTPPERC                                                       
*                                                                               
VCNX     B     XIT                                                              
         EJECT                                                                  
VMSPACK  DS    0H                                                               
         LR    R5,R1               R5 = CALLER'S PARM LIST                      
         LA    R4,WORK             R4 = A(STAPACK PARM BLOCK)                   
         USING STAPACKD,R4                                                      
*                                                                               
         XC    0(32,R4),0(R4)      FILL STAPACK PARM BLOCK                      
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPCTRY,SVAPROF+7                                               
         MVC   STAPMED,QMED                                                     
         MVC   STAPACOM,ACOMFACS                                                
         L     RE,0(R5)            GET A(MKT)                                   
         MVC   STAPQMKT,0(RE)                                                   
         L     RE,4(R5)            GET A(STA)                                   
         MVC   STAPQSTA(8),0(RE)                                                
         MVC   STAPQNET,QCBLNET                                                 
         GOTO1 STAPACK,(R4)                                                     
*                                                                               
         L     RE,8(R5)            RETURN MKTSTA                                
         MVC   0(5,RE),STAPMKST                                                 
*                                                                               
VPX      B     XIT                                                              
         EJECT                                                                  
VMSUNPK  DS    0H                                                               
         LR    R5,R1               R5 = CALLER'S PARM LIST                      
         LA    R4,WORK             R4 = A(STAPACK PARM BLOCK)                   
         USING STAPACKD,R4                                                      
*                                                                               
         XC    0(32,R4),0(R4)      FILL STAPACK PARM BLOCK                      
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPCTRY,SVAPROF+7                                               
         MVC   STAPMED,QMED                                                     
         MVC   STAPACOM,ACOMFACS                                                
         L     RE,0(R5)            GET A(MKTSTA)                                
         MVC   STAPMKST,0(RE)                                                   
         GOTO1 STAPACK,(R4)                                                     
*                                                                               
         L     RE,4(R5)            RETURN MKT                                   
         MVC   0(4,RE),STAPQMKT                                                 
         L     RE,8(R5)            RETURN STA                                   
         MVC   0(5,RE),STAPQSTA                                                 
         MVC   QCBLNET,STAPQNET                                                 
         CLI   QCBLNET,C' '        TEST CBLNET PRESENT                          
         BNH   VUX                 NO                                           
         TM    0(R5),X'80'         TEST PREPARED FOR CABLE                      
         BZ    VUX                                                              
         MVI   4(RE),C'/'                                                       
         MVC   5(3,RE),QCBLNET                                                  
*                                                                               
VUX      B     XIT                                                              
         EJECT                                                                  
         USING T218FFD,RA                                                       
ERRSNF   MVC   CONHEAD(37),=C'** ERROR CSO STATION RECORD NOT FOUND'            
         B     ERRX                                                             
*                                                                               
TRDERR   MVC   CONHEAD(28),=C'** ERROR TRADE ONLY ESTIMATE'                     
         B     ERRX                                                             
*                                                                               
ERRX     LA    R2,CONTAGH                                                       
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         GOTO1 ERREX2                                                           
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
XIT      XIT1                                                                   
         SPACE 1                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
* CONSTANTS TABLES, ETC *                                                       
         SPACE 1                                                                
RELO     DS    A                                                                
         SPACE 1                                                                
SYSVCON  DS    0F                                                               
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    V(MEDGET)                                                        
         DC    V(RECUP)                                                         
         DC    V(BINSRCH)                                                       
         DC    V(DPTRD)                                                         
         DC    V(EQVRD)                                                         
         DC    V(DUMMY)                                                         
         SPACE 1                                                                
NVTYPES  EQU   (*-SYSVCON)/4                                                    
         SPACE 1                                                                
CORETAB  DS    0X                                                               
         DC    AL1(QGENCON)                                                     
         DC    AL1(QOFFICER)                                                    
         DC    AL1(QDEMOCON)                                                    
         DC    AL1(QCLPACK)                                                     
         DC    AL1(QCLUNPK)                                                     
         DC    AL1(QGETDEM2)                                                    
         DC    AL1(QSTAPACK)                                                    
         DC    AL1(0)                                                           
         DC    AL1(QMOBILE)                                                     
         DC    AL1(QGETBROD)                                                    
CORES    EQU   (*-CORETAB)                                                      
         EJECT                                                                  
         PRINT GEN                                                              
*              DIRECTORY OF RECORDS AND ACTIONS                                 
         SPACE 1                                                                
RECACT   DS    0D                                                               
         SPACE 1                                                                
*                                  X'01' ENTRIES ARE AVAILABLE RECORDS          
*                                  CL8 EXPANDED RECORD NAME                     
*                                  CL1 RECORD NUMBER                            
*                                  CL1 PHASE NUMBER FOR DATA DICTIONARY         
*                                  CL1 PHASE NUMBER FOR HELP SCREEN             
         SPACE 1                                                                
         DC    X'01',C'PROGRAM ',AL1(01),X'00C2'                                
         DC    X'01',C'HELP    ',AL1(00),X'00CF'                                
         DC    X'01',C'STATION ',AL1(02),X'00C2'                                
         DC    X'01',C'ALLOC   ',AL1(03),X'00C2'                                
         DC    X'01',C'COMMENTS',AL1(04),X'00C2'                                
         DC    X'01',C'BUY     ',AL1(05),X'00C2'                                
         DC    X'01',C'DEMO    ',AL1(06),X'00C2'                                
         DC    X'01',C'MASTER  ',AL1(07),X'00C2'                                
         DC    X'01',C'6030    ',AL1(08),X'00C2'                                
         DC    X'01',C'NTPCALC ',AL1(09),X'00C2'                                
         DC    X'01',C'1530    ',AL1(10),X'00C2'                                
         DC    X'01',C'WEEK    ',AL1(11),X'00C2'                                
         DC    X'01',C'OVERNITE',AL1(12),X'00C2'                                
         DC    X'01',C'SHOW    ',AL1(13),X'00C2'                                
         DC    X'01',C'CONTRACT',AL1(14),X'00C2'                                
         SPACE 2                                                                
*                                  X'02' ENTRIES ARE AVAILABLE ACTIONS          
*                                  CL8 EXPANDED ACTION NAME                     
*                                  CL1 ACTION NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 SPARE                                    
         SPACE 1                                                                
         DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,01,00)                                  
         DC    X'02',C'DISPLAY ',AL1(03,01,00)                                  
         DC    X'02',C'DELETE  ',AL1(04,01,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,01,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,01,00)                                  
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
         DC    X'02',C'REPORT  ',AL1(12,12,00)                                  
         DC    X'02',C'STATUS  ',AL1(13,13,00)                                  
         DC    X'02',C'RECAP   ',AL1(14,14,00)                                  
         DC    X'02',C'GENERATE',AL1(15,15,00)                                  
         DC    X'02',C'TRANSFER',AL1(16,16,00)                                  
         DC    X'02',C'OVERRIDE',AL1(17,17,00)                                  
         DC    X'02',C'ADJUST  ',AL1(18,18,00)                                  
         DC    X'02',C'TREPORT ',AL1(19,19,00)                                  
         DC    X'02',C'CONVERT ',AL1(20,20,00)                                  
         DC    X'02',C'MOVE    ',AL1(21,21,00)                                  
         DC    X'02',C'EXTENT  ',AL1(22,22,00)                                  
         DC    X'02',C'HELP    ',AL1(00,00,00)                                  
         EJECT                                                                  
*              DIRECTORY OF PHASES FOR SELECTED RECORD/ACTION                   
         SPACE 1                                                                
*                                  X'03' ENTRIES ARE OK REC/ACT COMBOS          
*                                  CL1 RECORD NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 PHASE NUMBER FOR SCREEN                  
*                                  CL1 PHASE NUMBER FOR EDIT                    
*                                  CL1 PHASE NUMBER FOR SPECS                   
*                                  CL1 PHASE NUMBER FOR REPORT                  
*                                  CL1 WHEN OK BITS 80=SCREEN 40=NOW            
*                                      20=SOON 10=OV 08=DDS                     
*                                      01=USER MAINTENANCE                      
*                                  CL2 CODE FOR REPORTS                         
*                                  CL2 CODE FOR EOD HANDLING                    
         SPACE 1                                                                
         DC    X'03',AL1(01,01),X'F1010000C0',C'    '  PROGRAM  MAINT           
         DC    X'03',AL1(01,10),X'E1010000C0',C'    '           LIST            
         DC    X'03',AL1(02,01),X'F2020000C0',C'    '  STATION  MAINT           
         DC    X'03',AL1(02,10),X'E2020000C0',C'    '           LIST            
         DC    X'03',AL1(03,13),X'F3030000C0',C'    '  ALLOC    STATUS          
         DC    X'03',AL1(03,14),X'F4040000C0',C'    '  ALLOC    RECAP           
         DC    X'03',AL1(03,15),X'F5050000C0',C'    '  ALLOC   GENERATE         
         DC    X'03',AL1(03,12),X'F606000058',C'ARCS'  ALLOC    REPORT          
         DC    X'03',AL1(03,19),X'F606000058',C'ARCS'  ALLOC    TREPORT         
         DC    X'03',AL1(04,01),X'F7070000C0',C'    '  COMMENTS MAINT           
         DC    X'03',AL1(05,16),X'F8080000C0',C'    '  BUY     TRANSFER         
         DC    X'03',AL1(12,16),X'F808000018',C'CXCX'  OVERNITETRANSFER         
         DC    X'03',AL1(06,17),X'F9090000C0',C'    '  DEMO    OVERRIDE         
         DC    X'03',AL1(01,18),X'FA0A0000C0',C'    '  PROGRAM  ADJUST          
         DC    X'03',AL1(07,01),X'FB0B0000C0',C'    '  MASTER   MAINT           
         DC    X'03',AL1(08,20),X'FC0C0000C0',C'    '  6030     CONVERT         
         DC    X'03',AL1(09,01),X'FD0D0000C0',C'    '  NTPCALC  MAINT           
         DC    X'03',AL1(10,20),X'FE0C0000C0',C'    '  1530     CONVERT         
         DC    X'03',AL1(11,21),X'E414000040',C'CYCY'  WEEK     MOVE            
         DC    X'03',AL1(12,21),X'E414000018',C'CYCY'  OVERNITE MOVE            
         DC    X'03',AL1(13,01),X'D1210000C0',C'    '  SHOW     MAINT           
         DC    X'03',AL1(13,10),X'C1210000C0',C'    '           LIST            
         DC    X'03',AL1(14,01),X'D2220000C0',C'    '  CONTRACT MAINT           
         DC    X'03',AL1(14,10),X'C2220000C0',C'    '           LIST            
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPCSOWORKD                                                     
         EJECT                                                                  
         ORG   SYSSPARE+1024                                                    
       ++INCLUDE DEDBLOCK                                                       
         ORG                                                                    
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
         EJECT                                                                  
       ++INCLUDE SPGENCSO                                                       
         EJECT                                                                  
       ++INCLUDE SPCSOFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDOFFICED                                                      
         EJECT                                                                  
       ++INCLUDE SPSTAPACKD                                                     
         EJECT                                                                  
       ++INCLUDE SPSTABLK                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DDCOREQUS                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FATIOB                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045SPCSO00   08/06/03'                                      
         END                                                                    
