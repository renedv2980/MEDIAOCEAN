*          DATA SET ACREQ01    AT LEVEL 093 AS OF 02/17/21                      
*PHASE T60401B                                                                  
         TITLE 'ACREQ01 - REQUEST - VALIDATE DEFN AND BUILD A SCREEN'           
*--------------------------------------------------------------------*          
* PID  LVL DATE    COMMENTS                                                     
* ---------------------------------                                             
* GHOA 090 02AUG19 SPEC-30973 SUPPORT DOLLAR TOLERANCE                          
* JSHA 091 15JAN20 SPEC-41535 Check Delivery Flag                               
* GHOA 092 22OCT20 SPEC-51025 1099 new requirements for 2020                    
* GHOA 093 20JAN21 SPEC-41535 Check Flag description change                     
*--------------------------------------------------------------------*          
T60401   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LWSX-LWS,T60401,RA,RR=R5                                         
         USING LWS,RC              RC=A(LOCAL WORKING STORAGE)                  
         L     R9,0(R1)                                                         
         USING GWS,R9              R9=A(GLOBAL WORKING SRORAGE)                 
         LA    R8,RCARDS                                                        
         USING ACQD,R8             R8=A(REQUEST CARDS)                          
         L     R3,ASAVE                                                         
         USING TWAD,R3             R3=A(TWA)                                    
         ST    R5,RELO                                                          
*                                                                               
         LA    R1,ADCONS                                                        
         LA    RF,ATYPE1                                                        
         LA    R0,NADCONS                                                       
RELOLOOP L     RE,0(R1)                                                         
         A     RE,RELO                                                          
         ST    RE,0(RF)                                                         
         LA    R1,4(R1)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,RELOLOOP                                                      
         EJECT                                                                  
***********************************************************************         
*              INITIALIZE ACCOUNT KEY & REQUEST RECORD                *         
***********************************************************************         
         SPACE 1                                                                
         MVC   KEY,SPACES          INITIALIZE KEY                               
         XC    REQNUM(16),REQNUM                                                
         XC    RHDR,RHDR           INITIALIZE REQ REC                           
         XC    REQRECN,REQRECN                                                  
         MVC   RCARD1,SPACES                                                    
         MVC   RCARD2,SPACES                                                    
         MVC   RCARD3,SPACES                                                    
         MVC   RCARD4,SPACES                                                    
         MVC   RCARD5,SPACES                                                    
         MVC   SOFFICE,SPACES                                                   
         MVI   FIND,0                                                           
         MVC   FERN,=AL2(FF)                                                    
         MVI   USRIDF,0                                                         
         MVI   LOFFPOS,0                                                        
         MVI   LOFFCHK,0                                                        
         EJECT                                                                  
***********************************************************************         
*              VALIDATE REQUESTOR NAME FIELD                          *         
***********************************************************************         
         SPACE 1                                                                
VALNAME  CLI   BVRNAMEH+5,0                                                     
         BE    VALNAMA                                                          
         CLI   BVRNAMEH+5,4                                                     
         BL    VALNM40                                                          
         SR    R1,R1                                                            
         IC    R1,BVRNAMEH+5                                                    
         SH    R1,=H'1'                                                         
         LA    RE,MENUTAB          VALID MENU NAMES                             
VALNM10  CLI   0(RE),X'FF'                                                      
         BE    VALNM20                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),BVRNAME                                                  
         BE    *+12                                                             
         LA    RE,L'MENUTAB(RE)                                                 
         B     VALNM10                                                          
*                                                                               
         MVC   MENUS,15(RE)        MOVE IN VALID MENU BITS                      
         CLI   MENUS,ALL-1         ONLY DDS TERMINALS CAN                       
         BL    *+12                SEE EVERYTHING                               
         CLI   DDS,1                                                            
         BNE   VALNM40             REGARD AS REQUESTOR NAME                     
         XC    NEXTAD,NEXTAD       CLEAR CONTINUATION ADDRESS                   
         B     VALNM30                                                          
*                                                                               
VALNM20  CLC   BVRNAME(8),=C'NEXTMENU'                                          
         BNE   VALNM40                                                          
VALNM30  MVC   REQNDX1,=X'FFFC'    SET MENU SCREEN ID                           
         B     VALDEF1                                                          
*                                                                               
VALNM40  CLI   DDS,0               DDS TERMINALS CAN HAVE KEYWORDS              
         BNE   VALNM50                                                          
         CLI   BVRNAMEH+5,12       USER TERM HAS REQUESTOR NAME ONLY            
         BH    INVNAME                                                          
         SR    R1,R1                                                            
         IC    R1,BVRNAMEH+5       MAX LEN IS 12 CHRS                           
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACQESTOR(0),BVRNAME                                              
         OI    FIND,FIINP          SET REQUESTOR NAME INPUT                     
         B     VALNAMA                                                          
*                                                                               
VALNM50  LA    R7,TEMP                                                          
         GOTO1 SCANNER,PLIST,BVRNAMEH,(3,(R7))                                  
         SR    R5,R5                                                            
         IC    R5,4(R1)                                                         
         LTR   R5,R5               R5=NUM OF INPUT FIELDS                       
         BZ    INVNAME                                                          
*                                                                               
VALNM60  CLI   1(R7),0             ORDINARY FIELD IS REQUESTOR NAME             
         BNE   VALNM70                                                          
         CLI   0(R7),12            MAX LEN IS 12 CHRS                           
         BH    INVNAME                                                          
         MVC   ACQESTOR,12(R7)                                                  
         OI    FIND,FIINP          SET REQUESTOR NAME INPUT                     
         B     VALNM160                                                         
*                                                                               
VALNM70  CLI   12(R7),C'U'         U=XXX... FOR USERID INPUT                    
         BNE   VALNM120                                                         
         CLI   1(R7),3                                                          
         BL    INVNAME                                                          
         BH    VALNM80                                                          
         CLC   22(3,R7),=C'ALL'    U=ALL CHANGE TO AGY=00                       
         BNE   VALNM80                                                          
         MVI   1(R7),2                                                          
         MVI   3(R7),X'20'                                                      
         MVC   22(2,R7),=C'00'                                                  
         B     VALNM150                                                         
*                                                                               
VALNM80  CLI   1(R7),10                                                         
         BH    INVNAME                                                          
         L     R4,AIO1             READ USER ID REC                             
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKID,22(R7)                                                    
         GOTO1 DATAMGR,DMCB,(0,DMREAD),CTFILE,(R4),(R4)                         
         CLI   8(R1),0                                                          
         BNE   INVNAME                                                          
         LA    RE,CTIDATA                                                       
         SR    RF,RF                                                            
*                                                                               
VALNM90  CLI   0(RE),0             SEARCH FOR SYSTEM ELEMENT                    
         BE    INVNAME                                                          
         CLI   0(RE),X'21'                                                      
         BE    VALNM110                                                         
         CLI   0(RE),X'02'                                                      
         BNE   *+10                                                             
         MVC   DUB(2),2(RE)        SAVE USER ID NUM                             
VALNM100 IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     VALNM90                                                          
*                                                                               
VALNM110 CLC   SYSNUMOV,CTSYSNUM-CTSYSD(RE)                                     
         BNE   VALNM100                                                         
         MVC   DUB+2(1),CTSYSSE-CTSYSD(RE)                                      
         MVC   DUB+3(1),CTSYSAGB-CTSYSD(RE)                                     
         XC    DUB+4(2),DUB+4                                                   
*                                                                               
         CLC   SYSNUM,DUB+2        MUST BE SAME SYSTEM AS CONNECT               
         BNE   INVNAME                                                          
         CLI   USRIDF,0                                                         
         BNE   INVNAME             ONLY ONE U= OR A=                            
         OI    USRIDF,X'80'                                                     
         MVC   CMPY,DUB+3          SET NEW COMPANY CODE                         
         MVC   USRID,DUB           SET NEW USERID NUMBER                        
         B     VALNM160                                                         
*                                                                               
VALNM120 CLI   12(R7),C'T'         T=Y TO DEFINE UNKNOWN REQUEST                
         BNE   *+12                                                             
         OI    FIND,FIVAL                                                       
         B     VALNM130                                                         
         CLC   12(3,R7),=C'CPY'    CPY=X TO DEFINE AGY X (OLD SYNTAX)           
         BE    VALNM150                                                         
         CLI   12(R7),C'C'         C=Y TO DEFINE CARD REQUEST                   
         BNE   *+12                                                             
         OI    FIND,FIALT                                                       
         B     VALNM130                                                         
         CLI   12(R7),C'1'         1=Y TO DEFINE DDS ONLY FLDS INCLUDED         
         BNE   *+12                                                             
         OI    FIND,X'14'                                                       
         B     VALNM130                                                         
         CLI   12(R7),C'2'         2=Y TO DEFINE DDS/2UP FLDS DISPLAY           
         BNE   VALNM140                                                         
         OI    FIND,X'34'                                                       
*                                                                               
VALNM130 CLI   1(R7),1                                                          
         BNE   INVNAME                                                          
         CLI   22(R7),C'Y'                                                      
         BNE   INVNAME                                                          
         B     VALNM160                                                         
*                                                                               
VALNM140 CLI   12(R7),C'A'         A=X TO DEFINE AGY X (OR 00 FOR ALL)          
         BE    VALNM150                                                         
         CLI   12(R7),C'D'         D=X TO DEFINE AGY X AND DDS OFFICE           
         BNE   INVNAME             INVALID KEYWORD LETTER                       
         MVC   REQOFFC,=C'DDS*'                                                 
VALNM150 MVC   CMPY,22(R7)                                                      
         CLI   USRIDF,0                                                         
         BNE   INVNAME             ONLY ONE A= OR U=                            
         OI    USRIDF,X'40'                                                     
         CLI   1(R7),1             ONE CHR CODE ASSUMED ALPHA                   
         BE    VALNM160                                                         
         CLI   1(R7),2             TWO CHR CODE ASSUMED HEX                     
         BNE   INVNAME                                                          
         TM    3(R7),X'20'                                                      
         BZ    INVNAME                                                          
         GOTO1 HEXIN,PLIST,22(R7),CMPY,2                                        
         OC    12(4,R1),12(R1)                                                  
         BZ    INVNAME                                                          
*                                                                               
VALNM160 LA    R7,32(R7)           BUMP TO NEXT FIRLD                           
         BCT   R5,VALNM60                                                       
         B     VALNAMA                                                          
*                                                                               
INVNAME  MVC   FERN,=AL2(INVINPT)  INVALID INPUT FIELD                          
         B     *+10                                                             
MISNAME  MVC   FERN,=AL2(FLDMIS)                                                
         LA    R7,BVRNAMEH                                                      
         ST    R7,FADR                                                          
         B     EXIT                                                             
*                                                                               
*                                                                               
* READ AND SAVE COMPANY DATA IF NEW COMPANY                                     
*                                                                               
VALNAMA  MVC   ACQCPY,CMPY         SET COMPANY IN REQ REC AND KEY               
         MVC   KEY(1),CMPY                                                      
         CLC   CMPY,LKEY                                                        
         BE    VALNAMX                                                          
         XC    SAVEOFFA,SAVEOFFA   CLEAR OFFAL SAVE AREA                        
         CLI   CMPY,0                                                           
         BE    VALNAMX                                                          
         GOTO1 AIOREAD             READ COMPANY RECORD                          
         BE    *+14                                                             
         TM    USRIDF,X'40'                                                     
         BO    INVNAME                                                          
         DC    H'0'                DIE IF COMPANY RECORD NOT FOUND              
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'10'        GET COMPANY ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                DIE IF CANT FIND COMPANY ELEMENT             
*                                                                               
         USING CPYELD,R6                                                        
         CLI   CPYLN,CPYLN4Q       DO WE HAVE THE GL ELEMENT                    
         BL    *+10                                                             
         MVC   COMPGLM,CPYGLMOA                                                 
*                                                                               
         MVC   LREQUL,CPYGLU       SAVE UNIT/LEDGERS                            
         MVC   LREQID,CPYUID       SAVE PRINCIPAL ID NUM                        
         MVC   COMPSTA1,CPYSTAT1   SAVE COMPANY STATUS BYTES                    
         MVC   COMPSTA2,CPYSTAT2                                                
         MVC   COMPSTA3,CPYSTAT3                                                
         MVC   COMPSTA4,CPYSTAT4                                                
         MVC   COMPSTA5,CPYSTAT5                                                
         MVC   COMPSTA6,CPYSTAT6                                                
         MVC   COMPSTA7,CPYSTAT7                                                
         MVC   COMPMOSX,CPYMOSX                                                 
*                                                                               
         MVI   DPS,X'7B'           DEFAULT                                      
         TM    CPYSTAT3,CPYSDPST   DPS-TYPE COMPANY                             
         BZ    *+10                                                             
         MVC   DPS,CMPY                                                         
*                                                                               
         MVI   CMPSTAT,0           INITIALIZE INTERNAL COMPANY STATUS           
         TM    CPYSTAT4,CPYSOFF2   X'01' = COMPANY USING NEW OFFICES            
         BZ    *+8                                                              
         OI    CMPSTAT,CMPNOFF                                                  
         DROP  R6                                                               
*                                                                               
         MVI   COMPSTAA,0                                                       
         USING CPYELD,R6                                                        
         CLI   CPYLN,CPYLN3Q                                                    
         BL    *+10                                                             
         MVC   COMPSTAA,CPYSTATA                                                
*                                                                               
***********************************************************************         
*        GOTO1 DATAMGR,DMCB,=C'DTFADD',=C'ACCOUNT'                    *         
*        L     RE,12(R1)           GET A(ACCOUNT DCB)                 *         
*        TM    ISFTYPE-ISDTF(RE),ISFTEMU                              *         
*        BZ    *+8                                                    *         
***********************************************************************         
*                                                                               
         OI    CMPSTAT,CMPEMUL     COMPANY IS EMULATED FILE                     
*                                                                               
* MAKE CALL TO SECRET TO CHECK SECURITY                                         
*                                                                               
         XC    DMCB(24),DMCB       INIT SECRET                                  
         GOTO1 SECRET,DMCB,('SECPINIT',ASECBLK)                                 
         BE    *+6                                                              
         DC    H'0'                BLOCK NOT BIG ENOUGH                         
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVI   BYTE,SALARYQ                CHECK ACCESS TO VIEW SALARY          
         GOTO1 SECRET,DMCB,('SECPFLDP',ASECBLK),BYTE                            
         CLI   DMCB,SECPYES                                                     
         BNE   *+12                                                             
         OI    CMPSTAT,CMPNSWT     READ/WRITE                                   
         B     VALANM10                                                         
         CLI   DMCB,SECPREAD                                                    
         BNE   VALANM10                                                         
         OI    CMPSTAT,CMPNSRD     READ                                         
*                                                                               
VALANM10 XC    SV1RLEVS,SV1RLEVS   GET HIERARCHY OF U/L = 1R                    
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),ACQCPY                                                    
         MVC   KEY+1(2),=C'1R'                                                  
         GOTO1 AIOREAD                                                          
         BNE   VALANM20                                                         
         L     R6,AIO1                                                          
         MVI   ELCODE,X'16'        GET HIERARCHY ELEMENT                        
         BAS   RE,GETEL                                                         
         BNE   VALANM20                                                         
*                                                                               
         USING ACLELD,R6                                                        
         MVC   SV1RLEV1,ACLELLVA   SAVE L'ACCT LEVEL 1                          
         MVC   SV1RLEV2,ACLELLVB   SAVE L'ACCT LEVEL 2                          
         MVC   SV1RLEV3,ACLELLVC   SAVE L'ACCT LEVEL 3                          
         MVC   SV1RLEV4,ACLELLVD   SAVE L'ACCT LEVEL 4                          
         DROP  R6                                                               
*                                                                               
VALANM20 XC    LREQLEVA(2),LREQLEVA                                             
         MVC   KEY+1(2),LREQJOB                                                 
         GOTO1 AIOREAD             READ PRODUCTION LEDGER RECORD                
         BNE   VALNAMX                                                          
         L     R6,AIO1                                                          
         MVI   ELCODE,X'16'        GET HIERARCHY ELEMENT                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ACLELD,R6                                                        
         MVC   LREQLEVA,ACLELLVA   SAVE L'ACCT LEVEL 1                          
         MVC   LREQLEVB,ACLELLVB   SAVE L'ACCT LEVEL 2                          
         DROP  R6                                                               
*                                                                               
VALNAMX  MVC   KEY+1(2),SPACES     RESET ACCOUNT KEY                            
         MVC   FERN,=AL2(FF)       RESET TO NO ERROR                            
         EJECT                                                                  
***********************************************************************         
*              VALIDATE REQUEST NUMBER & ACTION                       *         
***********************************************************************         
*                                                                               
VALNUM   CLI   BVRNUMH+5,0                                                      
         BE    MISNUM                                                           
         MVC   IFLDH,BVRNUMH       COPY FIELD INTO IFLD                         
         MVC   IFLD,SPACES                                                      
         SR    R1,R1                                                            
         IC    R1,IFLDH+5                                                       
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   IFLD(0),BVRNUM                                                   
         CLI   IFLDH+5,2                                                        
         BL    INVNUM                                                           
         BE    *+12                                                             
         CLI   IFLD+2,C','                                                      
         BNE   VALNUM1                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   IFLD+1(0),BVRNUM    CONVERT TWO CHR ID TO #XX FORMAT             
         MVI   IFLD,C'#'                                                        
         SR    R1,R1                                                            
         IC    R1,IFLDH+5                                                       
         LA    R1,1(R1)                                                         
         STC   R1,IFLDH+5                                                       
*                                                                               
VALNUM1  L     R7,AREQTBL          SEARCH REQUEST TABLE                         
         USING HDRD,R7                                                          
         CLC   IFLD(3),=C'ALL'     CHECK FOR ID=ALL IF DDS REQUESTOR            
         BNE   VALNUM1A                                                         
         CLI   DDS,1                                                            
         BNE   INVNUM                                                           
         MVI   REQNUM,255          SET ALL VALUE IN REQNUM                      
         XC    BVRRNAM,BVRRNAM                                                  
         OI    BVRRNAMH+6,X'80'                                                 
         CLI   IFLD+3,C','         ACTION MUST BE DISPLAY                       
         BNE   INVNUM                                                           
         MVC   REQACTN,IFLD+4                                                   
         CLI   REQACTN,C'D'                                                     
         BNE   INVACTN                                                          
         MVI   REQOPTN,C'S'                                                     
         MVC   REQINCR,=H'1'                                                    
         B     VALNUM4                                                          
VALNUM1A OC    HDRLEN,HDRLEN       TEST FOR END OF TABLE                        
         BZ    VALNUM1C                                                         
         SR    R2,R2                                                            
         ICM   R2,3,HDRLEN         R2=TABLE ENTRY LENGTH                        
         CLI   IFLD,C'#'                                                        
         BE    VALNUM1B                                                         
         CLC   HDRNAME(3),IFLD     MATCH ON THREE CHR MNENOMIC                  
         BE    VALNUM1D                                                         
         AR    R7,R2                                                            
         B     VALNUM1A                                                         
VALNUM1B LA    RF,0(R7,R2)         POINT TO LAST TWO BYTES OF ENTRY             
         SH    RF,=H'2'                                                         
         CLC   0(2,RF),IFLD+1      MATCH ON TWO CHR REQUEST ID                  
         BE    VALNUM1D                                                         
         AR    R7,R2                                                            
         B     VALNUM1A                                                         
*                                                                               
VALNUM1C TM    FIND,X'0C'          REQUEST NOT FOUND                            
         BZ    INVNUM              OK FOR CARD/TEST OPTION                      
         CLI   IFLD,C'#'                                                        
         BNE   INVNUM                                                           
         L     R7,AREQTBL          POINT TO FIRST ENTRY                         
         USING HDRD,R7                                                          
         SR    R2,R2                                                            
         ICM   R2,3,HDRLEN                                                      
         MVC   ACQPROG,IFLD+1                                                   
         B     VALNUM1E                                                         
*                                                                               
VALNUM1D CLI   HDRNUM,0             REQUEST FOUND                               
         BE    INVNUM                                                           
         MVC   REQNUM,HDRNUM       SAVE INTERNAL BINARY REQUEST NUM             
         MVI   REQNUM+1,0                                                       
         LA    RF,0(R2,R7)                                                      
         SH    RF,=H'2'                                                         
         MVC   ACQPROG,0(RF)          SAVE REQ ID IN REQ REC                    
VALNUM1E MVC   BVRRNAM(22),HDRNAME DISPLAY REQ NAME AND #ID OR MNEMONIC         
         CLI   IFLD,C'#'                                                        
         BE    *+14                                                             
         MVI   BVRRNAM,C'#'                                                     
         MVC   BVRRNAM+1(2),ACQPROG                                             
         OI    BVRRNAMH+6,X'80'                                                 
*                                                                               
VALNUM2  MVC   REQNUMB,REQNUM      R7=A(REQTBL ENTRY)                           
         MVI   REQACTN,C'N'        SET DEFAULT VALUES                           
         MVI   REQOPTN,C'S'                                                     
         MVC   REQINCR,=H'1'                                                    
         CLI   IFLDH+5,3           ONLY NUM INPUT                               
         BE    VALNUM3A            YES                                          
         CLI   IFLD+3,C','         NO MUST DELIMIT WITH ,                       
         BNE   INVNUM                                                           
*                                                                               
         MVC   REQACTN,IFLD+4      CHECK ACTION VALUE                           
         CLI   REQACTN,C'A'        AMEND                                        
         BE    VALNUM3                                                          
         CLI   REQACTN,C'D'        DISPLAY                                      
         BE    VALNUM4                                                          
         CLI   REQACTN,C'N'        NEW (DEFAULT)                                
         BE    VALNUM3                                                          
         B     INVACTN                                                          
*                                                                               
VALNUM3  CLI   IFLDH+5,5           NO OPTIONS FOR A OR N                        
         BNE   INVACTN                                                          
VALNUM3A MVC   BVROPT,SPACES                                                    
         MVI   BVROPTH+5,0         CLEAR LENGTH                                 
         OI    BVROPTH+1,X'20'     PROTECT OPTION FIELD                         
         OI    BVROPTH+6,X'80'     TRANSMIT OPT FLD                             
         OI    BVROPTLH+1,X'0C'    TURN OFF INTENSITY                           
         OI    BVROPTLH+6,X'80'    TRANSMIT OPT LABEL FLD                       
         CLI   CMPY,0              COMPANY MUST BE SPECIFIC FOR A OR N          
         BE    INVNAME                                                          
         TM    USRIDF,X'40'                                                     
         BO    INVNAME                                                          
         B     VALNUMX                                                          
*                                                                               
VALNUM4  DS    0H                                                               
         CLI   REQACTN,C'D'        DISPLAY                                      
         BNE   VALNUM5                                                          
         NI    BVROPTH+1,X'FF'-X'20'   UNPROTECT OPTION FIELD                   
         OI    BVROPTH+6,X'80'         TRANSMIT OPT FLD                         
         NI    BVROPTLH+1,X'FF'-X'04'  TURN ON INTENSITY                        
         OI    BVROPTLH+6,X'80'        TRANSMIT OPT LABEL FLD                   
*                                                                               
VALNUM5  CLI   IFLDH+5,5                                                        
         BE    VALNUMX             USE DEFAULT OPTION                           
         CLI   IFLD+5,C','                                                      
         BNE   INVACTN                                                          
         SR    R5,R5                                                            
         IC    R5,IFLDH+5                                                       
         SH    R5,=H'7'            R5=L'OPTION-1                                
         BM    INVACTN                                                          
         MVC   REQOPTN,IFLD+6      SAVE OPTION                                  
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   IFLD+6(0),=C'TOTAL'                                              
         BNE   *+16                                                             
         CLI   REQNUM,255          TOTAL OPTION ONLY VALID FOR ALL              
         BNE   INVOPTN                                                          
         B     VALNUMX                                                          
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   IFLD+6(0),=C'NEXT'                                               
         BE    VALNUMX                                                          
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   IFLD+6(0),=C'LAST'                                               
         BE    VALNUMX                                                          
         MVC   TEMP(5),=C'00000'   N THRU NNNNN OK                              
         LA    R6,4                                                             
         SR    R6,R5                                                            
         BM    INVOPTN                                                          
         LA    R6,TEMP(R6)                                                      
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),IFLD+6                                                   
         MVC   TEMP+5(5),=C'00000'                                              
         MVZ   TEMP+5(5),TEMP                                                   
         CLC   TEMP+5(5),=C'00000'                                              
         BNE   INVOPTN                                                          
         PACK  DUB,TEMP(5)                                                      
         CVB   R6,DUB                                                           
         LTR   R6,R6                                                            
         BZ    INVOPTN             LOWEST SEQUENCE NUM IS ONE                   
         STH   R6,REQINCR                                                       
         MVI   REQOPTN,C'S'        SET SEQUENCE NUM OPTION                      
*                                                                               
VALNUMX  DS    0H                                                               
         CLC   ACQPROG,=C'91'         CERTAIN PROGRAMS                          
         BE    CKPRINC                                                          
         CLC   ACQPROG,=C'25'                                                   
         BNE   VALDPS                                                           
         TM    TWAAUTH,X'10'       (FOR 25, IF X'10' BIT ON, IT'S OK)           
         BO    VALDPS                                                           
         B     *+14                                                             
CKPRINC  CLC   LREQID,USRID        SIGN-ON ID MUST MATCH PRINCIPAL ID           
         BE    VALDPS                                                           
         MVC   FERN,=AL2(SECLOCK)  SECURITY LOCKOUT                             
         B     INVNUM2                                                          
*                                                                               
VALDPS   DS    0H                  CHECK FOR DDS ONLY REQ SCREENS               
         CLI   DDS,1               OK IF DDS TERMINAL                           
         BE    VALDPS1                                                          
*                                                                               
         CLC   ACQPROG,=C'83'      ONLY HRSF CAN REQUEST THESE REPRTS           
         BE    VALDPSA             EVERYONE ELSE MUST USE SCRIBE                
         CLC   ACQPROG,=C'32'                                                   
         BE    VALDPSA                                                          
         CLC   ACQPROG,=C'33'                                                   
         BE    VALDPSA                                                          
         CLC   ACQPROG,=C'34'                                                   
         BE    VALDPSA                                                          
         CLC   ACQPROG,=C'40'                                                   
         BE    VALDPSA                                                          
         B     *+14                                                             
VALDPSA  CLC   TWAAGY,=C'HR'       HRSF                                         
         BNE   INVNUM                                                           
*                                                                               
         CLC   ACQPROG,=C'MB'      ZENITH MEDIA BILLING ONLY                    
         BNE   VALDPS1             REQUESTABLE BY ZENITH                        
         CLC   TWAAGY,=C'TH'       ZENITH                                       
         BE    VALDPS1                                                          
         MVC   FERN,=AL2(SECLOCK)  SECURITY LOCKOUT                             
         B     INVNUM2                                                          
*                                                                               
VALDPS1  TM    HDRSTA,HDDSO       TEST FOR DDS ONLY?                            
         BZ    VDPS2               NO                                           
         CLI   DDS,1               YES, IS THIS A DDS TERNIMAL?                 
         BNE   INVNUM              NO, ERROR                                    
*                                                                               
VDPS2    TM    HDRSTA,X'40'        X'08' BIT MUST BE OFF IF AT DDS              
         BZ    VALDEST                                                          
         CLI   DDS,1                                                            
         BNE   VALDEST                                                          
         TM    TWAAUTH,X'08'                                                    
         BO    INVNUM                                                           
         B     VALDEST                                                          
         DROP  R7                                                               
*                                                                               
MISNUM   MVC   FERN,=AL2(FLDMIS)                                                
         XC    BVRRNAM,BVRRNAM                                                  
         B     INVNUM2                                                          
INVNUM   XC    BVRRNAM,BVRRNAM                                                  
INVNUM1  MVC   FERN,=AL2(INVREQN)  INVALID REQUEST NUMBER                       
INVNUM2  OI    BVRRNAMH+6,X'80'                                                 
         LA    R7,BVRNUMH                                                       
         ST    R7,FADR                                                          
         B     EXIT                                                             
INVACTN  MVC   FERN,=AL2(INVACT)   INVALID ACTION                               
         B     INVNUM2                                                          
INVOPTN  MVC   FERN,=AL2(INVINPT)  INVALID INPUT                                
         B     INVNUM2                                                          
INVINCR  EQU   INVOPTN                                                          
         EJECT                                                                  
***********************************************************************         
*              VALIDATE DESTINATION ID NAME                           *         
***********************************************************************         
*                                                                               
         USING HDRD,R7                                                          
VALDEST  DS    0H                                                               
         MVC   REQORIG,USRID      SET ORIGIN ID NUM IN REQ REC HDR              
         TM    RFPSTAT,RFPINUSE                                                 
         BO    VALDESTX                                                         
         CLI   BVRDESTH+5,0                                                     
         BE    VALDESTX            NO DEST INPUT                                
         CLI   REQNUM,255                                                       
         BE    VDEST0                                                           
         CLI   REQACTN,C'D'                                                     
         BE    VDEST0                                                           
         TM    HDRINBS,HNDES      TEST IF DEST ALLOWED FOR THIS REQ             
         BO    INVDEST                                                          
*                                                                               
VDEST0   SR    R1,R1                                                            
         IC    R1,BVRDESTH+5                                                    
         SH    R1,=H'1'                                                         
         MVC   IFLD,SPACES                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   IFLD(0),BVRDEST                                                  
*                                                                               
         L     R4,AIO1             READ ORIGIN ID REC                           
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKID+8(2),REQORIG                                              
*                                                                               
         CLC   IFLD(10),=CL10'SJR'                                              
         BNE   *+10                                                             
         MVC   CTIKID(10),IFLD                                                  
*                                                                               
         CLC   IFLD(10),=CL10'DDS'                                              
         BNE   VDEST2                                                           
         MVC   REQDEST,=H'90'      DEST ID NUMBER                               
         B     VALDESTX                                                         
*                                                                               
VDEST2   GOTO1 DATAMGR,DMCB,(0,DMREAD),CTFILE,(R4),(R4)                         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,DATAMGR                                                       
         L     R6,AIO1                                                          
         GOTO1 GETIDS,PLIST,(C'D',(R6)),0,(R5)                                  
         CLI   PLIST,0                                                          
         BE    INVDEST             NO DESTS FOUND                               
         CLI   PLIST,X'FF'                                                      
         BNE   *+6                 DISK ERROR                                   
         DC    H'0'                                                             
         SR    R5,R5                                                            
         IC    R5,PLIST             NUMBER OF DESTS                             
         L     R6,PLIST+4           ADDR OF BLOCK OF DESTS                      
*                                                                               
VDEST4   CLI   0(R6),X'FF'         END OF TABLE                                 
         BE    INVDEST                                                          
         CLC   IFLD(10),0(R6)                                                   
         BNE   VDEST5                                                           
         MVC   REQDEST,10(R6)      DEST ID NUMBER                               
         B     VALDESTX                                                         
*                                                                               
VDEST5   LA    R6,12(R6)                                                        
         BCT   R5,VDEST4                                                        
         B     INVDEST                                                          
*                                                                               
INVDEST  MVC   FERN,=AL2(INVINPT)  INVALID INPUT                                
         LA    R7,BVRDESTH                                                      
         ST    R7,FADR                                                          
         B     EXIT                                                             
*                                                                               
VALDESTX DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE OUTPUT TYPE                                   *         
***********************************************************************         
         SPACE 1                                                                
VALOUT   DS    0H                                                               
         OC    OUTSTAT,OUTSTAT                                                  
         BNZ   VOUTX                                                            
         TM    RFPSTAT,RFPINUSE                                                 
         BO    VOUTX                                                            
*                                                                               
         CLC   ACQPROG,=C'TT'         1099'S                                    
         BE    VOUT10                                                           
         CLC   ACQPROG,=C'FR'         TOYOTA'S FEE REPORT                       
         BE    VOUT10                                                           
         CLC   ACQPROG,=C'I1'         BILLING INTERFACE                         
         BE    VOUT10                                                           
         CLC   ACQPROG,=C'C8'         CHARGE RATE REPORT                        
         BE    VOUT10                                                           
         CLC   ACQPROG,=C'CF'         COST OF FINANCE                           
         BE    VOUT10                                                           
         CLC   ACQPROG,=C'FI'         APG DOWNLOAD OPTION                       
         BE    VOUT10                                                           
         CLC   ACQPROG,=C'M2'                                                   
         BE    VOUT10                                                           
*                                                                               
VOUT10   CLC   BVROUT(4),=C'DOWN'                                               
         BNE   VOUT20                                                           
         MVI   ACQOPT7,C'Y'        FLAG REQUEST CARD FOR DOWNLOAD               
*                                                                               
VOUT20   CLC   BVROUT(5),=C'SOON,' IF OUTPUT TYPE NOT SOON                      
         BE    VOUT110                                                          
         CLI   BVROUTH+5,0                                                      
         BE    VOUTX               NO OUTPUT TYPE INPUT                         
         CLI   BVROUTH+5,6                                                      
         BH    VOUT170                                                          
         CLI   REQNUM,255                                                       
         BE    VOUT30                                                           
         CLI   REQACTN,C'D'                                                     
         BE    VOUT30                                                           
         TM    HDRINBS,HNOUT      TEST IF OUTTYP ALLOWED FOR THIS REQ           
         BO    VOUT170                                                          
*                                                                               
VOUT30   CLC   BVROUT(4),=C'DOWN'                                               
         BNE   VOUT35                                                           
         TM    HDRINBS,HNDWN       IS DOWN ALLOWED?                             
         BO    VOUT170             NO, ERROR                                    
*                                                                               
VOUT35   SR    R1,R1                                                            
         IC    R1,BVROUTH+5                                                     
         SH    R1,=H'1'                                                         
         MVC   IFLD,SPACES         SET OUTPUT TYPE IN IFLD                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   IFLD(0),BVROUT                                                   
*                                                                               
         CLC   IFLD(7),=C'DIRECT ' ALLOW DIRECT                                 
         BE    VOUT40                                                           
         CLC   ACQPROG,=C'21'      NO OUTPUT TYPE FOR BILLING                   
         BE    VOUT170                                                          
         CLC   ACQPROG,=C'23'      NO OUTPUT TYPE FOR BILLING                   
         BE    VOUT170                                                          
         CLC   ACQPROG,=C'27'      NO OUTPUT TYPE FOR BILLING                   
         BE    VOUT170                                                          
         CLC   ACQPROG,=C'29'      NO OUTPUT TYPE FOR BILLING                   
         BE    VOUT170                                                          
         CLC   IFLD(6),=C'DIRCHK'      IF DIRCHK                                
         BNE   VOUT40                                                           
         OC    REQDEST,REQDEST     DEST MUST BE BLANK                           
         BNZ   INVDEST                                                          
*                                                                               
VOUT40   DS    0H                                                               
         L     R4,AIO1             READ OUTPUT TYPE RECORD                      
         USING CTOREC,R4                                                        
         XC    CTOKEY,CTOKEY                                                    
         MVI   CTOKEY,C'O'                                                      
         MVC   CTOKID,IFLD                                                      
         GOTO1 DATAMGR,DMCB,(0,DMREAD),CTFILE,(R4),(R4)                         
         CLI   8(R1),0                                                          
         BNE   VOUT170                                                          
         CLC   IFLD(6),=C'DIRCHK'                                               
         BNE   VOUT50                                                           
         CLI   REQNUM,55           ONLY FOR CHECKS                              
         BNE   VOUT170                                                          
         B     VOUTX               ACCEPT BUT DON'T STORE IN REQ                
*                                                                               
VOUT50   DS    0H                                                               
         MVC   REQOUT,IFLD         SET OUTPUT TYPE IN REQ REC HDR               
*                                                                               
         CLI   REQNUM,21           AC21 CAN ONLY HAVE OUTPUT=DIR                
         BNE   VOUT60                                                           
         CLC   IFLD(3),=C'DIR'                                                  
         BE    VOUT80                                                           
         B     VOUT170                                                          
VOUT60   CLI   REQNUM,27           AC27 CAN ONLY HAVE OUTPUT=DIR                
         BNE   VOUT70                                                           
         CLC   IFLD(3),=C'DIR'                                                  
         BE    VOUT80                                                           
         B     VOUT170                                                          
VOUT70   CLI   REQNUM,55           AC55 CANT HAVE OUTPUT                        
         BNE   VOUT80                                                           
         CLC   IFLD(6),=C'DIRCHK'      ACCEPT ONLY DIRCHK                       
         BE    VOUT80                                                           
         B     VOUT170                                                          
*                                                                               
VOUT80   DS    0H                                                               
         LA    R6,CTOREC+28                                                     
VOUT90   CLI   0(R6),X'38'                                                      
         BE    VOUT100                                                          
         CLI   0(R6),0             END OF REC                                   
         BE    VOUT170                                                          
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     VOUT90                                                           
*                                                                               
VOUT100  DS    0H                                                               
         USING CTOUTD,R6                                                        
         TM    CTOUTSTA,X'80'      SEE IF REQUESTABLE OUTPUT TYPE               
         BZ    VOUT170             NO                                           
         B     VOUTX                                                            
*                                                                               
* SOONABLE ONLY REQUEST                                                         
*                                                                               
VOUT110  TM    HDRINBS,HSOON       IS THIS A SOONABLE REPORT?                   
         BZ    VOUT170             NO                                           
         TM    HDRINBS,HBILN       RESTRICTED TO NEW BILLING?                   
         BNO   *+12                NO,                                          
         TM    COMPSTAA,CPYSADBL   TEST COMPANY ON NEW BILLING                  
         BNO   VOUT170             NO, TOO BAD                                  
         TM    HDRINBS,HDDSS       YES, FOR DDS ONLY?                           
         BZ    VOUT120             NO                                           
         CLI   DDS,1               YES, IS THIS A DDS TERMINAL?                 
         BNE   VOUT170                                                          
*                                                                               
VOUT120  CLC   ACQPROG,=C'FI'      HARD CODE FOR FI REPORT                      
         BNE   VOUT130                                                          
         CLC   TWAAGY,=C'NW'       SOON ALLOWED ONLY FOR AYER                   
         BNE   VOUT170                                                          
         B     VOUT150                                                          
*                                                                               
VOUT130  CLC   ACQPROG,=C'27'      HARD CODE FOR 27/29 REPORT                   
         BE    VOUT140                                                          
         CLC   ACQPROG,=C'29'                                                   
         BNE   VOUT150                                                          
*                                                                               
VOUT140  CLI   DDS,1               IS THIS A DDS TERMINAL?                      
         BE    VOUT150             YES                                          
         CLC   TWAUSRID,=X'11CA'   NO, MUST BE YBMREQ                           
         BE    VOUT150                                                          
         CLC   TWAUSRID,=X'24B2'   OR CWNY                                      
         BE    VOUT150                                                          
         CLC   TWAUSRID,=X'24F9'   OR CWC1                                      
         BE    VOUT150                                                          
         CLC   TWAUSRID,=X'1CC0'   OR JWTO                                      
         BE    VOUT150                                                          
         CLC   TWAUSRID,=X'32E1'   OR JWTENT                                    
         BE    VOUT150                                                          
         CLC   TWAUSRID,=X'3AAD'   OR DNRT                                      
         BE    VOUT150                                                          
         CLC   TWAUSRID,=X'3AAE'   OR DNATT                                     
         BE    VOUT150                                                          
         CLC   TWAUSRID,=X'272D'   OR DNSFTH                                    
         BE    VOUT150                                                          
         CLC   TWAUSRID,=X'3713'   OR DNSL                                      
         BNE   VOUT170                                                          
*                                                                               
VOUT150  CLI   BVROUTH+5,5         INSURE ID INPUT                              
         BNH   VOUT170                                                          
         B     VOUTX                                                            
*                                                                               
VOUT160  CLI   BVROUTH+5,0         IF NO OUTPUT TYPE                            
         BNE   VOUT170                                                          
         MVC   FERN,=AL2(FLDMIS)   MISSING INPUT FIELD                          
         B     *+10                                                             
VOUT170  MVC   FERN,=AL2(INVINPT)  INVALID INPUT                                
         LA    R7,BVROUTH                                                       
         ST    R7,FADR                                                          
         B     EXIT                                                             
*                                                                               
VOUTX    DS    0H                                                               
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE OPTIONS LINE                                               *         
***********************************************************************         
         SPACE 1                                                                
VALOPT   DS    0H                                                               
         XC    FLTID,FLTID         ASSUME NO FILTERING                          
         CLI   BVROPTH+5,0         ANY OPTIONS?                                 
         BE    VALOPTX                                                          
*                                                                               
         LA    R7,TEMP                                                          
         GOTO1 SCANNER,PLIST,BVROPTH,(3,(R7)),C',=  '                           
         CLI   PLIST+4,0                                                        
         BE    VALOPTE                                                          
*                                                                               
         USING SCANBLKD,R7                                                      
VALOPT10 OC    SCLINE,SCLINE                                                    
         BZ    VALOPTX                                                          
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,SC1STLEN                                                    
         BZ    VALOPTX                                                          
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SC1STFLD(0),=C'UID'                                              
         BNE   VALOPTE                                                          
*                                                                               
         L     R4,AIO1             READ USER ID REC                             
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKID,SPACES       CLEAR FIELD TO SPACES                        
         SR    R1,R1                                                            
         ICM   R1,1,SC2NDLEN                                                    
         BZ    VALOPTE                                                          
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CTIKID(0),SC2NDFLD                                               
         GOTO1 DATAMGR,DMCB,(0,DMREAD),CTFILE,(R4),(R4)                         
         CLI   8(R1),0                                                          
         BNE   *+14                NOT AN ID SEE IF IT'S YES                    
         MVC   FLTID,SC2NDFLD      FILTER ON ID GIVEN                           
         B     VALOPTX                                                          
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,SC2NDLEN                                                    
         BZ    VALOPTX                                                          
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SC2NDFLD(0),=C'YES'                                              
         BE    VALOPTX                                                          
*                                                                               
VALOPTE  MVC   FERN,=AL2(INVOPT2)   INVALID OPTION                              
         LA    R7,BVROPTH                                                       
         ST    R7,FADR                                                          
         B     EXIT                                                             
*                                                                               
VALOPTX  DS    0H                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK IF REQUESTOR IS COMPATIBLE WITH REQUEST NUMBER                *         
***********************************************************************         
         SPACE 1                                                                
         USING HDRD,R7                                                          
VALREQ   CLI   REQACTN,C'D'                                                     
         BNE   VALREQ0                                                          
         MVC   REQNDX1(2),=X'FFFE' SET ENQ SCR REQUIRED                         
         LA    R1,BVROPTH                                                       
         CLI   5(R1),0                                                          
         BE    *+10                                                             
         MVC   REQNDX1(2),=X'FFEF' SET NOW SCR REQUIRED                         
*                                                                               
         CLI   REQOPTN,C'T'                                                     
         BNE   VALREQ7                                                          
         MVC   REQNDX1(2),=X'FFFC' SET MENU SCREEN FOR  TOTAL OPTION            
         MVI   MENUS,ALL           SET TO DISPLAY ALL REPORTS                   
         B     VALREQ7                                                          
VALREQ0  TM    FIND,FIALT          CARD REQUEST                                 
         BZ    VALREQ1                                                          
         MVC   REQNDX1(2),=X'FFFD'                                              
         B     VALREQ7                                                          
*                                                                               
VALREQ1  TM    HDRINBS,HREQR       IS REQUESTOR REQUIRED                        
         BZ    VALREQ2             NO                                           
         CLI   BVRNAMEH+5,0        WAS REQUESTOR INPUT                          
         BE    MISNAME             NO ERROR                                     
VALREQ2  LA    R2,HDRQLEN                                                       
*                                                                               
VALREQ6  L     R6,AREQTBL                                                       
         SR    R2,R6               SET SCR LIST REQUIRED = ..                   
         STH   R2,REQNDX1          SAVE INDEX TO REQTBL                         
         AR    R2,R6                                                            
*                                                                               
VALREQ7  L     R6,AREQTBL                                                       
         SR    R7,R6                                                            
         STH   R7,REQNDX           SAVE INDEX TO REQTBL                         
         AR    R7,R6                                                            
         MVC   REQFMT(1),FIND                                                   
         EJECT                                                                  
*                                                                               
*        CHECK IF THIS REQUEST IS COMPATIBLE WITH THE PREVIOUS REQUEST          
*                                                                               
VALDEFN  CLI   REQACTN,C'A'                                                     
         BNE   VALDEF1                                                          
         CLI   PREQACTN,C'N'       AMEND ONLY VALID AFTER NEW                   
         BNE   INVACTN                                                          
         CLC   REQNDX1(2),PREQNDX1 SAME SCREEN REQUIRED                         
         BNE   INVACTN             NO - CAN'T AMEND                             
         MVC   TEMP(1),REQFMT                                                   
         XC    TEMP(1),LREQFMT                                                  
         TM    TEMP,X'04'                                                       
         BO    INVACTN                                                          
         B     VALIPT              OK TO AMEND                                  
*                                                                               
VALDEF1  CLI   REQNDX1,X'FF'       ENQ OR CARD SCR REQUIRED                     
         BNE   VALIPT                                                           
         CLC   PREQNDX1(2),REQNDX1 IS IT ALREADY LOADED                         
         BNE   VALDEF1B            NO- LOAD                                     
         CLI   REQNDX1+1,X'FC'     YES-SKIP LOAD EXCEPT FOR MENU SCREEN         
         BNE   VALDEF2                                                          
VALDEF1B XC    DISPFLDS(2),DISPFLDS                                             
         MVC   PLIST+4(4),=X'D90604FF'                                          
         MVC   PLIST+7(1),REQNDX1+1                                             
         GOTO1 CALLOV,PLIST,BVRFRSTH                                            
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   PREQNDX1(2),REQNDX1 SAVE SCR LOADED                              
*                                                                               
         LA    R6,BVRFRSTH         RETRANSMIT HDR FIELDS                        
         SR    R7,R7                                                            
         LA    R2,64(R3)                                                        
         OI    6(R2),OI1T                                                       
         IC    R7,0(R2)                                                         
         AR    R2,R7                                                            
         CR    R2,R6                                                            
         BNH   *-12                                                             
*                                                                               
         CLI   REQNDX1+1,X'FD'     BUILD REQMAP FOR CARD REQ SCREEN             
         BNE   VALDEF2                                                          
         LA    R5,BVRFRSTH         FIND 1ST UNPROT DATA FIELD                   
         SR    R6,R6                                                            
         TM    1(R5),X'20'                                                      
         BZ    *+14                                                             
         IC    R6,0(R5)                                                         
         AR    R5,R6                                                            
         B     *-14                                                             
         SR    R5,R3                                                            
         STH   R5,DUB                                                           
         MVI   LREQMAP,126                                                      
         MVC   LREQMAP+1(2),DUB                                                 
         MVI   LREQMAP+3,LREQMAPX                                               
         MVI   STATUS,1            SET REQUEST DATA REQUIRED                    
         B     DEFAULT                                                          
*                                                                               
VALDEF2  CLI   REQNDX1+1,X'FE'                                                  
         BE    VALDEF2A                                                         
         CLI   REQNDX1+1,X'EF'                                                  
         BNE   VALDEF3                                                          
VALDEF2A MVI   STATUS,3            SET ENQ/CANC STATUS                          
         MVC   REQFLTR,FIND        SAVE ENQ/CANC FILTERS                        
         B     SAVEDATA                                                         
VALDEF3  CLI   REQNDX1+1,X'FD'     CARD REQ                                     
         BE    VALIPT1                                                          
         MVI   STATUS,4            SET MENU DISPLAY STATUS                      
         BAS   RE,BLDMENU                                                       
         CLI   REQOPTN,C'T'                                                     
         BNE   SAVEDATA                                                         
         OC    NEXTAD,NEXTAD       IF MORE TO COME                              
         BZ    *+14                                                             
         MVC   BVRNAME,TEMP        RESTORE SAVED NAME FIELD                     
         B     *+10                                                             
         XC    BVRNUM,BVRNUM       CLEAR NUMBER FIELD AT END                    
         B     VALDEF2A                                                         
         EJECT                                                                  
*        VALID TO INPUT BEYOND HEADR FOR STATUS=0 ONLY IF A NEW SCREEN          
*        IS NOT REQUIRED FOR NEW REQUEST DEFINITION                             
VALIPT   DS    0H                                                               
         CLC   PREQNDX1(2),REQNDX1 SCR FLD LIST CHANGED                         
         BNE   BUILDER             YES MUST BUILD SCR                           
         MVC   TEMP(1),REQFMT                                                   
         XC    TEMP(1),LREQFMT                                                  
         TM    TEMP,X'04'                                                       
         BO    BUILDER                                                          
*                                                                               
VALIPT1  LA    R5,BVRFRSTH         FIND 1ST UNPROT DATA FLD                     
         SR    R6,R6                                                            
         TM    1(R5),X'20'                                                      
         BZ    *+14                                                             
         IC    R6,0(R5)                                                         
         AR    R5,R6                                                            
         B     *-14                                                             
         CLI   LREQMAP,LREQMAPX    ZERO INPUT REQUEST                           
         BE    *+12                YES                                          
         C     R5,ALASTF           ANY INPUT IN DATA AREA                       
         BH    VALIPT3             NO                                           
VALIPT2  MVI   STATUS,2            SET REQUEST DATA INPUT                       
         ST    R5,AFIRSTF                                                       
         B     DEFAULT                                                          
*                                                                               
VALIPT3  CLI   REQNDX1,X'FF'       NO DATA INPUT IN DATA AREA                   
         BNE   VALIPT4                                                          
         MVI   STATUS,1            CARD REQUEST                                 
         B     VALIPT2                                                          
VALIPT4  MVI   STATUS,1            DATA REQUEST                                 
         B     VALIPT2                                                          
         EJECT                                                                  
***********************************************************************         
*              BUILD A NEW SCREEN                                     *         
***********************************************************************         
*                                                                               
*              R7 = A(REQTBL ENTRY)                                             
*              R3 = A(START OF TWA)                                             
*                                                                               
         USING RQTBD,R4                                                         
BUILDER  MVI   STATUS,1            SET REQUEST DATA REQUIRED                    
         LA    R2,LREQMAP          COVERS REQUEST MAP                           
         MVI   LREQMAP,LREQMAPX    DEFAULT IS TAB AS FIRST LINE                 
         XC    HALF,HALF           CONTAINS OFFSET FOR COLUMN ON SCREEN         
         LA    R4,HDRFLDS          R4=A(START OF REQTBL)                        
         LA    R6,BVRFRSTH         R6=A(NEXT TWA BYTE TO BUILD SCREEN)          
         LA    R5,4                STARTING ROW = ROW #5                        
*                                                                               
BUILD10  L     R7,AFLDNUMT         R7=A(FIELD TABLE)                            
         USING FLDD,R7                                                          
*                                                                               
BUILD15  CLC   RQFLD,FLDNUM        FIND MATCHING FIELD ENTRY                    
         BE    BUILD20                                                          
         CLI   RQFLD,0             END OF REQTBL?                               
         BE    BUILDX                                                           
         CLI   FLDNUM,0            TEST END OF FIELD TABLE                      
         BNE   *+6                                                              
         DC    H'0'                INVALID ENTRY IN REQTBL                      
         LA    R7,FLDLNQ(R7)                                                    
         B     BUILD15                                                          
*                                                                               
*              BUILD DESCRIPTION NAME FIELD                                     
*                                                                               
BUILD20  TM    RQSTAT1,FDDSO       DDS ONLY FIELD?                              
         BZ    *+12                                                             
         CLI   DDS,1               SKIP IF THIS NOT DDS                         
         BNE   BUILD75                                                          
*                                                                               
         TM    RQSTAT2,FCANO       DISPLAY ON CANADIAN SCREENS ONLY             
         BZ    *+12                                                             
         CLI   AGYCTRY,CTRYCAN                                                  
         BNE   BUILD75                                                          
*                                                                               
         TM    RQSTAT3,FUSAO       DISPLAY ON US SCREENS ONLY                   
         BZ    *+12                                                             
         CLI   AGYCTRY,CTRYUSA                                                  
         BNE   BUILD75                                                          
*                                                                               
         TM    RQSTAT2,FAPGS       DISPLAY IF APG SECURITY ACTIVATED            
         BZ    *+12                                                             
         TM    COMPSTA5,CPYAPGS    CHECK OLD SECURITY                           
         BZ    BUILD75                                                          
*                                                                               
         TM    RQSTAT2,FNBLO       DISPLAY IF NEW BILLING                       
         BZ    *+12                                                             
         TM    COMPSTAA,CPYSADBL                                                
         BZ    BUILD75                                                          
*                                                                               
         TM    RQSTAT2,FNSEC       CHECK IF NEW SECURITY                        
         BZ    BUILD21                                                          
         OI    CMPSTAT,CMPNSEC     COMPANY IS ON NEW SECURITY                   
*                                                                               
* Per PSHA - do not show the SALARY(Y/N) field until further notice             
* Field Control will control whether Salary is viewed or not and not            
* the SALARY(Y/N) field.                                                        
*                                                                               
         B     BUILD75                                                          
*                                                                               
         TM    CMPSTAT,CMPNSWT+CMPNSRD  EITHER READ/WRITE MUST BE ON            
         BZ    BUILD75                                                          
*                                                                               
BUILD21  TM    RQSTAT2,FNACO       DISPLAY ONLY ON NEWOFF & EMULATED            
         BZ    *+12                FILES (MUST BE BOTH TO BE DSPLYED)           
         TM    CMPSTAT,CMPNOFF+CMPEMUL                                          
         BNO   BUILD75                                                          
*                                                                               
         TM    RQSTAT1,FDPSL       DPS W/O LIMIT ACCESS                         
         BZ    BUILD22                                                          
         CLC   CMPY,DPS            SKIP IF THIS IS NOT DPS                      
         BNE   BUILD75                                                          
         CLI   TWAACCS,C'T'        OR IF COMMERCIAL UNIT PRESENT                
         BE    BUILD75                                                          
*                                                                               
         USING TWAELEMD,RF                                                      
BUILD22  XC    HALF,HALF           CONTAINS OFFSET FOR COLUMN ON SCREEN         
         LA    RF,TWAELEM                                                       
         XC    TWAELEM,TWAELEM     CLEAR OUT ELEMENT                            
         MVI   TWAELCD,1           CAN BE ANYTHING NON ZERO                     
         TM    RQSTAT1,FRHS        DISPLAY ON RIGHT HAND SIDE?                  
         BZ    BUILD23                                                          
         MVI   HALF,33                                                          
         B     *+8                 AND DONT BUMP LINE NUMBER                    
*                                                                               
BUILD23  LA    R5,1(R5)            BUMP LINE NUMBER                             
         MVC   TWAECOL,HALF        PUT IN COLUMN 33                             
         STC   R5,TWAERLN          STORE LINE NUMBER                            
         OI    TWAERLN,X'80'       FLAG FOR ABSOLUTE LINE NUMBER                
         MVC   TWAEFLN,FLDTXTL     SAVE LENGTH OF PROTECTED FIELD               
*                                                                               
         TM    RQSTAT2,FHIGH       SET HIGH INTENSITY WITHOUT                   
         BZ    *+8                 REQUIRING INPUT TO FIELD TO BE               
         OI    TWAEATB,X'08'       CHECKED BY BASE.                             
*                                                                               
         TM    RQSTAT1,FOPT        TEST FOR OPTIONAL INPUT FIELD                
         BNZ   BUILD25                                                          
         OI    TWAEATB,X'08'       SET HIGH INTENSITY                           
         NI    TWAEATB,X'FB'       AND FLD REQUIRED INDICATOR                   
*                                                                               
BUILD25  OI    TWAEATB,X'20'       PROTECTED                                    
         SR    RE,RE                                                            
         ICM   RE,3,FLDTXT                                                      
         A     RE,ABASE                                                         
         SR    R1,R1                                                            
         IC    R1,FLDTXTL          DEFAULT - DISPLAY AS INTENDED                
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TWAEDTA(0),0(RE)                                                 
         LA    R1,TWAELLNQ+1(R1)   RESTORE ACTUAL LENGTH                        
         STC   R1,TWAELLN          STORE LENGTH OF ELEMENT                      
*                                                                               
         TM    RQSTAT1,FDDSO       *DDS ONLY*  FLOAT IN @                       
         BZ    BUILD30                                                          
         SR    R1,R1                                                            
         IC    R1,TWAELLN          FIX LENGTH OF ELEMENT                        
         LA    R1,1(R1)                                                         
         STC   R1,TWAELLN                                                       
         IC    R1,TWAEFLN          FLOAT IN AN ATTENTION GRABBING CHAR          
         LA    RE,TWAEDTA                                                       
         AR    RE,R1                                                            
         MVI   0(RE),C'*'                                                       
         IC    R1,TWAEFLN          BUMP FIELD COUNT                             
         LA    R1,1(R1)                                                         
         STC   R1,TWAEFLN                                                       
*                                                                               
BUILD30  XC    DMCB(24),DMCB                                                    
         USING TWAPARMD,R1                                                      
         LA    R1,DMCB                                                          
         ST    R3,TWAPATWA         A(TWA)                                       
         ST    RF,TWAPAFST         A(FIRST BUILD ELEMENT)                       
         ST    R6,TWAPAOUT         A(OUTPUT AREA)                               
         GOTO1 ATWABLD                                                          
         CLI   TWAPERRS,TWAPEOK                                                 
         BE    *+6                                                              
         DC    H'0'                ERROR IN BUILDING FIELD                      
         L     R6,TWAPANXT         GET NEXT FIELD                               
*                                                                               
         MVC   0(1,R2),RQFLD       BUILD REQMAP OF INPUT FIELD                  
         LR    RF,R6                                                            
         SR    RF,R3               GET OFFSET INTO TWA                          
         STH   RF,DUB                                                           
         MVC   1(2,R2),DUB                                                      
         LA    R2,3(R2)            BUMP TO NEXT SPOT                            
*                                                                               
* BUILD INPUT FIELD                                                             
*                                                                               
         USING TWAELEMD,RF                                                      
         LA    RF,TWAELEM                                                       
         XC    TWAELEM,TWAELEM     CLEAR OUT ELEMENT                            
         MVI   TWAELCD,2           CAN BE ANYTHING NON ZERO                     
         STC   R5,TWAERLN          STORE LINE NUMBER                            
         OI    TWAERLN,X'80'       FLAG FOR ABSOLUTE LINE NUMBER                
         SR    R1,R1                                                            
         IC    R1,HALF                                                          
         LA    R1,17(R1)           BUMP TO CREATE INPUT FIELD                   
         STC   R1,TWAECOL                                                       
         MVC   TWAEFLN,FLDUNPR     LENGTH OF UNPROTECTED FIELD                  
         SR    R1,R1                                                            
         IC    R1,FLDUNPR          FIND LENGTH OF ENTIRE ELEM                   
         SH    R1,=H'1'                                                         
         MVC   TWAEDTA(0),SPACES                                                
         TM    RQSTAT2,FNSEC       CHECK IF NEW SECURITY                        
         BZ    BUILD35                                                          
*                                                                               
         MVI   TWAEDTA,C'Y'                                                     
         TM    CMPSTAT,CMPNSRD     DO THEY HAVE READ ONLY?                      
         BNO   BUILD35                                                          
         OI    TWAEATB,X'20'       PROTECTED                                    
*                                                                               
BUILD35  LA    R1,1(R1)            RESTORE ACTUAL LENGTH                        
         SR    R0,R0                                                            
         LA    R0,TWAELLNQ                                                      
         AR    R1,R0                                                            
         STC   R1,TWAELLN          STORE LENGTH OF ELEMENT                      
*                                                                               
         XC    DMCB(24),DMCB                                                    
         USING TWAPARMD,R1                                                      
         LA    R1,DMCB                                                          
         ST    R3,TWAPATWA         A(TWA)                                       
         ST    RF,TWAPAFST         A(FIRST BUILD ELEMENT)                       
         ST    R6,TWAPAOUT         A(OUTPUT AREA)                               
         GOTO1 ATWABLD                                                          
         CLI   TWAPERRS,TWAPEOK                                                 
         BE    *+6                                                              
         DC    H'0'                ERROR IN BUILDING FIELD                      
         L     R6,TWAPANXT         GET NEXT FIELD                               
*                                                                               
*              BUILD COMMENT FIELD                                              
*                                                                               
         OC    RQCOMM,RQCOMM       ANY COMMENT FIELD?                           
         BZ    BUILD75                                                          
         USING COMTBD,RE                                                        
         L     RE,ACTWATBL         FIND COMMENT IN COMMENT TABLE                
*                                                                               
BUILD40  CLC   RQCOMM(2),COMNUM                                                 
         BE    BUILD50                                                          
         CLC   COMNUM,=X'0000'                                                  
         BNE   *+6                                                              
         DC    H'0'                DIE IF COMMENT NOT IN TABLE                  
         SR    R1,R1                                                            
         IC    R1,COMLEN                                                        
         LA    R1,4(R1)            ADD ON LENGTH OF OTHER FIELDS                
         AR    RE,R1                                                            
         B     BUILD40                                                          
*                                                                               
         USING TWAELEMD,RF                                                      
BUILD50  LA    RF,TWAELEM                                                       
         XC    TWAELEM,TWAELEM     CLEAR OUT ELEMENT                            
         MVI   TWAELCD,3           CAN BE ANYTHING NON ZERO                     
         MVI   TWAECOL,33          COMMENTS ALWAYS GO IN COLUMN 31              
         STC   R5,TWAERLN          STORE LINE NUMBER                            
         OI    TWAERLN,X'80'       FLAG FOR ABSOLUTE LINE NUMBER                
         MVC   TWAEFLN,COMLEN                                                   
         SR    R1,R1                                                            
         IC    R1,COMLEN           LENGTH OF COMMENT                            
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TWAEDTA(0),COMDATA                                               
         LA    R1,1(R1)            RESTORE ACTUAL LENGTH                        
         SR    R0,R0                                                            
         LA    R0,TWAELLNQ                                                      
         AR    R1,R0                                                            
         STC   R1,TWAELLN          STORE LENGTH OF ELEMENT                      
         MVI   TWAEATB,X'20'       PROTECTED FIELD                              
*                                                                               
         XC    DMCB(24),DMCB                                                    
         USING TWAPARMD,R1                                                      
         LA    R1,DMCB                                                          
         ST    R3,TWAPATWA         A(TWA)                                       
         ST    RF,TWAPAFST         A(FIRST BUILD ELEMENT)                       
         ST    R6,TWAPAOUT         A(OUTPUT AREA)                               
         GOTO1 ATWABLD                                                          
         CLI   TWAPERRS,TWAPEOK                                                 
         BE    *+6                                                              
         DC    H'0'                ERROR IN BUILDING FIELD                      
         L     R6,TWAPANXT         GET NEXT FIELD                               
*                                                                               
BUILD75  LA    R4,RQTBLNQ(R4)      BUMP TO NEXT TABLE ENTRY                     
         B     BUILD10                                                          
         DROP  R7                                                               
*                                                                               
BUILDX   MVI   0(R2),LREQMAPX      LAST FIELD MUST BE A TAB                     
         LR    RF,R6                                                            
         SR    RF,R3               GET OFFSET INTO TWA                          
         STH   RF,DUB                                                           
         MVC   1(2,R2),DUB                                                      
         MVC   PREQNDX1(2),REQNDX1           SET FLD LIST SCR LOADED            
         CLI   LREQMAP,LREQMAPX                                                 
         BNE   DEFAULT                                                          
         MVI   STATUS,2                                                         
         MVC   HALF,LREQMAP+1                                                   
         LH    R7,HALF                                                          
         AR    R7,R3                                                            
         ST    R7,AFIRSTF          SIMULATE INPUT                               
         B     DEFAULT                                                          
         EJECT                                                                  
*                                                                               
*        SET DEFAULT VALUES IN REQUEST RECORD (IF ANY)                          
*                                                                               
DEFAULT  L     R7,AREQTBL          R7=A(REQTBL ENTRY)                           
         USING HDRD,R7                                                          
         AH    R7,REQNDX                                                        
         SR    RF,RF               RF=DEFAULT ROUTINE NUM                       
         IC    RF,HDRROUT                                                       
         LTR   RF,RF                                                            
         BZ    SAVEDATA            NO DEFAULT VALUES                            
         SLA   RF,2                                                             
         LA    RF,REQROUTS(RF)                                                  
         L     RF,0(RF)                                                         
         A     RF,RELO             RF=A(DEFAULT ROUTINE)                        
         BASR  RE,RF               SET DEFAULT VALUES                           
         DROP  R7                                                               
*                                                                               
*              SAVE INITIALIZED DATA IN TWA                                     
*                                                                               
SAVEDATA MVC   LREQNUM(16),REQNUM                                               
         MVC   LKEY,KEY                                                         
*                                                                               
         LA    R0,REQREC                                                        
         SR    R1,R1                                                            
         ICM   R1,3,=Y(L'REQREC)                                                
         LR    RF,R1                                                            
         LA    RE,LREQREC                                                       
         MVCL  RE,R0                                                            
*                                                                               
         TM    FIND,FIALT                                                       
         BZ    EXIT                                                             
         CLI   REQACTN,C'N'                                                     
         BNE   EXIT                                                             
         CLI   STATUS,2                                                         
         BE    EXIT                                                             
         MVC   DUB(2),LREQMAP+1    DISPLAY NEW CARD DEFAULTS                    
         LH    R5,DUB                                                           
         AR    R5,R3                                                            
         FOUT  (R5),ACQPROG+2,78                                                
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
*              BUILD A MENU SCREEN                                    *         
***********************************************************************         
*                                                                               
BLDMENU  NTR1                                                                   
         LA    RE,BVRFRSTH+2       RE=A(1ST LINE OF SCREEN + 2)                 
         XR    RF,RF                                                            
         ZAP   HALF,=P'0'                                                       
         LA    R5,1                                                             
         SR    R7,R7                                                            
         IC    R7,MENUS            R7=VALID MENU BITS                           
         L     R1,AREQTBL          R1=A(REPORT TABLE)                           
         USING HDRD,R1                                                          
         L     R4,NEXTAD           IS THIS A CONTINUATION                       
         LTR   R4,R4                                                            
         BZ    BLDMENU1                                                         
         AR    R1,R4               BUMP TO NEXT TABLE ENTRY                     
         XC    NEXTAD,NEXTAD                                                    
         B     BLDMEN2B                                                         
BLDMENU1 SR    RF,RF                                                            
         ICM   RF,3,HDRLEN         RF=LENGTH OF TABLE ENTRY                     
         AR    R1,RF               NEXT TABLE ENTRY                             
         OC    HDRLEN,HDRLEN       END OF TABLE                                 
         BZ    BLDMENU9                                                         
BLDMEN2B CLI   MENUS,ALL-2         SPECIAL OPTIONS                              
         BL    BLDMEN2C                                                         
         CLI   MENUS,ALL           DISPLAY ALL                                  
         BE    BLDMEN2D                                                         
         TM    HDRINBS,HSOON       DISPLAY 'SOONABLE' REPORTS                   
         BZ    BLDMENU1                                                         
         CLI   MENUS,ALL-1         DISPLAY ALL SOONABLE REPORTS                 
         BE    BLDMEN2D                                                         
         TM    HDRINBS,HDDSS       EXCLUDE DDS-ONLY REPORTS                     
         BO    BLDMENU1                                                         
         CLI   HDRNUM,162          HARD CODE FOR FI REPORT                      
         BNE   *+14                                                             
         CLC   TWAAGY,=C'NW'       SOON ALLOWED ONLY FOR AYER                   
         BNE   BLDMENU1                                                         
         B     BLDMEN2D                                                         
BLDMEN2C EX    R7,*+8              SPECIFIC MENU OPTION                         
         B     *+8                                                              
         TM    HDRSTA2,0          DO I WANT TO DISPLAY THIS ONE                 
         BZ    BLDMENU1            NO                                           
         CLI   CMPY,X'7B'          SPECIAL FOR DDS PAYMENT SERVICES             
         BNE   BLDMEN2D                                                         
         TM    HDRSTA,HDDSO         DDS TERMINALS ONLY                          
         BZ    BLDMEN2D                                                         
         CLI   DDS,1               IF THIS ISN'T                                
         BNE   BLDMENU1            SKIP IT                                      
BLDMEN2D CP    HALF,=P'54'         IS SCREEN FULL                               
         BNL   BLDMENU9                                                         
BLDMENU3 BCT   R5,BLDMENU4                                                      
         LA    RE,6(RE)            BEGINNING OF NEW LINE                        
         LA    R5,3                NUMBER OF REPORTS PER LINE                   
BLDMENU4 MVC   3(22,RE),HDRNAME    REPORT ALPHA-CODE AND NAME                   
         LR    R4,R1                                                            
         SR    RF,RF                                                            
         ICM   RF,3,HDRLEN                                                      
         AR    R4,RF                                                            
         SH    R4,=H'2'                                                         
         MVC   0(2,RE),0(R4)       REPORT NUMERIC CODE                          
         LA    RE,27(RE)           NEXT SPOT ON SCREEN                          
         AP    HALF,=P'1'                                                       
         B     BLDMENU1                                                         
*                                                                               
BLDMENU9 XC    BVRHDR,BVRHDR                                                    
         MVC   TEMP(L'BVRNAME),BVRNAME  SAVE FOR TOTAL REQUEST OPTION           
         XC    BVRNAME,BVRNAME                                                  
         LA    R4,BVRNAMEH         CURSOR TO REQUEST NAME                       
         OC    HDRLEN,HDRLEN                                                    
         BZ    BLDMENUX                                                         
         L     R4,AREQTBL                                                       
         SR    R1,R4                                                            
         ST    R1,NEXTAD           NO, STORE DISPLACEMENT TO NEXT               
         MVC   BVRHDR+23(20),=C'- hit enter for next'                           
         MVC   BVRNAME(8),=C'NEXTMENU'                                          
         OI    BVRSRVH+1,X'01'     CHANGE TO MODIFIED FIELD                     
         LA    R4,BVRNUMH          CURSOR TO REQUEST NUMBER                     
BLDMENUX ST    R4,FADR                                                          
         B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
*        ROUTINES TO FILL IN DEFAULT VALUES IN REQUEST CARD           *         
***********************************************************************         
*                                                                               
REQR01   NTR1                                                                   
         DC    H'0'                NOT USED, BUT IF ENTERED                     
         MVI   ACQOPTS,C'P'                                                     
         B     EXIT                                                             
*                                                                               
REQR02   NTR1                                                                   
         MVC   ACQUNT(2),LREQJOB                                                
         MVC   KEY+1(2),ACQUNT                                                  
         BAS   RE,LEDGER           MUST READ LEDGER                             
         B     EXIT                                                             
*                                                                               
REQR03   NTR1                                                                   
         MVC   ACQUNT(2),LREQRECV                                               
         MVC   KEY+1(2),ACQUNT                                                  
         BAS   RE,LEDGER           MUST READ LEDGER                             
         B     EXIT                                                             
*                                                                               
REQR04   NTR1                                                                   
         DC    H'0'                NOT USED, BUT IF ENTERED                     
         MVI   ACQUNT,C'1'                                                      
         MVC   KEY+1(2),ACQUNT                                                  
         B     EXIT                                                             
*                                                                               
REQR05   NTR1                                                                   
         MVI   ACQUNT,C'1'                                                      
         MVI   ACQLDG,C'P'                                                      
         MVC   KEY+1(2),ACQUNT                                                  
         BAS   RE,LEDGER           MUST READ LEDGER                             
         B     EXIT                                                             
*                                                                               
REQR06   NTR1                                                                   
         MVI   ACQUNT,C'1'                                                      
         MVI   ACQLDG,C'C'                                                      
         MVC   KEY+1(2),ACQUNT                                                  
         BAS   RE,LEDGER           MUST READ LEDGER                             
         MVI   ACQOPTS+2,C'Y'                                                   
         B     EXIT                                                             
*                                                                               
REQR07   NTR1                                                                   
         DC    H'0'                NOT USED, BUT IF ENTERED                     
         MVC   ACQUNT(2),=C'2R'                                                 
         MVC   KEY+1(2),ACQUNT                                                  
         BAS   RE,LEDGER           MUST READ LEDGER                             
         B     EXIT                                                             
*                                                                               
REQR08   NTR1                                                                   
         MVI   ACQUNT,C'S'                                                      
         MVC   KEY+1(1),ACQUNT                                                  
         B     EXIT                                                             
*                                                                               
REQR09   NTR1                                                                   
         MVC   ACQUNT(2),LREQCSTN                                               
         MVC   KEY+1(2),ACQUNT                                                  
         BAS   RE,LEDGER           MUST READ LEDGER                             
         B     EXIT                                                             
*                                                                               
REQR10   NTR1                                                                   
         DC    H'0'                NOT USED, BUT IF ENTERED                     
         MVC   ACQUNT(2),=C'2Y'                                                 
         MVC   KEY+1(2),ACQUNT                                                  
         BAS   RE,LEDGER           MUST READ LEDGER                             
         B     EXIT                                                             
*                                                                               
REQR11   NTR1                                                                   
         MVI   ACQUNT,C'S'                                                      
         MVC   KEY+1(1),ACQUNT                                                  
         MVI   ACQOPTS+1,C'N'                                                   
         B     EXIT                                                             
*                                                                               
REQR12   NTR1                                                                   
         MVI   ACQOPTS+2,C'Y'                                                   
         MVI   ACQUNT,C'3'                                                      
         MVC   KEY+1(1),ACQUNT                                                  
         B     EXIT                                                             
*                                                                               
REQR13   NTR1                                                                   
         MVI   ACQUNT,C'3'                                                      
         MVC   KEY+1(1),ACQUNT                                                  
         B     EXIT                                                             
*                                                                               
REQR14   NTR1                                                                   
         DC    H'0'                NOT USED, BUT IF ENTERED                     
         MVI   ACQUNT,C'2'                                                      
         MVC   KEY+1(1),ACQUNT                                                  
         B     EXIT                                                             
*                                                                               
REQR15   NTR1                                                                   
         MVI   ACQUNT,C'S'                                                      
         MVI   ACQLDG,C'I'                                                      
         MVC   KEY+1(2),ACQUNT                                                  
         BAS   RE,LEDGER           MUST READ LEDGER                             
         B     EXIT                                                             
*                                                                               
REQR16   NTR1                                                                   
         MVI   ACQUNT,C'1'                                                      
         MVI   ACQLDG,C'R'                                                      
         MVC   KEY+1(2),ACQUNT                                                  
         BAS   RE,LEDGER           MUST READ LEDGER                             
         B     EXIT                                                             
REQR17   NTR1                                                                   
         MVI   ACQUNT,C'G'                                                      
         MVC   KEY+1(1),ACQUNT                                                  
         B     EXIT                                                             
*                                                                               
REQR18   NTR1                                                                   
         MVC   ACQUNT(2),=C'SJ'                                                 
         MVC   KEY+1(2),ACQUNT                                                  
         BAS   RE,LEDGER           MUST READ LEDGER                             
         B     EXIT                                                             
*                                                                               
REQR19   NTR1                                                                   
REQR19A  MVC   ACQUNT(2),=C'SM'                                                 
         CLC   CMPY,DPS            DPS USER                                     
         BNE   REQR19B             NO                                           
         CLI   TWAACCS,C'T'        IS U/L LIMIT ACCESS PRESENT                  
         BE    *+14                                                             
         MVC   ACQUNT(2),=C'T '    NO - READ FOR COMMERCIAL UNIT                
         B     REQR19B                                                          
         MVC   ACQUNT(2),TWAACCS   TWA+6                                        
REQR19B  MVC   KEY+1(2),ACQUNT                                                  
         BAS   RE,LEDGER                                                        
         B     EXIT                                                             
*                                                                               
REQR20   NTR1                                                                   
         MVC   ACQUNT(2),=C'SN'                                                 
         MVC   KEY+1(2),ACQUNT                                                  
         BAS   RE,LEDGER                                                        
REQR20X  B     EXIT                                                             
*                                                                               
REQR21   NTR1                                                                   
         DC    H'0'                NOT USED, BUT IF ENTERED                     
         MVC   ACQUNT(2),=C'29'                                                 
         MVC   KEY+1(2),ACQUNT                                                  
         BAS   RE,LEDGER                                                        
         B     EXIT                                                             
*                                                                               
REQR22   NTR1                                                                   
         MVC   ACQUNT(2),=C'1J'                                                 
         MVC   KEY+1(2),ACQUNT                                                  
         BAS   RE,LEDGER                                                        
         B     EXIT                                                             
*                                                                               
REQR23   NTR1                                                                   
         MVC   ACQUNT(2),=C'1F'                                                 
         MVC   KEY+1(2),ACQUNT                                                  
         BAS   RE,LEDGER                                                        
         MVC   ACQOPTS(3),=C'YNY'  SET DEFAULTS                                 
         MVC   ACQSTART+4(2),=C'01' SET DAY TO 01                               
         B     EXIT                                                             
*                                                                               
REQR24   NTR1                                                                   
         MVC   ACQUNT(2),=C'SR'                                                 
         MVC   KEY+1(2),ACQUNT     MUST READ LEDGER                             
         BAS   RE,LEDGER                                                        
         B     EXIT                                                             
*                                                                               
REQR25   NTR1                                                                   
         CLC   ACQPROG,=C'CI'      PAID ITEMS REPORT                            
         BNE   *+14                                                             
         MVI   ACQOPT2,C'A'        ALWAYS FORCE 'A'-(MNAS 9/00)                 
         MVC   ACQUNT(2),=C'SE'    DEFAULT U/L IS SE                            
         CLC   USRID,=HL2'1016'    CCAT GETS U/L SX                             
         BNE   *+10                                                             
         MVC   ACQUNT(2),=C'SX'                                                 
         MVC   KEY+1(2),ACQUNT     MUST READ LEDGER                             
         BAS   RE,LEDGER                                                        
         B     EXIT                                                             
*                                                                               
REQR26   NTR1                                                                   
         CLC   ACQPROG,=C'CP'      DIV/SECT CLT PROFT                           
         BNE   *+8                                                              
         MVI   ACQOPT2,C'A'        ALWAYS FORCE 'A'-(MNAS 9/00)                 
         B     EXIT                                                             
*                                                                               
REQR27   NTR1                                                                   
         MVC   ACQUNT(2),=C'SA'                                                 
         MVC   KEY+1(2),ACQUNT     MUST READ LEDGER                             
         BAS   RE,LEDGER                                                        
         B     EXIT                                                             
*                                                                               
REQR28   NTR1                                                                   
         MVI   ACQOPTS,C'N'        OPTION 1 = 'N'                               
         B     EXIT                                                             
*                                                                               
REQR29   NTR1                      FOR THE AA                                   
         MVI   ACQUNT,C'S'                                                      
         MVI   ACQOPT6,C'Y'        RUN MOS REQUEST                              
         B     EXIT                                                             
*                                                                               
REQR30   NTR1                                                                   
         DC    H'0'                NOT USED, BUT IF ENTERED                     
         MVC   ACQUNT(2),=C'SX'                                                 
         MVC   KEY+1(2),ACQUNT                                                  
         BAS   RE,LEDGER           MUST READ LEDGER                             
         B     EXIT                                                             
*                                                                               
REQR31   NTR1                      ** SPARE **                                  
         B     EXIT                                                             
*                                                                               
REQR32   NTR1                                                                   
         MVC   ACQUNT(2),LREQSUPP                                               
         MVC   KEY+1(2),ACQUNT                                                  
         BAS   RE,LEDGER           MUST READ LEDGER                             
         B     EXIT                                                             
*                                                                               
REQR33   NTR1                                                                   
         MVI   ACQUNT,C'1'                                                      
         MVI   ACQLDG,C'C'                                                      
         MVC   KEY+1(2),ACQUNT                                                  
         BAS   RE,LEDGER           MUST READ LEDGER                             
         B     EXIT                                                             
*                                                                               
REQR34   NTR1                                                                   
         MVI   ACQUNT,C'1'                                                      
         MVI   ACQLDG,C'F'                                                      
         MVC   KEY+1(2),ACQUNT                                                  
         BAS   RE,LEDGER           MUST READ LEDGER                             
         B     EXIT                                                             
*                                                                               
REQR35   NTR1                                                                   
         MVC   ACQUNT(2),=C'SG'                                                 
         MVC   KEY+1(2),ACQUNT                                                  
         BAS   RE,LEDGER           MUST READ LEDGER                             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              READ LEDGER RECORD/SAVE INFO                           *         
***********************************************************************         
*                                                                               
LEDGER   NTR1                                                                   
         GOTO1 AIOREAD                                                          
         BE    LEDGER10                                                         
         MVC   FERN,=AL2(INVINPT)  INVALID INPUT FIELD                          
         MVI   STATUS,0            ALTER STATUS TO REVALIDATE REQ               
         OI    BVRRNAMH+6,X'80'                                                 
         LA    R1,BVRNUMH                                                       
         ST    R1,FADR                                                          
         B     EXIT                                                             
*                                                                               
LEDGER10 L     R6,AIO1                                                          
         MVI   ELCODE,X'30'        CHECK SECURITY                               
         BAS   RE,GETEL                                                         
         BNE   LEDGER20                                                         
         CLC   ACQPROG,=C'CM'      **NO SECURITY FOR ACCM                       
         BE    LEDGER20                                                         
         CLC   ACQPROG,=C'C2'         OR ACC2                                   
         BE    LEDGER20                                                         
*                                                                               
         USING RSTELD,R6                                                        
         CLC   TWAAUTH+1(1),RSTSECY+1                                           
         BNL   LEDGER20                                                         
         MVC   FERN,=AL2(SECLOCK)  SECURITY LOCKOUT                             
         MVI   STATUS,0            ALTER STATUS TO REVALIDATE REQ               
         OI    BVRRNAMH+6,X'80'                                                 
         LA    R1,BVRNUMH                                                       
         ST    R1,FADR                                                          
         B     EXIT                                                             
*                                                                               
LEDGER20 L     R6,AIO1                                                          
         MVI   ELCODE,X'14'                                                     
         BAS   RE,GETEL                                                         
         BNE   LEDGERX                                                          
*                                                                               
         USING LDGELD,R6                                                        
         MVC   LOFFPOS,LDGOPOS                                                  
         MVC   LOFFCHK,LDGSTAT                                                  
         DROP  R6                                                               
*                                                                               
LEDGERX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              CONSTANTS                                              *         
***********************************************************************         
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
DATADISP DC    H'49'                                                            
DMREAD   DC    CL8'DMREAD'                                                      
CTFILE   DC    CL8'CTFILE'                                                      
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              ROUTINE BRANCH TABLE                                   *         
***********************************************************************         
*                                                                               
REQROUTS DC    F'0'                                                             
         DC    A(REQR01)           SPARE                                        
         DC    A(REQR02)                                                        
         DC    A(REQR03)                                                        
         DC    A(REQR04)           SPARE                                        
         DC    A(REQR05)                                                        
         DC    A(REQR06)                                                        
         DC    A(REQR07)           SPARE                                        
         DC    A(REQR08)                                                        
         DC    A(REQR09)                                                        
         DC    A(REQR10)           SPARE                                        
         DC    A(REQR11)                                                        
         DC    A(REQR12)                                                        
         DC    A(REQR13)                                                        
         DC    A(REQR14)           SPARE                                        
         DC    A(REQR15)                                                        
         DC    A(REQR16)                                                        
         DC    A(REQR17)                                                        
         DC    A(REQR18)                                                        
         DC    A(REQR19)                                                        
         DC    A(REQR20)                                                        
         DC    A(REQR21)           SPARE                                        
         DC    A(REQR22)                                                        
         DC    A(REQR23)                                                        
         DC    A(REQR24)                                                        
         DC    A(REQR25)                                                        
         DC    A(REQR26)           SPARE                                        
         DC    A(REQR27)                                                        
         DC    A(REQR28)                                                        
         DC    A(REQR29)                                                        
         DC    A(REQR30)           SPARE                                        
         DC    A(REQR31)           SPARE                                        
         DC    A(REQR32)                                                        
         DC    A(REQR33)                                                        
         DC    A(REQR34)                                                        
         DC    A(REQR35)                                                        
*                                                                               
ADCONS   DC    A(CTWATBL)                                                       
NADCONS  EQU   (*-ADCONS)/4                                                     
         EJECT                                                                  
***********************************************************************         
*              MENU CONSTANTS AND TABLES                              *         
***********************************************************************         
*                                                                               
MENU     EQU   X'80'                                                            
TALENT   EQU   X'40'                                                            
PROD     EQU   X'20'                                                            
CONFID   EQU   X'10'                                                            
CORP     EQU   X'08'                                                            
CASHPAK  EQU   X'04'                                                            
*              X'02' NOT DEFINED                                                
SALES    EQU   X'01'                                                            
ALL      EQU   X'FF'                                                            
*                                                                               
MENUTAB  DS    0CL16                                                            
         DC    CL15'MENU',AL1(MENU+PROD+CORP+CASHPAK+SALES)                     
         DC    CL15'TALENT',AL1(TALENT)                                         
         DC    CL15'PRODUCTION',AL1(PROD)                                       
         DC    CL15'CONFIDENTIAL',AL1(CONFID)                                   
         DC    CL15'CORPORATE',AL1(CORP)                                        
         DC    CL15'CASHPAK',AL1(CASHPAK)                                       
         DC    CL15'SALES',AL1(SALES)                                           
         DC    CL15'SOON',AL1(ALL-2)                                            
         DC    CL15'DDSSOON',AL1(ALL-1)                                         
         DC    CL15'DDSMENU',AL1(ALL)                                           
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* COMMENT TABLE                                                       *         
*                                                                     *         
*         NEXT AVAILABLE ENTRY - 12D                                  *         
*                                                                     *         
***********************************************************************         
*                                                                     *         
*        XL2   ENTRY NUMBER                                           *         
*        AL1   PROTECTED FIELD LENGTH (=P)                            *         
*        AL1   UNPROTECTED FIELD LENGTH (0 FOR COMMENTS)              *         
*        CLP   PROTECTED FIELD DATA                                   *         
*                                                                     *         
*        DO NOT USE NUMBER X'0000'                                    *         
*                          X'7F00' (TAB LINE)                         *         
*                          X'7E00' (CARD REQ)                         *         
***********************************************************************         
         SPACE 1                                                                
CTWATBL  DS    0C                                                               
         DC    XL2'0100'                                                        
         DC    AL1(37,00)                                                       
         DC    CL37'Y=DEDUCT VACATION FROM STANDARD HOURS'                      
*                                                                               
         DC    XL2'0101'                                                        
         DC    AL1(38,00)                                                       
         DC    CL38'Y=CONTRA-ACCOUNT DATA ON DETAIL REPORT'                     
*                                                                               
         DC    XL2'0102'                                                        
         DC    AL1(28,00)                                                       
         DC    CL28'B=B TIME ONLY, R=R TIME ONLY'                               
*                                                                               
         DC    XL2'0103'                                                        
         DC    AL1(26,00)                                                       
         DC    CL26'Y=SUPPRESS LOCKED ACCOUNTS'                                 
*                                                                               
         DC    XL2'0104'                                                        
         DC    AL1(33,00)                                                       
         DC    CL33'TYPE OF TIME:  B OR R, BLANK=BOTH'                          
*                                                                               
         DC    XL2'0105'                                                        
         DC    AL1(24,00)                                                       
         DC    CL24'TYPE OF TIME (N,B,R,C,A)'                                   
*                                                                               
         DC    XL2'0106'                                                        
         DC    AL1(48,00)                                                       
         DC    CL48'C=CLOSED JOBS ONLY, O=OPEN JOBS ONLY (BLANK=ALL)'           
*                                                                               
         DC    XL2'0107'                                                        
         DC    AL1(15,00)                                                       
         DC    CL15'W=WEEKLY (WADA)'                                            
*                                                                               
         DC    XL2'0108'                                                        
         DC    AL1(30,00)                                                       
         DC    CL30'D=DETAIL, T=CONTRA TOTALS ONLY'                             
*                                                                               
         DC    XL2'0109'                                                        
         DC    AL1(41,00)                                                       
         DC    CL41'Y=SUPPRESS:GST/PST, P=PST ONLY,G=GST ONLY'                  
*                                                                               
         DC    XL2'010A'                                                        
         DC    AL1(48,00)                                                       
         DC    CL48'Y=INCLUDE, N=EXCLUDE, O=ONLY'                               
*                                                                               
         DC    XL2'010B'                                                        
         DC    AL1(18,00)                                                       
         DC    CL18'A=APPROVED, H=HELD'                                         
*                                                                               
         DC    XL2'010C'                                                        
         DC    AL1(33,00)                                                       
         DC    CL33'Y=SUPPRESS PERSON DETAIL PRINTING'                          
*                                                                               
         DC    XL2'010D'                                                        
         DC    AL1(44,00)                                                       
         DC    CL44'UNHOLD: A=AH, B=BILLING, M=INV/MARKER, U=ALL'               
*                                                                               
         DC    XL2'010E'                                                        
         DC    AL1(42,00)                                                       
         DC    CL42'Y=LIVE, N=DRAFT, R=REPORT ON TRANSMISSIONS'                 
*                                                                               
         DC    XL2'010F'                                                        
         DC    AL1(44,00)                                                       
         DC    CL44'S=SUPPRESS LOCKED ACCTS, L=LOCKED ACCTS ONLY'               
*                                                                               
         DC    XL2'0110'                                                        
         DC    AL1(19,00)                                                       
         DC    CL19'MMMDD/YY FOR UNHOLD'                                        
*                                                                               
         DC    XL2'0111'                                                        
         DC    AL1(17,00)                                                       
         DC    CL17'F=FREEZE OVERHEAD'                                          
*                                                                               
         DC    XL2'0112'                                                        
         DC    AL1(35,00)                                                       
         DC    CL35'METHOD OF ALLOCATION (BLANK=ALL)'                           
*                                                                               
         DC    XL2'0113'                                                        
         DC    AL1(35,00)                                                       
         DC    CL35'Y=CASH DISCOUNT ITEMS ONLY'                                 
*                                                                               
         DC    XL2'0114'                                                        
         DC    AL1(35,00)                                                       
         DC    CL35'Y=SHOW ACCOUNT CODE IN HEADING'                             
*                                                                               
         DC    XL2'0115'                                                        
         DC    AL1(48,00)                                                       
         DC    CL48'N=SUPPRESS, O=ONLY, H=HIRE/TERM, Z=ZERO WKS'                
*                                                                               
         DC    XL2'0116'                                                        
         DC    AL1(48,00)                                                       
         DC    CL48'A=APP, U=UNAPP, N=NO EST, R=ADD''L R''S'                    
*                                                                               
         DC    XL2'0117'                                                        
         DC    AL1(48,00)                                                       
         DC    CL48'REPORT FORMAT:  I=INVOICE DETAILS'                          
*                                                                               
         DC    XL2'0118'                                                        
         DC    AL1(48,00)                                                       
         DC    CL48'O, P             SORT ON OFFICE/PERSON CODE'                
*                                                                               
         DC    XL2'0119'                                                        
         DC    AL1(48,00)                                                       
         DC    CL48'A, T, L          STATUS'                                    
*                                                                               
         DC    XL2'011A'                                                        
         DC    AL1(48,00)                                                       
         DC    CL48'Y                SHOW ONLY EXECUTIVES'                      
*                                                                               
         DC    XL2'011B'                                                        
         DC    AL1(48,00)                                                       
         DC    CL48'Y                SHOW ONLY PROD REQUIRED'                   
*                                                                               
         DC    XL2'011C'                                                        
         DC    AL1(48,00)                                                       
         DC    CL48'Y                SHOW ONLY JOB REQUIRED'                    
*                                                                               
         DC    XL2'011D'                                                        
         DC    AL1(48,00)                                                       
         DC    CL48'Y                SHOW ONLY ACTUAL'                          
*                                                                               
         DC    XL2'011E'                                                        
         DC    AL1(48,00)                                                       
         DC    CL48'Y=SHOW HIGHEST REVISION'                                    
*                                                                               
         DC    XL2'011F'                                                        
         DC    AL1(48,00)                                                       
         DC    CL48'Y=SUPPRESS BALANCE FOR CLOSED JOBS'                         
*                                                                               
         DC    XL2'0120'                                                        
         DC    AL1(23,00)                                                       
         DC    CL23'Y=INCLUDE GROUP REPORTS'                                    
*                                                                               
         DC    XL2'0121'                                                        
         DC    AL1(39,00)                                                       
         DC    CL39'A=RUN BY ACTIVITY DATE (POSPAY P/O REQ)'                    
*                                                                               
         DC    XL2'0122'                                                        
         DC    AL1(14,00)                                                       
         DC    CL14'FOR FUTURE USE'                                             
*                                                                               
         DC    XL2'0123'                                                        
         DC    AL1(8,00)                                                        
         DC    CL8'R,B OR N'                                                    
*                                                                               
         DC    XL2'0124'                                                        
         DC    AL1(16,00)                                                       
         DC    CL16'SORT ORDER (A,B)'                                           
*                                                                               
         DC    XL2'0125'                                                        
         DC    AL1(35,00)                                                       
         DC    CL35'S=SUPPRESS ALL REPORTS EXCEPT RECAP'                        
*                                                                               
         DC    XL2'0126'                                                        
         DC    AL1(45,00)                                                       
         DC    CL45'RECORDS THAT APPLY TO THIS DATE, ALL=ALL RECS'              
*                                                                               
         DC    XL2'0127'                                                        
         DC    AL1(39,00)                                                       
         DC    CL39'I=INPUT RULES ONLY, O=OUTPUT RULES ONLY'                    
*                                                                               
         DC    XL2'0128'                                                        
         DC    AL1(26,00)                                                       
         DC    CL26'1=ONLY ACCOUNTS NEVER USED'                                 
*                                                                               
         DC    XL2'0129'                                                        
         DC    AL1(37,00)                                                       
         DC    CL37'B=BREAKOUT INDIRECT PAGES BY PAY TYPE'                      
*                                                                               
         DC    XL2'012A'                                                        
         DC    AL1(34,00)                                                       
         DC    CL34'Y=YTD PROCESSING FOR PERIODS/SPANS'                         
*                                                                               
         DC    XL2'012B'                                                        
         DC    AL1(34,00)                                                       
         DC    CL34'START DATE/TRANSMISSION START DATE'                         
*                                                                               
         DC    XL2'012C'                                                        
         DC    AL1(30,00)                                                       
         DC    CL30'END DATE/TRANSMISSION END DATE'                             
*                                                                               
         DC    XL2'012D'                                                        
         DC    AL1(21,00)                                                       
         DC    CL21'N=No (Default), Y=Yes'                                      
*                                                                               
         DC    XL2'012E'                                                        
         DC    AL1(30,00)                                                       
         DC    CL30'A=PERSON REPORT/C=GROUP REPORT'                             
*                                                                               
         DC    XL2'012F'                                                        
         DC    AL1(40,00)                                                       
         DC    CL40'A=SHOW ALL ACCTS/(DEFAULT)=ERRORS ONLY'                     
*                                                                               
         DC    XL2'0130'                                                        
         DC    AL1(40,00)                                                       
         DC    CL40'C=SHOW COUNTRY TABLE'                                       
*                                                                               
         DC    XL2'0131'                                                        
         DC    AL1(40,00)                                                       
         DC    CL40'MEDIA CODE OR SYSTEM (+N, +P OR +S)'                        
*                                                                               
         DC    XL2'0200'                                                        
         DC    AL1(24,00)                                                       
         DC    CL24'N=SUPPRESS RULE PRINTING'                                   
*                                                                               
         DC    XL2'0201'                                                        
         DC    AL1(20,00)                                                       
         DC    CL20'Y=INCLUDE, N=EXCLUDE'                                       
*                                                                               
         DC    XL2'0202'                                                        
         DC    AL1(40,00)                                                       
         DC    CL40'A=ACCOUNT, D=DETAIL, O=OFFICE'                              
*                                                                               
         DC    XL2'0203'                                                        
         DC    AL1(40,00)                                                       
         DC    CL40'S=SHOW DOLLARS, N=NO DOLLARS'                               
*                                                                               
         DC    XL2'0204'                                                        
         DC    AL1(45,00)                                                       
         DC    CL45'Y=SAVED/CLOSED,UNAPPROVED AS CLOSED, O=ONLY'                
*                                                                               
         DC    XL2'0205'                                                        
         DC    AL1(22,00)                                                       
         DC    CL22'METRIC TYPE (P,B OR U)'                                     
*                                                                               
         DC    XL2'0206'                                                        
         DC    AL1(45,00)                                                       
         DC    CL45'J=JOB LEVEL, BLANK=PRODUCT LEVEL'                           
*                                                                               
         DC    XL2'0207'                                                        
         DC    AL1(45,00)                                                       
         DC    CL45'A=AGE, I=INC, W=INC WITH ACCRUAL, BLANK=A+W'                
*                                                                               
         DC    XL2'0208'                                                        
         DC    AL1(17,00)                                                       
         DC    CL17'Y=SHOW CD AMOUNTS'                                          
*                                                                               
         DC    XL2'0209'                                                        
         DC    AL1(40,00)                                                       
         DC    CL40'B=BATES, C=CAMPBELL, D=SAATCHI'                             
*                                                                               
         DC    XL2'020A'                                                        
         DC    AL1(40,00)                                                       
         DC    CL40'N=NET, P=PRINT, S=SPOT'                                     
*                                                                               
         DC    XL2'020B'                                                        
         DC    AL1(45,00)                                                       
         DC    CL45'O=OLD ORDER, N=NEW ORDER'                                   
*                                                                               
         DC    XL2'020C'                                                        
         DC    AL1(45,00)                                                       
         DC    CL45'L=LINKED ONLY, M=MISSING ONLY'                              
*                                                                               
         DC    XL2'020D'                                                        
         DC    AL1(36,00)                                                       
         DC    CL36'Y=SHOW SALARY INFO, N=NO SALARY INFO'                       
*                                                                               
         DC    XL2'0300'                                                        
         DC    AL1(24,00)                                                       
         DC    CL24'N=SUPPRESS FEES PRINTING'                                   
*                                                                               
         DC    XL2'0301'                                                        
         DC    AL1(40,00)                                                       
         DC    CL40'Y=DETAIL JOURNAL'                                           
*                                                                               
         DC    XL2'0302'                                                        
         DC    AL1(40,00)                                                       
         DC    CL40'Y=LEDGER/BATCH SUMMARY'                                     
*                                                                               
         DC    XL2'0303'                                                        
         DC    AL1(40,00)                                                       
         DC    CL40'Y=OFFICE SUMMARY'                                           
*                                                                               
         DC    XL2'0304'                                                        
         DC    AL1(40,00)                                                       
         DC    CL40'Y=BATCH SUMMARY'                                            
*                                                                               
         DC    XL2'0305'                                                        
         DC    AL1(40,00)                                                       
         DC    CL40'Y=EXCEPTION LIST'                                           
*                                                                               
         DC    XL2'0306'                                                        
         DC    AL1(40,00)                                                       
         DC    CL40'Y=DUE LIVE SOON LIST'                                       
*                                                                               
         DC    XL2'0307'                                                        
         DC    AL1(40,00)                                                       
         DC    CL40'INCLUDE CLOSED JOBS (N/O)'                                  
*                                                                               
         DC    XL2'0308'                                                        
         DC    AL1(40,00)                                                       
         DC    CL40'RANGE OF TIMESHEET #''S IN FORMAT 01-99'                    
*                                                                               
         DC    XL2'0309'                                                        
         DC    AL1(40,00)                                                       
         DC    CL40'SUPPRESS INVOICE DETAIL: Y=YES, N=NO'                       
*                                                                               
         DC    XL2'030A'                                                        
         DC    AL1(40,00)                                                       
         DC    CL40'Y=INCLUDE EXPENSE, O=ONLY EXPENSE'                          
*                                                                               
         DC    XL2'030B'                                                        
         DC    AL1(45,00)                                                       
         DC    CL45'SORT ORDER - C=LOC/PERSON CODE, P=PERSON/LOC'               
*                                                                               
         DC    XL2'030C'                                                        
         DC    AL1(48,00)                                                       
         DC    CL48'3=YTD(DATES MAY BE SPECIFIED), 4=MOA, 5=MOA(OFF)'           
*                                                                               
         DC    XL2'030D'                                                        
         DC    AL1(12,00)                                                       
         DC    CL12'Y=DOWNLOAD'                                                 
*                                                                               
         DC    XL2'030E'                                                        
         DC    AL1(43,00)                                                       
         DC    CL43'Y=CLIENT DETAILS,A=CLIENT DETAILS AND CODES'                
*                                                                               
         DC    XL2'030F'                                                        
         DC    AL1(26,00)                                                       
         DC    CL26'Y=A/P BREAKOUT BY LEDGER'                                   
*                                                                               
         DC    XL2'0310'                                                        
         DC    AL1(29,00)                                                       
         DC    CL29'Y=RANKING (NET CASH POSITION)'                              
*                                                                               
         DC    XL2'0311'                                                        
         DC    AL1(48,00)                                                       
         DC    CL48'Y=A/R BREAKDOWN,1=SINGLE LEDGER SPECIFIED,A=BOTH'           
*                                                                               
         DC    XL2'0312'                                                        
         DC    AL1(40,00)                                                       
         DC    CL40'U/L IF SINGLE LEDGER RUN OPTION SELECTED'                   
*                                                                               
         DC    XL2'0313'                                                        
         DC    AL1(48,00)                                                       
         DC    CL48'C=NON-GROUPED ONLY,G=GROUPED ONLY,DEFAULT IS ALL'           
*                                                                               
         DC    XL2'0314'                                                        
         DC    AL1(47,00)                                                       
         DC    CL47'C=CLI RECVBLES ONLY,E=EXCLUDE NON SJ CLI,A=BOTH'            
*                                                                               
         DC    XL2'0315'                                                        
         DC    AL1(13,00)                                                       
         DC    CL13'Y, N(DEFAULT)'                                              
*                                                                               
         DC    XL2'0316'                                                        
         DC    AL1(14,00)                                                       
         DC    CL14'SHOW SR DETAIL'                                             
*                                                                               
         DC    XL2'0317'                                                        
         DC    AL1(27,00)                                                       
         DC    CL27'APPROVE UP TO CASH POSITION'                                
*                                                                               
         DC    XL2'0318'                                                        
         DC    AL1(29,00)                                                       
         DC    CL29'N=No (Default), Y=Yes, O=Only'                              
*                                                                               
         DC    XL2'0319'                                                        
         DC    AL1(43,00)                                                       
         DC    CL43'N=N, R=R+N, I=I+R+N, S=S+I+R+N, P=P+S+I+R+N'                
*                                                                               
         DC    XL2'0320'                                                        
         DC    AL1(39,00)                                                       
         DC    CL39'Tolerance Percentage Override (0-10.99)'                    
*                                                                               
         DC    XL2'0321'                                                        
         DC    AL1(27,00)                                                       
         DC    CL27'Vendor Level Override (Y,N)'                                
*                                                                               
         DC    XL2'0322'                                                        
         DC    AL1(3,00)                                                        
         DC    CL3'N/A'                                                         
*                                                                               
         DC    XL2'0323'                                                        
         DC    AL1(40,00)                                                       
         DC    CL40'Tolerance Dollar Amt Override (0-255.99)'                   
*                                                                               
         DC    XL2'0324'                                                        
         DC    AL1(45,00)                                                       
         DC    CL45'Please reach out to MO for info on this field'              
*                                                                               
         DC    XL2'0325'                                                        
         DC    AL1(13,00)                                                       
         DC    CL13'M=MISC, N=NEC'                                              
*                                                                               
         DC    XL2'0500'                                                        
         DC    AL1(26,00)                                                       
         DC    CL26'S=SUPPRESS LOCKED ACCOUNTS'                                 
*                                                                               
         DC    XL2'0600'                                                        
         DC    AL1(19,00)                                                       
         DC    CL19'Y=WORK CODE DETAILS'                                        
*                                                                               
         DC    XL2'0700'                                                        
         DC    AL1(29,00)                                                       
         DC    CL29'A=APPROVED ONLY,U=URGENT ONLY'                              
*                                                                               
         DC    XL2'0800'                                                        
         DC    AL1(15,00)                                                       
         DC    CL15'A=APPROVED ONLY'                                            
*                                                                               
         DC    XL2'0A00'                                                        
         DC    AL1(26,00)                                                       
         DC    CL26'G=SHOW WORK-CODES AS GROSS'                                 
*                                                                               
         DC    XL2'0B00'                                                        
         DC    AL1(06,00)                                                       
         DC    CL06'0 - 99'                                                     
*                                                                               
         DC    XL2'0C00'                                                        
         DC    AL1(17,00)                                                       
         DC    CL17'D=DAILY, W=WEEKLY'                                          
*                                                                               
         DC    XL2'0D00'                                                        
         DC    AL1(22,00)                                                       
         DC    CL22'Y=NEW PAGE PER PRODUCT'                                     
*                                                                               
         DC    XL2'0E00'                                                        
         DC    AL1(26,00)                                                       
         DC    CL26'S=SUPPRESS CONTRA-ACCOUNTS'                                 
*                                                                               
         DC    XL2'0F00'                                                        
         DC    AL1(23,00)                                                       
         DC    CL23'P=PRODUCTION, E=EXPENSE'                                    
*                                                                               
         DC    XL2'1000'                                                        
         DC    AL1(30,00)                                                       
         DC    CL30'S=SUPPRESS HIGH LEVEL ACCOUNTS'                             
*                                                                               
         DC    XL2'1100'                                                        
         DC    AL1(07,00)                                                       
         DC    CL07'D=DRAFT'                                                    
*                                                                               
         DC    XL2'1200'                                                        
         DC    AL1(12,00)                                                       
         DC    CL12'C=CLOSE JOBS'                                               
*                                                                               
         DC    XL2'1300'                                                        
         DC    AL1(19,00)                                                       
         DC    CL19'REPORT OPTION = 1-8'                                        
*                                                                               
         DC    XL2'1400'                                                        
         DC    AL1(07,00)                                                       
         DC    CL07'G=GROSS'                                                    
*                                                                               
         DC    XL2'1500'                                                        
         DC    AL1(44,00)                                                       
         DC    CL44'J=SUPPRESS JOB LVL, P=SUPPRESS PROD/JOB LVLS'               
*                                                                               
         DC    XL2'1600'                                                        
         DC    AL1(31,00)                                                       
         DC    CL31'S=SUPPRESS CURRENT MONTH COLUMN'                            
*                                                                               
         DC    XL2'1800'                                                        
         DC    AL1(21,00)                                                       
         DC    CL21'A=Account or P=Person'                                      
*                                                                               
         DC    XL2'1900'                                                        
         DC    AL1(48,00)                                                       
         DC    CL48'AGE BY B=BILL DATE,D=DUE DATE,M=MONTH OF SERVICE'           
*                                                                               
         DC    XL2'1A00'                                                        
         DC    AL1(46,00)                                                       
         DC    CL46'Y=SHOW REF. DETAIL,N=DON''T,A=LIST DETAILS SEP.'            
*                                                                               
         DC    XL2'1B00'                                                        
         DC    AL1(40,00)                                                       
         DC    CL40'Y=COMM,P=PROF,B=BOTH,U=USER,A=ALL,N=NONE'                   
*                                                                               
         DC    XL2'1C00'                                                        
         DC    AL1(11,00)                                                       
         DC    CL11'M=MATERIALS'                                                
*                                                                               
         DC    XL2'1D00'                                                        
         DC    AL1(15,00)                                                       
         DC    CL15'MMM/YY(-MMM/YY)'                                            
*                                                                               
         DC    XL2'1E00'                                                        
         DC    AL1(15,00)                                                       
         DC    CL15'Y=TAPE, N=DRAFT'                                            
*                                                                               
         DC    XL2'1F00'                                                        
         DC    AL1(48,00)                                                       
         DC    CL48'STATUS (I=IN PROG, S=SUB, P=P.APP, A=APP, R=REJ)'           
*                                                                               
         DC    XL2'2000'                                                        
         DC    AL1(11,00)                                                       
         DC    CL11'IN HUNDREDS'                                                
*                                                                               
         DC    XL2'2100'                                                        
         DC    AL1(27,00)                                                       
         DC    CL27'Y=SUPPRESS OPENING BALANCES'                                
*                                                                               
         DC    XL2'2200'                                                        
         DC    AL1(40,00)                                                       
         DC    CL40'BUY TYPE:  A=AGENCY, B=BOTTLER, M=MCCANN'                   
*                                                                               
         DC    XL2'2300'                                                        
         DC    AL1(6,00)                                                        
         DC    CL6'1 - 15'                                                      
*                                                                               
         DC    XL2'2400'                                                        
         DC    AL1(23,00)                                                       
         DC    CL23'U=UNAPPROVED ITEMS ONLY'                                    
*                                                                               
         DC    XL2'2500'                                                        
         DC    AL1(24,00)                                                       
         DC    CL24'Y=OVER-BUDGET ITEMS ONLY'                                   
*                                                                               
         DC    XL2'2600'                                                        
         DC    AL1(35,00)                                                       
         DC    CL35'FORMAT IS X.XX WHERE X IS ANY DIGIT'                        
*                                                                               
         DC    XL2'2A00'                                                        
         DC    AL1(16,00)                                                       
         DC    CL16'Y=NET ITEMS ONLY'                                           
*                                                                               
         DC    XL2'2B00'                                                        
         DC    AL1(17,00)                                                       
         DC    CL17'Y=SUPPRESS DETAIL'                                          
*                                                                               
         DC    XL2'2C00'                                                        
         DC    AL1(30,00)                                                       
         DC    CL30'Y, N(DEFAULT), R=REGISTER ONLY'                             
*                                                                               
         DC    XL2'2E01'                                                        
         DC    AL1(40,00)                                                       
         DC    CL40'BRAND GROUP (N,A)'                                          
*                                                                               
         DC    XL2'2F00'                                                        
         DC    AL1(16,00)                                                       
         DC    CL16'C, D, M, T'                                                 
*                                                                               
         DC    XL2'3000'                                                        
         DC    AL1(21,00)                                                       
         DC    CL21'L=LIVE  (BLANK=DRAFT)'                                      
*                                                                               
         DC    XL2'3100'                                                        
         DC    AL1(18,00)                                                       
         DC    CL18'Y=REPORT BY DETAIL'                                         
*                                                                               
         DC    XL2'3200'                                                        
         DC    AL1(48,00)                                                       
         DC    CL48'TYPE- A=AGENCY, B=BOTTLER, M=MCCANN, T=ALL+TOTAL'           
*                                                                               
         DC    XL2'3300'                                                        
         DC    AL1(21,00)                                                       
         DC    CL21'M=MEDIA, P=PRODUCTION'                                      
*                                                                               
         DC    XL2'3400'                                                        
         DC    AL1(17,00)                                                       
         DC    CL17'GROUP BILL FORMAT'                                          
*                                                                               
         DC    XL2'3500'                                                        
         DC    AL1(36,00)                                                       
         DC    CL36'LEVEL OF REPORTING (DEFAULT=COMPANY)'                       
*                                                                               
         DC    XL2'3600'                                                        
         DC    AL1(26,00)                                                       
         DC    CL26'Y=PRINT W/E DATES IN BOXES'                                 
*                                                                               
         DC    XL2'3700'                                                        
         DC    AL1(25,00)                                                       
         DC    CL25'Y=PRINT TRANSACTION TRACE'                                  
*                                                                               
         DC    XL2'3800'                                                        
         DC    AL1(36,00)                                                       
         DC    CL36'Y=PRINT OUTSTANDING AMOUNTS IN BOXES'                       
*                                                                               
         DC    XL2'3900'                                                        
         DC    AL1(28,00)                                                       
         DC    CL28'B=BILLS, E=ESTIMATES, A=BOTH'                               
*                                                                               
         DC    XL2'3A00'                                                        
         DC    AL1(36,00)                                                       
         DC    CL36'S=SUPPRESS TRANSLATION TABLE LISTING'                       
*                                                                               
         DC    XL2'3B00'                                                        
         DC    AL1(38,00)                                                       
         DC    CL38'Y=RUN PHASE 2 EVEN WITH PHASE 1 ERRORS'                     
*                                                                               
         DC    XL2'3C00'                                                        
         DC    AL1(26,00)                                                       
         DC    CL26'Y=DOWNLOAD PRESENT BALANCE'                                 
*                                                                               
         DC    XL2'3D00'                                                        
         DC    AL1(46,00)                                                       
         DC    CL46'1-9 = NO. OF YEARS TO ADD TO "TO BUDGET" DATES'             
*                                                                               
         DC    XL2'3E00'                                                        
         DC    AL1(41,00)                                                       
         DC    CL41'ACCOUNT LEVEL=1, 2, 3 OR BLANK FOR LOWEST'                  
*                                                                               
         DC    XL2'3F00'                                                        
         DC    AL1(43,00)                                                       
         DC    CL43'S=SUPPRESS DESCRIPTION, N=SUPPRESS JOB NAME'                
*                                                                               
         DC    XL2'4000'                                                        
         DC    AL1(32,00)                                                       
         DC    CL32'S=SUPPRESS BILLING SOURCE TOTALS'                           
*                                                                               
         DC    XL2'4100'                                                        
         DC    AL1(38,00)                                                       
         DC    CL38'A=ALPHA,F=FILTER,1-9=A/C CODE,T=TAX ID'                     
*                                                                               
         DC    XL2'4200'                                                        
         DC    AL1(37,00)                                                       
         DC    CL37'A=APP, U=UNAPP, N=NO EST, R=ADD''L R''S'                    
*                                                                               
         DC    XL2'4300'                                                        
         DC    AL1(26,00)                                                       
         DC    CL26'C=CANADIAN DOLLAR PAYMENTS'                                 
*                                                                               
         DC    XL2'43DD'                                                        
         DC    AL1(48,00)                                                       
         DC    CL48'C=CANADIAN DOLLAR PAYMENTS'                                 
*                                                                               
         DC    XL2'4400'                                                        
         DC    AL1(48,00)                                                       
         DC    CL48'I=ORDERS WITH INV AMTS,Z=ORDERS WITH NO INV AMTS'           
*                                                                               
         DC    XL2'4500'                                                        
         DC    AL1(17,00)                                                       
         DC    CL17'B=BILLING SUMMARY'                                          
*                                                                               
         DC    XL2'4600'                                                        
         DC    AL1(28,00)                                                       
         DC    CL28'P=PAID, U=UNPAID, BLANK=BOTH'                               
*                                                                               
         DC    XL2'4700'                                                        
         DC    AL1(24,00)                                                       
         DC    CL24'O=OPEN, C=CLOSED, B=BOTH'                                   
*                                                                               
         DC    XL2'4900'                                                        
         DC    AL1(21,00)                                                       
         DC    CL21'LAST ACTUAL 1-9,A,B,C'                                      
*                                                                               
         DC    XL2'4A00'                                                        
         DC    AL1(19,00)                                                       
         DC    CL19'ACCOUNT LEVEL 1-3,S'                                        
*                                                                               
         DC    XL2'4B00'                                                        
         DC    AL1(16,00)                                                       
         DC    CL16'CONTRA LEVEL 0-3'                                           
*                                                                               
         DC    XL2'4C00'                                                        
         DC    AL1(24,00)                                                       
         DC    CL24'SHOW INACTIVE ACCOUNTS Y'                                   
*                                                                               
         DC    XL2'4D00'                                                        
         DC    AL1(46,00)                                                       
         DC    CL46'1C LEVEL: 1-4, S=CLI SUMMARY, BLANK=PROD LEVEL'             
*                                                                               
         DC    XL2'4E00'                                                        
         DC    AL1(48,00)                                                       
         DC    CL48'1R LEVEL: 1-4, E=EMPL SUMMARY, BLANK=ALL LEVELS'            
*                                                                               
         DC    XL2'5000'                                                        
         DC    AL1(16,00)                                                       
         DC    CL16'Y=SORT BY AGENCY'                                           
*                                                                               
         DC    XL2'5100'                                                        
         DC    AL1(28,00)                                                       
         DC    CL28'BLANK=ALL,D=DIFFERENCES ONLY'                               
*                                                                               
         DC    XL2'5200'                                                        
         DC    AL1(42,00)                                                       
         DC    CL42'D=DEBIT BALANCE JOBS,C=CREDIT BALANCE JOBS'                 
*                                                                               
         DC    XL2'5600'                                                        
         DC    AL1(37,00)                                                       
         DC    CL37'BLANK=ALL 1R LEVELS,S=SUPPRESS LEVELS'                      
*                                                                               
         DC    XL2'5700'                                                        
         DC    AL1(15,00)                                                       
         DC    CL15'D,P,T,9,C,8,E,V'                                            
*                                                                               
         DC    XL2'5C01'                                                        
         DC    AL1(31,00)                                                       
         DC    CL31'S=SUPPRESS TERMINATED EMPLOYEES'                            
*                                                                               
         DC    XL2'5D00'                                                        
         DC    AL1(18,00)                                                       
         DC    CL18'Y=PAY HELD VENDORS'                                         
*                                                                               
         DC    XL2'5E00'                                                        
         DC    AL1(25,00)                                                       
         DC    CL25'Y=SORT TRANSACTIONS,T=TAX'                                  
*                                                                               
         DC    XL2'6000'                                                        
         DC    AL1(40,00)                                                       
         DC    CL40'Y=INCLUDE SALARY EDIT,N=SALARY EDIT ONLY'                   
*                                                                               
         DC    XL2'6100'                                                        
         DC    AL1(32,00)                                                       
         DC    CL32'AGE BY: Y=TRANSACTION DATE,N=MOA'                           
*                                                                               
         DC    XL2'6200'                                                        
         DC    AL1(23,00)                                                       
         DC    CL23'AGING METHODS A,F,O,U,N'                                    
*                                                                               
         DC    XL2'6300'                                                        
         DC    AL1(31,00)                                                       
         DC    CL31'H=HIRE,T=TERM,B=BOTH,BLANK=NONE'                            
*                                                                               
         DC    XL2'6400'                                                        
         DC    AL1(31,00)                                                       
         DC    CL31'S=SUPPRESS TERMINATED EMPLOYEES'                            
*                                                                               
         DC    XL2'6600'                                                        
         DC    AL1(44,00)                                                       
         DC    CL44'A=CLIENT ANALYSIS, B=JOB INQUIRY, BLANK=BOTH'               
*                                                                               
         DC    XL2'6700'                                                        
         DC    AL1(15,00)                                                       
         DC    CL15'(MMM/YY)-MMM/YY'                                            
*                                                                               
         DC    XL2'6800'                                                        
         DC    AL1(14,00)                                                       
         DC    CL14'Y=$600 MINIMUM'                                             
*                                                                               
         DC    XL2'6801'                                                        
         DC    AL1(26,00)                                                       
         DC    CL26'C=CORRECTION,R=REPLACEMENT'                                 
*                                                                               
         DC    XL2'6802'                                                        
         DC    AL1(16,00)                                                       
         DC    CL16'REPLACEMENT CODE'                                           
*                                                                               
         DC    XL2'6B00'                                                        
         DC    AL1(32,00)                                                       
         DC    CL32'1=RETAINERS, 2=WRITEOFFS, 3=BOTH'                           
*                                                                               
         DC    XL2'6D00'                                                        
         DC    AL1(46,00)                                                       
         DC    CL46'1=EXCLUDE WRITE-OFFS AND TIME HELD'                         
*                                                                               
         DC    XL2'7100'                                                        
         DC    AL1(37,00)                                                       
         DC    CL37'SJ LEVEL:  J=JOB, P=PRODUCT, C=CLIENT'                      
*                                                                               
         DC    XL2'7101'                                                        
         DC    AL1(35,00)                                                       
         DC    CL35'1R LEVEL:  E=EMPLOYEE, N=NO 1R DATA'                        
*                                                                               
         DC    XL2'7102'                                                        
         DC    AL1(26,00)                                                       
         DC    CL26'TASK:  T=TASK HIGH, N=NONE'                                 
*                                                                               
         DC    XL2'7103'                                                        
         DC    AL1(37,00)                                                       
         DC    CL37'L=LIVE(UPDATE BUCKETS), E=ERRORS ONLY'                      
*                                                                               
         DC    XL2'7104'                                                        
         DC    AL1(37,00)                                                       
         DC    CL37'TASK:  T=TASK, N=NO TASK'                                   
*                                                                               
         DC    XL2'7105'                                                        
         DC    AL1(48,00)                                                       
         DC    CL48'C=CLI,P=PRO,1=CLI/MED,2=PRO/MED,3=MED/JOB'                  
*                                                                               
         DC    XL2'7106'                                                        
         DC    AL1(45,00)                                                       
         DC    CL45'Y=PAGE BREAK BY PRODUCT'                                    
*                                                                               
         DC    XL2'7107'                                                        
         DC    AL1(45,00)                                                       
         DC    CL45'S=SUPPRESS CLOSED ACCTS, C=CLOSED ACCTS ONLY'               
*                                                                               
         DC    XL2'7200'                                                        
         DC    AL1(24,00)                                                       
         DC    CL24'Y=REPORT BY POSTING DATE'                                   
*                                                                               
         DC    XL2'7300'                                                        
         DC    AL1(37,00)                                                       
         DC    CL37'DEFAULT=LEVEL 2, A=LEVEL 3, B=LEVEL 4'                      
*                                                                               
         DC    XL2'7400'                                                        
         DC    AL1(20,00)                                                       
         DC    CL20'LEVEL OF DETAIL  1-4'                                       
*                                                                               
         DC    XL2'7500'                                                        
         DC    AL1(21,00)                                                       
         DC    CL21'Y=PRINT DETAIL REPORT'                                      
*                                                                               
         DC    XL2'7600'                                                        
         DC    AL1(23,00)                                                       
         DC    CL23'C=COMBINE HIGHER LEVELS'                                    
*                                                                               
         DC    XL2'7700'                                                        
         DC    AL1(24,00)                                                       
         DC    CL24'S=SUPPRESS AGING COLUMNS'                                   
*                                                                               
         DC    XL2'7800'                                                        
         DC    AL1(42,00)                                                       
         DC    CL42'T=USE TRANSACTION MONTH, B=USE BATCH MONTH'                 
*                                                                               
         DC    XL2'7900'                                                        
         DC    AL1(15,00)                                                       
         DC    CL15'L=LIVE, D=DRAFT'                                            
*                                                                               
         DC    XL2'7A00'                                                        
         DC    AL1(31,00)                                                       
         DC    CL31'LIST CURRENT RECORDS IN USE Y/N'                            
*                                                                               
         DC    XL2'7B00'                                                        
         DC    AL1(17,00)                                                       
         DC    CL17'D=AGE BY DUE DATE'                                          
*                                                                               
         DC    XL2'7C00'                                                        
         DC    AL1(17,00)                                                       
         DC    CL17'REPORT FORMAT A-M'                                          
*                                                                               
         DC    XL2'7D00'                                                        
         DC    AL1(38,00)                                                       
         DC    CL38'M=MONTH OF SERVICE, T=TRANSACTION DATE'                     
*                                                                               
         DC    XL2'8000'                                                        
         DC    AL1(36,00),36C' '                                                
*                                                                               
         DC    XL2'8100'                                                        
         DC    AL1(28,00)                                                       
         DC    CL28'S=SUPPRESS INACTIVE ACCOUNTS'                               
*                                                                               
         DC    XL2'81DD'                                                        
         DC    AL1(48,00)                                                       
         DC    CL48'Y=REPORT FOR ONE COMPANY'                                   
*                                                                               
         DC    XL2'8200'                                                        
         DC    AL1(48,00)                                                       
         DC    CL48'BLANK=JOB,C=CLIENT,P=PRODUCT,E=EMPLOYEE'                    
*                                                                               
         DC    XL2'82DD'                                                        
         DC    AL1(48,00)                                                       
         DC    CL48'Y=SUMMARY ONLY'                                             
*                                                                               
         DC    XL2'8400'                                                        
         DC    AL1(30,00)                                                       
         DC    CL30'D=DIRECT,I=INDIRECT,BLANK=BOTH'                             
*                                                                               
         DC    XL2'84DD'                                                        
         DC    AL1(48,00)                                                       
         DC    CL48'VOID CHECK TAPE: (V,N)'                                     
*                                                                               
         DC    XL2'8500'                                                        
         DC    AL1(48,00)                                                       
         DC    CL48'Y=ALL JOBS,1-9,A-P=EXCPTN REASON,BLK=ALL EXCPTNS'           
*                                                                               
         DC    XL2'8600'                                                        
         DC    AL1(15,00)                                                       
         DC    CL15'Y=SUPPRESS CASH'                                            
*                                                                               
         DC    XL2'8700'                                                        
         DC    AL1(24,00)                                                       
         DC    CL24'S=SUPPRESS INACTIVE JOBS'                                   
*                                                                               
         DC    XL2'8800'                                                        
         DC    AL1(46,00)                                                       
         DC    CL46'S=SUMMARY, D=DETAIL SUMMARY, 1-3=SUMMARY LEVEL'             
*                                                                               
         DC    XL2'8900'                                                        
         DC    AL1(31,00)                                                       
         DC    CL31'B=BALANCES, C=CREDITS, D=DEBITS'                            
*                                                                               
         DC    XL2'8A00'                                                        
         DC    AL1(47,00)                                                       
         DC    CL47'C=INCOME REPORT,M=MONTH HEADS,I=INCREMENT HEADS'            
*                                                                               
         DC    XL2'8B00'                                                        
         DC    AL1(17,00)                                                       
         DC    CL17'Y=PRINT ADDRESSES'                                          
*                                                                               
         DC    XL2'8C00'                                                        
         DC    AL1(06,00)                                                       
         DC    CL06'MMM/YY'                                                     
*                                                                               
         DC    XL2'8D00'                                                        
         DC    AL1(31,00)                                                       
         DC    CL31'A=ALPHA ,F=FILTER ,1-9=A/C CODE'                            
*                                                                               
         DC    XL2'8E00'                                                        
         DC    AL1(25,00)                                                       
         DC    CL25'N=SUPPRESS REQUESTOR NAME'                                  
*                                                                               
         DC    XL2'8F00'                                                        
         DC    AL1(18,00)                                                       
         DC    CL18'Y=INCLUDE ALL JOBS'                                         
*                                                                               
         DC    XL2'9000'                                                        
         DC    AL1(39,00)                                                       
         DC    CL39'Y=ADD CD TO ALL COLS.,C=ALL BUT CURRENT'                    
*                                                                               
         DC    XL2'9200'                                                        
         DC    AL1(15,00)                                                       
         DC    CL15'P=MAKE POSTINGS'                                            
*                                                                               
         DC    XL2'9300'                                                        
         DC    AL1(34,00)                                                       
         DC    CL34'1=LEVEL#1, 2=LEVELS#1&&2, J=LEVEL#3'                        
*                                                                               
         DC    XL2'9400'                                                        
         DC    AL1(22,00)                                                       
         DC    CL22'D=DELINQUENT LIST ONLY'                                     
*                                                                               
         DC    XL2'9500'                                                        
         DC    AL1(14,00)                                                       
         DC    CL14'P=PROFILE INFO'                                             
*                                                                               
         DC    XL2'9600'                                                        
         DC    AL1(23,00)                                                       
         DC    CL23'NUMBER OF PERIODS (1-5)'                                    
*                                                                               
         DC    XL2'9700'                                                        
         DC    AL1(28,00)                                                       
         DC    CL28'P=PRINT ALL ACCOUNTS,X=DON''T'                              
*                                                                               
         DC    XL2'9801'                                                        
         DC    AL1(40,00)                                                       
         DC    CL40'L=LOCK TIMESHEET/SALARY MONTH'                              
*                                                                               
         DC    XL2'9A00'                                                        
         DC    AL1(27,00)                                                       
         DC    CL27'Y=SUPPRESS NET,PCT,AND COMM'                                
*                                                                               
         DC    XL2'9B00'                                                        
         DC    AL1(14,00)                                                       
         DC    CL14'S=SUMMARY ONLY'                                             
*                                                                               
         DC    XL2'9C00'                                                        
         DC    AL1(37,00)                                                       
         DC    CL37'1,2 OR 3=LEVEL OF SUMMARY , S=REQUEST'                      
*                                                                               
         DC    XL2'9D00'                                                        
         DC    AL1(11,00)                                                       
         DC    CL11'REPORT TYPE'                                                
*                                                                               
         DC    XL2'9E00'                                                        
         DC    AL1(18,00)                                                       
         DC    CL18'Y=C/A SUMMARY ONLY'                                         
*                                                                               
         DC    XL2'9F00'                                                        
         DC    AL1(19,00)                                                       
         DC    CL19'COLUMN MENU=1-9,A-J'                                        
*                                                                               
         DC    XL2'A000'                                                        
         DC    AL1(16,00)                                                       
         DC    CL16'Y=SUMMARY FORMAT'                                           
*                                                                               
         DC    XL2'A100'                                                        
         DC    AL1(22,00)                                                       
         DC    CL22'N=DON''T WRITE TO FILES'                                    
*                                                                               
         DC    XL2'A300'                                                        
         DC    AL1(25,00)                                                       
         DC    CL25'S=SUPPRESS ESTIMATE VALUE'                                  
*                                                                               
         DC    XL2'A400'                                                        
         DC    AL1(19,00)                                                       
         DC    CL19'Y=SORT TRANSACTIONS'                                        
*                                                                               
         DC    XL2'A401'                                                        
         DC    AL1(14,00)                                                       
         DC    CL14'C=COST LISTING'                                             
*                                                                               
         DC    XL2'A500'                                                        
         DC    AL1(29,00)                                                       
         DC    CL29'Y=PRINT VENDOR INVOICE NUMBER'                              
*                                                                               
         DC    XL2'A600'                                                        
         DC    AL1(28,00)                                                       
         DC    CL28'S=SUPPRESS HIGH LEVEL TOTALS'                               
*                                                                               
         DC    XL2'A700'                                                        
         DC    AL1(22,00)                                                       
         DC    CL22'T=TRIAL BALANCE FORMAT'                                     
*                                                                               
         DC    XL2'A800'                                                        
         DC    AL1(16,00)                                                       
         DC    CL16'Y=SUPPRESS CENTS'                                           
*                                                                               
         DC    XL2'A900'                                                        
         DC    AL1(44,00)                                                       
         DC    CL44'B=BILLED;S=SUPPRESS BILLED;A=ALLOC;U=UNALLOC'               
*                                                                               
         DC    XL2'AA00'                                                        
         DC    AL1(15,00)                                                       
         DC    CL15'D=DETAILED LIST'                                            
*                                                                               
         DC    XL2'AB00'                                                        
         DC    AL1(44,00)                                                       
         DC    CL44'Z=ZERO BALANCE ONLY, S=SUPPRESS ZERO BALANCE'               
*                                                                               
         DC    XL2'AC00'                                                        
         DC    AL1(26,00)                                                       
         DC    CL26'N=ESTIMATE NUMBER SEQUENCE'                                 
*                                                                               
         DC    XL2'AD00'                                                        
         DC    AL1(31,00)                                                       
         DC    CL31'U=URGENT,Y=URGENT+APPLY CREDITS'                            
*                                                                               
         DC    XL2'AE00'                                                        
         DC    AL1(32,00)                                                       
         DC    CL32'STATUS (S=SUB+APP, A=APP, R=REJ)'                           
*                                                                               
         DC    XL2'AF00'                                                        
         DC    AL1(23,00)                                                       
         DC    CL23'U=UNDISBURSED CASH ONLY'                                    
*                                                                               
         DC    XL2'B100'                                                        
         DC    AL1(19,00)                                                       
         DC    CL19'Y=RUN BEFORE CHECKS'                                        
*                                                                               
         DC    XL2'B300'                                                        
         DC    AL1(26,00)                                                       
         DC    CL26'T=TIME ONLY,Y=EXCLUDE TIME'                                 
*                                                                               
         DC    XL2'B500'                                                        
         DC    AL1(43,00)                                                       
         DC    CL43'S=SUPPRESS CLOSED ACCTS,C=CLOSED ACCTS ONLY'                
*                                                                               
         DC    XL2'B601'                                                        
         DC    AL1(20,00)                                                       
         DC    CL20'C=CLOSED JOBS ONLY'                                         
*                                                                               
         DC    XL2'B700'                                                        
         DC    AL1(43,00)                                                       
         DC    CL43'S=SUPPRESS LOCKED ACCTS,L=LOCKED ACCTS ONLY'                
*                                                                               
         DC    XL2'B800'                                                        
         DC    AL1(41,00)                                                       
         DC    CL41'Y=ALL PAYMTS,T=TODAY''S PAYMTS,N=NO PAYMTS'                 
*                                                                               
         DC    XL2'B900'                                                        
         DC    AL1(33,00)                                                       
         DC    CL33'Y=PRINT IN CODE AND SORTING ORDER'                          
*                                                                               
         DC    XL2'BB00'                                                        
         DC    AL1(11,00)                                                       
         DC    CL11'N=MARK FILE'                                                
*                                                                               
         DC    XL2'BD00'                                                        
         DC    AL1(30,00)                                                       
         DC    CL30'Y=C-A/C SUMMARY,N=SUMMARY ONLY'                             
*                                                                               
         DC    XL2'BE00'                                                        
         DC    AL1(24,00)                                                       
         DC    CL24'Y=CLIENT COMPARISON ONLY'                                   
*                                                                               
         DC    XL2'BF00'                                                        
         DC    AL1(11,00)                                                       
         DC    CL11'OFFICE CODE'                                                
*                                                                               
         DC    XL2'C000'                                                        
         DC    AL1(45,00)                                                       
         DC    CL45'A,B,C,NN,BLANK=0-10,11-20,21-30,31-40,41+OVER'              
*                                                                               
         DC    XL2'C100'                                                        
         DC    AL1(23,00)                                                       
         DC    CL23'SUBTOTAL ON THIS COLUMN'                                    
*                                                                               
         DC    XL2'C200'                                                        
         DC    AL1(30,00)                                                       
         DC    CL30'TARGET PERCENTAGE OVERRIDE 1-9'                             
*                                                                               
         DC    XL2'C300'                                                        
         DC    AL1(15,00)                                                       
         DC    CL15'S=SUPPRESS CASH'                                            
*                                                                               
         DC    XL2'C400'                                                        
         DC    AL1(26,00)                                                       
         DC    CL26'OFFICE FILTER LOCATION 1-4'                                 
*                                                                               
         DC    XL2'C500'                                                        
         DC    AL1(31,00)                                                       
         DC    CL31'LEVELS OF DETAILS TO PRINT  1-3'                            
*                                                                               
         DC    XL2'C700'                                                        
         DC    AL1(35,00)                                                       
         DC    CL35'Y=DEMAND TOTAL BILLING,R=RETAIL JOB'                        
*                                                                               
         DC    XL2'C800'                                                        
         DC    AL1(37,00)                                                       
         DC    CL37'A=ALL,C=CLEARED ONLY,U=UNCLEARED ONLY'                      
*                                                                               
         DC    XL2'C900'                                                        
         DC    AL1(19,00)                                                       
         DC    CL19'Y=URGENT CLEARANCES'                                        
*                                                                               
         DC    XL2'CC00'                                                        
         DC    AL1(48,00)                                                       
         DC    CL48'SORT:  S=SUPPLIER,D=DATE,U=DUE DATE,A=AUTHORIZER'           
*                                                                               
         DC    XL2'CE00'                                                        
         DC    AL1(45,00)                                                       
         DC    CL45'Y=ALL,T=MISSING TS,M=MOS CHECK,P=PROJECT TIME'              
*                                                                               
         DC    XL2'D000'                                                        
         DC    AL1(14,00)                                                       
         DC    CL14'1,2,3(J),4,A,B'                                             
*                                                                               
         DC    XL2'D200'                                                        
         DC    AL1(36,00)                                                       
         DC    CL36'Y=OUTPUT TAPE OR POSPAY TRANSMISSION'                       
*                                                                               
         DC    XL2'D300'                                                        
         DC    AL1(31,00)                                                       
         DC    CL31'D OR C  (DEBIT/CREDIT BALANCES)'                            
*                                                                               
         DC    XL2'D400'                                                        
         DC    AL1(32,00)                                                       
         DC    CL32'B=BILLED ITEMS, U=UNBILLED ITEMS'                           
*                                                                               
         DC    XL2'D600'                                                        
         DC    AL1(14,00)                                                       
         DC    CL14'PERIOD M,Q,H,Y'                                             
*                                                                               
         DC    XL2'D601'                                                        
         DC    AL1(40,00)                                                       
         DC    CL40'N=SORT BY PERSON CODE WITHIN DEPT'                          
*                                                                               
         DC    XL2'D602'                                                        
         DC    AL1(28,00)                                                       
         DC    CL28'M=MANUAL, T=TAPE, BLANK=BOTH'                               
*                                                                               
         DC    XL2'D603'                                                        
         DC    AL1(31,00)                                                       
         DC    CL31'COLUMN MENU (1, 2, 3, 4, BLANK)'                            
*                                                                               
         DC    XL2'D604'                                                        
         DC    AL1(48,00)                                                       
         DC    CL48'C=CODE D=DETAIL O=OFFICE P=PERIOD'                          
*                                                                               
         DC    XL2'D700'                                                        
         DC    AL1(16,00)                                                       
         DC    CL16'Y=SUMMARIES ONLY'                                           
*                                                                               
         DC    XL2'D800'                                                        
         DC    AL1(42,00)                                                       
         DC    CL42'Y=USE ORIGINAL NO. (DEFAULT = USE NEW NO.)'                 
*                                                                               
         DC    XL2'DA00'                                                        
         DC    AL1(07,00)                                                       
         DC    CL07'A=ALPHA'                                                    
*                                                                               
         DC    XL2'DB00'                                                        
         DC    AL1(14,00)                                                       
         DC    CL14'Y=IGNORE VOIDS'                                             
*                                                                               
         DC    XL2'DC00'                                                        
         DC    AL1(23,00)                                                       
         DC    CL23'Y=NEGATIVE AMOUNTS ONLY'                                    
*                                                                               
         DC    XL2'DD00'                                                        
         DC    AL1(22,00)                                                       
         DC    CL22'Y=SUPPRESS VENDOR NAME'                                     
*                                                                               
         DC    XL2'DE00'                                                        
         DC    AL1(13,00)                                                       
         DC    CL13'D=DEBITS ONLY'                                              
*                                                                               
         DC    XL2'DF00'                                                        
         DC    AL1(19,00)                                                       
         DC    CL19'1-3,C OR A(DEFAULT)'                                        
*                                                                               
         DC    XL2'E000'                                                        
         DC    AL1(19,00)                                                       
         DC    CL19'Y=WORK CODE SUMMARY'                                        
*                                                                               
         DC    XL2'E100'                                                        
         DC    AL1(34,00)                                                       
         DC    CL34'Y=REVERSE WITH AN ALLOCATED STATUS'                         
*                                                                               
         DC    XL2'E200'                                                        
         DC    AL1(08,00)                                                       
         DC    CL08'Y OR 2-9'                                                   
*                                                                               
         DC    XL2'E300'                                                        
         DC    AL1(16,00)                                                       
         DC    CL16'C=CORRECTION RUN'                                           
*                                                                               
         DC    XL2'E400'                                                        
         DC    AL1(10,00)                                                       
         DC    CL10'Y OR 01-90'                                                 
*                                                                               
         DC    XL2'E600'                                                        
         DC    AL1(38,00)                                                       
         DC    CL38'P=PERCENT,V=VARIANCE,I=INDEX,B=BALANCE'                     
*                                                                               
         DC    XL2'E700'                                                        
         DC    AL1(27,00)                                                       
         DC    CL27'Y=SUPPRESS NET + COMMISSION'                                
*                                                                               
         DC    XL2'E800'                                                        
         DC    AL1(39,00)                                                       
         DC    CL39'1=HOURS/COST,2=HOURS,3=COST,4=HOURS/MTH'                    
*                                                                               
         DC    XL2'E900'                                                        
         DC    AL1(09,00)                                                       
         DC    CL09'Y=DETAILS'                                                  
*                                                                               
         DC    XL2'EA00'                                                        
         DC    AL1(44,00)                                                       
         DC    CL44'S=WC, M=MP, D=DET, W=WC/DET, P=MP/DET'                      
*                                                                               
         DC    XL2'EB00'                                                        
         DC    AL1(17,00)                                                       
         DC    CL17'Y=CLT/PRD SUMMARY'                                          
*                                                                               
         DC    XL2'EC00'                                                        
         DC    AL1(23,00)                                                       
         DC    CL23'S=SUPPRESS PERSON PAGES'                                    
*                                                                               
         DC    XL2'ED00'                                                        
         DC    AL1(14,00)                                                       
         DC    CL14'N=NET, G=GROSS'                                             
*                                                                               
         DC    XL2'EE00'                                                        
         DC    AL1(29,00)                                                       
         DC    CL29'Y=PROJECTION ON YEARLY ACTUAL'                              
*                                                                               
         DC    XL2'EF00'                                                        
         DC    AL1(38,00)                                                       
         DC    CL38'R=REVERSAL,P=MAKE POSTINGS,BLANK=DRAFT'                     
*                                                                               
         DC    XL2'F000'                                                        
         DC    AL1(21,00)                                                       
         DC    CL21'Y=OFFICE IN SUMMARIES'                                      
*                                                                               
         DC    XL2'F100'                                                        
         DC    AL1(21,00)                                                       
         DC    CL21'N=DON''T REPORT INCOME'                                     
*                                                                               
         DC    XL2'F200'                                                        
         DC    AL1(14,00)                                                       
         DC    CL14'C,S,T,P,H OR U'                                             
*                                                                               
         DC    XL2'F300'                                                        
         DC    AL1(14,00)                                                       
         DC    CL14'Y=CLT/PRD ONLY'                                             
*                                                                               
         DC    XL2'F500'                                                        
         DC    AL1(20,00)                                                       
         DC    CL20'Y=3/12 MONTH FIGURES'                                       
*                                                                               
         DC    XL2'F600'                                                        
         DC    AL1(19,00)                                                       
         DC    CL19'COLUMN MENU=1-8,A-D'                                        
*                                                                               
         DC    XL2'F700'                                                        
         DC    AL1(19,00)                                                       
         DC    CL19'REASON CODE 1-9,A-P'                                        
*                                                                               
         DC    XL2'F900'                                                        
         DC    AL1(18,00)                                                       
         DC    CL18'R=REAL,BLANK=DRAFT'                                         
*                                                                               
         DC    XL2'F901'                                                        
         DC    AL1(28,00)                                                       
         DC    CL28'Y=SUPPRESS INACTIVE ACCOUNTS'                               
*                                                                               
         DC    XL2'FB00'                                                        
         DC    AL1(25,00)                                                       
         DC    CL25'S=SUPPRESS OVERHEAD PAGES'                                  
*                                                                               
         DC    XL2'FD00'                                                        
         DC    AL1(20,00)                                                       
         DC    CL20'SORT ON NAME, Y OR N'                                       
*                                                                               
         DC    XL2'FE01'                                                        
         DC    AL1(27,00)                                                       
         DC    CL27'STANDARD HOURS OVERRIDE 1-9'                                
*                                                                               
         DC    XL2'FE02'                                                        
         DC    AL1(08,00)                                                       
         DC    CL8'MMMDD/YY'                                                    
*                                                                               
         DC    XL2'FE03'                                                        
         DC    AL1(26,00)                                                       
         DC    CL26'Y=Suppress Office Security'                                 
*                                                                               
         DC    XL2'FE04'                                                        
         DC    AL1(22,00)                                                       
         DC    CL22'Y=Use G/L Mos Passives'                                     
*                                                                               
CTWATBLX DC    X'00'               LAST ENTRY                                   
         EJECT                                                                  
***********************************************************************         
* LOCAL WORKING STORAGE                                               *         
***********************************************************************         
*                                                                               
LWS      DSECT                                                                  
RELO     DS    F                                                                
FILTPFK  DS    CL1                 SHOW EXTRA FILTER PFKEY                      
BLOCK    DS    CL20                WORK BLOCK                                   
LWSX     EQU   *                                                                
                                                                                
***********************************************************************         
*              COVER COMMENT ENTRY                                    *         
***********************************************************************         
*                                                                               
COMTBD   DSECT                                                                  
COMNUM   DS    XL2                 COMMENT NUMBER                               
COMLEN   DS    XL1                 LENGTH OF COMMENT                            
         DS    XL1                 SPARE                                        
COMDATA  DS    0C                  COMMENT                                      
         EJECT                                                                  
       ++INCLUDE ACREQWORK                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'093ACREQ01   02/17/21'                                      
         END                                                                    
