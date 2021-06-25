*          DATA SET PEREQ01    AT LEVEL 005 AS OF 06/19/12                      
*PHASE TE0401A                                                                  
         TITLE 'PEREQ01 - REQUEST - VALIDATE DEFN AND BUILD A SCREEN'           
         PRINT NOGEN                                                            
TE0401   CSECT                                                                  
         NMOD1 0,**RQ01**,RR=R7                                                 
         L     R9,0(R1)                                                         
         USING REQTEMP,R9          R9=A(W/S)                                    
         L     R3,ASAVE                                                         
         USING REQSAVE,R3          R3=A(TWA)                                    
         ST    R7,RELO                                                          
         SPACE 2                                                                
*        INITIALISE KEY & REQUEST RECORD                                        
*                                                                               
         XC    KEY,KEY             INITIALISE KEY                               
         XC    REQNUM(18),REQNUM                                                
*                                                                               
         XC    RHDR,RHDR           INITIALISE REQ REC                           
         MVI   RNUM,C' '                                                        
         MVC   RNUM+1(79),RNUM                                                  
         SPACE 2                                                                
         MVI   FIND,0                                                           
         MVI   FERN,X'FF'                                                       
         MVI   USRIDF,0                                                         
         EJECT                                                                  
*        VALIDATE REQUESTOR NAME & SET FIND X'01' = NAME INPUT                  
*                                                                               
VALNAME  CLI   BVRNAMEH+5,0                                                     
         BE    VALNAMA                                                          
         CLC   BVRNAME(4),=C'MENU '                                             
         BNE   *+14                                                             
         MVC   REQNDX1,=X'FFFC'    SET MENU SCREEN ID                           
         B     VALDEF1                                                          
         CLI   DDS,0               DDS TERMINALS CAN HAVE KEYWORDS              
         BNE   VALNAM1                                                          
         CLI   BVRNAMEH+5,12       USER TERM HAS REQUESTOR NAME ONLY            
         BH    INVNAME                                                          
         ZIC   R7,BVRNAMEH+5       MAX LEN IS 12 CHRS                           
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   RNAME(0),BVRNAME                                                 
         OI    FIND,X'01'          SET REQUESTOR NAME INPUT                     
         B     VALNAMA                                                          
*                                                                               
VALNAM1  LA    R7,TEMP                                                          
         GOTO1 SCANNER,PLIST,BVRNAMEH,(3,(R7))                                  
         ZIC   R5,4(R1)                                                         
         LTR   R5,R5               R5=NUM OF INPUT FIELDS                       
         BZ    INVNAME                                                          
*                                                                               
VALNAM2  CLI   1(R7),0             ORDINARY FIELD IS REQUESTOR NAME             
         BNE   VALNAM3                                                          
         CLI   0(R7),12            MAX LEN IS 12 CHRS                           
         BH    INVNAME                                                          
         MVC   RNAME,12(R7)                                                     
         OI    FIND,X'01'          SET REQUESTOR NAME INPUT                     
         B     VALNAM9                                                          
*                                                                               
VALNAM3  CLI   12(R7),C'U'         U=XXX... FOR USERID INPUT                    
         BNE   VALNAM4                                                          
         CLI   1(R7),3                                                          
         BL    INVNAME                                                          
         BH    VALNAM30                                                         
         CLC   22(3,R7),=C'ALL'    U=ALL CHANGE TO AGY=00                       
         BNE   VALNAM30                                                         
         MVI   1(R7),2                                                          
         MVI   3(R7),X'20'                                                      
         MVC   22(2,R7),=C'00'                                                  
         B     VALNAM5A                                                         
VALNAM30 CLI   1(R7),10                                                         
         BH    INVNAME                                                          
         LA    R4,FILREC           READ USER ID REC                             
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
VALNAM3A CLI   0(RE),0             SEARCH FOR SYSTEM ELEMENT                    
         BE    INVNAME                                                          
         CLI   0(RE),X'21'                                                      
         BE    VALNAM3C                                                         
         CLI   0(RE),X'02'                                                      
         BNE   *+10                                                             
         MVC   DUB(2),2(RE)        SAVE USER ID NUM                             
         CLI   0(RE),X'06'                                                      
         BNE   *+10                                                             
         MVC   DUB+4(2),2(RE)      SAVE AGENCY ALPHA ID                         
VALNAM3B IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     VALNAM3A                                                         
VALNAM3C CLC   SYSNUMOV,CTSYSNUM-CTSYSD(RE)                                     
         BNE   VALNAM3B                                                         
         MVC   DUB+2(1),CTSYSSE-CTSYSD(RE)                                      
         MVC   DUB+3(1),CTSYSAGB-CTSYSD(RE)                                     
*                                                                               
VALNAM3D CLC   SYSNUM,DUB+2        MUST BE SAME SYSTEM AS CONNECT               
         BNE   INVNAME                                                          
         CLI   USRIDF,0                                                         
         BNE   INVNAME             ONLY ONE U= OR A=                            
         OI    USRIDF,X'80'                                                     
         MVC   AGYB,DUB+3          SET NEW AGENCY CODES                         
         MVC   AGY,DUB+4                                                        
         MVC   USRID,DUB           SET NEW USERID NUMBER                        
         B     VALNAM9                                                          
*                                                                               
VALNAM4  CLI   12(R7),C'T'         T=Y TO DEFINE UNKNOWN REQUEST                
         BNE   *+12                                                             
         OI    FIND,X'04'                                                       
         B     VALNAM4A                                                         
         CLI   12(R7),C'C'         C=Y TO DEFINE CARD REQUEST                   
         BNE   *+12                                                             
         OI    FIND,X'08'                                                       
         B     VALNAM4A                                                         
         CLI   12(R7),C'1'         1=Y TO DEFINE DDS ONLY FLDS INCLUDED         
         BNE   *+12                                                             
         OI    FIND,X'14'                                                       
         B     VALNAM4A                                                         
         CLI   12(R7),C'2'         2=Y TO DEFINE DDS/2UP FLDS DISPLAY           
         BNE   *+12                                                             
         OI    FIND,X'34'                                                       
         B     VALNAM4A                                                         
         B     VALNAM5                                                          
VALNAM4A CLI   1(R7),1                                                          
         BNE   INVNAME                                                          
         CLI   22(R7),C'Y'                                                      
         BNE   INVNAME                                                          
         B     VALNAM9                                                          
*                                                                               
VALNAM5  CLI   12(R7),C'A'         A=XX TO DEFINE AGY XX (00 FOR ALL)           
         BE    VALNAM5A                                                         
         CLI   12(R7),C'D'         D=XX TO DEFINE AGY XX AND DDS OFFICE         
         BNE   VALNAM6                                                          
         MVC   REQOFFC,=C'DDS*'                                                 
VALNAM5A MVC   AGY,22(R7)                                                       
         MVI   AGYB,0                                                           
         CLI   USRIDF,0                                                         
         BNE   INVNAME             ONLY ONE A= OR U=                            
         OI    USRIDF,X'40'                                                     
         CLI   1(R7),1                                                          
         BNE   VALNAM5B                                                         
         CLI   22(R7),C'*'         A=* MEANS ALL AGENCYS                        
         BNE   INVNAME                                                          
         XC    AGY,AGY                                                          
         B     VALNAM9                                                          
VALNAM5B CLI   1(R7),2                                                          
         BNE   INVNAME                                                          
         CLC   22(2,R7),=C'00'     A=00 MEANS ALL AGENCYS                       
         BNE   VALNAM9                                                          
         XC    AGY,AGY                                                          
         B     VALNAM9                                                          
*                                                                               
VALNAM6  B     INVNAME             INVALID KEYWORD LETTER                       
*                                                                               
VALNAM9  LA    R7,32(R7)           BUMP TO NEXT FIRLD                           
         BCT   R5,VALNAM2                                                       
         B     VALNAMA                                                          
*                                                                               
INVNAME  MVI   FERN,FLDINV                                                      
         B     *+8                                                              
MISNAME  MVI   FERN,FLDMIS                                                      
         LA    R7,BVRNAMEH                                                      
         ST    R7,FADR                                                          
         B     EXIT                                                             
         SPACE 2                                                                
* SAVE AGENCY DATA                                                              
*                                                                               
VALNAMA  MVC   RAGY,AGY            SET AGENCY IN REQ REC AND KEY                
         MVC   KEY(1),AGYB                                                      
         EJECT                                                                  
*        VALIDATE REQUEST NUMBER & ACTION                                       
*                                                                               
VALNUM   CLI   BVRNUMH+5,0                                                      
         BE    MISNUM                                                           
         MVC   IFLDH,BVRNUMH       COPY FIELD INTO IFLD                         
         MVI   IFLD,C' '                                                        
         MVC   IFLD+1(L'IFLD-1),IFLD                                            
         SR    R1,R1                                                            
         IC    R1,IFLDH+5                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   IFLD(0),BVRNUM                                                   
         CLI   IFLDH+5,2                                                        
         BL    INVNUM                                                           
         BE    *+12                                                             
         CLI   IFLD+2,C','                                                      
         BNE   VALNUM1                                                          
         EX    R1,*+8              CONVERT TWO CHR ID TO #XX FORMAT             
         B     *+10                                                             
         MVC   IFLD+1(0),BVRNUM                                                 
         MVI   IFLD,C'#'                                                        
         IC    R1,IFLDH+5                                                       
         LA    R1,1(R1)                                                         
         STC   R1,IFLDH+5                                                       
*                                                                               
VALNUM1  L     R7,AREQTBL          SEARCH REQUEST TABLE                         
         SR    R8,R8                                                            
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
VALNUM1A CLI   0(R7),0             TEST FOR END OF TABLE                        
         BE    VALNUM1C                                                         
         IC    R8,0(R7)            R8=TABLE ENTRY LENGTH                        
         CLI   IFLD,C'#'                                                        
         BE    VALNUM1B                                                         
         CLC   4(3,R7),IFLD        MATCH ON THREE CHR MNENOMIC                  
         BE    VALNUM1D                                                         
         AR    R7,R8                                                            
         B     VALNUM1A                                                         
VALNUM1B LA    RF,0(R7,R8)         POINT TWO LAST TWO BYTES OF ENTRY            
         SH    RF,=H'2'                                                         
         CLC   0(2,RF),IFLD+1      MATCH ON TWO CHR REQUEST ID                  
         BE    VALNUM1D                                                         
         AR    R7,R8                                                            
         B     VALNUM1A                                                         
*                                                                               
VALNUM1C TM    FIND,X'0C'          REQUEST NOT FOUND                            
         BZ    INVNUM              OK FOR CARD/TEST OPTION                      
         CLI   IFLD,C'#'                                                        
         BNE   INVNUM                                                           
         L     R7,AREQTBL          POINT TO FIRST ENTRY                         
         IC    R8,0(R7)                                                         
         MVC   RNUM,IFLD+1                                                      
         B     VALNUM1E                                                         
*                                                                               
VALNUM1D CLI   1(R7),0             REQUEST FOUND                                
         BE    INVNUM                                                           
         MVC   REQNUM,1(R7)        SAVE INTERNAL BINARY REQUEST NUM             
         MVI   REQNUM+1,0                                                       
         LA    RF,0(R8,R7)                                                      
         SH    RF,=H'2'                                                         
         MVC   RNUM,0(RF)          SAVE REQ ID IN REQ REC                       
         CLI   DDS,0                                                            
         BNE   VALNUM1E                                                         
         SH    RF,=H'2'                                                         
         MVC   HALF(1),0(RF)                                                    
         NC    HALF(1),12(R3)      REQ GROUP ANDED WITH TWA AUTH                
         CLI   HALF,0                                                           
         BE    VALNUM1E                                                         
         MVC   BVRRNAM(22),4(R7)                                                
         MVI   FERN,23             ERROR NOT AUTHORISED                         
         B     INVNUM2                                                          
VALNUM1E MVC   BVRRNAM(22),4(R7)   DISPLAY REQ NAME AND #ID OR MNEMONIC         
         CLI   IFLD,C'#'                                                        
         BE    *+14                                                             
         MVI   BVRRNAM,C'#'                                                     
         MVC   BVRRNAM+1(2),RNUM                                                
         OI    BVRRNAMH+6,X'80'                                                 
*                                                                               
VALNUM2  MVC   RHDR+10(1),REQNUM   R7=A(REQTBL ENTRY)                           
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
         CLI   REQACTN,C'L'        LATE                                         
         BE    VALNUM2B                                                         
         CLI   REQACTN,C'S'        SOON                                         
         BE    VALNUM2B                                                         
         B     INVACTN                                                          
VALNUM2B MVI   REQACTN,C'N'        SOON/LATE ARE NEW REQUESTS                   
         CLI   IFLD+4,C'L'                                                      
         BNE   *+12                                                             
*NOP*    OI    REQFLAG,X'04'       SET LATE FLAG IN REQ REC HDR                 
         OI    REQFLAG,X'02'       LATE FLAG NO LONGER EXISTS                   
         B     VALNUM3                                                          
         OI    REQFLAG,X'02'       SET SOON FLAG IN REQ REC HDR                 
         CLI   DDS,1               DDS CAN SOON ANY REQUEST                     
         BE    VALNUM3                                                          
         TM    3(R7),X'40'                                                      
         BZ    INVACTN             CANT SOON THIS REQUEST                       
*                                                                               
VALNUM3  CLI   IFLDH+5,5           NO OPTIONS FOR A OR N                        
         BNE   INVACTN                                                          
VALNUM3A OC    AGY,AGY             AGENCY MUST BE SPECIFIC FOR A OR N           
         BZ    INVNAME                                                          
         TM    USRIDF,X'40'                                                     
         BO    INVNAME                                                          
         B     VALNUMX                                                          
*                                                                               
VALNUM4  CLI   IFLDH+5,5                                                        
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
VALNUMX  B     VALLIST                                                          
*                                                                               
MISNUM   MVI   FERN,01                                                          
         XC    BVRRNAM,BVRRNAM                                                  
         B     INVNUM2                                                          
INVNUM   XC    BVRRNAM,BVRRNAM                                                  
INVNUM1  MVI   FERN,44             INV REQ NUM                                  
INVNUM2  OI    BVRRNAMH+6,X'80'                                                 
         LA    R7,BVRNUMH                                                       
         ST    R7,FADR                                                          
         B     EXIT                                                             
INVACTN  MVI   FERN,10             INV ACTION                                   
         B     INVNUM2                                                          
INVOPTN  MVI   FERN,2              INV INPUT                                    
         B     INVNUM2                                                          
INVINCR  EQU   INVOPTN             INV INPUT                                    
         EJECT                                                                  
*        VALIDATE LIST NAME                                                     
*                                                                               
VALLIST  MVI   SPF,0               CLEAR SPECIAL BITS                           
         XC    SPF1N(4*L'SPF1N),SPF1N                                           
         MVI   LST,0               PRESET TO ALL LISTS                          
         MVC   LSTNAME,BVRLIST                                                  
         CLI   BVRLISTH+5,0        IF OMITTED , TREAT AS ALL                    
         BE    VLI2X                                                            
VLI00    CLC   BVRLIST(3),=C'ALL'  CHECK IF ALL SPECIFIED                       
         BE    VLI2X                                                            
         CLI   AGYB,0              IF AGYB=0 ,LISTNAME IS MEANINGLESS           
         BNE   VLI001              AS IT CANNOT BE RESOLVED TO A NUMBER         
         MVI   FERN,2              WITHOUT AN AGENCY ID                         
         B     LISTERR                                                          
VLI001   XC    KEY,KEY             READ LIST NAME INDEX FOR AGY                 
         MVC   KEY(1),AGYB                                                      
         GOTO1 ARFIL                                                            
         BZ    ELNF                                                             
         BH    VLI01                                                            
         DC    H'0'                                                             
ELNF     MVI   FERN,20             ACCESS NOT ALLOWED IF NRF                    
LISTERR  OI    BVRLISTH+6,X'80'                                                 
         LA    R7,BVRLISTH                                                      
         ST    R7,FADR                                                          
         B     EXIT                                                             
VLI01    IC    R1,BVRLISTH+5       GET INPUT LENGTH                             
         BCTR  R1,0                                                             
         LA    R2,FILREC+PEQSE                                                  
         XR    R0,R0                                                            
VLI02    CLI   0(R2),X'03'         LOOK FOR 03 ELEMENTS                         
         BE    VLI1                                                             
VLI03    IC    R0,1(R2)                                                         
         AR    R2,R0               GET FIRST 03 ELEMENT                         
         CLI   0(R2),0                                                          
         BNE   VLI02                                                            
         B     ELNF                                                             
*                                                                               
         USING PELSTD,R2                                                        
VLI1     EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   BVRLIST(0),PELSTSNM     IS THIS THE LIST REQUESTED ?             
         BNE   VLI03                                                            
*                                                                               
VLI2     MVC   LST,PELSTNUM        SAVE LIST NUMBER                             
         MVC   LSTNAME,PELSTSNM    SAVE COMLETE SHORT LIST NAME                 
         DROP  R2                                                               
VLI2X    MVC   LSTNUM,=C'ALL'                                                   
         CLI   LST,0               DID WE SAY ALL ?                             
         BE    VLI2X1              YES - SKIP                                   
         ZIC   R1,LST              CONVERT LIST TO DISPLAY FORM                 
         CVD   R1,DUB                                                           
         UNPK  LSTNUM,DUB+6(2)                                                  
         OI    LSTNUM+2,X'F0'                                                   
VLI2X1   MVC   RLIST,LSTNUM                                                     
         CLI   REQACTN,C'N'        ACCEPT LIST=ALL EXCEPT ON NEW OR             
         BE    VLI3                                                             
         CLI   REQACTN,C'A'        AMEND                                        
         BNE   VLIX                                                             
VLI3     CLI   LST,0               DID WE SAY ALL ?                             
         BE    VLIX                NOP BE ELNF                 D                
         MVC   KEY+1(1),LST                                                     
         MVI   KEY+2,X'02'         GET FIELD DEFINITION RECORD                  
         GOTO1 ARFIL                                                            
         BZ    VLIX                DONT DO OWT IF NOT THERE                     
         BH    VLI31                                                            
         DC    H'0'                                                             
VLI31    LA    R2,FILREC+PEQSE                                                  
         XR    R0,R0                                                            
VLI32    CLI   0(R2),X'08'         LOOK FOR 08 ELEMENTS                         
         BE    VLI4                                                             
VLI33    IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   VLI32                                                            
         B     VLIX                                                             
*                                                                               
         USING PEFLDD,R2                                                        
VLI4     CLI   PEFLDELC,0          LOOK FOR 'SPECIAL' ELEMENTS                  
         BNE   VLI33                                                            
         OC    SPF,PEFLDEOF        COPY BIT FLAGS                               
         NI    SPF,X'0F'           ENSURE ONLY RIGHT 4 BITS USED                
         TM    PEFLDEOF,X'08'      SET UP FIELD NAMES                           
         BZ    *+10                                                             
         MVC   SPF1N,PEFLDSNM                                                   
         TM    PEFLDEOF,X'04'      SET UP FIELD NAMES                           
         BZ    *+10                                                             
         MVC   SPF2N,PEFLDSNM                                                   
         TM    PEFLDEOF,X'02'      SET UP FIELD NAMES                           
         BZ    *+10                                                             
         MVC   SPF3N,PEFLDSNM                                                   
         TM    PEFLDEOF,X'01'      SET UP FIELD NAMES                           
         BZ    *+10                                                             
         MVC   SPF4N,PEFLDSNM                                                   
         B     VLI33                                                            
         DROP  R2                                                               
*                                                                               
VLIX     MVC   BVRLIST,LSTNAME     ENSURE SCREEN HAS FULL LIST NAME             
         OI    BVRLISTH+6,X'80'                                                 
         OC    SPF1N,=CL9' '       FORCE UPPER CASE                             
         OC    SPF2N,=CL9' '                                                    
         OC    SPF3N,=CL9' '                                                    
         OC    SPF4N,=CL9' '                                                    
         EJECT                                                                  
*        VALIDATE DESTINATION ID NAME                                           
*                                                                               
VALDEST  MVC   REQORIG,USRID       SET ORIGIN ID NUM IN REQ REC HDR             
         CLI   BVRDESTH+5,0                                                     
         BE    VALDESTX            NO DEST INPUT                                
         SR    R1,R1                                                            
         IC    R1,BVRDESTH+5                                                    
         BCTR  R1,0                                                             
         MVI   IFLD,C' '           SET DEST ID NAME IN IFLD                     
         MVC   IFLD+1(9),IFLD                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   IFLD(0),BVRDEST                                                  
*                                                                               
         LA    R4,FILREC           READ ORIGIN ID REC                           
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKID+8(2),REQORIG                                              
         CLC   IFLD(10),=CL10'DDS'                                              
         BNE   VDEST2                                                           
         MVC   CTIKID(10),IFLD                                                  
VDEST2   GOTO1 DATAMGR,DMCB,(0,DMREAD),CTFILE,(R4),(R4)                         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,CTIDATA          FIND VALID DESTINATIONS                      
         SR    RF,RF                                                            
VDEST3   CLI   0(RE),0                                                          
         BE    INVDEST                                                          
         CLI   0(RE),X'34'                                                      
         BE    VDEST5                                                           
VDEST4   IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     VDEST3                                                           
VDEST5   LR    R4,RE                                                            
         USING CTVALD,R4                                                        
         CLC   CTVALDST,IFLD                                                    
         BNE   VDEST4                                                           
         MVC   REQDEST,CTVALNUM    SET DEST ID NUM IN REQ REC HDR               
         B     VALDESTX                                                         
*                                                                               
INVDEST  MVI   FERN,FLDINV                                                      
         LA    R7,BVRDESTH                                                      
         ST    R7,FADR                                                          
         B     EXIT                                                             
*                                                                               
VALDESTX EQU   *                                                                
         EJECT                                                                  
*        VALIDATE OUTPUT TYPE                                                   
*                                                                               
VALOUT   CLI   BVROUTH+5,0                                                      
         BE    VALOUTX             NO OUTPUT TYPE INPUT                         
         CLI   BVROUTH+5,6                                                      
         BH    INVOUT                                                           
         SR    R1,R1                                                            
         IC    R1,BVROUTH+5                                                     
         BCTR  R1,0                                                             
         MVI   IFLD,C' '           SET OUTPUT TYPE IN IFLD                      
         MVC   IFLD+1(9),IFLD                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   IFLD(0),BVROUT                                                   
*                                                                               
         LA    R4,FILREC           READ OUTPUT TYPE RECORD                      
         USING CTOREC,R4                                                        
         XC    CTOKEY,CTOKEY                                                    
         MVI   CTOKEY,C'O'                                                      
         MVC   CTOKID,IFLD                                                      
         GOTO1 DATAMGR,DMCB,(0,DMREAD),CTFILE,(R4),(R4)                         
         CLI   8(R1),0                                                          
         BNE   INVOUT                                                           
         MVC   REQOUT,IFLD         SET OUTPUT TYPE IN REQ REC HDR               
         B     VALOUTX                                                          
*                                                                               
INVOUT   MVI   FERN,FLDINV                                                      
         LA    R7,BVROUTH                                                       
         ST    R7,FADR                                                          
         B     EXIT                                                             
*                                                                               
VALOUTX  EQU   *                                                                
         DROP  R4                                                               
         EJECT                                                                  
*        CHECK IF REQUESTOR IS CONSISTENT WITH REQUEST NUM                      
*                                                                               
VALREQ   TM    3(R7),X'04'         FOR DDS ONLY                                 
         BZ    *+12                                                             
         CLI   DDS,1               YES MUST BE DDS TERMINAL                     
         BNE   INVNUM                                                           
         CLI   REQACTN,C'D'                                                     
         BNE   VALREQ0                                                          
         MVC   REQNDX1(2),=X'FFFE' SET ENQ SCR REQUIRED                         
         CLI   REQOPTN,C'T'                                                     
         BNE   VALREQ7                                                          
         MVC   REQNDX1(2),=X'FFFC' SET MENU SCREEN FOR  TOTAL OPTION            
         B     VALREQ7                                                          
VALREQ0  TM    FIND,X'08'          CARD REQUEST                                 
         BZ    VALREQ1                                                          
         MVC   REQNDX1(2),=X'FFFD'                                              
         B     VALREQ7                                                          
*                                                                               
VALREQ1  TM    3(R7),X'08'         ONLY AVAIL AS CARD REQUEST                   
         BO    INVNAME                                                          
         TM    3(R7),X'01'         IS REQUESTOR REQUIRED                        
         BZ    VALREQ2             NO                                           
         TM    FIND,X'01'          WAS REQUESTOR INPUT                          
         BZ    MISNAME             NO ERROR                                     
*                                                                               
VALREQ2  LA    R8,28(R7)           POINT TO ENTRY                               
         B     VALREQ6                                                          
*                                                                               
VALREQ6  L     R6,AREQTBL                                                       
         SR    R8,R6               SET SCR LIST REQUIRED = ..                   
         STH   R8,REQNDX1          SAVE INDEX TO REQTBL                         
         AR    R8,R6                                                            
*                                                                               
VALREQ7  L     R6,AREQTBL                                                       
         SR    R7,R6                                                            
         STH   R7,REQNDX           SAVE INDEX TO REQTBL                         
         AR    R7,R6                                                            
         MVC   REQFMT(1),FIND                                                   
         EJECT                                                                  
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
         TM    TEMP,X'30'                                                       
         BNZ   INVACTN                                                          
         B     VALIPT              OK TO AMEND                                  
*                                                                               
VALDEF1  CLI   REQNDX1,X'FF'       ENQ OR CARD SCR REQUIRED                     
         BNE   VALIPT                                                           
         CLC   PREQNDX1(2),REQNDX1 IS IT ALREADY LOADED                         
         BNE   *+12                NO- LOAD                                     
         CLI   REQNDX1+1,X'FC'     YES-SKIP LOAD EXCEPT FOR MENU SCREEN         
         BNE   VALDEF2                                                          
         XC    DISPFLDS(2),DISPFLDS                                             
         MVC   PLIST+4(4),=X'D90E04FF'                                          
         MVC   PLIST+7(1),REQNDX1+1                                             
         GOTO1 CALLOV,PLIST,BVRTABH                                             
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   PREQNDX1(2),REQNDX1 SAVE SCR LOADED                              
*                                                                               
         LA    R6,BVRTABH          RETRANSMIT HDR FIELDS                        
         SR    R7,R7                                                            
         LA    R8,64(R3)                                                        
         OI    6(R8),OI1T                                                       
         IC    R7,0(R8)                                                         
         AR    R8,R7                                                            
         CR    R8,R6                                                            
         BNH   *-12                                                             
*                                                                               
         CLI   REQNDX1+1,X'FD'     BUILD REQMAP FOR CARD REQ SCREEN             
         BNE   VALDEF2                                                          
         LA    R5,BVRTABH          FIND 1ST UNPROT DATA FIELD                   
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
         MVI   LREQMAP+3,127                                                    
         MVI   STATUS,1            SET REQUEST DATA REQUIRED                    
         B     DEFAULT                                                          
*                                                                               
VALDEF2  CLI   REQNDX1+1,X'FE'                                                  
         BNE   VALDEF3                                                          
VALDEF2A MVI   STATUS,3            SET ENQ/CANC STATUS                          
         MVC   REQFLTR,FIND        SAVE ENQ/CANC FILTERS                        
         B     SAVEDATA                                                         
VALDEF3  CLI   REQNDX1+1,X'FD'     CARD REQ                                     
         BE    VALIPT1                                                          
         MVI   STATUS,4            SET MENU DISPLAY STATUS                      
         CLI   REQOPTN,C'T'                                                     
         BE    VALDEF2A                                                         
         B     SAVEDATA                                                         
         EJECT                                                                  
*        VALID TO INPUT BEYOND HEADR FOR STATUS=0 ONLY IF A NEW SCREEN          
*        IS NOT REQUIRED FOR NEW REQUEST DEFINITION                             
*                                                                               
VALIPT   CLC   PREQNDX1(2),REQNDX1 SCR FLD LIST CHANGED                         
         BNE   BUILDER             YES MUST BUILD SCR                           
         MVC   TEMP(1),REQFMT                                                   
         XC    TEMP(1),LREQFMT                                                  
         TM    TEMP,X'30'                                                       
         BNZ   BUILDER                                                          
*                                                                               
VALIPT1  LA    R5,BVRTABH          FIND 1ST UNPROT DATA FLD                     
         SR    R6,R6                                                            
         TM    1(R5),X'20'                                                      
         BZ    *+14                                                             
         IC    R6,0(R5)                                                         
         AR    R5,R6                                                            
         B     *-14                                                             
         CLI   LREQMAP,127         ZERO INPUT REQUEST                           
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
*        BUILD A NEW SCREEN FROM REQTBL - R7=A(REQTBL ENTRY)                    
*                                                                               
BUILDER  MVI   STATUS,1            SET REQUEST DATA REQUIRED                    
         LA    RA,LREQMAP          R2=A(REQMAP ENTRY)                           
         LA    R4,2(R8)            R4=A(REQTBL ENTRY 2 BYTES)                   
         LA    R8,BVRTABH          R8=A(NEXT TWA BYTE)                          
         LA    R5,5                R5=LAST TWA LINE NUMBER                      
         SR    R6,R6                                                            
         MVC   HALF,=H'40'                                                      
         B     REQLOO5B                                                         
*                                                                               
REQLOOP  CLI   0(R4),X'31'         LOOK FOR SPECIALS                            
         BNE   *+16                                                             
         TM    SPF,X'08'                                                        
         BO    REQLOOP0                                                         
         B     REQLOIGN            IGNORE IF NOT DEFINED ON THIS LIST           
         CLI   0(R4),X'32'         LOOK FOR SPECIALS                            
         BNE   *+16                                                             
         TM    SPF,X'04'                                                        
         BO    REQLOOP0                                                         
         B     REQLOIGN            IGNORE IF NOT DEFINED ON THIS LIST           
         CLI   0(R4),X'33'         LOOK FOR SPECIALS                            
         BNE   *+16                                                             
         TM    SPF,X'02'                                                        
         BO    REQLOOP0                                                         
         B     REQLOIGN            IGNORE IF NOT DEFINED ON THIS LIST           
         CLI   0(R4),X'34'         LOOK FOR SPECIALS                            
         BNE   *+16                                                             
         TM    SPF,X'01'                                                        
         BO    REQLOOP0                                                         
         B     REQLOIGN            IGNORE IF NOT DEFINED ON THIS LIST           
*                                                                               
REQLOOP0 LA    R7,TWATBL           R7=A(TWATBL ENTRY)                           
*                                                                               
TWALOOP  IC    R6,1(R7)            FIND ENTRY IN TWATBL                         
         LTR   R6,R6                                                            
         BNZ   TWALOOP1                                                         
         LR    RF,RA               FLD MUST BE A SUB FLD                        
         SH    RF,=H'3'                                                         
         MVC   0(1,RA),0(R4)       SET FLD NUM                                  
         MVC   1(2,RA),1(RF)       SET FLD ADR TO PREVIOUS                      
         LA    RA,3(RA)            BUMP REQ MAP ENTRY                           
         B     REQLOOP5                                                         
TWALOOP1 CLC   0(1,R7),0(R4)                                                    
         BE    REQLOOP1                                                         
         LA    R7,3(R7,R6)                                                      
         B     TWALOOP                                                          
*                                                                               
REQLOOP1 TM    REQFMT,X'20'        2UP OPTION ON REQUESTOR                      
         BZ    REQLOOP2                                                         
         TM    0(R4),X'80'                                                      
         BZ    *+10                                                             
         SR    R6,R6               IGNORE ALL COMMENTS                          
         B     REQLOOP5                                                         
         OC    HALF,HALF                                                        
         BNZ   REQLOO25                                                         
         MVC   HALF,=H'40'         SET TO SECOND HALF                           
         B     REQLOOP3                                                         
*                                                                               
REQLOOP2 TM    0(R4),X'80'         STD OPTION                                   
         BO    *+14                SKIP IF SAME LINE COMMENT                    
REQLOO25 XC    HALF,HALF           BUMP TO NEW LINE                             
         LA    R5,1(R5)                                                         
         CH    R5,=H'24'                                                        
         BNH   *+6                                                              
         DC    H'0'                TOO MANY LINES                               
*                                                                               
REQLOOP3 MVC   0(8,R8),=X'1820010200008010'  SET PROT FLD HDR LEN=16            
         CLI   0(R7),127                                                        
         BL    REQLOO3A                                                         
         BH    *+14                                                             
         MVC   0(8,R8),=X'0920000200008001'  TAB PROT FLD HDR                   
         B     REQLOO3A                                                         
         STC   R6,7(R8)                                                         
         LA    RF,8(R6)                                                         
         STC   RF,0(R8)                                                         
         MVC   2(2,R8),=X'002A'    SET COL#42 FOR COMMENTS                      
REQLOO3A SR    RF,RF               SET PROT FLD TXT                             
         IC    RF,7(R8)                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    8(0,R8),8(R8)                                                    
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R8),3(R7)                                                    
         CLI   0(R4),X'31'         LOOK FOR SPECIALS                            
         BL    REQLOO3B                                                         
         BNE   *+18                                                             
         EX    R6,*+8                                                           
         B     REQLOO3B                                                         
         MVC   8(0,R8),SPF1N                                                    
         CLI   0(R4),X'34'         LOOK FOR SPECIALS                            
         BH    REQLOO3B                                                         
         BNE   *+18                                                             
         EX    R6,*+8                                                           
         B     REQLOO3B                                                         
         MVC   8(0,R8),SPF4N                                                    
         CLI   0(R4),X'32'         LOOK FOR SPECIALS                            
         BNE   *+18                                                             
         EX    R6,*+8                                                           
         B     REQLOO3B                                                         
         MVC   8(0,R8),SPF2N                                                    
         EX    R6,*+8                                                           
         B     REQLOO3B                                                         
         MVC   8(0,R8),SPF3N                                                    
REQLOO3B LA    R6,8(RF)                                                         
         IC    RF,2(R7)                                                         
         LTR   RF,RF                                                            
         BZ    REQLOO3X                                                         
         LA    RE,0(R8,R6)                                                      
         MVC   0(8,RE),=X'0000001400008000'  SET UNPROT FLD HDR                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    8(0,RE),8(RE)       NULL TEXT                                    
         STC   RF,7(RE)                                                         
         LA    RF,8(RF)                                                         
         STC   RF,0(RE)                                                         
REQLOO3X AR    R6,RF               SET R6=TOTAL TWA LENGTH                      
*                                                                               
         SR    R0,R0               CALC ABSOLUTE SCREEN ADDR                    
         LR    R7,R8               R7=A(FLD IN TWA)                             
REQLOOP4 CLI   0(R7),0                                                          
         BE    REQLOOP5                                                         
         MVC   DUB(2),2(R7)        DUB=X'XXCC' CC=COLNUM                        
         CLI   0(R4),127                                                        
         BNL   REQLOO45                                                         
         CLI   DUB,0               SPECIAL ATTRIBUTE REQUIRED                   
         BE    REQLOO45            NO                                           
         TM    1(R4),X'01'         OPTIONAL INPUT FIELD                         
         BO    REQLOO45            YES                                          
         OI    1(R7),X'08'         SET HIGH INTENSITY                           
         NI    1(R7),X'FB'                                                      
         IC    R0,0(R7)            SET FLD REQUIRED FLAG                        
         LR    RF,R7                                                            
         AR    RF,R0                                                            
         BCTR  RF,0                                                             
         MVI   0(RF),C'='                                                       
REQLOO45 MVI   DUB,0               DUB=X'00CC'                                  
         LH    RE,DUB                                                           
         AH    RE,HALF                                                          
         BCTR  RE,0                                                             
         LR    RF,R5                                                            
         BCTR  RF,0                                                             
         MH    RF,=H'80'                                                        
         AR    RF,RE                                                            
         STH   RF,DUB              DUB=X'AAAA'=ABSOLUTE SCR ADR                 
         MVC   2(2,R7),DUB                                                      
         TM    1(R7),X'20'         IS FLD PROTECTED                             
         BO    REQLOO47            YES                                          
         LR    RF,R7               NO GET RELATIVE TWA ADDR                     
         SR    RF,R3                                                            
         MVC   0(1,RA),0(R4)       SET FLD NUM                                  
         STC   RF,2(RA)            SET FLD ADR TO RELATIVE TWA ADDR             
         SRL   RF,8                                                             
         STC   RF,1(RA)                                                         
         LA    RA,3(RA)            BUMP REQ MAP ENTRY                           
REQLOO47 IC    R0,0(R7)                                                         
         AR    R7,R0                                                            
         B     REQLOOP4                                                         
*                                                                               
REQLOIGN MVC   0(1,RA),0(R4)       SET DUMMY TABLE ENTRY IF IGNORED             
         LA    RA,3(RA)            BUMP REQ MAP ENTRY                           
REQLOIG1 CLI   2(R4),128           IS NEXT A COMMENT                            
         BL    REQLOO5A            NO - SKIP                                    
         TM    2(R4),1             IS IT SAME LINE COMMENT                      
         BZ    REQLOO5A            NO - SKIP                                    
         LA    R4,2(,R4)           ELSE IGNORE IT TOO                           
         B     REQLOO5A                                                         
*                                                                               
REQLOOP5 AR    R8,R6               UPDATE NEXT TWA ADR                          
REQLOO5A CLI   0(R4),127           BUMP REQ TBL ENTRY                           
         BE    REQLOOPX            LAST ENTRY WAS TAB                           
         BL    *+12                                                             
         LA    R4,2(R4)            LAST ENTRY WAS COMMENT                       
         B     *+8                                                              
         LA    R4,2(R4)            LAST ENTRY WAS DATA                          
REQLOO5B CLI   0(R4),0                                                          
         BE    REQLOOP6                                                         
         CLI   0(R4),127                                                        
         BH    REQLOOP                                                          
         TM    1(R4),X'80'         DDS ONLY ENTRY IN REQTBL                     
         BZ    REQLOOP             NO                                           
         TM    REQFMT,X'10'        YES ONLY FOR 1UP/2UP REQUESTOR               
         BZ    REQLOO5A                                                         
         B     REQLOOP                                                          
*                                                                               
REQLOOP6 LA    R4,=X'7F0000'       SET FOR TAB LINE                             
         B     REQLOOP                                                          
*                                                                               
REQLOOPX MVC   0(3,R8),=X'000100'  SET B,A=CLEAR,NOTHING                        
         LA    R8,64(R3)           RETRANSMIT REQ DEFN SCR                      
         SR    R7,R7                                                            
         LA    R6,BVRTABH                                                       
REQLXL   OI    6(R8),OI1T                                                       
         IC    R7,0(R8)                                                         
         AR    R8,R7                                                            
         CR    R8,R6                                                            
         BNH   REQLXL                                                           
*                                                                               
         MVC   PREQNDX1(2),REQNDX1 SET FLD LIST SCR LOADED                      
         CLI   LREQMAP,127         IS 1ST FLD TAB LINE                          
         BNE   DEFAULT             NO                                           
         MVI   STATUS,2            YES SET REQ DATA INPUT                       
         MVC   HALF,LREQMAP+1                                                   
         LH    R7,HALF                                                          
         AR    R7,R3                                                            
         ST    R7,AFIRSTF          SIMULATE INPUT                               
         ST    R7,ALASTF                                                        
         EJECT                                                                  
*        SET DEFAULT VALUES IN REQUEST RECORD (IF ANY)                          
*                                                                               
DEFAULT  L     R7,AREQTBL          R7=A(REQTBL ENTRY)                           
         AH    R7,REQNDX                                                        
         SR    RF,RF               RF=DEFAULT ROUTINE NUM                       
         IC    RF,26(R7)                                                        
         LTR   RF,RF                                                            
         BZ    ACCESS              NO DEFAULT VALUES                            
         SLA   RF,2                                                             
         LA    RF,REQROUTS(RF)                                                  
         L     RF,0(RF)                                                         
         A     RF,RELO                                                          
         BASR  RE,RF               SET DEFAULT VALUES                           
         CLI   FERN,X'FF'                                                       
         BE    ACCESS                                                           
         MVI   STATUS,0                                                         
         B     EXIT                                                             
         SPACE 2                                                                
*        SET VALUES DEFINED BY RESTRICTED ACCESS CODE IN TWA                    
*                                                                               
ACCESS   LA    R6,6(R3)            R6=A(TWAACCS)                                
         CLI   0(R6),0                                                          
         BE    ACCESS2                                                          
         B     ACCESS2                                                          
ACCESS2  CLI   1(R6),0                                                          
         BE    ACCESS4                                                          
         B     ACCESS4                                                          
ACCESS4  EQU   *                                                                
         SPACE 2                                                                
*        SAVE INITIALISED DATA IN TWA                                           
*                                                                               
SAVEDATA MVC   LREQNUM(18),REQNUM                                               
         MVC   LKEY,KEY                                                         
         MVC   LREQREC(106),REQREC                                              
         TM    FIND,X'08'                                                       
         BZ    EXIT                                                             
         CLI   REQACTN,C'N'                                                     
         BNE   EXIT                                                             
         CLI   STATUS,2                                                         
         BE    EXIT                                                             
         MVC   DUB(2),LREQMAP+1    DISPLAY NEW CARD DEFAULTS                    
         LH    R5,DUB                                                           
         AR    R5,R3                                                            
         MVC   8(78,R5),REQREC+28                                               
         OI    6(R5),X'80'                                                      
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
*        ROUTINES TO FILL IN DEFAULT VALUES IN REQUEST RECORD                   
         SPACE 2                                                                
*        1 = PRESET STATUS TO 'B' TO READ ALL MEMBERS                           
REQR01   DS    0H                                                               
         MVI   RSTATUS,C'B'                                                     
         BR    RE                                                               
*        2 = NOT USED                                                           
REQR02   DS    0H                                                               
         BR    RE                                                               
         SPACE 2                                                                
*        THIS TABLE CONTAINS THE ADDRESSES OF ROUTINES THAT FILL IN             
*        DEFAULT VALUES OF VARIOUS REQUEST RECORD FIELDS. IT IS INDEXED         
*        BY A ROUTINE NUMBER STORED AT REQTBL-ENTRY+26(1).                      
*                                                                               
REQROUTS DC    F'0'                00                                           
         DC    A(REQR01)           01                                           
         DC    A(REQR02)           02                                           
         SPACE 2                                                                
FLDMIS   EQU   1                                                                
FLDINV   EQU   2                                                                
DMREAD   DC    CL8'DMREAD'                                                      
CTFILE   DC    CL8'CTFILE'                                                      
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*        EACH ENTRY IN THIS TABLE DEFINES THE TEXT OF A PROTECTETED             
*        DATA FIELD. THE ENTRY CAN ALSO DEFINE THE LENGTH OF AN                 
*        ASSOCIATED UNPROTECTED INPUT FIELD. ENTRY FORMAT -                     
*        AL1   ENTRY NUM                                                        
*        AL1   PROTECTED FIELD LENGTH (=P) MAX=15 OR 38                         
*        AL1   UNPROTECTED FIELD LENGTH    MAX=20                               
*        CLP   PROTECTED FIELD DATA                                             
*                                                                               
*                                                                               
*        THE ENTRY NUMBER DEFINES THE TYPE OF FIELD. ALL ENTRIES WITH           
*        NUMBER LESS THAN OR EQUAL TO 127 ARE ENTRIES CONTAINING AN             
*        INPUT FIELD (127 IS THE LAST OR TAB INPUT ENTRY). ALL ODD              
*        NUMBERED ENTRIES GREATER THAN 127 ARE SAME LINE COMMENTS, AND          
*        ALL EVEN NUMBERED ENTRIES GREATER THAN 127 ARE NEW LINE                
*        COMMENTS.                                                              
*                                                                               
TWATBL   DS    0C                                                               
*                                                                               
         DC    AL1(011,04,09),C'DATE'                                           
         DC    AL1(012,10,13),C'TOWN/PCODE'                                     
         DC    AL1(013,03,02),C'T/P'                                            
         DC    AL1(014,10,09),C'START DATE'                                     
         DC    AL1(015,08,09),C'END DATE'                                       
         DC    AL1(016,09,09),C'DATE TYPE'                                      
         DC    AL1(017,09,06),C'AGE RANGE'                                      
         DC    AL1(018,03,07),C'SEX'                                            
         DC    AL1(019,07,21),C'COUNTRY'                                        
         DC    AL1(020,06,07),C'STATUS'                                         
*                                                                               
         DC    AL1(033,08,02),C'OPTION#1'                                       
         DC    AL1(034,08,02),C'OPTION#2'                                       
         DC    AL1(035,08,02),C'OPTION#3'                                       
         DC    AL1(036,08,02),C'OPTION#4'                                       
         DC    AL1(037,08,02),C'OPTION#5'                                       
         DC    AL1(038,08,02),C'OPTION#6'                                       
         DC    AL1(039,08,02),C'OPTION#7'                                       
*                                                                               
         DC    AL1(049,09,02),C'SPECIAL#1'                                      
         DC    AL1(050,09,02),C'SPECIAL#2'                                      
         DC    AL1(051,09,02),C'SPECIAL#3'                                      
         DC    AL1(052,09,02),C'SPECIAL#4'                                      
*                                                                               
         DC    AL1(127,01,01),C' '                                              
*                                                                               
         DC    AL1(131,29,00),C'TOWN OR POSTCODE FILTER VALUE'                  
         DC    AL1(133,29,00),C'FILTER TYPE,T=TOWN,P=POSTCODE'                  
         DC    AL1(139,36,00),C'ADD,BORN,CHANGE,DUE,JOIN,LAPSE,RENEW'           
         DC    AL1(147,27,00),C'ACTIVE(DEFAULT),LAPSED,BOTH'                    
         DC    AL1(149,22,00),C'Y OR N OR BLANK=EITHER'                         
         DC    AL1(191,34,00),C'Y=SPLIT NEW MEMBERS BY SOURCE CODE'             
         DC    AL1(193,23,00),C'1-6=FIRST SORT SEQUENCE'                        
         DC    AL1(195,24,00),C'1-6=SECOND SORT SEQUENCE'                       
         DC    AL1(197,23,00),C'1-6=THIRD SORT SEQUENCE'                        
         DC    AL1(199,17,00),C'Y=PRINT ADDRESSES'                              
         DC    AL1(225,33,00),C'1ST SORT SEQ. C=COUNTRY, R=REGION'              
         DC    AL1(227,22,00),C'2ND SORT SEQ. C=COUNTY'                         
         DC    AL1(229,20,00),C'3RD SORT SEQ. T=TOWN'                           
         DC    AL1(231,35,00),C'4TH SORT SEQ. N=SURNAME, P=POSTCODE'            
         DC    AL1(233,25,00),C'COLUMN VALUES 1=SEX,2=AGE'                      
         DC    AL1(235,23,00),C'ROW VALUES, A THROUGH G'                        
*                                                                               
TWATBLX  DC    X'0000'                                                          
         EJECT                                                                  
*PEREQSAVE                                                                      
       ++INCLUDE PEREQSAVE                                                      
         EJECT                                                                  
         ORG   REQSAVE+64                                                       
*PEREQFFD                                                                       
       ++INCLUDE PEREQFFD                                                       
         EJECT                                                                  
*PEREQTEMP                                                                      
       ++INCLUDE PEREQTEMP                                                      
         EJECT                                                                  
       ++INCLUDE DDFLDIND                                                       
         EJECT                                                                  
*PEGENFILE                                                                      
       ++INCLUDE PEGENFILE                                                      
         EJECT                                                                  
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005PEREQ01   06/19/12'                                      
         END                                                                    
