*          DATA SET CPREQ01    AT LEVEL 023 AS OF 05/01/02                      
*PHASE TC0401A                                                                  
*INCLUDE GETIDS                                                                 
         TITLE 'CPREQ01 - REQUEST - VALIDATE DEFN AND BUILD A SCREEN'           
         PRINT NOGEN                                                            
TC0401   CSECT                                                                  
         NMOD1 000,TC0401,RR=R9                                                 
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     R9,0(R1)                                                         
         USING REQTEMP,R9          R9=A(W/S)                                    
         L     R3,ASAVE                                                         
         USING REQSAVE,R3          R3=A(TWA)                                    
         EJECT                                                                  
*        INITIALISE KEY & REQUEST RECORD                                        
*                                                                               
         XC    KEY,KEY             INITIALISE KEY                               
         MVC   KEY+1(2),AGY                                                     
         XC    REQNUM(18),REQNUM                                                
*                                                                               
         XC    RHDR,RHDR           INITIALISE REQ REC                           
         MVI   RNUM,C' '                                                        
         MVC   RNUM+1(79),RNUM                                                  
         MVC   RAGY,AGY                                                         
         SPACE 2                                                                
         LR    R2,R3               R2=A(TWA SCREEN FF)                          
         USING TC04FFD,R2                                                       
         MVI   FIND,0                                                           
         MVI   FERN,X'FF'                                                       
         SPACE 2                                                                
*        VALIDATE REQUESTOR NAME & SET FIND X'01' = NAME INPUT                  
*                                                                               
VALNAME  CLI   BVRNAMEH+5,0                                                     
         BE    VALNUM                                                           
         CLC   BVRNAME(4),=C'MENU '                                             
         BNE   *+14                                                             
         MVC   REQNDX1,=X'FFFC'    SET MENU SCREEN ID                           
         B     VALDEF1                                                          
         SR    R7,R7                                                            
*                                                                               
VALNAME1 CLI   DDS,1               DDS TERMINAL CAN HAVE KEYWORD                
         BNE   VALNAME3                                                         
         CLC   BVRNAME(3),=C'TST'  TST=Y TO DEFINE UNKNOWN REQUEST              
         BNE   *+12                                                             
         OI    FIND,X'04'                                                       
         B     VALNAME2                                                         
         CLC   BVRNAME(3),=C'CRD'  CRD=Y TO DEFINE CARD REQUEST                 
         BNE   *+12                                                             
         OI    FIND,X'08'                                                       
         B     VALNAME2                                                         
         CLC   BVRNAME(3),=C'1UP'  1UP=Y TO DEFINE DDS FIELDS INCLUDED          
         BNE   *+12                                                             
         OI    FIND,X'14'                                                       
         B     VALNAME2                                                         
         CLC   BVRNAME(3),=C'2UP'  2UP=Y TO DEFINE DDS/2UP DISPLAY              
         BNE   *+12                                                             
         OI    FIND,X'34'                                                       
         B     VALNAME2                                                         
         CLC   BVRNAME(3),USERCODE USR=XX TO DEFINE USER                        
         BNE   *+14                                                             
         MVC   RAGY,BVRNAME+4      *****                                        
         B     VALNAME2                                                         
         CLC   BVRNAME(3),=C'DDS'  DDS=XX TO DEFINE USER FOR DDS OFFICE         
         BNE   *+20                                                             
         MVC   RAGY,BVRNAME+4      *****                                        
         MVC   REQOFFC,=C'DDS*'                                                 
         B     VALNAME2                                                         
         B     VALNAME3                                                         
*                                                                               
VALNAME2 CLI   BVRNAME+3,C'='      CHECK SYNTAX OF KEYWORD                      
         BNE   INVNAME                                                          
         TM    FIND,X'3C'                                                       
         BZ    VALNAM2A                                                         
         CLI   BVRNAME+4,C'Y'                                                   
         BNE   INVNAME                                                          
         CLI   BVRNAMEH+5,5        CHECK FOR XXX=X,REQUESTOR                    
         BE    VALNUM                                                           
         BL    INVNAME                                                          
         CLI   BVRNAME+5,C','                                                   
         BNE   INVNAME                                                          
         LA    R7,6                                                             
         B     VALNAME3                                                         
*                                  CHECK FOR ALL USER CODE                      
VALNAM2A CLC   ALLCODE,BVRNAME+4                                                
         BNE   VALNAM2B                                                         
         CLI   BVRNAMEH+5,4+L'ALLCODE                                           
         BE    VALNUM                                                           
         BL    INVNAME                                                          
         CLI   BVRNAME+4+L'ALLCODE,C','                                         
         BNE   INVNAME                                                          
         LA    R7,5+L'ALLCODE                                                   
         B     VALNAME3                                                         
*                                  CHECK FOR SPECIFIC USER CODE                 
VALNAM2B CLI   BVRNAMEH+5,4+SPECLEN                                             
         BE    VALNUM                                                           
         BL    INVNAME                                                          
         CLI   BVRNAME+4+SPECLEN,C','                                           
         BNE   INVNAME                                                          
         LA    R7,5+SPECLEN                                                     
         B     VALNAME3                                                         
*                                                                               
VALNAME3 SR    R8,R8                                                            
         IC    R8,BVRNAMEH+5                                                    
         SR    R8,R7                                                            
         BZ    VALNUM                                                           
         OI    FIND,X'01'          SET REQUESTOR NAME INPUT                     
         CH    R8,=H'12'                                                        
         BH    INVNAME                                                          
         LA    R7,BVRNAME(R7)                                                   
         BCTR  R8,R0                                                            
         EX    R8,*+8                                                           
         B     VALNUM                                                           
         MVC   RNAME(0),0(R7)                                                   
*                                                                               
INVNAME  MVI   FERN,FLDINV                                                      
         B     *+8                                                              
MISNAME  MVI   FERN,FLDMIS                                                      
         LA    R7,BVRNAMEH                                                      
         ST    R7,FADR                                                          
         B     EXIT                                                             
*                                                                               
SPECLEN  EQU   2                   *****DEFINES LEN OF SPECIFIC ID              
USERCODE DC    C'AGY'              *****DEFINES VALUE OF USER NAME              
ALLCODE  DC    C'*'                *****DEFINES LEN/VALUE OF ALL CODE           
         DS    0H                                                               
         SPACE 2                                                                
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
         BE    VALNUMX             YES                                          
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
         BE    VALNUMX                                                          
         B     INVACTN                                                          
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
VALNUMX  B     VALMED                                                           
*                                                                               
MISNUM   MVI   FERN,01                                                          
         XC    BVRRNAM,BVRRNAM                                                  
         B     INVNUM2                                                          
INVNUM   XC    BVRRNAM,BVRRNAM                                                  
INVNUM1  MVI   FERN,34             INV REQ NUM                                  
INVNUM2  OI    BVRRNAMH+6,X'80'                                                 
         LA    R7,BVRNUMH                                                       
         ST    R7,FADR                                                          
         B     EXIT                                                             
INVACTN  MVI   FERN,12             INV ACTION                                   
         B     INVNUM2                                                          
INVOPTN  MVI   FERN,2              INV INPUT                                    
         B     INVNUM2                                                          
INVINCR  EQU   INVOPTN             INV INPUT                                    
         SPACE 2                                                                
*        VALIDATE MEDIA & SET FIND X'02' = MEDIA INPUT                          
*                                                                               
VALMED   DS    0H                                                               
         MVI   BVRMED,C'T'         *** MEDIA NOT INPUT ***                      
         B     *+12                *** MEDIA NOT INPUT ***                      
         CLI   BVRMEDH+5,0                                                      
         BE    VALDEST                                                          
         LA    R4,MEDTBL                                                        
VALMED1  CLI   0(R4),0                                                          
         BE    INVMED                                                           
         CLC   0(1,R4),BVRMED                                                   
         BE    VALMED2                                                          
         LA    R4,L'MEDTBL(R4)                                                  
         B     VALMED1                                                          
VALMED2  MVC   REQMED,0(R4)        SAVE MEDIA VALUE                             
         MVC   RMED,0(R4)          MEDIA TO REQ REC                             
         MVC   REQMED1,1(R4)       SAVE MEDIA BIT MASK                          
         OC    KEY+3(1),2(R4)      MEDIA TO KEY                                 
         OI    FIND,X'02'                                                       
         B     VALDEST                                                          
MISMED   MVI   FERN,1                                                           
         B     *+8                                                              
INVMED   MVI   FERN,2                                                           
         LA    R7,BVRMEDH                                                       
         ST    R7,FADR                                                          
         B     EXIT                                                             
*                                                                               
MEDTBL   DS    0CL3                INPUT VALUE/BIT MASK/KEY VALUE               
         DC    C'T',B'00000001',C'T'                                            
MEDTBLX  DC    X'00'                                                            
         DS    0H                                                               
         EJECT                                                                  
*        VALIDATE DESTINATION ID NAME                                           
*                                                                               
VALDEST  EQU   *                                                                
         MVC   REQORIG,10(R3)      SET ORIGIN ID NUM IN REQ REC HDR             
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
         L     R5,DATAMGR                                                       
         GOTO1 =V(GETIDS),PLIST,(C'D',FILREC),0,(R5),RR=RELO                    
         CLI   PLIST,0                                                          
         BE    INVDEST             NO DESTS FOUND                               
         CLI   PLIST,X'FF'                                                      
         BNE   *+6                 DISK ERROR                                   
         DC    H'0'                                                             
         ZIC   R5,PLIST             NUMBER OF DESTS                             
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
VALOUT   EQU   *                                                                
         CLI   BVROUTH+5,0                                                      
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
         LA    R6,CTOREC+28                                                     
VALOUT3  CLI   0(R6),X'38'                                                      
         BE    VALOUT4                                                          
         CLI   0(R6),0             END OF REC                                   
         BE    INVOUT                                                           
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     VALOUT3                                                          
*                                                                               
VALOUT4  DS    0H                                                               
         USING CTOUTD,R6                                                        
         TM    CTOUTSTA,X'80'      SEE IF REQUESTABLE OUTPUT TYPE               
         BZ    INVOUT              NO                                           
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
         DROP  R6                                                               
         EJECT                                                                  
*        CHECK IF REQUESTOR AND MEDIA ARE CONSISTENT WITH REQUEST NUM           
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
VALREQ2  TM    3(R7),X'02'         IS MEDIA REQUIRED                            
         BO    VALREQ3             YES                                          
         TM    FIND,X'02'          WAS MEDIA INPUT                              
         BO    VALREQ3                                                          
         LA    R4,MEDTBL           FIND DEFAULT MEDIA                           
         CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   DUB(1),1(R4)                                                     
         NC    DUB(1),29(R7)                                                    
         BNZ   *+12                                                             
         LA    R4,L'MEDTBL(R4)                                                  
         B     *-30                                                             
         MVC   BVRMED,0(R4)                                                     
         MVC   BVRMEDH+4(2),=X'C001'                                            
         OI    BVRMEDH+6,X'80'                                                  
         B     VALMED                                                           
*                                                                               
VALREQ3  TM    FIND,X'02'          WAS MEDIA INPUT                              
         BZ    MISMED              NO ERROR                                     
         MVC   DUB(1),REQMED1                                                   
         NC    DUB(1),2(R7)                                                     
         BZ    INVMED                                                           
*                                                                               
VALREQ4  LA    R8,28(R7)           FIND FLD LIST FOR MEDIA                      
         SR    R6,R6                                                            
VALREQ5  CLI   0(R8),0             R8=A(REQTBL MEDIA ENTRY)                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   DUB(1),1(R8)                                                     
         NC    DUB(1),REQMED1                                                   
         BNZ   VALREQ6                                                          
         IC    R6,0(R8)                                                         
         AR    R8,R6                                                            
         B     VALREQ5                                                          
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
         MVC   PLIST+4(4),=X'D90C04FF'                                          
         MVC   PLIST+7(1),REQNDX1+1                                             
         GOTO1 CALLOV,PLIST,BVRFRSTH                                            
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   PREQNDX1(2),REQNDX1 SAVE SCR LOADED                              
*                                                                               
         LA    R6,BVRFRSTH         RETRANSMIT HDR FIELDS                        
         SR    R7,R7                                                            
         LA    R8,64(R3)                                                        
         OI    6(R8),OI1T                                                       
         IC    R7,0(R8)                                                         
         AR    R8,R7                                                            
         CR    R8,R6                                                            
         BNH   *-12                                                             
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
VALIPT   CLC   PREQNDX1(2),REQNDX1 SCR FLD LIST CHANGED                         
         BNE   BUILDER             YES MUST BUILD SCR                           
         MVC   TEMP(1),REQFMT                                                   
         XC    TEMP(1),LREQFMT                                                  
         TM    TEMP,X'30'                                                       
         BNZ   BUILDER                                                          
*                                                                               
VALIPT1  LA    R5,BVRFRSTH         FIND 1ST UNPROT DATA FLD                     
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
VALIPT3  CLI   REQNDX1,X'FF'                                                    
         BNE   VALIPT2                                                          
         MVI   STATUS,1            SET REQUEST DATA REQUIRED                    
         SR    R5,R3                                                            
         STH   R5,DUB              BUILD REQMAP FOR CARD SCREEN                 
         MVI   LREQMAP,126                                                      
         MVC   LREQMAP+1(2),DUB                                                 
         MVI   LREQMAP+3,127                                                    
         B     DEFAULT                                                          
         EJECT                                                                  
*        BUILD A NEW SCREEN FROM REQTBL - R7=A(REQTBL ENTRY)                    
*                                                                               
BUILDER  MVI   STATUS,1            SET REQUEST DATA REQUIRED                    
         LA    RA,LREQMAP          R2=A(REQMAP ENTRY)                           
         LA    R4,2(R8)            R4=A(REQTBL ENTRY 2 BYTES)                   
         LA    R8,BVRFRSTH         R8=A(NEXT TWA BYTE)                          
         LA    R5,5                R5=LAST TWA LINE NUMBER                      
         SR    R6,R6                                                            
         MVC   HALF,=H'40'                                                      
         B     REQLOO5B                                                         
*                                                                               
REQLOOP  LA    R7,TWATBL           R7=A(TWATBL ENTRY)                           
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
REQLOOP2 TM    0(R4),X'81'         STD OPTION                                   
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
         LA    R6,8(RF)                                                         
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
         LA    R6,BVRFRSTH                                                      
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
         BZ    SAVEDATA            NO DEFAULT VALUES                            
         SLA   RF,2                                                             
         LA    RF,REQROUTS(RF)                                                  
         L     RF,0(RF)                                                         
         A     RF,RELO             RF=A(DEFAULT ROUTINE)                        
         BASR  RE,RF               SET DEFAULT VALUES                           
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
         MVC   DUB(2),LREQMAP+1    DISPLAY NEW CARD DEFAULTS                    
         LH    R5,DUB                                                           
         AR    R5,R3                                                            
         FOUT  (R5),REQREC+28,78                                                
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
*        ROUTINES TO FILL IN DEFAULT VALUES IN REQUEST RECORD                   
         SPACE 2                                                                
REQR01   MVC   RMKT(3),=C'ALL'                                                  
         BR    RE                                                               
         SPACE 2                                                                
*        THIS TABLE CONTAINS THE ADDRESSES OF ROUTINES THAT FILL IN             
*        DEFAULT VALUES OF VARIOUS REQUEST RECORD FIELDS. IT IS INDEXED         
*        BY A ROUTINE NUMBER STORED AT REQTBL-ENTRY+26(1).                      
*                                                                               
REQROUTS DC    F'0'                00                                           
         DC    A(REQR01)           01                                           
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
*        AL1   PROTECTED FIELD LENGTH (=P) MAX=15                               
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
         DC    AL1(001,05,13),C'RANGE'                                          
         DC    AL1(002,10,06),C'MARKET SEQ'                                     
         DC    AL1(003,06,09),C'MARKET'                                         
         DC    AL1(004,08,08),C'UNIVERSE'                                       
         DC    AL1(005,14,18),C'DATA TYPE LIST'                                 
         DC    AL1(006,15,18),C'START/END DATES'                                
         DC    AL1(008,09,02),C'TIME ZONE'                                      
         DC    AL1(009,11,04),C'SPOT LENGTH'                                    
         DC    AL1(010,08,05),C'DAY PART'                                       
         DC    AL1(011,11,02),C'AFFILIATION'                                    
         DC    AL1(012,11,08),C'REPORT DEMO'                                    
         DC    AL1(013,09,10),C'BASE DATE'                                      
         DC    AL1(014,14,02),C'PRJCTN FORMULA'                                 
         DC    AL1(015,14,02),C'RATING SERVICE'                                 
         DC    AL1(16,12,02),C'PROGRAM TYPE'                                    
         DC    AL1(017,08,02),C'OPTION#1'                                       
         DC    AL1(018,08,02),C'OPTION#2'                                       
         DC    AL1(019,08,02),C'OPTION#3'                                       
         DC    AL1(020,08,02),C'OPTION#4'                                       
         DC    AL1(021,08,02),C'OPTION#5'                                       
         DC    AL1(022,08,02),C'OPTION#6'                                       
         DC    AL1(23,15,08),C'TARGET UNIVERSE'                                 
         DC    AL1(24,15,02),C'DAYPART CONTROL'                                 
         DC    AL1(25,10,08),C'DEMO GROUP'                                      
         DC    AL1(26,15,05),C'REQUEST PROGRAM'                                 
         DC    AL1(027,12,02),C'MARKET SORT?'                                   
         DC    AL1(028,07,13),C'CLT/OFF'                                        
         DC    AL1(127,01,02),C' '                                              
         DC    AL1(129,24,00),C'                        '                       
         DC    AL1(131,18,00),C'Y=SUPPRESS WEIGHTS'                             
         DC    AL1(133,23,00),C'Y=SUPPRESS SPOT LENGTHS'                        
         DC    AL1(135,14,00),C'Y=TARGETS ONLY'                                 
         DC    AL1(137,03,00),C'1-9'                                            
         DC    AL1(139,06,00),C'MMM/YY'                                         
         DC    AL1(141,13,00),C'Y=MARKET SORT'                                  
         DC    AL1(143,09,00),C'N,S,K,F,R'                                      
*                                                                               
TWATBLX  DC    X'0000'                                                          
         EJECT                                                                  
*CPREQSAVE                                                                      
       ++INCLUDE CPREQSAVE                                                      
*CPREQTEMP                                                                      
       ++INCLUDE CPREQTEMP                                                      
*CPREQFF                                                                        
       ++INCLUDE CPREQFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDFLDIND                                                       
         SPACE 2                                                                
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023CPREQ01   05/01/02'                                      
         END                                                                    
