*          DATA SET SPREQ01    AT LEVEL 186 AS OF 02/26/20                      
*PHASE T20801A                                                                  
*INCLUDE MEDGET                                                                 
         TITLE 'SPREQ01 - REQUEST - VALIDATE DEFN AND BUILD SCREEN'             
* ------------------------------------------------------------------- *         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* AKAT CUSTENH3341 10/28/16 ALLOW MEDIA N FOR DZ REPORT               *         
* AKAT SPSUG-85    07/08/16 ALLOW AGENCY OU TO REQUEST GT REPORT      *         
* AKAT SPSUG-86    07/08/16 ALLOW AGENCY OO TO REQUEST GT REPORT      *         
* AKAT CSD-477     07/08/16 ALLOW AGENCY UB TO REQUEST GT REPORT      *         
* ------------------------------------------------------------------- *         
T20801   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 000,T20801,RR=R9                                                 
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     R9,0(R1)                                                         
         USING REQTEMP,R9                    R9=A(W/S)                          
         L     R3,ASAVE                                                         
         USING TWAD,R3                    R3=A(TWA)                             
         LA    RA,T20801+4095                                                   
         LA    RA,1(RA)                                                         
         USING T20801+4096,RA      ** NOTE USE OF SECOND BASE REG **            
         EJECT                                                                  
*        INITIALISE SPOT KEY & REQUEST RECORD                                   
*                                                                               
         XC    KEY,KEY                       INITIALISE KEY                     
         XC    REQNUM(18),REQNUM                                                
*                                                                               
         XC    RHDR,RHDR                     INITIALISE REQ REC                 
         MVI   RNUM,C' '                                                        
         MVC   RNUM+1(159),RNUM                                                 
         SPACE 2                                                                
         LR    R2,R3                         R2=A(TWA SCREEN FF)                
         USING T208FFD,R2                                                       
         MVI   FIND,0                                                           
         MVC   FERN,=AL2(FF)                                                    
         MVI   USRIDF,0                                                         
         XC    CLISAVE(6),CLISAVE                                               
         XC    ESTDATES,ESTDATES                                                
         XC    CLIPROF,CLIPROF                                                  
         MVI   CLIOFFC,0                                                        
         XC    CLIEXTRA,CLIEXTRA                                                
         XC    SAVEREP,SAVEREP                                                  
         XC    MYFLAG,MYFLAG                                                    
         SPACE 2                                                                
*        VALIDATE REQUESTOR NAME & SET FIND X'01' = NAME INPUT                  
*                                                                               
VALNAME  CLI   BVRNAMEH+5,0                                                     
         BE    VALNAMA                                                          
*                                                                               
         CLI   BVRNAME,C'='        ACCEPT "=NNN"                                
         BNE   VALEQUX                                                          
         CLI   BVRNAMEH+5,12                                                    
         BH    INVNAME                                                          
         ZIC   R1,BVRNAMEH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   RNAME(0),BVRNAME                                                 
         OI    FIND,X'01'                                                       
         B     VALNAMA                                                          
VALEQUX  EQU   *                                                                
*                                                                               
         CLC   BVRNAME(4),=C'MENU '          CHECK FOR MENU REQUEST             
         BNE   *+14                                                             
         MVC   REQNDX1,=X'FFFC'              SET MENU SCREEN ID                 
         B     VALDEF1                                                          
         CLI   DDS,0               DDS TERMINALS CAN HAVE KEYWORDS              
         BNE   VALNAM1                                                          
VALNAM   CLI   BVRNAMEH+5,12       USER TERM HAS REQUESTOR NAME ONLY            
         BH    INVNAME                                                          
         ZIC   R7,BVRNAMEH+5       MAX LEN IS 12 CHRS                           
         BCTR  R7,0                                                             
         EX    R7,*+8                                                           
         B     *+10                                                             
         MVC   RNAME(0),BVRNAME                                                 
         OI    FIND,X'01'          SET REQUESTOR NAME INPUT                     
         B     VALMC                                                            
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
         LA    R4,SPTREC                                                        
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
         BNE   *+14                                                             
         MVC   DUB(2),2(RE)        SAVE USER ID NUM                             
         B     VALNAM3B                                                         
         CLI   0(RE),X'06'                                                      
         BNE   *+10                                                             
         MVC   DUB+6(2),2(RE)      SAVE AGY CODE                                
VALNAM3B IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     VALNAM3A                                                         
VALNAM3C CLC   SYSNUMOV,CTSYSNUM-CTSYSD(RE)                                     
         BNE   VALNAM3B                                                         
         MVC   DUB+2(1),CTSYSSE-CTSYSD(RE)                                      
         MVC   DUB+3(1),CTSYSAGB-CTSYSD(RE)                                     
*        MVC   DUB+4(2),CTSYSAGA-CTSYSD(RE)                                     
         XC    DUB+4(2),DUB+4                                                   
*                                                                               
VALNAM3D CLC   SYSNUM,DUB+2        MUST BE SAME SYSTEM AS CONNECT               
         BNE   INVNAME                                                          
         CLI   USRIDF,0                                                         
         BNE   INVNAME             ONLY ONE U= OR A=                            
         OI    USRIDF,X'80'                                                     
         MVC   AGY,DUB+6           SET NEW AGY CODE                             
         MVC   AGYB,DUB+3                                                       
         MVC   USRID,DUB           SET NEW USERID NUMBER                        
         B     VALNAM9                                                          
*                                                                               
VALNAM4  CLI   12(R7),C'T'         T=Y TO DEFINE UNKNOWN REQUEST                
         BNE   *+12                                                             
         OI    FIND,X'04'                                                       
         B     VALNAM4A                                                         
         CLC   12(3,R7),=C'AGY'        ACCEPT OLD SYNTAX                        
         BE    VALNAM5A                                                         
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
VALNAM5  CLI   12(R7),C'A'         A=X TO DEFINE AGY X (OR 00 FOR ALL)          
         BE    VALNAM5A                                                         
         CLI   12(R7),C'D'         D=X TO DEFINE AGY X AND DDS OFFICE           
         BNE   VALNAM6                                                          
         MVC   REQOFFC,=C'DDS*'                                                 
VALNAM5A MVC   AGY(2),22(R7)                                                    
         CLC   AGY,=C'00'                                                       
         BNE   *+8                                                              
         MVI   AGY,C'*'                                                         
*                                                                               
         CLI   USRIDF,0                                                         
         BNE   INVNAME             ONLY ONE A= OR U=                            
         OI    USRIDF,X'40'                                                     
         B     VALNAM9                                                          
*                                                                               
*  GROSS=BF/999.9999 SAME AS VALIDATION IN OPTRTN IN SPREQ04                    
*                                                                               
VALNAM6  CLC   12(5,R7),=C'GROSS'    ACCEPT GROSS= AS REQUESTOR                 
         BE    VALNAM                                                           
         CLI   12(R7),C'G'           ACCEPT G=                                  
         BE    VALNAM                                                           
         CLC   12(4,R7),=C'SIZE='     ACCEPT SIZE=AS REQUESTOR NAME             
         BE    VALNAM              REEDIT                                       
         B     INVNAME                                                          
         EJECT                                                                  
*                                                                               
VALNAM9  LA    R7,32(R7)           BUMP TO NEXT FIRLD                           
         BCT   R5,VALNAM2                                                       
         B     VALMC                                                            
INVNAME  MVC   FERN,=AL2(FLDINV)                                                
         B     *+10                                                             
MISNAME  MVC   FERN,=AL2(FLDMIS)                                                
         LA    R7,BVRNAMEH                                                      
         ST    R7,FADR                                                          
         B     EXIT                                                             
*                                                                               
VALMC    CLC   AGY,=C'MC'        SEE IF MCE                                     
         BNE   VALMC2           NO THEN NO $ IN RNAME                           
         CLI   RNAME,C'$'                                                       
         BNE   VALNAMA                                                          
         LA    R5,4                                                             
         LA    R6,MCETAB                                                        
VALMC1   CLC   RNAME+1(1),0(R6)                                                 
         BE    VALNAMA                                                          
         LA    R6,1(R6)                                                         
         BCT   R5,VALMC1                                                        
         B     INVNAME                                                          
*                                                                               
VALMC2   CLI   RNAME,C'$'                                                       
         BE    INVNAME                                                          
         B     VALNAMA                                                          
*                                                                               
MCETAB   DC    C'WTSM'                                                          
         DS    0H                                                               
*                                                                               
*                                                                               
         DROP  R4                                                               
VALNAMA  MVC   RAGY,AGY                                                         
         MVC   KEY+1(1),AGYB                                                    
         SPACE 2                                                                
*        VALIDATE REQUEST NUMBER & ACTION                                       
*                                                                               
VALNUM   CLI   BVRNUMH+5,0                                                      
         BE    MISNUM                                                           
***********************   READ AGY HEADER TO SEE IF CANADIAN AGENCY             
         CLC   RAGY,=C'*0'         IF 'ALL' AGENCIES, SKIP                      
         BE    VALNM0                                                           
         MVC   KEYS(13),KEY                                                     
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'                                                        
         MVC   KEY+1(2),RAGY                                                    
         GOTO1 ARSPT                                                            
         CLC   FERN,=AL2(FE)                                                    
         BH    *+6                                                              
         DC    H'0'                                                             
         LA    R1,SPTREC                                                        
         USING AGYHDR,R1                                                        
         MVC   CANAGY,AGYPROF+7       C=CANADIAN                                
         MVC   KEY(13),KEYS           RESET KEY                                 
         DROP  R1                                                               
*                                                                               
***???   BRAS  RE,SORN  IF ITS NETWORK ***DON'T THINK IT'S NEEDED               
*                                                                               
* SECURITY FOR WESTERN SPOT REQUESTS                                            
* CHECK HERE TO PREVENT REQUESTS BY CODE (SAL) INSTEAD OF NO (D4)               
* FROM SLIPPING THROUGH - LATER CHECK ACTUALLY CHANGES IFLD                     
VALNM0   BRAS  RE,CHKID            WESTERN SPOT REQUESTS?                       
         BNE   INVNUM                                                           
*                                                                               
         MVC   IFLDH,BVRNUMH       COPY FIELD INTO IFLD                         
         MVI   IFLD,C' '                                                        
         MVC   IFLD+1(L'IFLD-1),IFLD                                            
         SR    R1,R1                                                            
         IC    R1,IFLDH+5                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   IFLD(0),BVRNUM                                                   
*                                                                               
         CLC   =C'MY',IFLD                                                      
         BE    DDSCHK                                                           
*        CLC   =C'DZ',IFLD                                                      
*        BE    DDSCHK                                                           
         CLC   =C'7U',IFLD                                                      
         BE    DDSCHK                                                           
         CLC   =C'07',IFLD         DDS ONLY REQUEST                             
         BNE   SKIPSAL                                                          
DDSCHK   CLI   DDS,1                                                            
         BNE   INVNUM                                                           
SKIPSAL  CLC   =C'IMR',IFLD        FUDGE FOR IMR/I2/N2 REPORT                   
         BNE   CHKLB                                                            
         BRAS  RE,SORN             IF ITS NETWORK                               
         BNE   VALNUM0                                                          
         MVI   IFLD,C'#'           FUDGE IT TO N2                               
         MVC   IFLD+1(2),=C'N2'                                                 
         B     VALNUM1                                                          
*                                                                               
CHKLB    CLC   =C'LB',IFLD         CHECK FOR LB REPORT                          
         BNE   CHKWB                                                            
         CLI   IFLDH+5,2           HAS TO BE EXACT MATCH                        
         BE    *+12                                                             
         CLI   IFLD+2,C','         CHECK IF REQUIRING SOME ACTION               
         BNE   VALNUM0             NO, WILL CATCH ERROR LATER                   
         EX    R1,*+8              CONVERT TWO CHR ID TO #XX FORMAT             
         B     *+10                                                             
         MVC   IFLD+1(0),BVRNUM                                                 
         MVI   IFLD,C'#'           FUDGE IT TO B1                               
         MVC   IFLD+1(2),=C'B1'                                                 
         BRAS  RE,SORN             IF ITS NETWORK                               
         BNE   *+10                                                             
         MVC   IFLD+1(2),=C'BU'    FUDGE IT TO BU                               
         IC    R1,IFLDH+5                                                       
         LA    R1,1(R1)                                                         
         STC   R1,IFLDH+5                                                       
         OI    MYFLAG,LBREPT       TURN ON LB FLAG                              
         B     VALNUM1                                                          
*                                                                               
CHKWB    CLC   =C'WB',IFLD         CHECK FOR WB REPORT                          
         BNE   VALNUM0                                                          
         CLI   IFLDH+5,2           HAS TO BE EXACT MATCH                        
         BE    *+12                                                             
         CLI   IFLD+2,C','         CHECK IF REQUIRING SOME ACTION               
         BNE   VALNUM0             NO, WILL CATCH ERROR LATER                   
         EX    R1,*+8              CONVERT TWO CHR ID TO #XX FORMAT             
         B     *+10                                                             
         MVC   IFLD+1(0),BVRNUM                                                 
         MVI   IFLD,C'#'           FUDGE IT TO ZB  WARNER BROS. FILE            
         MVC   IFLD+1(2),=C'ZB'                                                 
         IC    R1,IFLDH+5                                                       
         LA    R1,1(R1)                                                         
         STC   R1,IFLDH+5                                                       
         B     VALNUM1                                                          
*                                                                               
***NVN   CLC   =C'NN',IFLD         FUDGE FOR NN/NV REPORT                       
***      BNE   VALNUM0                                                          
***      CLI   IFLDH+5,2           HAS TO BE EXACT MATCH                        
***      BE    *+12                                                             
***      CLI   IFLD+2,C','         CHECK IF REQUIRING SOME ACTION               
***      BNE   VALNUM0             NO, WILL CATCH ERROR LATER                   
***      EX    R1,*+8              CONVERT TWO CHR ID TO #XX FORMAT             
***      B     *+10                                                             
***      MVC   IFLD+1(0),BVRNUM                                                 
***      BRAS  RE,SORN             ONLY FOR NETWORK                             
***      BNE   INVNUM              ERROR FOR SPOT                               
***      MVI   IFLD,C'#'           FUDGE IT TO NV                               
***      MVC   IFLD+1(2),=C'NV'                                                 
***      IC    R1,IFLDH+5                                                       
***      LA    R1,1(R1)                                                         
***      STC   R1,IFLDH+5                                                       
***      OI    MYFLAG,NNREPT       TURN ON NN FLAG                              
***      B     VALNUM1                                                          
*                                                                               
VALNUM0  CLI   IFLDH+5,2                                                        
         BL    INVNUM                                                           
         BE    *+12                                                             
         CLI   IFLD+2,C','                                                      
         BNE   VALNUM1                                                          
         EX    R1,*+8              CONVERT TWO CHR ID TO #XX FORMAT             
         B     *+10                                                             
         MVC   IFLD+1(0),BVRNUM                                                 
         CLC   BVRNUM(2),=C'I2'    IF IT'S I2                                   
         BNE   VALN10                                                           
         BRAS  RE,SORN             AND NETWORK                                  
         BNE   VALN20                                                           
         MVC   IFLD+1(2),=C'N2'    FUDGE IT TO N2                               
VALN10   CLC   BVRNUM(2),=C'D1'    IF IT'S D1                                   
         BNE   VALN12                                                           
         BRAS  RE,SORN             AND NETWORK                                  
         BE    INVNUM              IT'S INVALID                                 
*                                                                               
VALN12   CLC   BVRNUM(2),=C'K5'    ...IF IT'S K5                                
         BNE   VALN14                                                           
         BRAS  RE,SORN             ... MUST BE NETWORK                          
         BNE   INVNUM                                                           
         B     VALN20                                                           
*                                                                               
VALN14   CLC   BVRNUM(2),=C'K4'    ...IF IT'S K4                                
         BE    VALN14A                                                          
         CLC   BVRNUM(2),=C'KL'    ...IF IT'S KL                                
         BNE   VALN15                                                           
VALN14A  BRAS  RE,SORN             ... MUST NOT BE NETWORK                      
         BE    INVNUM                                                           
         B     VALN20                                                           
*                                                                               
VALN15   CLC   =C'CM',BVRNUM       CM ONLY CANADA                               
         BNE   VALN20                                                           
         CLI   CANAGY,C'C'                                                      
         BNE   INVNUM                                                           
*>>> TEMP DISABLE CLIENT REQUEST OF CM AS HAD MANY PROBLEMS                     
*****    CLI   DDS,1                                                            
*****    BE    *+14                                                             
*****    MVC   FERN,=AL2(1187)                                                  
*****    B     INVNUM2                                                          
*>>>                                                                            
*                                                                               
VALN20   DS    0H                                                               
         BRAS  RE,CHKID            WESTERN SPOT REQUESTS?                       
         BNE   INVNUM                                                           
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
         TM    MYFLAG,LBREPT       FOR LB REPORT CHANGE REPORT NAME             
         BO    LBMSG                                                            
***      TM    MYFLAG,NNREPT       FOR NN REPORT CHANGE REPORT NAME             
***      BO    NNMSG                                                            
         CLI   REQNUM,61                                                        
         BE    VALNUM1F                                                         
         CLI   REQNUM,62                                                        
         BE    VALNUM1F                                                         
         B     VALNUM1G                                                         
*                                                                               
LBMSG    MVC   BVRRNAM(22),=C'LIVE UPDATIVE BILLING '                           
         B     VALNUM1G                                                         
*                                                                               
***SG    MVC   BVRRNAM(2),=C'NN'                                                
***      B     VALNUM1G                                                         
*                                                                               
VALNUM1F MVC   BVRRNAM(3),=C'60*'                                               
*                                                                               
VALNUM1G FOUT  BVRRNAMH                                                         
         B     VALNUM2                                                          
*                                                                               
VALNUM2  MVC   RHDR+10(1),REQNUM   R7=A(REQTBL ENTRY)                           
         MVI   REQACTN,C'N'        SET DEFAULT VALUES                           
         MVI   REQOPTN,C'S'                                                     
         MVC   REQINCR,=H'1'                                                    
         CLI   IFLDH+5,3           ONLY NUM INPUT                               
         BE    VALNUM3A             YES                                         
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
VALNUM3A CLI   AGY,C'*'            AGY MUST BE SPECIFIC FOR A OR N              
         BNE   *+24                                                             
         CLC   =C'*1',AGY          ALLOW *1 AND *B, SPECIAL DDS                 
         BE    *+14                                                             
         CLC   =C'*B',AGY                                                       
         BNE   INVNAME                                                          
*                                                                               
         TM    USRIDF,X'40'                                                     
         BO    INVNAME                                                          
         B     VALNUMX                                                          
*                                                                               
VALNUM4  CLI   IFLDH+5,5                                                        
         BE    VALNUMX                                                          
***      TM    MYFLAG,NNREPT       WAS IT NN REPORT                             
***      BZ    VALNUMX             USE DEFAULT OPTION                           
***      MVC   RNUM,=C'NN'         YES,SWITCH BACK TO NN                        
***      B     VALNUMX                                                          
VALNUM5  CLI   IFLD+5,C','                                                      
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
         B     CHKOK                                                            
                                                                                
*************************************************************                   
CHKOK    DS    0H                  CHECK IF UPDATE ALLOWED ON THIS SYS          
         CLC   =C'AC',RNUM                                                      
         BE    CHUPDT                                                           
         CLC   =C'B1',RNUM                                                      
         BE    CHUPDT                                                           
         CLC   =C'BU',RNUM                                                      
         BE    CHUPDT                                                           
         CLC   =C'C2',RNUM                                                      
         BE    CHUPDT                                                           
         CLC   =C'CD',RNUM                                                      
         BE    CHUPDT                                                           
         CLC   =C'CM',RNUM                                                      
         BE    CHUPDT                                                           
         CLC   =C'CW',RNUM                                                      
         BE    CHUPDT                                                           
         CLC   =C'DL',RNUM                                                      
         BE    CHUPDT                                                           
         CLC   =C'DX',RNUM                                                      
         BE    CHUPDT                                                           
         CLC   =C'K1',RNUM                                                      
         BE    CHUPDT                                                           
         CLC   =C'K2',RNUM                                                      
         BE    CHUPDT                                                           
         CLC   =C'K3',RNUM                                                      
         BE    CHUPDT                                                           
         CLC   =C'ML',RNUM                                                      
         BE    CHUPDT                                                           
         CLC   =C'MV',RNUM                                                      
         BE    CHUPDT                                                           
         CLC   =C'MY',RNUM                                                      
         BE    CHUPDT                                                           
         CLC   =C'SC',RNUM                                                      
         BE    CHUPDT                                                           
         CLC   =C'SX',RNUM                                                      
         BNE   VAL49                                                            
CHUPDT   L     RF,APARM                                                         
         L     RF,16(RF)           GET A(COMFACS)                               
         ICM   RF,15,(CXTRAINF-COMFACSD)(RF)                                    
         BZ    UPDOK               ? I GUESS ALLOW IT ?                         
         USING XTRAINFD,RF                                                      
         TM    XIFLAG1,XIROSYS+XIROMODE+XIWRONGF                                
         BZ    UPDOK                                                            
         DROP  RF                                                               
         MVC   FERN,=AL2(1250)                                                  
         B     INVNUM2                                                          
*                                                                               
UPDOK    DS    0H                                                               
*                                                                               
VAL49    CLC   =C'49',RNUM         IF 49 REQUEST                                
         BNE   SKP49CHK                                                         
         CLC   AGY,=C'WI'          INVALID FOR WI (WILA)                        
         BE    INVNUM                                                           
         CLC   AGY,=C'WR'                      WR (WRLA)                        
         BE    INVNUM                                                           
*                               WT - NOW WITO - GIVEN ACCESS 3/17/08            
         B     VALMEDIA                                                         
*                                                                               
SKP49CHK CLC   =C'GM',RNUM         IF GM REQUEST                                
         BNE   VALBGL                                                           
         CLC   AGY,=C'GZ'          ONLY GMMRE                                   
         BE    VALMED                                                           
         CLC   AGY,=C'UB'          AND CARAT                                    
         BE    VALMED                                                           
         CLC   AGY,=C'SJ'          OR SJ FOR TESTING                            
         BE    VALMED                                                           
         B     INVNUM                                                           
*                                                                               
VALBGL   CLC   RNUM(2),=C'96'      BRL FOR COMPTON ONLY                         
         BNE   VALXAA                                                           
         CLC   AGY,=C'CO'                                                       
         BE    VALMED                                                           
         CLC   AGY,=C'CW'            OR CUNNINGHAM AND WALSH                    
         BE    VALMED                                                           
         CLC   AGY,=C'DF'          OR DANCER                                    
         BE    VALMED                                                           
         B     INVNUM                                                           
*                                                                               
VALXAA   CLC   RNUM,=C'J1'         J1 FOR JWT ONLY                              
         BNE   VALXAAA                                                          
         CLC   AGY,=C'JW'                                                       
         BE    VALMED                                                           
         CLC   AGY,=C'H7'                                                       
         BE    VALMED                                                           
         CLC   AGY,=C'FR'                                                       
         BE    VALMED                                                           
         B     INVNUM                                                           
*                                                                               
VALXAAA  CLC   RNUM,=C'CW'         CW                                           
         BNE   VALXA                                                            
         CLC   AGY,=C'WI'                                                       
         BE    VALMED                                                           
         CLC   AGY,=C'WJ'                                                       
         BE    VALMED                                                           
         B     INVNUM                                                           
*                                                                               
VALXA    CLC   RNUM,=C'C0'         C0 ONLY FOR YNR                              
         BNE   VALPH                                                            
         CLC   AGY,=C'YN'                                                       
         BE    VALMED                                                           
         CLC   AGY,=C'H7'                                                       
         BE    VALMED                                                           
         B     INVNUM                                                           
*                                                                               
VALPH    CLC   RNUM,=C'PH'         PH ONLY FOR H9                               
         BNE   VALSE                                                            
         CLC   AGY,=C'H9'                                                       
         BE    VALMED                                                           
         B     INVNUM                                                           
*                                                                               
VALSE    CLC   RNUM,=C'SE'         SE ONLY FOR H9 OR H7                         
         BNE   VALCI                                                            
         CLC   AGY,=C'H9'                                                       
         BE    VALMED                                                           
         CLC   AGY,=C'H7'          MINDSHARE                                    
         BE    VALMED                                                           
         B     INVNUM                                                           
*                                                                               
VALCI    CLC   RNUM,=C'CI'         CI ONLY FOR H9                               
         BNE   VALLT                                                            
         CLC   AGY,=C'H9'                                                       
         BE    VALMED                                                           
*        CLC   AGY,=C'DU'          OR MVNYNR (DU)                               
*        BE    VALMED                                                           
         B     INVNUM                                                           
*                                                                               
VALLT    CLC   RNUM,=C'LT'         LT ONLY FOR U# (AND HDTO FOR NOW)            
         BNE   VALLO                                                            
         CLC   AGY,=C'U#'                                                       
         BE    VALMED                                                           
         CLC   AGY,=C'HD'     HDTO - TESTING                                    
         BE    VALMED                                                           
         B     INVNUM                                                           
*                                                                               
VALLO    CLC   RNUM,=C'LO'        LO ONLY FOR MC                                
         BNE   VALJW                                                            
         CLC   AGY,=C'MC'                                                       
         BE    VALMED                                                           
         B     INVNUM                                                           
*                                                                               
VALJW    CLC   RNUM,=C'JW'        JW ONLY CERTAIN AGYS                          
         BNE   VALTD                                                            
         CLC   AGY,=C'FR'                                                       
         BE    VALMED                                                           
         CLC   AGY,=C'H7'                                                       
         BE    VALMED                                                           
         CLC   AGY,=C'JS'                                                       
         BE    VALMED                                                           
         CLC   AGY,=C'SJ'                                                       
         BE    VALMED                                                           
         CLC   AGY,=C'H0'          MSHTOA                                       
         BE    VALMED                                                           
         B     INVNUM                                                           
*                                                                               
VALTD    CLC   RNUM,=C'TD'        TD ONLY FOR M2                                
         BNE   VAL7U                                                            
         CLC   AGY,=C'M2'                                                       
         BE    VALMED                                                           
         B     INVNUM                                                           
*                                                                               
VAL7U    CLC   RNUM,=C'7U'        07 AND 7U                                     
         BE    *+14                                                             
         CLC   RNUM,=C'07'                                                      
         BNE   VALXB                                                            
*                                                                               
         BRAS  RE,CHKACC          CHK ACCESS TO ACC                             
         BE    VALMED                                                           
         B     INVACC                                                           
*                                                                               
VALXB    DS    0H                                                               
         CLC   RNUM,=C'GT'                                                      
         BNE   VALXC                                                            
         CLC   AGY,=C'DR'                                                       
         BE    VALMED                                                           
         CLC   AGY,=C'TC'                                                       
         BE    VALMED                                                           
         CLC   AGY,=C'H9'                                                       
         BE    VALMED                                                           
         CLC   AGY,=C'O0'                                                       
         BE    VALMED                                                           
         CLC   AGY,=C'HY'                                                       
         BE    VALMED                                                           
         CLC   AGY,=C'UB'                                                       
         BE    VALMED                                                           
         CLC   AGY,=C'OO'                                                       
         BE    VALMED                                                           
         CLC   AGY,=C'OU'                                                       
         BE    VALMED                                                           
         CLC   AGY,=C'T1'          TCH1 (FOR TESTING)                           
         BE    VALMED                                                           
         B     INVNUM                                                           
*                                                                               
VALXC    DS    0H                                                               
         CLC   RNUM,=C'ZB'        WARNER BROS. INTERFACE                        
         BNE   VALXC1             (REALLY THE WB)                               
         CLC   AGY,=C'OO'         OMG                                           
         BE    VALMED                                                           
         CLC   AGY,=C'OU'         OR OMDTOA                                     
         BE    VALMED                                                           
         CLC   AGY,=C'SJ'         SJR - TESTING                                 
         BE    VALMED                                                           
         B     INVNUM                                                           
VALXC1   DS    0H                                                               
         CLC   RNUM,=C'IN'        NISSAN INTERFACE                              
         BNE   VALXC2                                                           
         CLC   AGY,=C'OU'         FOR OMDTOA                                    
         BE    VALMED                                                           
         CLC   AGY,=C'OO'         OR OMDUSEC (USA)                              
         BE    VALMED                                                           
         B     INVNUM                                                           
*                                                                               
VALXC2   DS    0H                                                               
         CLC   RNUM,=C'CH'        CHOICE HOTELS                                 
         BNE   VALXC3                                                           
         CLC   AGY,=C'FM'         FMNY                                          
         BE    VALMED                                                           
         B     INVNUM                                                           
*                                                                               
VALXC3   DS    0H                                                               
         CLC   RNUM,=C'PZ'        PFIZER INTERFACE                              
         BNE   VALXC4                                                           
         CLC   AGY,=C'UB'         CARNY                                         
         BE    VALMED                                                           
         B     INVNUM                                                           
*                                                                               
VALXC4   DS    0H                                                               
         CLC   RNUM,=C'AI'        AT&T INTERFACE                                
         BNE   VALXC6                                                           
         CLC   AGY,=C'H7'         GROUPM                                        
         BE    VALMED                                                           
         B     INVNUM                                                           
*                                                                               
VALXC6   CLC   RNUM(2),=C'BU'      FOR NETWORK ONLY                             
         BE    VALMEDIA                                                         
         CLC   RNUM(2),=C'DU'      FOR NETWORK ONLY                             
         BE    VALMEDIA                                                         
         CLC   RNUM(2),=C'B1'      FOR SPOT ONLY                                
         BE    VALMEDIA                                                         
         CLC   RNUM(2),=C'7U'      FOR NETWORK ONLY                             
         BE    VALMEDIA                                                         
         CLC   RNUM(2),=C'07'      FOR SPOT ONLY                                
         BE    VALMEDIA                                                         
         CLC   RNUM(2),=C'SC'      FOR SPOT ONLY                                
         BE    VALMEDIA                                                         
         CLC   RNUM(2),=C'NV'      FOR SPOT ONLY                                
         BE    VALMEDIA                                                         
         CLC   RNUM(2),=C'Z5'      FOR SPOT ONLY                                
         BE    VALMEDIA                                                         
         CLC   RNUM(2),=C'Z7'      FOR NETWORK ONLY                             
         BE    VALMEDIA                                                         
         B     VALXD                                                            
VALMEDIA BRAS  RE,SORN             CHK SPOT OR NET                              
         BE    VALNET                                                           
         CLC   RNUM(2),=C'SC'      FOR SPOT                                     
         BE    VALXD                                                            
         CLC   RNUM(2),=C'NV'      FOR SPOT                                     
         BE    VALXD                                                            
         CLC   RNUM(2),=C'Z5'      FOR SPOT                                     
         BE    VALXD                                                            
         CLC   RNUM(2),=C'B1'      FOR SPOT                                     
         BE    VALXD                                                            
         CLC   RNUM(2),=C'07'      FOR SPOT                                     
         BE    VALXD                                                            
         CLC   RNUM(2),=C'49'      FOR SPOT                                     
         BE    VALXD                                                            
         B     INVACTN                                                          
VALNET   CLC   RNUM(2),=C'BU'      FOR NET                                      
         BE    VALXD                                                            
         CLC   RNUM(2),=C'DU'      FOR NET                                      
         BE    VALXD                                                            
         CLC   RNUM(2),=C'7U'      FOR NET                                      
         BE    VALXD                                                            
         CLC   RNUM(2),=C'Z7'      FOR NET                                      
         BE    VALXD                                                            
         CLC   RNUM(2),=C'Z5'      NOT FOR NET                                  
         BE    INVZ5               MY OWN ERROR                                 
         B     INVACTN                                                          
*                                                                               
VALXD    CLC   RNUM(2),=C'XD'      XD(151) FOR DF ONLY                          
         BNE   VALSX                                                            
         CLC   AGY,=C'DF'                                                       
         BE    VALMED                                                           
         B     INVNUM                                                           
         SPACE                                                                  
*                                                                               
VALSX    CLC   =C'SX',RNUM                                                      
         BNE   VALSHZ                                                           
         BRAS  RE,CHKSX                                                         
         BE    VALMED                                                           
         B     INVNUM                                                           
*                                                                               
VALSHZ   CLC   RNUM(2),=C'XS'      SCHILTZ EXTRACT                              
         BE    VALJWT                                                           
         CLC   RNUM(2),=C'A7'      MKT ACTIVITY ANAL                            
         BNE   VALBJ                                                            
VALJWT   CLC   AGY,=C'JW'     FOR JWT ONLY                                      
         BE    VALMED                                                           
         CLC   AGY,=C'RP'     FOR RPLA                                          
         BE    VALMED                                                           
         CLC   AGY,=C'H7'     FOR MSNY                                          
         BE    VALMED                                                           
         CLC   AGY,=C'FR'     FOR FR                                            
         BE    VALMED                                                           
         B     INVNUM                                                           
*                                                                               
VALBJ    CLC   RNUM(2),=C'XJ'       XJ EXTRACT                                  
         BNE   VALCHK                                                           
         CLC   AGY,=C'BJ'    FOR BJ                                             
         BE    VALMED                                                           
         CLC   AGY,=C'US'    FOR US                                             
         BE    VALMED                                                           
         B     INVNUM                                                           
*                                                                               
VALCHK   CLC   RNUM,=C'30'                                                      
         BL    VALCOKE                                                          
         CLC   RNUM,=C'32'                                                      
         BH    VALCOKE                                                          
         B     INVNUM                                                           
*                                                                               
VALCOKE  CLC   AGY,=C'ME'          X1 ONLY FOR COKE CANADA                      
         BE    VALACC                                                           
         CLC   AGY,=C'MT'                                                       
         BE    VALACC                                                           
         CLC   AGY,=C'YR'                                                       
         BE    VALACC                                                           
         CLC   RNUM(2),=C'X1'                                                   
         BE    INVNUM                                                           
*                                                                               
VALACC   DS    0H                                                               
         CLC   AGY,=C'PE'                                                       
         BE    VALMKTA                                                          
         CLC   AGY,=C'GA'                                                       
         BE    VALMKTA                                                          
         CLC   AGY,=C'SJ'     ALSO ACCEPT SJR                                   
         BE    VALMKTA                                                          
         CLC   AGY,=C'BD'                                                       
         BE    VALMKTA                                                          
         CLC   AGY,=C'TR'         TRACY-LOCKE                                   
         BE    VALMKTA                                                          
         CLC   RNUM(2),=C'XA'                                                   
         BE    INVNUM                                                           
         CLC   RNUM(2),=C'XB'                                                   
         BE    INVNUM                                                           
*                                                                               
VALMKTA  DS    0H                                                               
         CLI   6(R3),C'+'          MKT LIMIT ACCESS                             
         BNE   VALMED              NO                                           
         CLC   RNUM,=C'06'         DON'T ALLOW ANY REQ THAT SET MKT TO          
         BE    ACCINV              ALL                                          
         CLC   RNUM,=C'52'                                                      
         BE    ACCINV                                                           
         CLC   RNUM,=C'56'                                                      
         BE    ACCINV                                                           
         CLC   RNUM,=C'S1'                                                      
         BE    ACCINV                                                           
         CLC   RNUM,=C'M1'                                                      
         BE    ACCINV                                                           
         B     VALMED                                                           
*                                                                               
ACCINV   MVC   FERN,=AL2(ACCERR)                                                
         B     INVNUM2                                                          
*                                                                               
*                                                                               
MISNUM   MVC   FERN,=AL2(FLDMIS)                                                
         XC    BVRRNAM,BVRRNAM                                                  
         B     INVNUM2                                                          
INVNUM   XC    BVRRNAM,BVRRNAM                                                  
INVNUM1  MVC   FERN,=AL2(NUMINV)                   INV REQ NUM                  
INVNUM2  FOUT  BVRRNAMH                                                         
         LA    R7,BVRNUMH                                                       
         ST    R7,FADR                                                          
         B     EXIT                                                             
INVZ5    MVC   FERN,=AL2(Z5ERR)                                                 
         B     INVNUM2                                                          
INVACC   MVC   FERN,=AL2(1071)                     NO ACCESS TO ACC             
         B     INVNUM2                                                          
INVACTN  MVC   FERN,=AL2(ACTINV)                   INV ACTION                   
         B     INVNUM2                                                          
INVOPTN  MVC   FERN,=AL2(OPTINV)                   INV INPUT                    
         B     INVNUM2                                                          
INVINCR  EQU   INVOPTN                       INV INPUT                          
         EJECT                                                                  
*        VALIDATE MEDIA & SET FIND X'02' = MEDIA INPUT                          
*                                                                               
VALMED   CLI   BVRMEDH+5,0                                                      
         BE    VALDEST                                                          
         LA    R4,MEDTBL                                                        
VALMED1  CLI   0(R4),0                                                          
         BE    INVMED                                                           
         CLC   0(1,R4),BVRMED                                                   
         BE    VALMED2                                                          
         LA    R4,L'MEDTBL(R4)                                                  
         B     VALMED1                                                          
VALMED2  MVC   REQMED,0(R4)                  SAVE MEDIA VALUE                   
         MVC   RMED,0(R4)                    MEDIA TO REQ REC                   
         MVC   REQMED1,1(R4)                 SAVE MEDIA BIT-MASK                
         OC    KEY+1(1),2(R4)                MEDIA TO KEY                       
         MVC   BAGYMD,KEY+1                 SAVE AGY/MEDIA BINARY               
         OI    FIND,X'02'                                                       
         CLI   RMED,C'*'        MULTI-MEDIA REQUEST                             
         BE    VALMED4          SKIP MEDGET CALL                                
         CLI   RMED,C'A'                                                        
         BNE   VALMED3                                                          
         MVI   RMED,C' '                                                        
         B     VALMED4                                                          
*                                                                               
VALMED3  DS    0H                  USE MEDGET TO VALIDATE MEDIA                 
         GOTO1 =V(MEDGET),DMCB,(RMED,RAGY),DATAMGR,TEMP,RR=RELO                 
         CLI   DMCB+8,X'FF'                                                     
         BE    INVMED                                                           
*                                                                               
VALMED4  DS    0H                                                               
***************************************************                             
*                                                                               
CHKK1    DS    0H                                                               
         CLI   BVRMED+1,C'*'                                                    
         BNE   *+8                                                              
         MVI   RCARD2+19,C'Y'     SPECIAL TO LOOK AT ORIGINAL DATA              
*                                                                               
         CLC   RNUM,=C'C2'        C2 FOR N ONLY                                 
         BNE   *+12                                                             
         CLI   RMED,C'N'                                                        
         BNE   INVMED                                                           
*                                                                               
         CLC   RNUM,=C'DZ'        DZ FOR C ONLY                                 
         BNE   CHK1                                                             
         CLI   RMED,C'C'                                                        
         BE    CHK1                                                             
         CLI   RMED,C'N'                                                        
         BNE   INVMED                                                           
*                                                                               
CHK1     CLC   RNUM,=C'KL'        KL FOR N ONLY                                 
         BNE   CHK2                                                             
         CLI   RMED,C'C'                                                        
         BNE   INVMED                                                           
*                                                                               
CHK2     CLC   RNUM,=C'K2'        K2  NOT FOR N                                 
         BE    *+14                                                             
         CLC   RNUM,=C'K4'        K4  NOT FOR N                                 
         BNE   CHKSS                                                            
         CLI   RMED,C'N'                                                        
         BE    INVMED                                                           
*                                                                               
CHKSS    CLC   RNUM,=C'SS'        SS FOR R,T OR X                               
         BNE   CHKSC                                                            
         CLI   RMED,C'T'                                                        
         BE    CHKSC                                                            
         CLI   RMED,C'R'                                                        
         BE    CHKSC                                                            
         CLI   RMED,C'X'                                                        
         BNE   INVMED                                                           
*                                                                               
CHKSC    DS    0H                                                               
         CLI   CANAGY,C'C'        CANADIAN ?                                    
         BNE   CHKGT              NO                                            
         CLC   RNUM,=C'SC'        SC NOT FOR N OR T (CANADA ONLY)               
         BNE   CHKGT                                                            
         CLI   RMED,C'T'                                                        
         BE    INVMED                                                           
         CLI   RMED,C'N'                                                        
         BE    INVMED                                                           
*                                                                               
CHKGT    CLC   RNUM,=C'GT'        GT FOR R,T,N,X                                
         BNE   CHKK2                                                            
         CLI   RMED,C'N'                                                        
         BE    CHKK2                                                            
         BRAS  RE,SORN             SEE IF NETWORK                               
         BE    INVMED              MUST BE N                                    
         CLI   RMED,C'T'                                                        
         BE    CHKK2                                                            
         CLI   RMED,C'R'                                                        
         BE    CHKK2                                                            
         CLI   RMED,C'X'                                                        
         BE    CHKK2                                                            
         CLI   RMED,C'*'           ALL MEDIA                                    
         BE    CHKK2                                                            
         B     INVMED              OTHER MEDIA INVALID                          
*                                                                               
CHKK2    CLC   RNUM,=C'K1'         K1 INVALID FOR C                             
         BNE   VALMD4                                                           
         CLI   DDS,1               EXCEPT FOR DDS TERMINAL                      
         BE    VALMD4                                                           
         CLI   RMED,C'C'                                                        
         BE    INVMED                                                           
*                                                                               
VALMD4   CLC   RNUM,=C'SX'         IF CANADA AND SX                             
         BE    *+14                ONLY MEDIA C AND R                           
         CLC   RNUM,=C'CM'         IF CM ONLY MEDIA C                           
         BNE   VALMD4A                                                          
*                                                                               
         CLI   CANAGY,C'C'                                                      
         BNE   VALMD4A                                                          
         CLI   RMED,C'C'                                                        
         BE    VALMD4A                                                          
         CLC   RNUM,=C'CM'         ONLY MEDIA C FOR CM                          
         BE    INVMED                                                           
         CLI   RMED,C'R'                                                        
         BNE   INVMED                                                           
*                                                                               
VALMD4A  CLC   RNUM,=C'DN'                                                      
         BE    VALMED6                                                          
         CLC   RNUM,=C'N5'                                                      
         BE    VALMED6                                                          
         CLC   RNUM,=C'BU'                                                      
         BE    VALMED6                                                          
         CLC   RNUM,=C'DU'                                                      
         BE    VALMED6                                                          
         CLC   RNUM,=C'7U'                                                      
         BE    VALMED6                                                          
         CLC   RNUM,=C'L7'                                                      
         BE    VALMED6                                                          
         CLC   RNUM,=C'QC'                                                      
         BE    VALMED4A                                                         
         TM    MYFLAG,MVMFIQ       MVMFI ONLY REQUEST MED T                     
         BO    VALMED4A                                                         
         CLC   RNUM,=C'DL'                                                      
         BE    VALMED10                                                         
         B     VALMED8                                                          
*                                                                               
VALMED4A CLI   RMED,C'T'           REQ QC FOR T ONLY                            
         BE    VALDEST                                                          
         B     INVMED                                                           
*                                                                               
VALMED6  CLI   RMED,C'N'       REQ 44,DN,N5,L7,BU,DU,7U-NETWORK ONLY            
         BNE   INVMED                                                           
         B     VALDEST                                                          
*                                                                               
VALMED8  DS    0H                                                               
****     CLC   RAGY,=C'CD'        ..FOR AGENCY CD                               
****     BNE   VMED8D                                                           
****     CLC   RNUM,=C'D5'        ..FOR REPORT D5                               
****     BNE   VMED8D                                                           
****     CLI   RMED,C'C'          ..MEDIA C OR N IS VERBOTEN                    
****     BE    INVMED                                                           
****     CLI   RMED,C'N'                                                        
****     BE    INVMED                                                           
VMED8D   CLI   RMED,C'C'           COMBINED                                     
         BNE   VALDEST                                                          
         CLC   RNUM,=C'04'         NO BILLING FOR COMBINED                      
         BE    INVMED                                                           
         CLC   RNUM,=C'06'                                                      
         BE    INVMED                                                           
         CLC   RNUM,=C'NV'                                                      
         BE    INVMED                                                           
         CLC   RNUM,=C'B1'                                                      
         BE    INVMED                                                           
         CLC   RNUM,=C'D1'                                                      
         BE    INVMED                                                           
         B     VALDEST                                                          
*                                                                               
VALMED10 CLI   RMED,C'C'           DL NOT FOR T,C                               
         BE    INVMED                                                           
         CLI   RMED,C'T'                                                        
         BE    INVMED                                                           
         B     VALDEST                                                          
*                                                                               
MISMED   MVC   FERN,=AL2(FLDMIS)                   MISSING MEDIA                
         B     *+10                                                             
INVMED   MVC   FERN,=AL2(FLDINV)                   INVALID MEDIA                
         LA    R7,BVRMEDH                                                       
         ST    R7,FADR                                                          
         B     EXIT                                                             
*                                                                               
MEDTBL   DS    0CL3                          MEDIA VALUE,BIT-MASK,KEY           
         DC    C'A',B'00100000',X'01'                                           
         DC    C'T',B'10000000',X'01'                                           
         DC    C'R',B'01000000',X'02'                                           
         DC    C'N',B'10000000',X'03'                                           
         DC    C'X',B'01000000',X'04'                                           
         DC    C'C',B'10000000',X'08'                                           
         DC    C'*',B'00100000',X'01'                                           
MEDTBLX  DC    X'00'                                                            
         DS    0H                                                               
         EJECT                                                                  
*        VALIDATE DESTINATION ID NAME                                           
*                                                                               
VALDEST  EQU   *                                                                
         MVC   REQORIG,USRID        SET ORIGIN ID NUM                           
         TM    RFPSTAT,RFPINUSE                                                 
         BO    VALDESTX                                                         
         CLI   BVRDESTH+5,0        ANY INPUT?                                   
         BE    VALDESTX            GET OUT                                      
         SR    R1,R1                                                            
         IC    R1,BVRDESTH+5                                                    
         BCTR  R1,0                                                             
         MVI   IFLD,C' '          SET ID NAME IN IFLD                           
         MVC   IFLD+1(9),IFLD                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   IFLD(0),BVRDEST                                                  
*                                                                               
         LA    R4,SPTREC                                                        
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKID+8(2),REQORIG                                              
         CLC   IFLD(10),=CL10'DDS'        IF DDS DESTINATION                    
         BNE   VDEST0                                                           
         MVC   REQDEST(2),=X'005A'        DON'T BOTHER TO VALIDATE              
         B     VALDESTX                                                         
VDEST0   CLC   IFLD(10),=CL10'SJR'                                              
         BE    VDEST1                                                           
         B     VDEST2                                                           
*                                                                               
VDEST1   MVC   CTIKID(10),IFLD                                                  
VDEST2   GOTO1 DATAMGR,DMCB,(0,DMREAD),CTFILE,(R4),(R4)                         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,DATAMGR                                                       
         GOTO1 GETIDS,PLIST,(C'D',SPTREC),0,(R5)                                
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
INVDEST  MVC   FERN,=AL2(FLDINV)                                                
         LA    R7,BVRDESTH                                                      
         ST    R7,FADR                                                          
         B     EXIT                                                             
*                                                                               
VALDESTX EQU   *                                                                
         SPACE 2                                                                
*        VALIDATE OUTPUT TYPE                                                   
*                                                                               
VALOUT   EQU   *                                                                
         TM    RFPSTAT,RFPINUSE                                                 
         BO    VALOUTX                                                          
         CLI   BVROUTH+5,0                                                      
         BNE   VALOUT1                                                          
         CLC   RNUM,=C'PF'         PF MUST BE DIRECT OR SOON                    
         BE    INVOUT                                                           
         TM    MYFLAG,LBREPT       LB MUST ONLY BE SOON                         
         BO    INVOUT                                                           
         B     VALOUTX                                                          
VALOUT1  CLC   =C'SOON,',BVROUT    CHK SOON REQUEST                             
         BE    VALSOON                                                          
         TM    MYFLAG,LBREPT       LB MUST ONLY BE SOON                         
         BO    INVOUT                                                           
         CLI   BVROUTH+5,6                                                      
         BH    INVOUT                                                           
         SR    R1,R1         SET OUTPUT TYPE IN IFLD                            
         IC    R1,BVROUTH+5                                                     
         BCTR  R1,0                                                             
         MVI   IFLD,C' '                                                        
         MVC   IFLD+1(9),IFLD                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   IFLD(0),BVROUT                                                   
         CLC   IFLD(7),=C'DIRECT '  ALLOW DIRECT FOR BILLING                    
         BE    VALOUT2                                                          
         CLC   RNUM,=C'B1'         NO OUTPUT TYPE FOR BILLING                   
         BE    INVOUT                                                           
         CLC   RNUM,=C'BU'         NO OUTPUT TYPE FOR NTB REPORT                
         BE    INVOUT                                                           
         CLC   RNUM,=C'D1'         NO OUTPUT TYPE FOR BILLING                   
         BE    INVOUT                                                           
         CLC   RNUM,=C'DU'         NO OUTPUT TYPE FOR NTB REPORT                
         BE    INVOUT                                                           
         CLC   RNUM,=C'PF'         NO OUTPUT TYPE FOR PF                        
         BE    INVOUT                                                           
*                                                                               
VALOUT2  LA    R4,SPTREC                                                        
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
         MVC   REQOUT,IFLD           SAVE OUTPUT TYPE NAME                      
         B     VALOUTX                                                          
*                                                                               
INVOUT   MVC   FERN,=AL2(FLDINV)                                                
         LA    R7,BVROUTH                                                       
         ST    R7,FADR                                                          
         B     EXIT                                                             
*                                                                               
VALSOON  DS    0H                            SOONABLE REQUESTS                  
         CLC   =C'B1',RNUM     B1 CANNOT BE RUN SOON, THE BIT IS THERE          
         BNE   *+12            FOR LB REPORT                                    
         TM    MYFLAG,LBREPT                                                    
         BZ    INVOUT                                                           
*                                                                               
         CLC   =C'BU',RNUM     BU CANNOT BE RUN SOON, THE BIT IS THERE          
         BNE   *+12            FOR LB REPORT                                    
         TM    MYFLAG,LBREPT                                                    
         BZ    INVOUT                                                           
*                                                                               
         CLI   BVROUTH+5,5                                                      
         BNH   INVOUT                                                           
         TM    3(R7),X'10'         CHK IF REQ ALLOWS SOON                       
         BZ    INVOUT                                                           
*                                                                               
VALSA2   DS    0H                                                               
*                                                                               
VALOUTX  EQU   *                                                                
         DROP  R4                                                               
         DROP  R6                                                               
         EJECT                                                                  
*        CHECK IF REQUESTOR IS COMPATIBLE WITH REQUEST NUMBER                   
*                                                                               
VALREQ   TM    3(R7),X'04'                   FOR DDS ONLY                       
         BZ    *+12                                                             
         CLI   DDS,1                         YES MUST BE DDS TERM               
         BNE   INVNUM                                                           
         CLI   REQACTN,C'D'                                                     
         BNE   VALREQ0                                                          
         MVC   REQNDX1(2),=X'FFFE'           SET ENQ SCR REQUIRED               
         CLI   REQOPTN,C'T'                                                     
         BNE   VALREQ7                                                          
         MVC   REQNDX1(2),=X'FFFC'     SET MENU SCREEN FOR TOTAL OPT            
         B     VALREQ7                                                          
         B     VALREQ7                                                          
VALREQ0  TM    FIND,X'08'                    CARD REQUEST                       
         BZ    VALREQ1                                                          
         MVC   REQNDX1(2),=X'FFFD'           SET CARD SCR REQUIRED              
         B     VALREQ7                                                          
VALREQ1  TM    3(R7),X'08'                   ONLY AVAIL AS CARD REQ             
         BO    INVNAME                                                          
         TM    3(R7),X'01'                   IS REQUESTOR REQUIRED              
         BZ    VALREQ2                       NO                                 
         TM    FIND,X'01'                    WAS REQUESTOR INPUT                
         BZ    MISNAME                       NO ERROR                           
VALREQ2  TM    3(R7),X'02'                   IS MEDIA REQUIRED                  
         BO    VALREQ3                       YES                                
         TM    FIND,X'02'                    WAS MEDIA INPUT                    
         BO    VALREQ3                       YES                                
         LA    R4,MEDTBL                     FIND DEFAULT MEDIA                 
         CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   DUB(1),1(R4)                                                     
         NC    DUB(1),29(R7)                                                    
         BNZ   *+12                                                             
         LA    R4,L'MEDTBL(R4)                                                  
         B     *-30                                                             
         MVC   BVRMED(1),0(R4)                                                  
         MVC   BVRMEDH+4(2),=X'C001'                                            
         B     VALMED                                                           
*                                                                               
VALREQ3  TM    FIND,X'02'                    WAS MEDIA INPUT                    
         BZ    MISMED                        NO ERROR                           
         MVC   DUB(1),REQMED1                YES CHECK WITH REQ VALUES          
         NC    DUB(1),3(R7)                                                     
         BZ    INVMED                                                           
*                                                                               
VALREQ4  LA    R8,28(R7)                     FIND FLD LIST FOR MEDIA            
         SR    R6,R6                                                            
VALREQ5  CLI   0(R8),0                       R8=A(REQTBL MEDIA ENTRY)           
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
         SR    R8,R6                         SET SCR LIST REQUIRED = ..         
         STH   R8,REQNDX1                    SAVE INDEX TO REQTBL               
         AR    R8,R6                                                            
*                                                                               
VALREQ7  L     R6,AREQTBL                                                       
         SR    R7,R6                                                            
         STH   R7,REQNDX                     SAVE INDEX TO REQTBL               
         AR    R7,R6                                                            
         MVC   REQFMT,FIND                                                      
         EJECT                                                                  
*        CHECK IF THIS REQUEST IS COMPATIBLE WITH THE PREVIOUS REQUEST          
*                                                                               
VALDEFN  CLI   REQACTN,C'A'                                                     
         BNE   VALDEF1                                                          
         CLI   PREQACTN,C'N'                 AMEND ONLY VALID AFTER NEW         
         BNE   INVACTN                                                          
         CLC   REQNDX1(2),PREQNDX1           SAME SCREEN REQUIRED               
         BNE   INVACTN                       NO CANT AMEND                      
         MVC   TEMP(1),REQFMT                                                   
         XC    TEMP(1),LREQFMT                                                  
         TM    TEMP,X'04'                                                       
         BO    INVACTN                                                          
         B     VALIPT                        OK TO AMEND                        
*                                                                               
VALDEF1  CLI   REQNDX1,X'FF'                 ENQ OR CARD SCR REQUIRED           
         BNE   VALIPT                        NO                                 
         CLC   PREQNDX1(2),REQNDX1           YES IS IT ALREADY LOADED           
         BNE   *+12                NO - LOAD                                    
         CLI   REQNDX1+1,X'FC'     YES - SKIP LOAD EXCEPT FOR MENU              
         BNE   VALDEF2                                                          
         XC    DISPFLDS(2),DISPFLDS          SET NO REQUESTS DISPLAYED          
         MVC   PLIST+4(4),=X'D90208FF'                                          
         MVC   PLIST+7(1),REQNDX1+1                                             
         GOTO1 CALLOV,PLIST,BVRFRSTH                                            
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   PREQNDX1(2),REQNDX1           SAVE SCR LOADED                    
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
         MVI   LREQMAP+3,127                                                    
         MVI   STATUS,1            SET REQUEST DATA REQUIRED                    
         B     DEFAULT                                                          
*                                                                               
VALDEF2  CLI   REQNDX1+1,X'FE'                                                  
         BNE   VALDEF3                                                          
VALDEF2A MVI   STATUS,3                      SET ENQ/CANC STATUS                
         MVC   REQFLTR,FIND                  SAVE ENQ/CANC FILTERS              
         B     SAVEDATA                                                         
VALDEF3  CLI   REQNDX1+1,X'FD'               CARD REQUEST                       
         BE    VALIPT1                       YES                                
         MVI   STATUS,4                      SET MENU DISPLAY STATUS            
         CLI   REQOPTN,C'T'                                                     
         BE    VALDEF2A                                                         
*        BAS   RE,BILDMNU                                                       
         B     SAVEDATA                                                         
*                                                                               
BILDMNU  NTR1                        BUILD MENU FROM REQUEST TABLE              
*        LA    R3,MNUSTRTH                                                      
*        LA    R4,REQTBL                                                        
BIM20    MVC   11(22,R3),4(R4)   NAME                                           
         ZIC   R1,0(R4)               BUMP REQTBL                               
         AR    R4,R1                                                            
*        MVC   8(2,R3),-2(R4)    ID                                             
         OI    6(R3),X'80'                                                      
*        MVC   R1,0(R3)               BUMP OUTSCREEN AREA                       
         ZIC   R1,0(R3)                                                         
         AR    R3,R1                                                            
         CLI   0(R3),9                                                          
         BH    BIM20                                                            
BIMX     B     EXIT                                                             
         EJECT                                                                  
*        VALID TO INPUT BEYOND HEADR FOR STATUS=0 ONLY IF A NEW SCREEN          
*        IS NOT REQUIRED FOR NEW REQUEST DEFINITION                             
VALIPT   CLC   PREQNDX1(2),REQNDX1           SCR FLD LIST CHANGED               
         BNE   BUILDER                       YES MUST BUILD SCR                 
         MVC   TEMP(1),REQFMT                                                   
         XC    TEMP(1),LREQFMT                                                  
         TM    TEMP,X'04'                                                       
         BO    BUILDER                                                          
*                                                                               
VALIPT1  LA    R5,BVRFRSTH                   FIND 1ST UNPROT DATA FLD           
         SR    R6,R6                                                            
         TM    1(R5),X'20'                                                      
         BZ    *+14                                                             
         IC    R6,0(R5)                                                         
         AR    R5,R6                                                            
         B     *-14                                                             
         CLI   LREQMAP,127                   ZERO INPUT REQUEST                 
         BE    *+12                          YES                                
         C     R5,ALASTF                     ANY INPUT IN DATA AREA             
         BH    VALIPT3                       NO                                 
VALIPT2  MVI   STATUS,2                      SET REQUEST DATA INPUT             
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
BUILDER  DS    0H                                                               
* - STEREO NEEDS SOME HELP - WHEN SWITCHING FROM DISPLAY BACK TO                
*   REQ SCREEN, SCREEN NUMBER IN SERVICE REQUEST FIELD DOES NOT CHANGE          
*   SO TELL SYSTEM WE ARE PASSING OWN SCREEN NUMBER                             
         BAS   RE,CHKSTREO         NEW 'HEAVY'/'LIGHT' STEREO CHK               
         BNO   BLD5                                                             
         CLC   =C'08',BVRSRV+2    IF STEREO                                     
         BNE   BLD5                                                             
         L     R1,APARM            GET FATIOB                                   
         L     R1,0(R1)                                                         
         USING TIOBD,R1                                                         
         OI    TIOBINDS,TIOBSCRN                                                
         MVI   TIOBCNT,X'FF'                                                    
         DROP  R1                                                               
*                                                                               
BLD5     MVI   STATUS,1                      SET REQUEST DATA REQUIRED          
         LA    R1,LREQMAP                    R1=A(REQMAP ENTRY)                 
         LR    R4,R8                         R4=A(REQTBL ENTRY)                 
         LA    R4,2(R4)                                                         
         LA    R8,BVRFRSTH                   R8=A(NEXT TWA BYTE)                
         LA    R5,6                          R5=LAST TWA LINE NUMBER            
         SR    R6,R6                                                            
         MVC   HALF,=H'40'                                                      
         B     REQLOO5B                                                         
*                                                                               
**REQLOOP  LA    R7,TWATBL                     R7=A(TWATBL ENTRY)               
REQLOOP  L     R7,=A(TWATBL)                 R7=A(TWATBL ENTRY)                 
         A     R7,RELO                                                          
*                                                                               
TWALOOP  IC    R6,1(R7)                      FIND ENTRY IN TWATBL               
         LTR   R6,R6                                                            
         BNZ   TWALOOP1                                                         
         LR    RF,R1                         FLD MUST BE A SUB FLD              
         SH    RF,=H'3'                                                         
         MVC   0(1,R1),0(R4)                 SET FLD NUM                        
         MVC   1(2,R1),1(RF)                 SET FLD ADR TO PREVIOUS            
         LA    R1,3(R1)                      BUMP REQ MAP ENTRY                 
         B     REQLOOP5                                                         
TWALOOP1 CLC   0(1,R7),0(R4)                                                    
         BE    REQLOOP1                                                         
         LA    R7,3(R7,R6)                                                      
         B     TWALOOP                                                          
*                                                                               
REQLOOP1 DS    0H                                                               
         TM    REQFMT,X'04'                                                     
         B     REQLOOP2                                                         
*        IF THE ABOVE INSTRUCTION IS CHANGED TO BZ THEN THE FOLLOWING           
*        CODE WILL CAUSE THE DDS OPTION TO OUTPUT TWO FIELDS PER LINE           
*        AND TO IGNORE ALL COMMENT FIELDS.                                      
         CLI   COMSW,1             SEE IF DOING COMMENT                         
         BNE   *+10                                                             
         SR    R6,R6                         IGNORE ALL COMMENTS                
         B     REQLOOP5                                                         
         OC    HALF,HALF                                                        
         BNZ   REQLOO25                                                         
         MVC   HALF,=H'40'                   SET TO 2ND HALF                    
         B     REQLOOP3                                                         
*                                                                               
REQLOOP2 CLI   COMSW,1             SEE IF DOING COMMENT                         
         BNE   REQLOO25            NO                                           
         TM    0(R4),X'01'         TEST ODD COMMENT                             
         BO    *+14                          SKIP IF SAME LINE COMMENT          
REQLOO25 XC    HALF,HALF                     BUMP TO NEW LINE                   
         LA    R5,1(R5)                                                         
         CH    R5,=H'24'                                                        
         BNH   *+6                                                              
         DC    H'0'                          TOO MANY LINES                     
*                                                                               
REQLOOP3 MVC   0(8,R8),=X'1A20010200008012'  MOVE STD PROT FLD TO TWA           
         CLI   COMSW,1             COMMENT                                      
         BE    REQLOO3B                                                         
         CLI   0(R7),127                                                        
         BNE   REQLOO3D                                                         
         MVC   0(8,R8),=X'0920000200008001'  MOVE TAB PROT FLD TO TWA           
         B     REQLOO3D                                                         
REQLOO3B STC   R6,7(R8)                      SET LENGTH FOR COMMENTS            
         LA    RF,8(R6)                                                         
         STC   RF,0(R8)                                                         
         MVC   2(2,R8),=X'002A'              SET COL#42 FOR COMMENTS            
REQLOO3D SR    RF,RF                         SET PROT FLD TEXT                  
         IC    RF,7(R8)                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    8(0,R8),8(R8)                                                    
         BCTR  R6,0                                                             
         CLI   COMSW,1             IF DOING COMMENT                             
         BE    REQLOO3F            SKIP STEREO                                  
         CLC   =X'7F0000',0(R4)    IF END OF REQTBL                             
         BE    REQLOO3F            SKIP STEREO                                  
         BAS   RE,CHKSTREO         LIGHT/HEAVY STEREO CHK                       
         BNO   REQLOO3F                                                         
         CLC   =C'08',BVRSRV+2    ..IF STEREO DATA REQUEST                      
         BE    DOSTEREO                                                         
         CLI   BVRSRV+2,X'40'      CAN BE BLANK IN STEREO IF SWITCHING          
         BH    REQLOO3F            FROM RFP TO REQ FOR EXAMPLE                  
DOSTEREO DS    0H                                                               
         BAS   RE,STEREOID         ..RETURN DATA FOR STEREO                     
         B     REQLOO3G            ..INSTEAD OF LABEL                           
REQLOO3F EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R8),3(R7)                                                    
REQLOO3G LA    R6,8(RF)                      R6=TOT PROT TWA LEN                
         IC    RF,2(R7)                                                         
         LTR   RF,RF                                                            
         BZ    REQLOO3X                                                         
         LA    RE,0(R8,R6)                                                      
         MVC   0(8,RE),=X'0000001500008000'  SET UNPROT FLD HDR                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    8(0,RE),8(RE)                 NULL TEXT                          
         STC   RF,7(RE)                                                         
         LA    RF,8(RF)                                                         
         STC   RF,0(RE)                                                         
REQLOO3X AR    R6,RF                         SET R6=TOTAL TWA LENGTH            
*                                                                               
         SR    R0,R0                         CALC ABS SCREEN ADDR               
         LR    R7,R8                         R7=A(FLD IN TWA)                   
REQLOOP4 CLI   0(R7),0                                                          
         BE    REQLOOP5                                                         
         MVC   DUB(2),2(R7)                  DUB=X'XXCC' CC=COL NUM             
         CLI   COMSW,1             SEE IF DOING COMMENT                         
         BE    REQLOO45                                                         
         CLI   DUB,0                         SPECIAL ATTRIBUTE REQD             
         BE    REQLOO45                      NO                                 
         TM    1(R4),X'01'                   OPTIONAL INPUT FLD                 
         BO    REQLOO45                      YES                                
         OI    1(R7),X'08'                   SET HIGH INTENSITY                 
         NI    1(R7),X'FB'                                                      
         IC    R0,0(R7)                      SET FLD REQUIRED INDIC             
         LR    RF,R7                                                            
         AR    RF,R0                                                            
         BCTR  RF,R0                                                            
         MVI   0(RF),C'='                                                       
REQLOO45 MVI   DUB,0                         DUB=X'00CC'                        
         LH    RE,DUB                                                           
         AH    RE,HALF                                                          
         BCTR  RE,R0                                                            
         LR    RF,R5                                                            
         BCTR  RF,R0                                                            
         MH    RF,=H'80'                                                        
         AR    RF,RE                                                            
         STH   RF,DUB                        DUB=X'AAAA'=ABS SCR ADR            
         MVC   2(2,R7),DUB                                                      
         TM    1(R7),X'20'                   IS FLD PROTECTED                   
         BO    REQLOO47                      YES                                
         LR    RF,R7                         NO GET RELATIVE TWA ADR            
         SR    RF,R3                                                            
         MVC   0(1,R1),0(R4)                 SET FLD NUM                        
         STC   RF,2(R1)                      SET FLD ADR TO REL TWA ADR         
         SRL   RF,8                                                             
         STC   RF,1(R1)                                                         
         LA    R1,3(R1)                      BUMP REQ MAP ENTRY                 
REQLOO47 IC    R0,0(R7)                                                         
         AR    R7,R0                                                            
         B     REQLOOP4                                                         
*                                                                               
REQLOOP5 AR    R8,R6                         UPDATE NEXT TWA ADR                
REQLOO5A CLI   0(R4),127                     BUMP REQ TBL ENTRY                 
         BE    REQLOOPX                      LAST ENTRY WAS TAB                 
         CLI   COMSW,1             SEE IF DOING COMMENT                         
         BNE   *+12                                                             
         LA    R4,1(R4)                      LAST ENTRY WAS COMMENT             
         B     *+8                                                              
         LA    R4,3(R4)                      LAST ENTRY WAS DATA                
REQLOO5B DS    0H                                                               
         MVI   COMSW,0                                                          
         CLI   0(R4),0                                                          
         BE    REQLOOP6                                                         
         CLI   0(R4),X'01'         COMMENT                                      
         BNE   REQLOO5D                                                         
         LA    R4,1(R4)                                                         
***      LA    R7,COMTBL                                                        
         L     R7,=A(COMTBL)                                                    
         A     R7,RELO                                                          
         MVI   COMSW,1                                                          
         B     TWALOOP                                                          
*                                                                               
REQLOO5D EQU   *                                                                
*                                MY REPORT FOR SALLY                            
         CLC   RNUM,=C'MY'                                                      
         BNE   REQL90                                                           
         CLI   0(R4),X'DA'        SPECIAL FIELD FOR NET ONLY                    
         BNE   REQL100                                                          
         BRAS  RE,SORN            ARE WE SPOT/NET                               
         BE    REQL100            NET                                           
         LA    R4,3(R4)           SPOT/BUMP PAST NET ONLY FIELD                 
         B     REQLOOP                                                          
*                                                                               
REQL90   CLC   RNUM,=C'KB'         IF KB                                        
         BNE   REQL100                                                          
         CLI   0(R4),X'F5'         IF NEWPROD FIELD                             
         BNE   REQL100                                                          
         CLC   RAGY,=C'DF'         IF SAATCHI                                   
         BE    REQL100             OK                                           
         CLC   RAGY,=C'SJ'         IF SJ                                        
         BE    REQL100             OK                                           
         LA    R4,3(R4)            ELSE SKIP IT                                 
         B     REQLOOP                                                          
*                                                                               
REQL100  TM    2(R4),X'80'                   DDS ENTRY                          
         BZ    REQLOOP                       NO OK FOR ALL SCREENS              
*                                                                               
* IF OUTPUT FIELD ON REQTBL IS 128 OR HIGHER (X'80')                            
* REQL100 BELOW INTERPRETS THIS AS SPECIAL DDS ENTRY FIELD                      
* SO FOR REPORTS THAT PUT DATA TO SECOND REQUEST CARD THIS IS                   
* A PROBLEM                                                                     
* IF KB REPORT SKIP THE CHECK                                                   
*                                                                               
         CLC   RNUM,=C'KB'                                                      
         BE    REQLOOP                                                          
                                                                                
         CLI   0(R4),X'DF'         K1 REPORT                                    
         BE    REQLOOP                                                          
         TM    REQFMT,X'04'                  YES ONLY FOR DDS FORMAT            
         BNZ   REQLOOP                                                          
*                                  SPECIAL FIELD FOR MCCANN M4 REQ              
         CLC   RNUM,=C'M2'                                                      
         BE    REQLOOP                                                          
         CLC   RNUM,=C'MC'                                                      
         BE    REQLOOP                                                          
         CLC   RNUM,=C'MD'                                                      
         BE    REQLOOP                                                          
         CLC   RNUM,=C'M4'                                                      
         BNE   REQLOO5A                                                         
REQLOO5E CLC   RAGY,=C'MC'         CHK FOR MCCANN                               
         BE    REQLOOP                                                          
         CLC   RAGY,=C'CC'         AND FOR COKE                                 
         BE    REQLOOP                                                          
         B     REQLOO5A                                                         
*                                                                               
REQLOOP6 LA    R4,=X'7F0000'                 SET FOR TAB LINE                   
         B     REQLOOP                                                          
*                                                                               
REQLOOPX MVC   0(3,R8),=X'000100'            SET B,A=CLEAR,NOTHING              
         LA    R8,64(R3)           RETRANSMIT REQ DEFN SCR                      
         SR    R7,R7                                                            
         LA    R6,BVRFRSTH                                                      
REQLXL   OI    6(R8),OI1T                                                       
         IC    R7,0(R8)                                                         
         AR    R8,R7                                                            
         CR    R8,R6                                                            
         BNH   REQLXL                                                           
*                                                                               
         MVC   PREQNDX1(2),REQNDX1           SET FLD LIST SCR LOADED            
         CLI   LREQMAP,127                   IS 1ST FLD TAB LINE                
         BNE   DEFAULT                       NO                                 
         MVI   STATUS,2                      YES SET REQ DATA INPUT             
         MVC   HALF,LREQMAP+1                                                   
         LH    R7,HALF                                                          
         AR    R7,R3                                                            
         ST    R7,AFIRSTF                    SIMULATE INPUT                     
         B     DEFAULT                                                          
*                                                                               
CHKSTREO      NTR1                                                              
*          DATA SET SPREQ00    AT LEVEL 116 AS OF 02/28/96                      
* CHECK FOR STEREO NOW THAT THERE ARE MORE THAN ONE                             
         L     RF,APARM                                                         
         L     RF,16(RF)           A(COMFACS)                                   
         USING COMFACSD,RF                                                      
         GOTO1 CGETFACT,DMCB,(X'80',0),F#UTLD                                   
         L     R1,0(R1)                                                         
         USING F@UTLD,R1                                                        
         TM    F@TSTAT6,TST6STRO+TST6STFU  ,, IF STEREO                         
*        BNO   NOTEST                                                           
         B     EXIT                                                             
         DROP  RF,R1                                                            
         EJECT                                                                  
* RETURNS DATA STEREO NEEDS TO PRE-VALIDATE REQUEST FIELDS                      
* - R4 POINTS TO REQUEST TABLE ENTRY / MULTNUM IS WORKAREA                      
*                                                                               
* - ROUTINE RETURNS HEX AS CHARACTER / X'01' AS C'0',C'1'                       
*                                                                               
STEREOID NTR1                                                                   
         LA    R2,2                                                             
STD10    BAS   R5,SPLITBYT         SPLIT REQ TABLE BYTE                         
         LA    R8,2(R8)                                                         
         LA    R4,1(R4)                                                         
         BCT   R2,STD10                                                         
         LA    R2,2                                                             
STD20    LA    R4,1(R4)            SKIP REQ CARD POSITION BYTE IN TBL           
         CLI   0(R4),X'01'         OPTIONAL COMMENT FIELD                       
         BNE   STDX                NO                                           
         LA    R4,1(R4)            YES                                          
         BAS   R5,SPLITBYT                                                      
         LA    R8,2(R8)            BUMP OUTAREA                                 
         BCT   R2,STD20            AND SEE IF ANY MORE COMMENTS                 
STDX     B     EXIT                                                             
*                                                                               
* X'1C' GOES OUT AS C'1',C'C'                                                   
SPLITBYT DS    0H                                                               
         MVC   MULTNUM,0(R4)       GET REQ TABLE BYTE                           
         ZIC   R1,MULTNUM                                                       
         SRA   R1,4                DEAL WITH FIRST HALF OF BYTE                 
         STC   R1,MULTNUM                                                       
         C     R1,=F'9'            IF OVER 9, SEND  C'A,B' ETC                  
         BNH   SPLT10                                                           
         LA    R1,1(R1)            IF > 9, MUST SEND C'A' ETC                   
         S     R1,=F'10'                                                        
         STC   R1,MULTNUM                                                       
         OI    MULTNUM,X'C0'                                                    
         B     *+8                                                              
SPLT10   OI    MULTNUM,X'F0'                                                    
         MVC   8(1,R8),MULTNUM     RETURN IT                                    
*                                                                               
         MVC   MULTNUM,0(R4)       DEAL WITH SECOND HALF OF BYTE                
         OI    MULTNUM,X'F0'                                                    
         CLI   MULTNUM,X'F9'       ..IF GREATER THAN 9, SEND C'A' ETC           
         BNH   SPLT20                                                           
         NI    MULTNUM,X'0F'       ..CLEAR FIRST HALF OF BYTRE                  
         ZIC   R1,MULTNUM                                                       
         LA    R1,1(R1)                                                         
         S     R1,=F'10'                                                        
         STC   R1,MULTNUM                                                       
         OI    MULTNUM,X'C0'                                                    
SPLT20   MVC   9(1,R8),MULTNUM     RETURN IT                                    
         BR    R5                                                               
         EJECT                                                                  
*        SET DEFAULT VALUES IN REQUEST RECORD (IF ANY)                          
*                                                                               
DEFAULT  L     R7,=A(REQROUTS)                                                  
         A     R7,RELO                                                          
*                                                                               
DEFAULT1 CLI   0(R7),0                                                          
         BE    SAVEDATA                      NOT IN TBL NO DEFAULTS             
         CLC   0(2,R7),REQNUM                                                   
         BE    DEFAULT2                                                         
         LA    R7,L'REQROUTS(R7)                                                
         B     DEFAULT1                                                         
DEFAULT2 MVC   DUB+1(3),2(R7)                                                   
         L     RF,DUB                                                           
         LA    RF,0(RF)                                                         
         A     RF,RELO                       RF=A(DEFAULT ROUTINE)              
         BASR  RE,RF                         SET DEFAULT VALUES                 
         SPACE 2                                                                
*        SAVE INITIALISED DATA IN TWA                                           
*                                                                               
SAVEDATA MVC   LREQNUM(18),REQNUM                                               
         MVC   LKEY,KEY                                                         
         MVC   LREQREC,REQREC                                                   
*                                                                               
         CLC   RNUM,=C'N2'         IF IT'S N2                                   
         BNE   SVD10                                                            
         CLI   REQACTN,C'N'        AND NOT NEW ADD                              
         BE    SVD10                                                            
         MVC   RNUM,=C'I2'         SET TO I2 FOR CANCEL OR AMEND                
         MVC   LREQOHDR+26(2),=C'I2'                                            
         MVI   REQNUMB,23                                                       
*                                                                               
SVD10    TM    FIND,X'08'                                                       
         BZ    EXIT                                                             
         CLI   REQACTN,C'N'                                                     
         BNE   EXIT                                                             
         CLI   STATUS,2                                                         
         BE    EXIT                                                             
         MVC   DUB(2),LREQMAP+1              DISPLAY NEW CARD DEFAULTS          
         LH    R5,DUB                                                           
         AR    R5,R3                                                            
         FOUT  (R5),RAGY,78                                                     
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
*        ROUTINES TO FILL IN DEFAULT VALUES IN REQUEST RECORD                   
         SPACE 2                                                                
REQRPH   MVC   RPRO,=C'ALL'                                                     
         MVC   RMARK(3),=C'ALL'                                                 
         MVC   RSTA(3),=C'ALL'                                                  
         MVC   REST(6),=C'001255'    WILL POPULATE REST & REST1                 
         MVI   RO1,C'I'            SET FOR INVOICES                             
         MVI   RO5,C'Y'                                                         
         BR    RE                                                               
         SPACE 2                                                                
REQRJV   MVC   RES,=C'ES'          FOR ESTIMATE DATES                           
         BR    RE                                                               
         SPACE 2                                                                
REQR09   LR    R5,RE                                                            
         MVC   RCLI(3),=C'ALL'                                                  
         MVC   RPRO(3),=C'ALL'                                                  
         MVC   REST(3),=C'ALL'                                                  
         MVC   RSTRD(6),=C'800101'            START=800101                      
         GOTO1 DATCON,PLIST,(5,0),(X'20',RENDD)                                 
         BR    R5                                                               
         SPACE 2                                                                
REQRCR   MVC   RCLI(3),=C'CC '                                                  
         MVC   RPRO(3),=C'ALL'                                                  
         MVC   REST(3),=C'ALL'                                                  
         MVC   RSTA(3),=C'ALL'                                                  
         BR    RE                                                               
         SPACE 2                                                                
REQR07   MVI   RO1,C'S'                                                         
         BR    RE                                                               
         SPACE 2                                                                
REQR14   MVI   RO1,C'N'                                                         
         BR    RE                                                               
         SPACE 2                                                                
REQR15   MVC   RCLI(3),=C'ALL'                                                  
         MVC   RPRO(3),=C'ALL'                                                  
         MVC   REST(3),=C'ALL'                                                  
         MVI   RO2,C'N'                                                         
         BR    RE                                                               
         SPACE 2                                                                
REQR17   BAS   R8,PROPOL                                                        
         MVC   REST(3),=C'NO '                                                  
         BR    RE                                                               
REQR23   MVC   RSTA(3),=C'ALL'                                                  
         BR    RE                                                               
         SPACE 2                                                                
REQR32   MVI   RHDR+10,31           CHANGE TO 31                                
         MVI   RNUM+1,C'1'                                                      
         BR    RE                                                               
         SPACE 2                                                                
REQR42   MVC   RPRO,=C'ALL'                                                     
         OI    PROSAVE,X'02'                                                    
         MVC   REST(6),=C'001255'                                               
         BR    RE                                                               
         SPACE 2                                                                
REQR42A  MVC   RPRO,=C'ALL'        PZ -PFIZER                                   
         OI    PROSAVE,X'02'                                                    
         MVC   REST(6),=C'001255'                                               
         MVI   RCARD2+16,C'*'      SET EST TYPE FILTER                          
         BR    RE                                                               
         SPACE 2                                                                
REQR43   MVC   RCLI,=C'BM '                                                     
         B     REQR56A                                                          
         SPACE 2                                                                
REQR46   MVC   RCLI,=C'ALL'                                                     
         BR    RE                                                               
         SPACE 2                                                                
REQR52   MVC   RMARK(3),=C'ALL'                                                 
         OI    MKTSAVE,X'02'                                                    
         BR    RE                                                               
         SPACE 2                                                                
REQR53   BAS   R8,CLIPG                                                         
REQR53A  MVI   ROAGY,C'Y'                                                       
         MVC   RSTA(3),=C'ALL'                                                  
         BR    RE                                                               
         SPACE 2                                                                
REQR55   MVC   RPRO,=C'ALL'                                                     
         OI    PROSAVE,X'02'                                                    
         MVC   REST(6),=C'001255'                                               
         BR    RE                                                               
         SPACE 2                                                                
REQR56   MVC   RCLI,=C'GF '                                                     
REQR56A  MVC   RPRO,=C'POL'                                                     
         MVC   RMARK(3),=C'ALL'                                                 
         MVC   RSTA(3),=C'ALL'                                                  
         BR    RE                                                               
         SPACE 2                                                                
REQR57   BAS   R8,CLIPG                                                         
         BR    RE                                                               
         SPACE 2                                                                
REQR76   MVI   RNUM+61,C'Y'                                                     
         BR    RE                                                               
         SPACE 2                                                                
REQR80   BAS   R8,PROPOL                                                        
         BR    RE                                                               
         SPACE 2                                                                
REQR86   BAS   R8,PROPOL                                                        
         MVC   RSTA(3),=C'ALL'                                                  
         BR    RE                                                               
         SPACE 2                                                                
REQR91   MVC   RMARK(3),=C'ALL'                                                 
         MVC   RSTA(3),=C'ALL'                                                  
REQR90   MVC   REST(6),=C'001255'                                               
         BR    RE                                                               
         SPACE 2                                                                
*REQR92   MVC   RNUM+35(2),=C'YY'                                               
*         BR    RE                                                              
         SPACE 2                                                                
REQR96   MVC   RES,=C'ES'                                                       
         MVC   RDIV(2),=C'NN'                                                   
         BR    RE                                                               
         SPACE 2                                                                
REQR98   BAS   R8,CLIPG                                                         
REQR98A  MVC   RSTA(3),=C'ALL'                                                  
         BR    RE                                                               
         SPACE 2                                                                
REQR101  MVC   RCLI,=C'ALL'                                                     
         MVC   RMARK(3),=C'ALL'                                                 
         BR    RE                                                               
         SPACE 2                                                                
REQR105  MVC   RCLI,=C'ALL'                                                     
REQR105B MVC   RPRO,=C'ALL'                                                     
         OI    PROSAVE,X'02'                                                    
         MVC   REST(2),=C'NO'                                                   
         BR    RE                                                               
         SPACE 2                                                                
REQR109  MVC   RSTA(3),=C'ALL'                                                  
         BR    RE                                                               
         SPACE 2                                                                
REQR111  MVC   RCLI,=C'ALL'                                                     
         MVC   REST(2),=C'NO'                                                   
REQR111B MVC   RPRO,=C'ALL'                                                     
         MVC   RMARK(3),=C'ALL'                                                 
         MVC   RSTA(3),=C'ALL'                                                  
         MVI   CLIPROF+3,C'A'      SET DEFAULT RATING SERVICE FOR               
*                                  BOOK-HUT READ                                
         BR    RE                                                               
         SPACE 2                                                                
REQR125  BAS   R8,PROPOL                                                        
         MVC   REST(2),=C'NO'                                                   
         BR    RE                                                               
         SPACE 2                                                                
REQR151  MVC   RMARK(3),=C'ALL'                                                 
         OI    MKTSAVE,X'02'                                                    
         MVC   RSTA(3),=C'ALL'                                                  
         BR    RE                                                               
         SPACE 2                                                                
REQR156  MVC   RMARK(3),=C'ALL'                                                 
         MVC   RSTA(3),=C'ALL'                                                  
         MVI   RO1,C'Y'                                                         
         MVI   RO4,C'Y'                                                         
         BR    RE                                                               
         SPACE 2                                                                
REQR178  MVC   RSTA(3),=C'ALL'                                                  
         BR    RE                                                               
         SPACE 2                                                                
REQR194  MVC   RPRO,=C'ALL'                                                     
         OI    PROSAVE,X'02'                                                    
         MVC   RES,=C'ES'                                                       
         MVI   RO1,C'N'                                                         
         BR    RE                                                               
         SPACE 2                                                                
REQR196  DS    0H                                                               
         MVI   R2USER+28,C'R'                                                   
         BR    RE                                                               
         SPACE 2                                                                
REQR197  DS    0H                                                               
         MVC   RSTA(3),=C'ALL'                                                  
         BR    RE                                                               
         SPACE 2                                                                
REQR199  MVC   REST(2),=C'NO'                                                   
         BR    RE                                                               
         SPACE 2                                                                
REQR228  MVI   MKTSAVE,0                                                        
REQR206  MVC   RCLI,=C'ALL'                                                     
         MVC   RPRO,=C'ALL'                                                     
         OI    PROSAVE,X'02'                                                    
         MVC   REST(2),=C'NO'                                                   
         BR    RE                                                               
         SPACE 2                                                                
REQR233  MVC   RCLI,=C'ALL'                                                     
         OI    CLISAVE,X'02'                                                    
         MVI   RDIST,C'K'                                                       
         MVC   RPRO,=C'ALL'                                                     
         MVC   RMARK(3),=C'ALL'                                                 
         MVC   RSTA(3),=C'ALL'                                                  
         MVC   RHUT(3),=C'NOI'                                                  
         BR    RE                                                               
         SPACE 2                                                                
REQR236  MVC   RCLI,=C'ALL'                                                     
         OI    CLISAVE,X'02'                                                    
         BR    RE                                                               
         SPACE 2                                                                
PROPOL   MVC   RPRO,=C'POL'                                                     
         MVC   KEY+4(3),=C'POL'                                                 
         OI    PROSAVE,X'08'                                                    
         BR    R8                                                               
         SPACE 2                                                                
CLIPG    MVC   RCLI,=C'PG '                                                     
         MVC   KEY+2(2),=X'BCDF'                                                
         OI    CLISAVE,X'04'                                                    
         BR    R8                                                               
         SPACE 2                                                                
RO14AA   MVI   RNUM+61,C'A'                                                     
         MVI   RNUM+64,C'A'                                                     
         BR    R8                                                               
         EJECT                                                                  
         SPACE 2                                                                
*                                                                               
DMREAD   DC    CL8'DMREAD'                                                      
COMSW    DS    CL1                                                              
MYFLAG   DS    X                                                                
MVMFIQ   EQU   X'80'                                                            
LBREPT   EQU   X'40'                                                            
NNREPT   EQU   X'20'                                                            
CTFILE   DC    CL8'CTFILE'                                                      
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
FLDMIS   EQU   01                            MISSING INPUT FLD                  
FLDINV   EQU   02                            INVALID INPUT FLD                  
NUMINV   EQU   10                            REQUEST NUMBER INVALID             
ACTINV   EQU   12                            REQUEST ACTION INVALID             
OPTINV   EQU   02                            REQUEST OPTION INVALID             
ACCERR   EQU   207                 LMT ACCESS ERROR                             
Z5ERR    EQU   1205                          Z5 IS FOR SPOT ONLY                
         EJECT                                                                  
********************************************************                        
* LIMITED ACCESS FOR SIGN-ON IDS  WWAT,MNAT,MNNY,LHNC,BESAT                     
* THESE ID'S ARE ONLY AUTHORIZED FOR REPORTS D4,DC,M2,M9,m4                     
*                                                                               
*   NOTE R2 RESERVED - POINTS TO SCREEN                                         
*                                                                               
CHKID    NTR1  BASE=*,LABEL=*                                                   
         LA    R4,SPTREC                                                        
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKNUM,USRID                                                    
         GOTO1 DATAMGR,DMCB,(0,DMREAD),CTFILE,(R4),(R4)                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,28(R4)                                                        
CHKID05  CLI   0(R4),X'02'                                                      
         BE    CHKID10                                                          
         ZIC   RE,1(R4)                                                         
         CLI   0(R4),2                                                          
         BNH   *+6                                                              
         DC    H'0'                                                             
         AR    R4,RE                                                            
         B     CHKID05                                                          
         DROP  R4                                                               
         USING CTDSCD,R4                                                        
CHKID10  CLC   =C'WWAT',CTDSC                                                   
         BE    CHKID20                                                          
         CLC   =C'MNAT',CTDSC                                                   
         BE    CHKID20                                                          
         CLC   =C'MNNY',CTDSC                                                   
         BE    CHKID20                                                          
         CLC   =C'LHNC',CTDSC                                                   
         BE    CHKID20                                                          
         CLC   =C'BESAT',CTDSC                                                  
         BE    CHKID20                                                          
         CLC   =C'LJKY',CTDSC       LJKY LIMITED TO 2 REPORTS                   
         BE    CHKID30                                                          
         CLC   =CL10'STAR',CTDSC    STAR CANNOT REQUEST BU/B1                   
         BE    CHKID40                                                          
         CLC   =CL10'STACHO',CTDSC  STACHO CANNOT REQUEST BU/B1                 
         BE    CHKID40                                                          
         CLC   =CL10'STARPG',CTDSC  STARPG CANNOT REQUEST BU/B1                 
         BE    CHKID40                                                          
         CLC   =CL10'START',CTDSC   START CANNOT REQUEST BU/B1                  
         BE    CHKID40                                                          
         CLC   =CL10'STARPMT',CTDSC  STARPMT CANNOT REQUEST BU/B1               
         BE    CHKID40                                                          
         CLC   =CL10'STARLA',CTDSC   STARLA CANNOT REQUEST BU/B1                
         BE    CHKID40                                                          
         CLC   =CL10'STARM',CTDSC    STARM CANNOT REQUEST BU/B1                 
         BE    CHKID40                                                          
*        CLC   =CL10'MVNY',CTDSC     MVNY CANNOT REQUEST BU/B1                  
*        BE    CHKID40                                                          
*        CLC   =CL10'MVLA',CTDSC     MVLA CANNOT REQUEST BU/B1                  
*        BE    CHKID40                                                          
         CLC   =CL10'BRSA',CTDSC     BRSA CANNOT REQUEST BU/B1                  
         BE    CHKID40                                                          
******   CLC   AGY,=C'H7'     FOR MINDSHARE ONLY SPECIFIC IDS                   
******   BE    CHKID50        CAN REQUEST BU/B1                                 
         CLC   =CL10'MVMFI',CTDSC    MVMFI ONLY D8,DD AND M REPORTS             
         BE    CHKID60                                                          
         B     CHKIDOK                                                          
                                                                                
* CHECK AUTHORIZATION AND FUDGE REQUESTED REPORT                                
CHKID20  LA    R1,WESTTBL                                                       
CHKID22  CLI   0(R1),0                                                          
         BE    CHKIDNO                                                          
         CLC   BVRNUM(2),0(R1)                                                  
         BE    CHKID24                                                          
         LA    R1,4(R1)                                                         
         CLI   0(R1),C'0'                                                       
         BE    CHKIDNO                                                          
         B     CHKID22                                                          
CHKID24  CLC   BVRNUM+2(2),=C',D'  DISPLAY ?                                    
         BE    CHKIDOK             THEN DON'T FUDGE REPORT ID                   
         MVC   IFLD+1(2),2(R1)     FUDGE REPORT IDS                             
         B     CHKIDOK                                                          
*                                                                               
CHKID30  CLC   =C'D4',BVRNUM                                                    
         BE    CHKIDOK                                                          
         CLC   =C'M9',BVRNUM                                                    
         BE    CHKIDOK                                                          
         B     CHKIDNO                                                          
*                                                                               
CHKID40  CLC   =C'BU',BVRNUM                                                    
         BE    CHKIDNO                                                          
         CLC   =C'B1',BVRNUM                                                    
         BE    CHKIDNO                                                          
         B     CHKIDOK                                                          
*&&DO                                                                           
CHKID50  CLC   =C'BU',BVRNUM                                                    
         BE    *+14                                                             
         CLC   =C'B1',BVRNUM                                                    
         BNE   CHKIDOK                                                          
*                                   FOR MINDSHARE ONLY FOLLOWING IDS            
         CLC   =CL10'MSCQ',CTDSC    CAN REQUEST BU/B1, THE REST CAN             
         BE    CHKIDOK              ONLY GET DRAFTS                             
         CLC   =CL10'MSNYBC',CTDSC                                              
         BE    CHKIDOK                                                          
         CLC   =CL10'MSNYFR',CTDSC                                              
         BE    CHKIDOK                                                          
         CLC   =CL10'MSNYJ',CTDSC                                               
         BE    CHKIDOK                                                          
         CLC   =CL10'MSNYJPDF',CTDSC                                            
         BE    CHKIDOK                                                          
         CLC   =CL10'MSNYM',CTDSC                                               
         BE    CHKIDOK                                                          
         CLC   =CL10'MSME',CTDSC                                                
         BE    CHKIDOK                                                          
         CLC   =CL10'MSGM',CTDSC                                                
         BE    CHKIDOK                                                          
         CLC   =CL10'MSNYTC',CTDSC                                              
         BE    CHKIDOK                                                          
         CLC   =CL10'MSNYO',CTDSC                                               
         BE    CHKIDOK                                                          
         CLC   =CL10'MSMES',CTDSC                                               
         BE    CHKIDOK                                                          
         CLC   =CL10'MSNYMPDF',CTDSC                                            
         BE    CHKIDOK                                                          
         CLC   =CL10'MSNYOPDF',CTDSC                                            
         BE    CHKIDOK                                                          
         CLC   =CL10'MSNYJCC',CTDSC                                             
         BE    CHKIDOK                                                          
         CLC   =CL10'MSNYMONE',CTDSC                                            
         BE    CHKIDOK                                                          
         CLC   =CL10'MSSOHO',CTDSC                                              
         BE    CHKIDOK                                                          
         CLC   =CL10'MSMOGM',CTDSC                                              
         BE    CHKIDOK                                                          
         CLC   =CL10'MSNYMAX',CTDSC                                             
         BE    CHKIDOK                                                          
         CLC   =CL10'MSNYMAXB',CTDSC                                            
         BE    CHKIDOK                                                          
         CLC   =CL10'MSNYBCH',CTDSC                                             
         BE    CHKIDOK                                                          
         CLC   =CL10'MXMMSB',CTDSC                                              
         BE    CHKIDOK                                                          
         CLC   =CL10'MSYRB',CTDSC                                               
         BE    CHKIDOK                                                          
         CLC   =CL10'MSMAXPDF',CTDSC                                            
         BE    CHKIDOK                                                          
         CLC   =CL10'MXMMSPDF',CTDSC                                            
         BE    CHKIDOK                                                          
         CLC   =CL10'MSNYMMS',CTDSC                                             
         BE    CHKIDOK                                                          
         CLC   =CL10'MSBRAND',CTDSC                                             
         BE    CHKIDOK                                                          
         CLC   =CL10'MSNEO',CTDSC                                               
         BE    CHKIDOK                                                          
         CLC   =CL10'MSMEC',CTDSC                                               
         BE    CHKIDOK                                                          
         B     CHKIDNO                                                          
*&&                             MVMFI CAN ONLY REQUEST D8, DD AND               
CHKID60  OI    MYFLAG,MVMFIQ          REPORTS STARTING WITH M                   
         CLC   =C'D8',BVRNUM                                                    
         BE    CHKIDOK                                                          
         CLC   =C'DD',BVRNUM                                                    
         BE    CHKIDOK                                                          
         CLI   BVRNUM,C'M'                                                      
         BE    CHKIDOK                                                          
         B     CHKIDNO                                                          
*                                                                               
*                                                                               
CHKIDOK  SR    R1,R1                                                            
CHKIDNO  LTR   R1,R1                                                            
CHKIDX   XIT1                                                                   
         DROP  R4                                                               
* TABLE OF WESTERN SPOT REQUESTS                                                
* CL2 = AUTHORIZED REPORT/CL2 IT'S FUDGED EQUIVALENT                            
* D4=W4 DC=WC M2=W2 M9=W9 M4=m4                                                 
WESTTBL  DC    C'D4W4DCWCM2W2M9W9M4m400'                                        
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*  ARE WE IN SPOT OR NET                                                        
*  RETURNS CONDITION CODE = IF NET                                              
SORN     NTR1  BASE=*,LABEL=*                                                   
         L     RF,APARM                                                         
         L     RF,16(RF)           GET A(COMFACS)                               
         USING COMFACSD,RF                                                      
         GOTO1 CGETFACT,DMCB,(2,0)                                              
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         CLI   FAOVSYS,2           IF SPOT                                      
         BNE   *+8                                                              
         OI    ODDMNTS,SPOTSYS                                                  
         CLI   FAOVSYS,3           IF NET                                       
         BNE   *+8                                                              
         OI    ODDMNTS,NETSYS                                                   
         CLI   FAOVSYS,3           2=SPOT,3=NET                                 
         BNE   *+6                                                              
         SR    R1,R1                                                            
         LTR   R1,R1                                                            
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
CHKACC   NTR1  BASE=*,LABEL=*                                                   
         LA    R4,SPTREC                                                        
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKNUM,USRID                                                    
         GOTO1 DATAMGR,DMCB,(0,DMREAD),CTFILE,(R4),(R4)                         
         CLI   8(R1),0                                                          
         BNE   CHKACCNO                                                         
         LA    RE,CTIDATA                                                       
*                                                                               
CHKACC10 CLI   0(RE),0             SEARCH FOR SYSTEM ELEMENT                    
         BE    CHKACCNO                                                         
         CLI   0(RE),X'21'                                                      
         BNE   *+12                                                             
         CLI   2(RE),X'06'         TEST ACCOUNTING                              
         BE    CHKACCEQ            HAVE ACCESS                                  
*                                                                               
         ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     CHKACC10                                                         
*                                                                               
CHKACCEQ SR    R1,R1                                                            
CHKACCNO LTR   R1,R1                                                            
         XIT1                                                                   
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
CHKSX    NTR1  BASE=*,LABEL=*                                                   
         CLC   AGY,=C'WI'                                                       
         BE    CHKSXOK                                                          
         CLC   AGY,=C'BN'                                                       
         BE    CHKSXOK                                                          
         CLC   AGY,=C'OO'                                                       
         BE    CHKSXOK                                                          
         CLI   DDS,1               ALLOW FOR DDS                                
         BE    CHKSXOK                                                          
         XC    TEMP(50),TEMP       CHECK PROFILE FOR ACCESS                     
         MVC   TEMP+30(4),=C'SOSX'                                              
         MVC   TEMP+34(2),RAGY     ONLY AGY LEVEL PROFILE                       
         GOTO1 GETPROF,DMCB,(0,TEMP+30),TEMP,DATAMGR                            
         CLI   TEMP+1,C'Y'                                                      
         BNE   *+6                                                              
CHKSXOK  SR    R1,R1                                                            
CHKSXNO  LTR   R1,R1                                                            
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*        THIS TABLE CONTAINS THE ROUTINE ADDRESS FOR EACH REQUEST THAT          
*        REQUIRES DEFAULT VALUES                                                
*                                                                               
REQROUTS DS    0CL5                                                             
         DC    AL1(03,0),AL3(REQRPH)        FOR PH REPORT                       
         DC    AL1(04,0),AL3(REQRJV)        FOR JV REPORT                       
         DC    AL1(09,0),AL3(REQR09)                                            
         DC    AL1(10,0),AL3(REQRCR)        COKE CATEGORY REPT                  
         DC    AL1(12,0),AL3(REQR07)        SPOT UNBILLING                      
         DC    AL1(13,0),AL3(REQR07)        NET UNBILLING                       
         DC    AL1(14,0),AL3(REQR14)        GT EXTRACT                          
         DC    AL1(15,0),AL3(REQR15)        SS SUPERDESK                        
         DC    AL1(16,0),AL3(REQR46)        JUST PUT IN "ALL" CLIENT            
         DC    AL1(17,0),AL3(REQR17)                                            
         DC    AL1(18,0),AL3(REQR23)        SAME AS 23                          
         DC    AL1(19,0),AL3(REQR55)        SAME AS 55                          
         DC    AL1(20,0),AL3(REQR55)        SAME AS 55                          
         DC    AL1(22,0),AL3(REQR23)        SAME AS 23                          
         DC    AL1(23,0),AL3(REQR23)                                            
         DC    AL1(24,0),AL3(REQR55)        SAME AS 55                          
         DC    AL1(26,0),AL3(REQR46)        JUST PUT IN "ALL" CLIENT            
         DC    AL1(27,0),AL3(REQR55)         SAME AS 55                         
         DC    AL1(28,0),AL3(REQR55)         SAME AS 55  (TD)                   
         DC    AL1(32,0),AL3(REQR32)                                            
         DC    AL1(42,0),AL3(REQR42)         CH                                 
         DC    AL1(43,0),AL3(REQR42A)        PZ - SAME AS PART of CH            
         DC    AL1(44,0),AL3(REQR46)         SAME AS 46                         
         DC    AL1(45,0),AL3(REQR46)         SAME AS 46                         
         DC    AL1(46,0),AL3(REQR46)                                            
         DC    AL1(48,0),AL3(REQR46)         SAME AS 46                         
         DC    AL1(49,0),AL3(REQR46)         SAME AS 46                         
         DC    AL1(52,0),AL3(REQR46)                                            
         DC    AL1(53,0),AL3(REQR53)                                            
         DC    AL1(54,0),AL3(REQR53A)                                           
         DC    AL1(55,0),AL3(REQR55)                                            
         DC    AL1(56,0),AL3(REQR56)                                            
         DC    AL1(57,0),AL3(REQR57)                                            
         DC    AL1(59,0),AL3(REQR43)                                            
         DC    AL1(74,0),AL3(REQR46)         SAME AS 46                         
         DC    AL1(76,0),AL3(REQR76)                                            
         DC    AL1(80,0),AL3(REQR80)                                            
         DC    AL1(86,0),AL3(REQR86)                                            
         DC    AL1(90,0),AL3(REQR90)                                            
         DC    AL1(91,0),AL3(REQR91)                                            
****     DC    AL1(92,0),AL3(REQR92)                                            
         DC    AL1(96,0),AL3(REQR96)                                            
         DC    AL1(98,0),AL3(REQR98)                                            
         DC    AL1(101,0),AL3(REQR101)                                          
         DC    AL1(105,0),AL3(REQR105)                                          
         DC    AL1(106,0),AL3(REQR111B)                                         
         DC    AL1(109,0),AL3(REQR109)                                          
         DC    AL1(104,0),AL3(REQR46)                                           
         DC    AL1(111,0),AL3(REQR111)                                          
         DC    AL1(114,0),AL3(REQR17)                                           
         DC    AL1(118,0),AL3(REQR86)                                           
         DC    AL1(119,0),AL3(REQR98A)                                          
         DC    AL1(125,0),AL3(REQR125)                                          
         DC    AL1(131,0),AL3(REQR46)       SAME AS 46                          
         DC    AL1(134,0),AL3(REQR46)       SAME AS 46                          
         DC    AL1(138,0),AL3(REQR86)       SAME AS 86                          
         DC    AL1(140,0),AL3(REQR86)       SAME AS 86                          
         DC    AL1(142,0),AL3(REQR86)        SAME AS 86                         
         DC    AL1(151,0),AL3(REQR151)                                          
         DC    AL1(156,0),AL3(REQR156)                                          
         DC    AL1(158,0),AL3(REQR109)       SAME AS 109                        
         DC    AL1(160,0),AL3(REQR98A)                                          
         DC    AL1(161,0),AL3(REQR98A)                                          
         DC    AL1(162,0),AL3(REQR98A)                                          
         DC    AL1(167,0),AL3(REQR17)      SAME AS 17                           
         DC    AL1(168,0),AL3(REQR98A)                                          
         DC    AL1(170,0),AL3(REQR98A)                                          
         DC    AL1(171,0),AL3(REQR80)        SAME AS 80                         
         DC    AL1(172,0),AL3(REQR80)        SAME AS 80                         
         DC    AL1(192,0),AL3(REQR52)            SAME AS 52                     
         DC    AL1(193,0),AL3(REQR52)            SAME AS 52                     
         DC    AL1(178,0),AL3(REQR178)                                          
         DC    AL1(194,0),AL3(REQR194)                                          
         DC    AL1(195,0),AL3(REQR197)      SAME AS 197                         
         DC    AL1(196,0),AL3(REQR196)                                          
         DC    AL1(197,0),AL3(REQR197)                                          
         DC    AL1(199,0),AL3(REQR199)                                          
         DC    AL1(204,0),AL3(REQR14)       WB - SAME AS GT                     
*******  DC    AL1(205,0),AL3(REQR55)       IN - WAS SAME AS 55                 
         DC    AL1(206,0),AL3(REQR206)                                          
         DC    AL1(207,0),AL3(REQR196)      SAME AS B1                          
         DC    AL1(211,0),AL3(REQR86)       SAME AS 86                          
         DC    AL1(221,0),AL3(REQR17)       SAME AS 17                          
         DC    AL1(228,0),AL3(REQR228)                                          
         DC    AL1(232,0),AL3(REQR23)       SAME AS 23                          
         DC    AL1(233,0),AL3(REQR233)                                          
         DC    AL1(236,0),AL3(REQR236)                                          
         DC    AL1(240,0),AL3(REQR80)        SAME AS 80                         
REQROUTX DC    AL1(00,0)                                                        
         EJECT                                                                  
*        EACH ENTRY IN THIS TABLE DEFINES THE TEXT OF A PROTECTED               
*        DATA FIELD. THE ENTRY CAN ALSO DEFINE THE LENGTH OF AN                 
*        ASSOCIATED UNPROTECTED INPUT FIELD. ENTRY FORMAT IS :                  
*        AL1   ENTRY NUMBER                                                     
*        AL1   PROTECTED FIELD LENGTH (=P)                                      
*        AL1   UNPROTECTED FIELD LENGTH                                         
*        CLP   PROTECTED FIELD DATA                                             
*                                                                               
*                                                                               
TWATBL   CSECT                                                                  
*                                                                               
         DC    AL1(002,11,10),C'CLIENT CODE'                                    
         DC    AL1(003,12,03),C'PRODUCT CODE'                                   
         DC    AL1(004,12,11),C'PRODUCT CODE'                                   
         DC    AL1(005,12,09),C'ESTIMATE NUM'                                   
         DC    AL1(006,10,07),C'MARKET NUM'                                     
         DC    AL1(007,12,40),C'STATION CODE'                                   
         DC    AL1(008,07,03),C'REP NUM'                                        
         DC    AL1(009,15,17),C'START,END DATES'                                
         DC    AL1(010,16,14),C'MONTH OF SERVICE'                               
         DC    AL1(011,12,09),C'INVOICE DATE'                                   
         DC    AL1(012,16,09),C'PAY PERIOD START'                               
         DC    AL1(013,14,09),C'PAY PERIOD END'                                 
***      DC    AL1(014,08,09),C'BOOK-HUT'                                       
         DC    AL1(014,08,25),C'BOOK-HUT'                                       
         DC    AL1(015,15,01),C'REVISIONS ONLY?'                                
         DC    AL1(016,16,01),C'ALLOCATION BASIS'                               
         DC    AL1(017,15,03),C'FLAG PERCENTAGE'                                
         DC    AL1(018,18,01),C'INCLUDE ALL MEDIA?'                             
**       DC    AL1(019,17,01),C'PROC BY DIVISION?'                              
**       DC    AL1(020,17,01),C'PROC BY DISTRICT?'                              
         DC    AL1(021,13,01),C'PRIOR MONTHS?'                                  
         DC    AL1(022,13,01),C'LATER MONTHS?'                                  
         DC    AL1(023,11,01),C'MARK FILES?'                                    
         DC    AL1(024,15,01),C'ORDERED OR PAID'                                
         DC    AL1(025,12,01),C'DATA COMPARE'                                   
         DC    AL1(26,04,01),C'TYPE'                                            
         DC    AL1(027,16,01),C'ALLOCATE EXCESS?'                               
         DC    AL1(028,16,01),C'EXCEPTIONS ONLY?'                               
         DC    AL1(29,05,01),C'STAGE'                                           
         DC    AL1(030,14,01),C'DAYPART DETAIL'                                 
**       DC    AL1(31,13,01),C'REPLACE DATA?'                                   
         DC    AL1(032,12,01),C'ID SEQUENCE?'                                   
         DC    AL1(033,17,01),C'INCLUD PAID DATA?'                              
         DC    AL1(034,16,01),C'XCLUD AFFIDAVIT?'                               
         DC    AL1(035,11,01),C'AMOUNT TYPE'                                    
         DC    AL1(036,14,01),C'BILLING OPTION'                                 
         DC    AL1(037,13,01),C'ANALYSIS TYPE'                                  
         DC    AL1(038,12,01),C'PAID TOTALS?'                                   
         DC    AL1(039,17,01),C'AUTHORISD TOTALS?'                              
         DC    AL1(40,11,03),C'DEMOGRAPHIC'                                     
         DC    AL1(041,15,09),C'BROADCAST MONTH'                                
         DC    AL1(042,13,09),C'CURRENT MONTH'                                  
         DC    AL1(043,12,01),C'BUY DETAILS?'                                   
         DC    AL1(044,17,01),C'XCLUD CLI DETAIL?'                              
         DC    AL1(045,17,01),C'XCLUD NET AMOUNT?'                              
         DC    AL1(046,14,01),C'CLIENT TOTALS?'                                 
         DC    AL1(047,17,01),C'UNCONFIRMED ONLY?'                              
         DC    AL1(048,14,01),C'RATING SERVICE'                                 
         DC    AL1(049,13,01),C'AUDIENCE TYPE'                                  
         DC    AL1(50,10,04),C'DATA TYPES'                                      
         DC    AL1(051,13,01),C'TAPE EXTRACT?'                                  
         DC    AL1(052,17,01),C'TAPE BUYS = TEST?'                              
         DC    AL1(053,11,01),C'GOALS TAPE?'                                    
         DC    AL1(054,10,01),C'BUYS TAPE?'                                     
         DC    AL1(055,15,01),C'ANALYZE BY OFF?'                                
         DC    AL1(056,05,01),C'RERUN'                                          
***->    DC    AL1(057,14,01),C'BILL OVERRIDE?'                                 
         DC    AL1(057,15,02),C'REVISION NUMBER'                                
**       DC    AL1(058,17,01),C'BNP/PNB OVERRIDE?'                              
         DC    AL1(059,12,09),C'CUT-OFF DATE'                                   
         DC    AL1(060,13,01),C'ES-LIN ORDER?'                                  
         DC    AL1(061,07,01),C'SPACING'                                        
         DC    AL1(062,12,01),C'SPECIAL REP?'                                   
         DC    AL1(063,12,01),C'XCLUD COSTS?'                                   
         DC    AL1(064,12,01),C'XCLUD DEMOS?'                                   
         DC    AL1(065,14,02),C'DAYPART FILTER'                                 
         DC    AL1(066,14,16),C'OVERRIDE DEMOS'                                 
         DC    AL1(067,15,06),C'MARKET SEQUENCE'                                
         DC    AL1(068,09,01),C'FEE BASIS'                                      
         DC    AL1(069,12,01),C'PRINT OPTION'                                   
**       DC    AL1(70,13,01),C'-E BUYS ONLY?'                                   
         DC    AL1(71,14,09),C'DATE ON CHECKS'                                  
         DC    AL1(72,09,01),C'AFFILIATE'                                       
         DC    AL1(073,15,01),C'S REP BREAKOUT?'                                
         DC    AL1(74,14,09),C'CORPORATE BOOK'                                  
         DC    AL1(75,12,01),C'DAYPART CODE'                                    
         DC    AL1(76,05,16),C'DEMOS'                                           
         DC    AL1(77,15,03),C'TARGET AUDIENCE'                                 
         DC    AL1(78,10,01),C'XCLUD CODE'                                      
         DC    AL1(79,15,01),C'BASED ON AFFID?'                                 
         DC    AL1(80,11,01),C'REALLOCATE?'                                     
         DC    AL1(81,13,01),C'MKT ANALYSIS?'                                   
         DC    AL1(82,08,03),C'PROPERTY'                                        
         DC    AL1(83,13,01),C'OWNER FILTER?'                                   
         DC    AL1(84,13,01),C'DETAIL OPTION'                                   
         DC    AL1(85,11,01),C'RERATE TYPE'                                     
         DC    AL1(86,12,02),C'PROGRAM TYPE'                                    
         DC    AL1(87,12,01),C'TOTAL OPTION'                                    
         DC    AL1(88,15,01),C'DAYPART TOTALS?'                                 
         DC    AL1(89,16,01),C'DAYPART OVERRIDE'                                
         DC    AL1(90,16,01),C'WEEKLY ANALYSIS?'                                
         DC    AL1(91,15,01),C'MTHLY ANALYSIS?'                                 
         DC    AL1(92,15,01),C'BRAND ANALYSIS?'                                 
         DC    AL1(93,14,11),C'PRD OR PRD GRP'                                  
         DC    AL1(94,14,10),C'MKT OR MKT GRP'                                  
         DC    AL1(95,11,01),C'DATE OPTION'                                     
         DC    AL1(96,12,01),C'RECAP OPTION'                                    
         DC    AL1(97,11,01),C'DATA OPTION'                                     
         DC    AL1(98,14,01),C'REPLACE DEMOS?'                                  
         DC    AL1(99,16,01),C'REPLACE OVRIDES?'                                
         DC    AL1(100,14,01),C'EXTRACT GOALS?'                                 
         DC    AL1(101,13,01),C'EXTRACT BUYS?'                                  
         DC    AL1(102,16,01),C'EXTRACT DETAILS?'                               
         DC    AL1(103,13,01),C'XCLUD HIATUS?'                                  
         DC    AL1(104,15,01),C'EXTRACT AFFIDS?'                                
         DC    AL1(105,08,02),C'DUE DAYS'                                       
         DC    AL1(106,15,01),C'PRODUCT GROUPS?'                                
         DC    AL1(107,14,01),C'MARKET GROUPS?'                                 
         DC    AL1(108,13,02),C'MKTGRP SCHEME'                                  
         DC    AL1(109,13,01),C'LEVEL CONTROL'                                  
         DC    AL1(110,15,01),C'SUMMARIES ONLY?'                                
         DC    AL1(111,16,01),C'STATIONS PER MKT'                               
         DC    AL1(112,16,01),C'GOAL/AUTH TOTALS'                               
         DC    AL1(113,07,06),C'NETWORK'                                        
         DC    AL1(114,06,17),C'AMOUNT'                                         
         DC    AL1(115,12,01),C'BILLING TYPE'                                   
         DC    AL1(116,09,01),C'TEST RUN?'                                      
         DC    AL1(117,14,01),C'LIST UNPOSTED?'                                 
         DC    AL1(118,17,01),C'LIST PREV POSTED?'                              
         DC    AL1(119,15,01),C'ESTIMATE OPTION'                                
         DC    AL1(120,14,01),C'PRDS TOGETHER?'                                 
         DC    AL1(121,06,03),C'REPORT'                                         
         DC    AL1(122,16,01),C'SUPPRESS SPTLEN?'                               
         DC    AL1(123,13,01),C'SCHEME CHECK?'                                  
         DC    AL1(124,16,01),C'MULT TARGET MODE'                               
         DC    AL1(125,14,01),C'RPT BY MONTHS?'                                 
*                                                                               
*              DON'T USE 126 - CARD REQ                                         
*                                                                               
         DC    AL1(127,01,01),C' '                                              
         DC    AL1(128,15,01),C'RPT BY FLIGHTS?'                                
         DC    AL1(129,16,01),C'RPT BY QUARTERS?'                               
         DC    AL1(130,13,01),C'RPT BY YEARS?'                                  
         DC    AL1(131,11,01),C'RECORD TYPE'                                    
         DC    AL1(132,17,01),C'PRINT ALL BRANDS?'                              
         DC    AL1(133,13,04),C'PARTIAL MATCH'                                  
         DC    AL1(134,12,01),C'POST AFFIDS?'                                   
         DC    AL1(135,15,01),C'PRINT COMMENTS?'                                
         DC    AL1(136,16,01),C'PRINT NET AFFIL?'                               
         DC    AL1(137,17,09),C'PURCHASE END DATE'                              
         DC    AL1(138,15,34),C'REALLOCATE PRDS'                                
         DC    AL1(139,09,05),C'DEMO MENU'                                      
         DC    AL1(140,09,01),C'ADD BUYS?'                                      
         DC    AL1(141,07,04),C'REP NUM'                                        
         DC    AL1(142,16,08),C'DEMO MENU FILTER'                               
         DC    AL1(143,16,03),C'FILM TYPE FILTER'                               
         DC    AL1(144,04,05),C'SHOW'                                           
         DC    AL1(145,16,01),C'CHANGE INVOICES?'                               
         DC    AL1(146,12,01),C'SPILL OPTION'                                   
         DC    AL1(147,16,08),C'WEIGHT OVERRIDES'                               
         DC    AL1(148,10,04),C'SPOTLENGTH'                                     
         DC    AL1(149,10,06),C'SORT FIELD'                                     
         DC    AL1(150,13,01),C'BUYING PERIOD'                                  
         DC    AL1(151,07,01),C'CUTINS?'                                        
         DC    AL1(152,15,01),C'COST OVERRIDES?'                                
         DC    AL1(153,12,01),C'LOCAL DEMOS?'                                   
         DC    AL1(154,06,01),C'REGION'                                         
         DC    AL1(155,13,08),C'COMMERCIAL ID'                                  
         DC    AL1(156,13,01),C'PRODUCE FILE?'                                  
         DC    AL1(157,18,04),C'SPOT LENGTH FILTER'                             
         DC    AL1(158,14,01),C'GROSS/NET/BOTH'                                 
         DC    AL1(159,15,01),C'REPORT/LETTERS?'                                
         DC    AL1(160,06,09),C'MKTGRP'                                         
         DC    AL1(161,08,08),C'END DATE'                                       
         DC    AL1(162,17,01),C'SUPPRESS MKT DTL?'                              
         DC    AL1(163,17,01),C'SUPPRESS MATCHED?'                              
         DC    AL1(164,13,01),C'DETAIL FORMAT'                                  
         DC    AL1(165,17,01),C'GROSS/NET DOLLARS'                              
         DC    AL1(166,13,01),C'PERIOD FORMAT'                                  
         DC    AL1(167,10,03),C'TO PRODUCT'                                     
         DC    AL1(168,13,01),C'DOLLAR FORMAT'                                  
         DC    AL1(169,17,08),C'DOLLAR ADJUSTMENT'                              
         DC    AL1(170,14,08),C'BILL/PAY START'                                 
         DC    AL1(171,12,08),C'BILL/PAY END'                                   
         DC    AL1(172,9,20),C'OPTIONS ?'                                       
         DC    AL1(173,12,08),C'LETTER DATE?'                                   
         DC    AL1(174,15,01),C'XTRA GRID LINES'                                
         DC    AL1(175,16,01),C'CML CLASS DETAIL'                               
         DC    AL1(176,14,01),C'SUMMARY OPTION'                                 
         DC    AL1(177,19,01),C'UNKNOWN CML DETAILS'                            
         DC    AL1(178,13,01),C'ACTUAL BOOK ?'                                  
         DC    AL1(179,16,02),C'INCLUDE REPORTS?'                               
         DC    AL1(180,18,01),C'BILLED TODAY ONLY?'                             
         DC    AL1(181,15,01),C'BY SPOT LENGTH?'                                
         DC    AL1(183,18,01),C'FILM REPORT OPTION'                             
         DC    AL1(184,07,01),C'QUARTER'                                        
         DC    AL1(185,10,01),C'DOWNLOAD ?'                                     
         DC    AL1(186,11,01),C'30''S ONLY ?'                                   
         DC    AL1(187,15,01),C'STATION BY SIZE'                                
         DC    AL1(188,15,01),C'PRINT CHANNEL ?'                                
         DC    AL1(189,17,01),C'XCLUDE SPCL REP $'                              
         DC    AL1(190,09,01),C'DATA TYPE'                                      
         DC    AL1(191,17,01),C'INCLUDE $0 LETTER'                              
         DC    AL1(192,13,01),C'PRINT REPORT?'                                  
         DC    AL1(193,12,01),C'POST (Y/N) ?'                                   
         DC    AL1(194,15,01),C'SUPPRESS COSTS?'                                
         DC    AL1(195,7,06),C'       '                                         
         DC    AL1(196,11,01),C'BILL OPTION'                                    
         DC    AL1(198,14,01),C'LIST PREV CONV'                                 
         DC    AL1(199,16,01),C'REPLACE INVOICES'                               
         DC    AL1(200,08,01),C'AUTO I2?'                                       
         DC    AL1(201,06,01),C'      '          HIDDEN FIELD                   
**       DC    AL1(202,18,01),C'CURRENCY OVERRIDE?'                             
         DC    AL1(203,07,60),C'OPTIONS'                                        
         DC    AL1(204,09,01),C'COST TYPE'                                      
         DC    AL1(205,09,03),C'BILL TYPE'                                      
         DC    AL1(206,14,09),C'INTERFACE DATE'                                 
         DC    AL1(207,09,08),C'FILM CODE'                                      
         DC    AL1(208,16,01),C'CLIENT EXCLUSION'                               
         DC    AL1(209,12,01),C'POL BREAKOUT'                                   
****     DC    AL1(210,15,01),C'PRD CHANGE RPT '                                
         DC    AL1(210,15,01),C'DO NOT USE     '                                
         DC    AL1(211,14,01),C'STATION BREAKS'                                 
         DC    AL1(212,06,01),C'RERATE'                                         
         DC    AL1(213,16,01),C'STATION BY AFFIL'                               
         DC    AL1(214,04,09),C'DATE'                                           
         DC    AL1(215,09,01),C'DATE TYPE'                                      
         DC    AL1(216,14,13),C'BILL NUMBER(S)'                                 
         DC    AL1(217,16,04),C'AFFILIATE FILTER'                               
         DC    AL1(218,12,01),C'STATION TYPE'                                   
         DC    AL1(219,15,15),C'PRD CODE (/LEN)'                                
         DC    AL1(220,16,19),C'REALLOCATE PRD 1'                               
         DC    AL1(221,16,19),C'REALLOCATE PRD 2'                               
         DC    AL1(222,16,19),C'REALLOCATE PRD 3'                               
         DC    AL1(223,16,19),C'REALLOCATE PRD 4'                               
         DC    AL1(224,12,01),C'INCLUDE PB''S'                                  
**       DC    AL1(225,16,19),C'REALLOCATE PRD 5'                               
         DC    AL1(226,16,03),C'CML CLASS FILTER'                               
         DC    AL1(227,16,01),C'SUPPRESS -S WKLY'                               
         DC    AL1(228,16,09),C'PROD GRP        '                               
         DC    AL1(229,07,03),C'LINE NO'                                        
         DC    AL1(231,07,01),C'VERSION'                                        
         DC    AL1(232,11,01),C'NCT OPTION?'                                    
         DC    AL1(233,17,01),C'DEACTIVATED ONLY?'                              
         DC    AL1(234,17,01),C'CABLE BY SYSTEM ?'                              
         DC    AL1(235,12,20),C'PROGRAM NAME'                                   
         DC    AL1(236,13,01),C'EASI INVOICES'                                  
         DC    AL1(237,18,01),C'INCLUDE AFFADAVIT?'                             
         DC    AL1(238,14,03),C'TO CLIENT CODE'                                 
         DC    AL1(239,14,03),C'TO ESTIMATE   '                                 
         DC    AL1(240,10,10),C'PRD=WEIGHT'                                     
         DC    AL1(241,16,01),C'NO CHARGE SPOTS?'                               
         DC    AL1(242,17,01),C'PRE-EMPTED SPOTS?'                              
         DC    AL1(243,07,03),C'NEW EST'                                        
         DC    AL1(244,13,08),C'NEW PER START'                                  
         DC    AL1(245,11,03),C'NEW PRODUCT'                                    
         DC    AL1(246,13,03),C'PRINT COSTS ?'                                  
         DC    AL1(247,13,03),C'PRINT DEMOS ?'                                  
         DC    AL1(248,15,04),C'START INVOICE #'                                
         DC    AL1(249,13,04),C'END INVOICE #'                                  
         DC    AL1(250,06,05),C'SOURCE'                                         
         DC    AL1(251,15,01),C'SPILL MKT SORT?'                                
         DC    AL1(252,18,01),C'INCLUDE SPILL MKT?'                             
         DC    AL1(253,16,01),C'PRD SUMMARY FMT?'                               
TWATBLX  DC    X'0000'                                                          
*                                                                               
*              COMTBL ENTRIES SAME AS TWABLE ENTRIES                            
*              ODD COMMENTS APPEAR ON SAME LINE                                 
*              EVEN COMMENTS APPEAR ON NEXT LINE                                
*                                                                               
COMTBL   CSECT                                                                  
*                                                                               
*              DON'T USE 1,126,127                                              
*                                                                               
         DC    AL1(3,25,00),C'R=REPORT,L=LETTERS,B=BOTH'                        
         DC    AL1(2,15,00),C'S=L+FAX,F=B+FAX'                                  
         DC    AL1(5,07,00),C'R=RERUN'                                          
**                                                                              
         DC    AL1(7,17,00),C'Y=YES,N=NO,O=ONLY'                                
         DC    AL1(9,24,00),C'H=DESCENDING,L=ASCENDING'                         
         DC    AL1(10,12,00),C'A=ALPHABETIC'                                    
         DC    AL1(11,16,00),C'INVOICE RUN DATE'                                
         DC    AL1(13,21,00),C'1 INVOICE NUMBER ONLY'                           
         DC    AL1(15,17,00),C'GROSS $ (HEADERS)'                               
         DC    AL1(17,17,00),C'T=TEST WITH TRACE'                               
         DC    AL1(19,22,00),C'MONTH OF SERVICE RANGE'                          
         DC    AL1(21,18,00),C'BILLING DATE RANGE'                              
         DC    AL1(23,24,00),CL24'OLD MARKET NUM'                               
         DC    AL1(25,24,00),CL24'NEW MARKET NUM'                               
         DC    AL1(27,24,00),C'D=DUE DATE, S=START DATE'                        
         DC    AL1(29,21,00),C'PRINT TOTALS ONLY Y/N'                           
         DC    AL1(31,12,00),C'CONTROL DATE'                                    
         DC    AL1(33,10,00),C'S=STATIONS'                                      
         DC    AL1(35,20,00),C'ENTER BUYLINE NUMBER'                            
         DC    AL1(37,24,00),CL24'OVERRIDE HOME MARKET'                         
         DC    AL1(39,13,00),C'N=LIVE,Y=TEST'                                   
*                                                                               
*                                                                               
         DC    AL1(128,24,00),C'                        '                       
         DC    AL1(129,24,00),C'                        '                       
         DC    AL1(130,15,00),C'OLD EST-NEW EST'                                
         DC    AL1(131,28,00),C'D=DAY,N=NIGHT,S=N SUSTAINING'                   
         DC    AL1(133,34,00),C'N=SUPPRESS DELIVERY,C=PRINT CLIENT'             
         DC    AL1(134,11,00),C'G=GOAL ONLY'                                    
         DC    AL1(135,30,00),C'Y=SUPPRESS MARKET GROUP TOTALS'                 
         DC    AL1(137,13,00),C'G=GOAL,A=AUTH'                                  
         DC    AL1(139,24,00),C'N=NETWORK,S=SHOW,L=SPILL'                       
         DC    AL1(141,27,00),C'(RATING SRVC FOR SPILL RPT)'                    
         DC    AL1(143,19,00),C'(SHOW FOR SHOW RPT)'                            
         DC    AL1(145,15,00),C'U=UNPAID REPORT'                                
         DC    AL1(147,07,00),C'4,5,6,7'                                        
         DC    AL1(149,37,00),C'B=BILLING,I=INV MATCH,M=MEDIA SUMMARY'          
         DC    AL1(150,33,00),C'P=PRD SUMMARY,*=SPECIAL,BLANK=ALL'              
         DC    AL1(151,34,00),C'S=SPOTS,$=DOLLARS,D=DEMOS(DEFAULT)'             
         DC    AL1(153,11,00),C'DEFAULT=YES'                                    
         DC    AL1(155,18,00),C'C=CLIENT,P=PRODUCT'                             
         DC    AL1(157,03,00),C'1-7'                                            
         DC    AL1(159,13,00),C'W,M,E,R,A,F,Q'                                  
         DC    AL1(161,14,00),C'1-7,L(DEFAULT)'                                 
         DC    AL1(163,37,00),C'1=GL VS PUR(DEF),2=GL ONLY,3=PUR ONLY'          
         DC    AL1(165,24,00),C'2=SHOW ESTIMATES,N=DON''T'                      
         DC    AL1(167,29,00),C'M=OMIT MTH OF SERVICE,N=DON''T'                 
         DC    AL1(169,35,00),C'INCLUDE 1=PRIOR,2=SUBSEQUENT,3=BOTH'            
         DC    AL1(171,37,00),C'1=UNPAID+STA,2=UNPAID ONLY,3=STA ONLY'          
         DC    AL1(173,06,00),C'MMM/YY'                                         
         DC    AL1(175,30,00),C'A=ACTIVE,I=INACTIVE,B=BILLABLE'                 
         DC    AL1(177,34,00),C'$=$ COLUMNS ONLY,D=DEMOS,ETC. ONLY'             
         DC    AL1(179,24,00),C'PRINT COSTS=Y,N OR BLANK'                       
         DC    AL1(181,31,00),C'Y=LIST CURRENT INVOICES,N=DON''T'               
         DC    AL1(183,21,00),C'1=PRIMARY,2=SECONDARY'                          
         DC    AL1(185,32,00),C'Y=SHOW $ ONLY FOR REQUESTED MTHS'               
         DC    AL1(187,09,00),C'B=BILLING'                                      
         DC    AL1(189,33,00),C'A=ALL,R=REPS,M=MARKETS,S=STATIONS'              
         DC    AL1(191,20,00),C'* OR 1-4 OF TLCPDWAF'                           
         DC    AL1(193,22,00),C'Y,N OR P=POST PARTIALS'                         
         DC    AL1(195,33,00),C'A=A2 COMMENTS,B=BILLING,BLANK=ALL'              
         DC    AL1(196,30,00),C'M=MEDIA COMMENTS,3=A3 COMMENTS'                 
         DC    AL1(197,10,00),C'Y,N,T=TEST'                                     
         DC    AL1(199,34,00),C'P=PURCHASED ONLY,S=+STATION DETAIL'             
         DC    AL1(201,16,00),C'NEW CALL LETTERS'                               
         DC    AL1(203,18,00),C'Y,N OR Q=QUARTERLY'                             
         DC    AL1(205,33,00),C'FOR EXCPTNS ONLY-PRECEDE WITH ''E'''            
         DC    AL1(207,34,00),C'Y,N OR WEEK 1-9,A=10,B=11,...,E=14'             
         DC    AL1(209,17,00),C'$=DOLLARS,D=DEMOS'                              
         DC    AL1(211,14,00),C'SEE K4 PROFILE'                                 
         DC    AL1(213,14,00),C'BILL RUN DATES'                                 
         DC    AL1(215,33,00),C'ENTER 6 CHARS - 1=STA,2=SEQ,3=DAY'              
         DC    AL1(216,31,00),C'4=TIME,5=DAYPART,6=PROGRAM TYPE'                
         DC    AL1(217,19,00),C'N=NETPAK UNITS ONLY'                            
         DC    AL1(219,5,00),C'MMMYY'                                           
         DC    AL1(221,10,00),C'DEFAULT=NO'                                     
         DC    AL1(223,16,00),C'Y=SUPPRESS SPILL'                               
         DC    AL1(225,26,00),C'P=PAY PERIOD,B=BILL PERIOD'                     
         DC    AL1(227,13,00),C'DEFAULT=TODAY'                                  
         DC    AL1(229,26,00),C'E=SPOT SPENDING,S=SCHEDULE'                     
         DC    AL1(230,13,00),C'P=PERFORMANCE'                                  
         DC    AL1(231,34,00),C'A=AOR ONLY,X=NO AOR,C=COMMISS ONLY'             
         DC    AL1(232,35,00),C'N=NO COMMISS ONLY,S=SOON BILLS ONLY'            
         DC    AL1(233,09,00),C'Y=AUTO I2'                                      
         DC    AL1(235,27,00),C'T,I,U,B,S,X,E,A,L,O,1,2,3,4'                    
         DC    AL1(236,32,00),C'(1=T+I,2=SPECIALS,3=2+T+I,4=T+2)'               
         DC    AL1(237,25,00),C'1=MARKET,2=CITY,3=SYSCODE'                      
         DC    AL1(239,22,00),C'Y=YES,N=NO,O=ONLY EASI'                         
         DC    AL1(241,20,00),C'M=MONTH,A=DATE ADDED'                           
         DC    AL1(243,23,00),C'F=FINAL,R=REVISED FINAL'                        
COMTBLX  DC    X'0000'                                                          
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPREQSAVE                                                      
*                                                                               
       ++INCLUDE SPREQTEMP                                                      
*                                                                               
       ++INCLUDE SPREQFFBD                                                      
*                                                                               
         EJECT                                                                  
       ++INCLUDE FLDIND                                                         
         SPACE 2                                                                
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAUTL                                                          
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE FAXTRAINF                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'186SPREQ01   02/26/20'                                      
         END                                                                    
