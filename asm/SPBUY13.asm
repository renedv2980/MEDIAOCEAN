*          DATA SET SPBUY13    AT LEVEL 065 AS OF 10/17/19                      
*PHASE T21113C                                                                  
                                                                                
*====================================================================           
* DEC/13  MHER NOP NEW CABLE MAKEGOOD CODE FOR EXPANDED CODES                   
*         AFFECTED CODE IS MARKED WITH **CBL**                                  
* SEP/13  MHER CABLE CROSS-NETWORK MAKEGOODS                                    
* APR/05  MHER FOR CANAD NTWK, EXTRACT ZERO/LOOKUP FROM SPGENDOV REC            
*====================================================================           
                                                                                
         TITLE 'T21113 - SPOTPAK BUY - CHANGE LOGIC II'                         
T21113   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21113,RR=R8                                                   
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T21113+4096,R9                                                   
*                                                                               
         L     RC,0(R1)                                                         
         USING SPBUYWKD,RC                                                      
         L     RA,VBUYSAVE                                                      
         USING BUYSAVE,RA                                                       
         L     R3,VBUYTWA                                                       
         USING T211FFD,R3                                                       
*                                                                               
         C     R8,RELO                                                          
         BE    HAVERELO                                                         
         L     RF,VCOMFACS                                                      
         L     RF,CPROTOFF-COMFACSD(RF)                                         
         BASR  RE,RF                                                            
         ST    R8,RELO                                                          
         L     RF,VCOMFACS                                                      
         L     RF,CPROTON-COMFACSD(RF)                                          
         BASR  RE,RF                                                            
*                                                                               
HAVERELO CLI   EDTVAL,LKUPEDT      FOR CANAD NTWK ONLY                          
         BE    CHG500                                                           
*                                                                               
         MVI   ELCDLO,5            SET SRCH ARGS                                
         MVI   ELCDHI,5                                                         
         XC    BUINDREF,BUINDREF   CLEAR IND REF LINE NUM                       
         XC    BUSVMAS,BUSVMAS     CLEAR REAL MASTER LINE NUM                   
         MVI   DMINBTS,X'C0'       SET FOR NO DELETES                           
*                                                                               
         MVI   RCLOPT,RCLREF                                                    
         MVC   BYTE,BUREFTYP                                                    
         NI    BYTE,X'0F'          DROP 2-BYTE LINE FLAG                        
         CLI   BYTE,2              PKG MSTR/SLV                                 
         BNH   CHG100                                                           
         CLI   BYTE,6              REV MSTR/SLV                                 
         BNH   CHG200                                                           
         CLI   BYTE,8              MG MSTR/SLV                                  
         BNH   CHG300                                                           
         DC    H'0'                                                             
         EJECT                                                                  
EQXIT    CR    RB,RB                                                            
         J     *+6                                                              
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
BUYERR   GOTO1 ERROR                                                            
RELO     DC    A(0)                                                             
         EJECT                                                                  
CHG100   DS    0H                  PKG                                          
CHG200   DS    0H                  REV                                          
*                                                                               
         BAS   RE,PKG                                                           
*                                                                               
         OC    SVNDEF(16),SVNDEF   TEST CANAD NTWK                              
         BNZ   CHG150                                                           
         B     CHGX                                                             
*                                                                               
CHG125   DS    0H                                                               
         DC    H'0'                                                             
* FOR MASTER EST REPEAT FOR EACH SUB-EST *                                      
         SPACE 1                                                                
CHG150   MVC   DUB(4),AREC1        FROM                                         
         MVC   DUB+4(4),AREC3      TO                                           
         GOTO1 MOVEREC                                                          
*                                                                               
         L     R6,AREC3                                                         
         LA    R6,24(R6)                                                        
CHG152   MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   CHGX                                                             
* READ EXPLODED KEY/REC                                                         
         MVC   KEY,SVKEY                                                        
         MVC   KEY+4(5),2(R6)      MKT/STA                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   CHG152                                                           
         MVC   BUEXPKEY,KEY                                                     
         GOTO1 GETREC                                                           
*                                                                               
         BAS   RE,PKG              PKG DOES OWN GETPKGEL CALL                   
         B     CHG152                                                           
         SPACE 2                                                                
CHG300   DS    0H                                                               
         XC    BUEXPKEY,BUEXPKEY                                                
         OC    SVNDEF(16),SVNDEF                                                
         BNZ   CHG310                                                           
*                                                                               
         BAS   RE,MG                                                            
         B     CHGX                                                             
*                                                                               
CHG310   XC    SVCUTLST,SVCUTLST                                                
         MVC   SVCUTLST(5),SVKEY+4       MOVE MKT/STA                           
         MVC   SVCUTLST+5(14),BUMGDATA   SAVE MG INPUT DATA                     
         BAS   RE,MG                                                            
         MVC   SVCUTLST+20(2),BUMGCODE   SAVE EBCDIC MG CODE USED               
         MVC   SVCUTLST+22(1),BUMGBCOD    AND BINARY EQUIVALENT                 
*                                                                               
         MVC   DUB(4),AREC1                                                     
         MVC   DUB+4(4),AREC3                                                   
         GOTO1 MOVEREC                                                          
*                                                                               
         L     R6,AREC3                                                         
         LA    R6,24(R6)                                                        
*                                                                               
CHG320   GOTO1 VDATAMGR,DMCB,=C'DMUNLK',=C'SPTFILE'                             
         GOTO1 (RF),(R1),,=C'SPTDIR'                                            
*                                                                               
         MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   CHGX                                                             
*                                                                               
         MVC   KEY,SVKEY                                                        
         MVC   KEY+4(5),2(R6)                                                   
         MVC   SVCUTLST(5),2(R6)                                                
         MVC   BUMGDATA,SVCUTLST+5     RESTORE ORIGINAL DATA                    
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   CHG320                                                           
*                                                                               
         MVC   BUEXPKEY,KEY                                                     
         GOTO1 GETREC                                                           
         BRAS  RE,GETPKGEL                                                      
*                                                                               
         BAS   RE,MG                                                            
         B     CHG320                                                           
*                                                                               
CHGX     MVC   KEY,SVKEY           RE-READ SLAVE TO DISPLAY                     
         GOTO1 GETREC              SO NO NEED FOR GETPKGEL                      
         BRAS  RE,GETPKGEL                                                      
         TM    SVESTFL1,EF1NMG     TEST NEW MG                                  
         BO    *+8                                                              
         MVI   RCLOPT,RCLREF                                                    
         B     EXIT                                                             
         EJECT                                                                  
PKG      NTR1                                                                   
         MVC   BUREP,BDREP         SAVE SPECIAL REP                             
         XC    BUPROG(12),BUPROG                                                
         BAS   RE,GET70EL                                                       
         ICM   R6,15,FULL                                                       
         BZ    *+10                                                             
         MVC   BUPROG(12),3(R6)                                                 
*                                                                               
         BRAS  RE,GETPKGEL                                                      
         BE    PKG1                                                             
* NO PKGEL - UNREF REQUEST NOT VALID                                            
         MVI   ERRCD,NOTREFD                                                    
         OC    BUREFMAS,BUREFMAS                                                
         BZ    BUYERR                                                           
         B     PKG2                                                             
* PKGEL - MUST NOT BE MASTER AND MUST BE SAME REF TYPE                          
PKG1     MVI   ERRCD,NOMASPKG                                                   
         TM    2(R6),X'01'         TEST MASTER                                  
         BO    BUYERR                                                           
         MVI   ERRCD,NOCHGREF                                                   
         CLC   BUREFTYP,2(R6)      TEST SAME REF TYPE                           
         BNE   BUYERR                                                           
         BAS   RE,TESTREF          TEST FOR INDIRECT REF TO THIS LINE           
         BNE   BUYERR                                                           
         OC    BUREFMAS,BUREFMAS   TEST UN-REF REQUEST                          
         BZ    PKG14                                                            
*                                                                               
* READ NEW MASTER LINE                                                          
*                                                                               
PKG2     MVC   WORK2+24(20),KEY    SAVE SLAVE KEY/DA                            
         MVC   KEY+11(2),BUREFMAS                                               
         GOTO1 HIGH                                                             
         MVI   ERRCD,NOTFOUND                                                   
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BUYERR                                                           
         GOTO1 GETREC                                                           
         BRAS  RE,GETPKGEL                                                      
*                                                                               
         MVI   ERRCD,WRPKGREP                                                   
         CLC   BUREP,BDREP         TEST MSTR/SLV HAVE SAME REP                  
         BNE   BUYERR                                                           
         MVI   ERRCD,NOMTCHID                                                   
         BAS   RE,GET70EL                                                       
         ICM   R6,15,FULL                                                       
         BZ    PKG2A               NO ID IN MASTER                              
         CLC   3(12,R6),BUPROG     TEST MASTER/SLAVE HAVE SAME ID               
         BE    PKG2B                                                            
         OC    BUPROG(12),BUPROG   TEST SLAVE LINE HAD ID ELEMENT               
         BNZ   BUYERR              YES - THEN THEY SHOULD AGREE                 
         MVC   BUPROG,0(R6)        IF NOT, SAVE THE ELEM TO ADD LATER           
         B     PKG2B                                                            
*                                                                               
PKG2A    OC    BUPROG(12),BUPROG   TEST ID IN SLAVE                             
         BNE   BUYERR              ERROR - ID IN SLAVE BUT NOT MSTR             
*                                                                               
* UPDATE PKGEL IN NEW MASTER                                                    
*                                                                               
PKG2B    MVI   ELCDLO,5                                                         
         MVI   ELCDHI,5                                                         
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BE    *+12                                                             
         BAS   RE,NEWMAS                                                        
         B     PKG10                                                            
*                                                                               
PKG4     TM    2(R6),X'01'         IS LINE A MASTER                             
         BO    PKG8                YES                                          
         CLC   3(2,R6),SVKEY+11    TEST CIRCULAR REFERENCE                      
         BNE   *+12                                                             
         MVI   ERRCD,REFERR                                                     
         B     BUYERR                                                           
* REF LINE IS ITSELF A SLAVE - READ INDIRECT MASTER                             
         MVC   BUINDREF(2),BUREFMAS                                             
         MVC   KEY+11(2),3(R6)                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
         BRAS  RE,GETPKGEL                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         B     PKG4                                                             
         EJECT                                                                  
* ADD LINE TO PKGEL                                                             
*                                                                               
PKG8     MVI   ELCDLO,5                                                         
         MVI   ELCDHI,5                                                         
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,ADDPKGLN                                                      
*                                                                               
PKG10    MVI   BUWHY,X'41'                                                      
         GOTO1 SETCHGDT                                                         
         BRAS  RE,SETPKGEL                                                      
         GOTO1 PUTREC                                                           
         OI    SVUPDATE,X'40'      SET ADDS INTFC FLAG                          
*                                                                               
* REREAD SLAVE LINE                                                             
*                                                                               
PKG12    MVC   KEY(20),WORK2+24    RESTORE KEY/DA                               
         GOTO1 GETREC                                                           
         BRAS  RE,GETPKGEL                                                      
         BE    PKG14                                                            
* BUILD AND INSERT SLAVE ELEM                                                   
         BAS   RE,NEWSLV                                                        
*                                                                               
         CLI   BUPROG,X'70'        TEST SAVED ID ELEMENT                        
         BNE   PKG12X                                                           
*                                                                               
         SR    R0,R0                                                            
         LA    R6,BDELEM                                                        
*                                                                               
PKG12A   IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   PKG12A                                                           
         MVC   ELEM(18),BUPROG                                                  
         BAS   RE,ADDEL                                                         
*                                                                               
PKG12X   MVI   BUWHY,X'41'                                                      
         GOTO1 SETCHGDT                                                         
         BRAS  RE,SETPKGEL                                                      
         GOTO1 PUTREC                                                           
         OI    SVUPDATE,X'40'      SET ADDS INTFC FLAG                          
         B     EXIT                                                             
*                                                                               
* SLAVE IS A REF LINE                                                           
*                                                                               
PKG14    XC    HALF,HALF           SAVE OLD MASTER                              
         MVC   HALF,3(R6)                                                       
*                                                                               
         MVC   3(2,R6),BUREFMAS    SET NEW MASTER                               
         OC    BUREFMAS,BUREFMAS   TEST UN-REF                                  
         BNZ   PKG16                                                            
         MVI   ERRCD,NOCHGREF                                                   
         BAS   RE,DELEL                                                         
*                                                                               
PKG16    MVI   BUWHY,X'41'                                                      
         GOTO1 SETCHGDT                                                         
         BRAS  RE,SETPKGEL                                                      
         GOTO1 PUTREC                                                           
         OI    SVUPDATE,X'40'      SET ADDS INTFC FLAG                          
         EJECT                                                                  
* UPDATE OLD MASTER                                                             
*                                                                               
         MVC   KEY+11(2),HALF                                                   
PKG18    GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
         BRAS  RE,GETPKGEL                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    2(R6),X'01'         IS OLD MASTER A MASTER                       
         BO    PKG20               YES                                          
         MVC   KEY+11(2),3(R6)                                                  
         B     PKG18                                                            
*                                                                               
PKG20    BAS   RE,DELPKGLN                                                      
         MVI   BUWHY,X'41'                                                      
         GOTO1 SETCHGDT                                                         
         BRAS  RE,SETPKGEL                                                      
         GOTO1 PUTREC                                                           
         OI    SVUPDATE,X'40'      SET ADDS INTFC FLAG                          
         B     EXIT                                                             
         EJECT                                                                  
*=================================================================              
* BUILD AND INSERT NEW REF EL                                                   
*=================================================================              
                                                                                
NEWMAS   NTR1                                                                   
         ZIC   R0,BUREFTYP         MASTER CODE = SLAVE CODE-1                   
         BCTR  R0,0                                                             
         STC   R0,ELEM+2                                                        
         MVC   ELEM+3(2),SVKEY+11  SET SLAVE LINE                               
         B     NEWREF                                                           
*                                                                               
NEWSLV   NTR1                                                                   
         MVC   ELEM+2(1),BUREFTYP                                               
         MVC   ELEM+3(2),BUREFMAS                                               
*                                                                               
NEWREF   MVI   ELEM,5                                                           
         MVI   ELEM+1,5                                                         
* INSERT IN REC                                                                 
         LA    R6,BDELEM                                                        
         SR    R0,R0                                                            
NEWREF10 IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    NEWREF20                                                         
         CLI   0(R6),6                                                          
         BL    NEWREF10                                                         
* NEVER PUT PKGEL BETWEEN 02 AND 22 OR 03 AND 23 !!!                            
         CLI   0(R6),X'22'                                                      
         BE    NEWREF10                                                         
         CLI   0(R6),X'23'                                                      
         BE    NEWREF10                                                         
*                                                                               
NEWREF20 BRAS  RE,ADDEL                                                         
         B     EXIT                                                             
         EJECT                                                                  
*====================================================================           
* OLD PKG MASTER ELEM IS AT 0(R6)                                               
*====================================================================           
                                                                                
ADDPKGLN NTR1                                                                   
*                                                                               
         MVC   ELEM(3),0(R6)       CODE/LEN/TYPE                                
         LLC   R0,1(R6)                                                         
         AHI   R0,-3                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         SRL   R0,1                AND DIVIDE BY 2                              
*                                                                               
         LA    R4,ELEM+3                                                        
         LA    R7,3(R6)                                                         
*                                                                               
ADDPKG2  MVC   0(2,R4),0(R7)                                                    
         CLC   BUINDREF(2),0(R4)   MATCH INDIRECT REF                           
         BNE   *+14                                                             
         LA    R4,2(R4)                                                         
         MVC   0(2,R4),SVKEY+11         SET SLAVE LINE                          
*                                                                               
ADDPKG4  LA    R4,2(R4)                                                         
         LA    R7,2(R7)                                                         
         BCT   R0,ADDPKG2                                                       
         MVC   0(2,R4),SVKEY+11    THIS WONT BE INCLDED ON IND REF              
* UPDATE LEN                                                                    
         IC    RE,ELEM+1                                                        
         LA    RE,2(RE)                                                         
         STC   RE,ELEM+1                                                        
* DELETE OLD ELEM                                                               
ADDPKGX  BAS   RE,DELEL                                                         
*                                                                               
         CLI   ELEM+1,3            TEST NO LINES LEFT IN PKG                    
         BE    EXIT                                                             
* ADD NEW ELEM                                                                  
         BAS   RE,ADDEL                                                         
*                                                                               
         MVC   BUSVMAS(2),BUYREC+10 MOVE ACTUAL MASTER LINE                     
         B     EXIT                                                             
         EJECT                                                                  
* DELETE SLAVE LINE FROM PKGEL AT 0(R6)                                         
*                                                                               
DELPKGLN NTR1                                                                   
         MVC   ELEM(256),0(R6)                                                  
         ZIC   R8,ELEM+1                                                        
         LA    R8,ELEM(R8)                                                      
         MVI   0(R8),0             SET E-O-L FLAG                               
*                                                                               
         LA    R4,ELEM+3                                                        
DELPKG2  CLC   0(2,R4),SVKEY+11    MATCH SLAVE                                  
         BE    DELPKG4                                                          
         LA    R4,2(R4)                                                         
         OC    0(2,R4),0(R4)                                                    
         BNE   DELPKG2                                                          
         DC    H'0'                                                             
DELPKG4  MVC   0(2,R4),2(R4)                                                    
         LA    R4,2(R4)                                                         
         OC    0(2,R4),0(R4)                                                    
         BNE   DELPKG4                                                          
* CHANGE ELEM LEN                                                               
         IC    RE,ELEM+1                                                        
         AHI   RE,-2                                                            
         STC   RE,ELEM+1                                                        
         B     ADDPKGX                                                          
         EJECT                                                                  
*=====================================================================          
* READ OLD PACKAGE MASTER LINE TO TEST FOR REFERENCES TO THIS LINE.             
* IF FOUND, TELL USER TO DELETE REFERENCES FIRST                                
* ON ENTRY ELCDLO=ELCDHI=X'05' AND R6 POINTS TO PKGEL IN SLAVE LINE             
*=====================================================================          
TESTREF  NTR1                                                                   
         MVC   WORK2(24),KEY       SAVE SLAVE LINE KEY/DA                       
*                                                                               
TESTREF2 MVC   KEY+11(2),3(R6)     MASTER LINE NUM                              
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
* FIND PKG EL                                                                   
         BRAS  RE,GETPKGEL                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    2(R6),X'01'         TEST MASTER                                  
         BZ    TESTREF2            NO                                           
* FIND THIS SLAVE LINE IN MASTER PKGEL                                          
         LLC   R0,1(R6)                                                         
         AHI   R0,-3                                                            
         SRL   R0,1                                                             
         LA    R6,3(R6)                                                         
         CLC   0(2,R6),WORK2+11                                                 
         BE    TESTREF4                                                         
         LA    R6,2(R6)                                                         
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
*                                                                               
TESTREF4 AHI   R0,-2                                                            
         LTR   R0,R0               TEST ANY MORE LINES IN PKG                   
         BZ    TESTREF6            NO - OURS IS LAST                            
* MORE LINES - SEE IF NEXT ONE POINTS TO US                                     
         MVC   KEY+11(2),0(R6)                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
         BRAS  RE,GETPKGEL                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   3(2,R6),WORK2+11                                                 
         BNE   TESTREF6                                                         
         MVI   ERRCD,NODELIND                                                   
         B     NEQXIT                                                           
*                                                                               
TESTREF6 MVC   KEY(20),WORK2       RESTORE KEY/DA                               
         GOTO1 GETREC                                                           
         BRAS  RE,GETPKGEL         RE-FIX PKGEL                                 
         B     EQXIT                                                            
         EJECT                                                                  
MG       NTR1                                                                   
         MVI   ERRCD,NOMGALWD                                                   
         TM    BDSTAT,X'80'        TEST POL NPW                                 
         BO    BUYERR                                                           
         TM    SVCOPT1,X'40'       TEST INFOMERCIAL CLIENT                      
         BO    BUYERR                                                           
         MVC   BUREP,BDREP                                                      
         XC    PRDLIST,PRDLIST     CLEAR ADDED PRD LIST                         
         XC    BUPROG(12),BUPROG                                                
         BAS   RE,GET70EL                                                       
         ICM   R6,15,FULL                                                       
         BZ    *+16                                                             
         MVC   BUPROG(12),3(R6)    SAVE SLAVE ID                                
         OC    BUPROG(12),SPACES                                                
*                                                                               
         OC    BUREFMAS,BUREFMAS   TEST C,MG=0 OR C,MG=A0                       
         BNZ   MG0B                NO                                           
         TM    SVESTFL1,EF1NMG     TEST NEW MG                                  
         BZ    MG0B                NO                                           
         OC    BDMGDATE,BDMGDATE   IF NEW MG, BDMGDATE MAY BE 0                 
         BZ    MG0A                SO BE CAREFUL                                
         CLI   BDMGDATE,X'C0'      TEST FOR OLD STYLE MG                        
         BL    MG0B                YES                                          
*                                                                               
MG0A     TM    SVESTFL1,EF1NMG     TEST NEW MG                                  
         BO    NMG                                                              
         MVI   ERRCD,NOTREFD                                                    
         B     BUYERR                                                           
*                                                                               
MG0B     TM    SVESTFL1,EF1NMG     TEST NEW MG                                  
         BO    NMG                                                              
         DC    H'0'                                                             
*                                                                               
NMG      BRAS  RE,NEWMG                                                         
         TM    VCALLBAS,X'80'      TEST CALLBASE MODE                           
         BZ    *+10                                                             
         MVC   BMGEMGCD,BUMGCODE   RETURN MG CODE                               
         B     EXIT                                                             
*=======================================================*                       
* RE-CALCULATION OF NTWK DEMO OVRDS FOR CANAD NTWK BUYS *                       
* FIRST, READ THEM IN                                   *                       
*=======================================================*                       
                                                                                
CHG500   LA    R6,BDELEM           NEED TO GET RGN PCTG FROM DEMO EL            
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),2                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ICM   R0,15,20(R6)        GET PCTG                                     
         BNZ   *+8                                                              
         L     R0,=F'100000'       IF NOT THERE, ASSUME WHOLE NETWORK           
         ST    R0,BUDUB                                                         
*                                                                               
         L     R0,AREC             SAVE ORIGINAL AREC                           
         MVI   ERRCD,NODEMREC                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D17'                                                  
         MVC   KEY+2(1),SVKEY      A-M                                          
         MVC   KEY+3(2),SVKEY+6    PACKED NETWORK                               
         MVC   KEY+5(2),SVCLT      CLIENT OVERRIDE                              
         MVC   KEY+7(4),BDPROGRM   PGM                                          
         MVC   KEY+11(1),SVCPROF+3  RTG SVC                                     
CHG500A  DS    0H                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BE    CHG500A1                                                         
         MVC   KEY,KEYSAVE         RESET KEY                                    
         XC    KEY+5(2),KEY+5      CLEAR CLIENT                                 
         XC    KEY+12(1),KEY+12    CLEAR SEQ NUM                                
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE                                                  
         BNE   BUYERR                                                           
*                                                                               
CHG500A1 MVC   AREC,AREC4                                                       
         GOTO1 GETREC                                                           
*                                                                               
         L     RE,AREC                                                          
         LA    RE,24(RE)                                                        
CHG500B  ZIC   RF,1(RE)            LOOK FOR SHOW OVERRIDE ELEMENT               
         AR    RE,RF                                                            
         CLI   0(RE),0                                                          
         BE    CHG500X                                                          
         CLI   0(RE),99                                                         
         BNE   CHG500B                                                          
* FOUND IT - NOW USE IT                                                         
         MVC   KEYSAVE,KEY         RESTORE KEY                                  
         MVC   KEY+7(4),2(RE)      SET PROGRAM                                  
         B     CHG500A                                                          
         EJECT                                                                  
* CHECK FOR SECOND DEMO OVRD REC *                                              
         SPACE 1                                                                
CHG500X  L     R5,AREC4                                                         
*                                                                               
         L     RE,AREC5                                                         
         XC    0(13,RE),0(RE)      CLEAR IN CASE NOT FOUND                      
         GOTO1 SEQ                                                              
         CLC   KEY(12),KEYSAVE                                                  
         BNE   CHG501                                                           
         MVC   AREC,AREC5                                                       
         GOTO1 GETREC                                                           
* NEED TO CHECK THERE IS SOMETHING IN RECORD                                    
         L     RE,AREC5                                                         
         LA    R6,24(RE)                                                        
         ZIC   RF,1(R6)                                                         
         AR    R6,RF                                                            
         CLI   0(R6),0             TEST FOR SECOND ELEMENT                      
         BNE   CHG501              GOT IT - GO ON                               
         XC    0(13,RE),0(RE)      ELSE PRETEND RECORD ISN'T THERE              
         B     CHG501X                                                          
*                                                                               
CHG501   L     R5,AREC5                                                         
*                                                                               
CHG501X  ST    R0,AREC             RESTORE ORIGINAL AREC                        
         MVC   KEY,SVKEY           RESTORE NETWORK KEY                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC              REREAD NETWORK BUY                           
         BRAS  RE,GETPKGEL                                                      
*                                                                               
         BAS   RE,GETDEMOS         GET NETWORK IMPS                             
         GOTO1 SETCHGDT                                                         
         BRAS  RE,SETPKGEL                                                      
         GOTO1 PUTREC                                                           
*                                                                               
         MVC   DUB(4),AREC1        FROM                                         
         MVC   DUB+4(4),AREC3      TO                                           
         GOTO1 MOVEREC             SAVE NETWORK BUY IN REC3                     
*                                                                               
         L     R6,AREC3                                                         
         LA    R6,24(R6)                                                        
         XC    NERRCD,NERRCD      AT END, WILL CHECK FOR DOVERR                 
*                                                                               
CHG502   MVI   ELCDLO,X'68'                                                     
         MVI   ELCDHI,X'68'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   CHG510                                                           
         LR    R7,R6               SAVE POINTER TO X'68' ELEM                   
         EJECT                                                                  
* NOW READ EXPLODED BUYS *                                                      
         SPACE 1                                                                
         MVC   KEY,SVKEY                                                        
         MVC   KEY+4(5),2(R6)      MOVE MKT/STA                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     SHOULD ALWAYS FIND EXPLODED BUY              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BUEXPKEY,KEY                                                     
         GOTO1 GETREC                                                           
         BRAS  RE,GETPKGEL                                                      
*                                                                               
         BAS   RE,GETDEMOS         RECALCULATE DEMO OVRDS                       
*                                                                               
         GOTO1 SETCHGDT                                                         
         BRAS  RE,SETPKGEL                                                      
         GOTO1 PUTREC                                                           
         B     CHG502                                                           
*                                                                               
CHG510   MVC   DUB(4),AREC3        FROM                                         
         MVC   DUB+4(4),AREC1      TO                                           
         GOTO1 MOVEREC                                                          
* CHECK FOR ANY LOOKUP ERRORS                                                   
         CLC   NERRCD,=AL2(DOVERR)                                              
         BE    BUYERR                                                           
         B     EXIT                                                             
         EJECT                                                                  
**************************************************************                  
*       SUBR TO EXTRACT DEMO VALUES FROM SAVED OVRD DATA     *                  
*                                                            *                  
*       OVERRIDE RECS(IN REC4/REC5) ELEMENT LENGTHS ARE      *                  
*                                                            *                  
*                 ....NEW.....                               *                  
*                                                            *                  
*        01      12 + 3*N'DEMOS                              *                  
*        02       2 + 5*N'DEMOS                              *                  
*        05       5 + 2*N'DEMOS                              *                  
*                                                            *                  
* R7 POINTS TO X'68' ELEM IN NETWORK BUY                     *                  
**************************************************************                  
         SPACE 1                                                                
GETDEMOS NTR1                                                                   
         XC    SVSPLMKT,SVSPLMKT   REBUILD ORIGINIATING DEMO ELEMENT            
         MVI   BUDEMSW,0           FORCE REBUILD OF DEMO ELEMENT                
         GOTO1 VBLDDEM                                                          
*                                                                               
         MVC   FULL,AREC4          SET CURRENT RECORD ADDRESS                   
*                                                                               
         XC    ELEM,ELEM           CLEAR WORK AREA (5 BYTES/DEMO)               
         LA    R2,ELEM             SET WORK AREA POINTER                        
*                                                                               
GETDEM2  L     R6,FULL             POINT TO CURRENT REC                         
         LA    R6,24(R6)           POINT TO FIRST ELEM                          
*                                                                               
         MVI   ELCDLO,2            TEST 02 (IMP OVRD ELEM)                      
         MVI   ELCDHI,2                                                         
         BAS   RE,NEXTEL                                                        
         BNE   GETDEM18                                                         
*                                                                               
GETDEM4  ZIC   RF,1(R6)            GET ELEMENT LEN                              
         AHI   RF,-2                                                            
         BNP   GETDEM18                                                         
*                                                                               
         ZIC   R0,1(R6)            GET ELEM LEN AGAIN                           
         SRDL  R0,32                                                            
         D     R0,=F'5'            NOTE REMAINDER IS DROPPED                    
         LR    RF,R1               SET FOR BCT                                  
*                                                                               
GETDEM6  LA    R6,2(R6)            POINT TO FIRST IMP (IN 02 EL)                
         EJECT                                                                  
GETDEM8  DS    0H                                                               
         SR    R1,R1                                                            
         ICM   R1,3,3(R6)          GET VALUE                                    
         STH   R1,HALF                                                          
         OC    BUYKMKT,BUYKMKT     TEST NETWORK BUY                             
         BZ    GETDEM12                                                         
         AR    R1,R1               X 2                                          
         M     R0,BUDUB            X RGN SHARE OF NETWORK                       
         D     R0,=F'100000'                                                    
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         ICM   R0,15,7(R7)         GET STATION SHARE OF BUY                     
         AR    R0,R0               X 2                                          
         MR    R0,R0                                                            
         D     R0,=F'100000'                                                    
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         STH   R1,HALF                                                          
*                                                                               
GETDEM12 MVC   0(3,R2),0(R6)       MOVE DEMO NUMBER                             
         NI    0(R2),X'7F'         CLEAR 'INPUT' FLAG                           
         MVC   3(2,R2),HALF        MOVE VALUE                                   
         OC    3(2,R2),3(R2)       TEST ZERO                                    
         BNZ   *+8                                                              
         OI    3(R2),X'80'         SET OVERRIDE FLAG                            
         LA    R2,5(R2)            NEXT SLOT                                    
         LA    R6,5(R6)            NEXT DEMO VALUE                              
         CLI   1(R6),0             TEST RAN OUT OF DEMOS                        
         BE    GETDEM18            YES - DONE                                   
         BCT   RF,GETDEM8                                                       
         SPACE 1                                                                
GETDEM18 CLC   FULL,AREC4          TEST FIRST DEMO OVRD REC                     
         BNE   GETDEM20            NO                                           
         L     R6,AREC5                                                         
         ST    R6,FULL                                                          
         CLI   0(R6),0             TEST FOR SECOND RECORD                       
         BE    GETDEM20            NO - GO PROCESS                              
* NEED TO MAKE SURE THERE ARE 05 ELEMENTS IN RECORD                             
         LA    R6,24(R6)                                                        
         SR    R0,R0                                                            
GETDEM19 CLI   0(R6),0                                                          
         BE    GETDEM19X           NONE - IGNORE RECORD                         
         CLI   0(R6),5                                                          
         BE    GETDEM2                                                          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     GETDEM19                                                         
*                                                                               
GETDEM19X L    RE,AREC5                                                         
          MVI  0(RE),0             SUPPRESS SECOND RECORD                       
         EJECT                                                                  
GETDEM20 MVC   FULL,AREC4          RESET REC ADDRESS=AREC4                      
*                                                                               
         MVI   ELCDLO,2            FIND O2 DEMO ELEM (IN BUYREC)                
         MVI   ELCDHI,2                                                         
         LA    R6,BDELEM                                                        
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
* NOW FIND 05 ELEM FOR THIS STATION (R7 POINTS TO STA ELEMENT)   *              
         SPACE 1                                                                
GETDEM21 OC    BUYKMKT,BUYKMKT     TEST NETWORK BUY                             
         BZ    GETDEM37            YES - DO NOT PROCESS 05 ELEMS                
         L     R5,FULL             POINT TO DEMO OVRD REC                       
         LA    R5,24(R5)                                                        
         SR    RE,RE                                                            
         ICM   RE,7,4(R7)          GET PACKED CALL LETTERS (FROM ELEM)          
         CLI   6(R7),X'B0'         TEST CABLE BUY                               
         BNL   GETDEM24            YES                                          
         SRL   RE,5                DROP MEDIA BITS                              
         CLI   SVSTVRSN,C'N'                                                    
         BNE   *+8                                                              
         SRL   RE,3                                                             
         B     GETDEM24                                                         
*                                                                               
GETDEM22 SR    RF,RF                                                            
         ICM   RF,7,2(R5)                                                       
         CLI   4(R5),X'B0'         TEST CABLE BUY                               
         BNL   GETDEM23            YES -USE ALL THE BYTES                       
         SRL   RF,5                                                             
         CLI   SVSTVRSN,C'N'                                                    
         BNE   *+8                                                              
         SRL   RF,3                                                             
GETDEM23 CR    RE,RF               TEST RIGHT STATION                           
         BE    GETDEM26                                                         
*                                                                               
GETDEM24 ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLI   0(R5),0                                                          
         BE    GETDERR1                                                         
         CLI   0(R5),5                                                          
         BE    GETDEM22                                                         
         B     GETDEM24                                                         
*                                                                               
GETDEM26 CLI   0(R6),2             TEST SPILL MKT DEMOS                         
         BE    GETDEM30            NO                                           
         EJECT                                                                  
* FIND SPILL MKT OVRD *                                                         
         SPACE 1                                                                
GETDEM28 ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLI   0(R5),5                                                          
         BNE   GETDERR2                                                         
         CLI   2(R5),0             TEST SPILL OVRD                              
         BNE   GETDERR2                                                         
         CLC   3(2,R5),4(R6)       RIGHT MKT                                    
         BNE   GETDEM28                                                         
         SPACE 1                                                                
* NOW EXTRACT VALUES FROM 05 ELEMENT *                                          
         SPACE 1                                                                
GETDEM30 L     R4,FULL             FULL HAS DEMO OVRD REC ADDRESS               
         LA    RE,24(R4)                                                        
         ZIC   RF,1(RE)                                                         
         AHI   RF,-12                                                           
         BNP   GETDEM56                                                         
         SR    RE,RE                                                            
         D     RE,=F'3'            RF NOW SET FOR BCT                           
         L     RE,FULL                                                          
         LA    RE,24+12(RE)        OVRD LIST (IN O1 EL)                         
         LA    R4,5(R5)            POINT TO DEMO OVRD VAL (IN 05 EL)            
*                                                                               
GETDEM32 MVC   0(3,R2),0(RE)       MOVE DEMO NUM TO WORK AREA                   
         OC    0(3,R2),0(R2)       TEST DELETED ENTRY                           
         BNZ   GETDEM34            NO                                           
         MVC   0(3,R2),=3X'FF'     MAKE IT LOOK LIKE SOMETHING                  
         B     GETDEM36                                                         
*                                                                               
GETDEM34 MVC   3(2,R2),0(R4)       MOVE DEMO VALUE                              
         NI    0(R2),X'7F'         DROP 'INPUT' IND                             
*                                                                               
GETDEM36 LA    R2,5(R2)            NEXT WORK AREA SLOT                          
         LA    R4,2(R4)            NEXT DEMO VALUE                              
         LA    RE,3(RE)            NEXT DEMO NUMBER                             
         BCT   RF,GETDEM32                                                      
*                                                                               
GETDEM37 CLC   FULL,AREC4          TEST POINTING TO FIRST OVRD REC              
         BNE   GETDEM40                                                         
         L     RE,AREC5            POINT TO SECOND                              
         CLI   0(RE),0             TEST IT IS THERE                             
         BE    GETDEM40            NO - DONE                                    
         ST    RE,FULL             ELSE SET CURRENT OVRD REC ADDR               
         B     GETDEM21            GO AND PROCESS IT                            
         EJECT                                                                  
* NOW MOVE VALUES TO DEMO EL *                                                  
         SPACE 1                                                                
GETDEM40 ZIC   RF,1(R6)                                                         
         AHI   RF,-24                                                           
         BNP   EXIT                                                             
         LA    RE,24(R6)                                                        
         SRL   RF,3                SET FOR BCT                                  
*                                                                               
GETDEM51 LA    R2,ELEM             POINT TO WORK AREA                           
*                                                                               
GETDEM52 CLC   0(3,R2),0(RE)       MATCH DEMO NUM                               
         BE    GETDEM54                                                         
         LA    R2,5(R2)                                                         
         CLI   1(R2),0                                                          
         BNE   GETDEM52                                                         
         B     GETDEM56                                                         
GETDEM54 XC    4(4,RE),4(RE)       CLEAR OLD VALUE IN DEMO ELEMENT              
         OC    3(2,R2),3(R2)       TEST 'LOOK-UP'                               
         BZ    GETDEM56                                                         
         MVC   6(2,RE),3(R2)       MOVE DEMO VALUE                              
         CLC   =X'8000',6(RE)      TEST ZERO                                    
         BNE   *+10                                                             
         XC    6(2,RE),6(RE)                                                    
         OI    4(RE),X'80'         SET OVRD IND                                 
         MVI   3(RE),100           SET HUT                                      
*                                                                               
GETDEM56 LA    RE,8(RE)                                                         
         BCT   RF,GETDEM51                                                      
         EJECT                                                                  
GETDEM57 DS    0H                                                               
         L     R5,AREC4                                                         
*        MVC   2(2,R6),DOVBBK      SET BASE BOOK                                
*                                                                               
GETDEM58 XC    ELEM,ELEM           CLEAR                                        
         LA    R2,ELEM              AND RESET POINTER                           
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),3                                                          
         BNE   GETDEMX                                                          
* REBUILD SPILL DEMO ELEMENT                                                    
         MVC   SVSPLMKT,4(R6)      SET SPILL MARKET NUMBER                      
         MVI   BUDEMSW,0           FORCE REBUILD OF DEMO ELEMENT                
         GOTO1 VBLDDEM                                                          
         CLI   1(R6),24            TEST ANY DEMOS IN ELEMENT                    
         BNH   GETDEM58                                                         
         MVC   FULL,AREC4          POINT TO RECORD 1                            
         B     GETDEM21                                                         
         SPACE 1                                                                
*======================================================*                        
* CALL DEMO LOOK-UPS TO SUPPLY MISSING OVERRIDES       *                        
* UNFORTUNATELY, THE DEMO LOOKUPS FIND THE POST-BUY    *                        
* DEMO ELEMENTS IF THEY ARE PRESENT, AND WE DO NOT     *                        
* THEM. SO CHANGE THE ELEMENT CODES BEFORE THE LOOK-UPS*                        
* AND THEN CHANGE THEM BACK                            *                        
*======================================================*                        
         SPACE 1                                                                
GETDEMX  OC    BUYKMKT,BUYKMKT     TEST NETWORK BUY                             
         BZ    EXIT                                                             
         MVI   ELCDLO,X'22'        NEED TO MODIFY POST BUY DEMELS               
         MVI   ELCDHI,X'23'                                                     
         LA    R6,BDELEM                                                        
GETDEMX2 BAS   RE,NEXTEL                                                        
         BNE   GETDEMX4                                                         
         OI    0(R6),X'80'                                                      
         B     GETDEMX2                                                         
*                                                                               
GETDEMX4 DS    0H                                                               
         GOTO1 DEMLKUP                                                          
*                                                                               
         MVI   ELCDLO,X'A2'        NEED TO MODIFY POST BUY DEMELS               
         MVI   ELCDHI,X'A3'                                                     
         LA    R6,BDELEM                                                        
GETDEMX6 BAS   RE,NEXTEL                                                        
         BNE   EXIT                                                             
         NI    0(R6),X'FF'-X'80'                                                
         B     GETDEMX6                                                         
         SPACE 2                                                                
GETDERR1 DS    0H                  SET STATION DEMOS MISSING FLAG               
GETDERR2 DS    0H                  SET SPILL DEMOS MISSING FLAG                 
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(DOVERR) THIS ERROR CODE CHECKED ON EXIT              
         B     GETDEMX             *** DO NOT STOP ON ERROR ***'                
         MVI   ERRAREA,X'FE'       CAUSE DC H'0',$ABEND                         
         B     BUYERR                                                           
         EJECT                                                                  
*                                                                               
* TEST SPOT AT 0(R6), BILLED OR PAID                                            
TESTBP   NTR1                                                                   
         MVI   ERRCD,BLLDPAID                                                   
*                                                                               
         OC    4(2,R6),4(R6)                                                    
         BNZ   NEQXIT                                                           
         B     EQXIT                                                            
         SPACE 2                                                                
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         LTR   R0,R0                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
ADDEL    NTR1                                                                   
         GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
         J     EXIT                                                             
*                                                                               
DELEL    NTR1                                                                   
         GOTO1 VRECUP,DMCB,BUYREC,(0,(R6))                                      
         J     EXIT                                                             
         SPACE 2                                                                
GET70EL  NTR1                                                                   
         XC    FULL,FULL                                                        
         LA    R6,BDELEM                                                        
*                                                                               
GET70EL2 ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         JE    NEQXIT                                                           
         CLI   0(R6),X'70'                                                      
         JNE   GET70EL2                                                         
         ST    R6,FULL                                                          
         J     EQXIT                                                            
*                                                                               
         LTORG                                                                  
         DROP  R9                                                               
         EJECT                                                                  
*=====================================================================          
* MAKEGOOD INPUT TYPES             MGDATE MGSTA MGCODE CDTYPE MGFLG             
* C,MG=1JAN21 OR 1JAN21/CNB        JAN21  CNB     --    NEW                     
* C,MG=-1JAN21 OR -1JAN21/CNB      JAN21  CNB           EXIST BUMGNEG           
* C,MG=A0                            --    --     A0    INPUT                   
* C,MG=0                             --    --     0     INPUT BUMGCLR           
*=====================================================================          
                                                                                
*=====================================================================          
* SINCE TABLE WILL NOT BE SAVED, USE MAX SPACE TO GET NEXT CODE                 
*=====================================================================          
                                                                                
         DS    0D                                                               
NEWMG    NTR1  BASE=*,LABEL=*                                                   
* SET FLAG TO NOT CHECK FOR DARE LOCKS SO MG'S ALWAYS ACCEPTED                  
         L     RE,ASVDARE                                                       
         OI    SVDRFLG2-SVDARED(RE),SVDRFLG2_IGN                                
*                                                                               
         USING MGENTRYD,ENTRY      <==== REGARDEZ LA                            
*                                                                               
         BAS   RE,SETMGAD                                                       
*                                                                               
         MVI   RCLOPT,0                                                         
         CLI   SVMGINIT,C'A'                                                    
         BE    NMG10                                                            
         MVI   SVMGINIT,C'A'                                                    
         MVI   BYTE,MGAQBLD        BUILD TABLE                                  
         BAS   RE,CALLMGA          BUILD MG TABLE                               
         BE    NMG10                                                            
*                                                                               
         MVC   NERRCD,=AL2(TABLFULL)                                            
         CLI   BYTE,MGAQTFUL       TEST TOO MANY TABLE ENTRIES                  
         BE    BUYERR1                                                          
         CLI   BYTE,MGAQEOF        OR TEST TSAR FULL                            
         BE    BUYERR1                                                          
         MVC   NERRCD,=AL2(NOMGAV)                                              
         B     BUYERR1             NO MORE CODES AVAILABLE                      
*                                                                               
NMG10    OC    BUREFMAS,BUREFMAS   TEST C,MG=0 OR C,MG=A0                       
         BZ    NMG20                YES, SKIP 53 WEEK EST CHECK                 
*                                                                               
         OC    SVNDEF,SVNDEF       TEST CANAD NETWORK                           
         BZ    NMG15               NO                                           
         OC    BUEXPKEY,BUEXPKEY   TEST EXPLODED BUY?                           
         BNZ   NMG20                SKIP IT, ONLY DO CHK ONCE @NTWK LVL         
*                                                                               
NMG15    BAS   RE,CHK53EST         CHK 53WK EST, ADJUST BUMGDATE,IF REQ         
*                                                                               
NMG20    MVC   KEY,SVKEY                                                        
         OC    SVNDEF,SVNDEF       TEST CANAD NETWORK                           
         BZ    NMG22               NO                                           
         OC    BUEXPKEY,BUEXPKEY   TEST EXPLODED BUY                            
         BZ    *+10                NO                                           
         MVC   KEY,BUEXPKEY                                                     
*                                                                               
NMG22    GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   *+2                                                              
         MVC   AREC,AREC1                                                       
         GOTO1 GETREC                                                           
*                                                                               
         BAS   RE,GETINFO                                                       
*                                                                               
         BAS   RE,VALINPUT         MAKE SURE CAN DO WHAT WE WANT TO             
         BAS   RE,MISPOT           HANDLE MISSED SPOT                           
         BAS   RE,MGSPOT           HANDLE MAKEGOOD SPOT                         
         TM    WRKRUPSW,WRKRUPSW_NEWDRMG    THIS FOR NEW DARE                   
         BO    XIT                 YES, SKIP REBUILD                            
         MVI   SVMGINIT,0          FORCE TABLE REBUILD                          
         B     XIT                                                              
         EJECT                                                                  
*=================================================================              
* IF 53 WEEK ESTIMATE, THEN CHECK MISSED LINE BDSTART/BDEND, AND                
* IF NEEDED, ADD A YEAR TO BUMGDATE                                             
*=================================================================              
CHK53EST NTR1                                                                   
         MVC   FULL,AREC                                                        
         MVC   AREC,AREC2                                                       
*                                                                               
         CLC   SVSTART(2),SVEND    TEST EST ALL IN ONE YEAR                     
         BE    C53EX                                                            
         CLC   SVEND+2(4),SVSTART+2 MMDD OF ESTEND & ESTSTART OVERLAP?          
         BL    C53EX                 NO                                         
*                                                                               
* HAVE A 53 WEEK EST AND MMDD OVERLAP, WHICH YEAR TO USE?                       
* LETS READ MISSED LINE TO SEE IF CAN DETERMINE THE YEAR                        
*                                                                               
C53E010  MVC   KEY+4(2),SVKEY+4    MISSED MKT                                   
         OC    BUMGSTA,BUMGSTA     OTHER STATION ENTERED?                       
         BZ    *+10                                                             
         MVC   KEY+6(3),BUMGSTA    MISSED STA                                   
         MVC   KEY+11(2),BUREFMAS  MISSED LINE                                  
         GOTO1 HIGH                                                             
         MVI   ERRCD,NOTFOUND                                                   
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BUYERR2                                                          
         GOTO1 GETREC                                                           
*                                                                               
         GOTO1 VDATCON,DMCB,(2,BUMGDATE),(3,WORK)                               
*                                                                               
         USING BUYREC,R6           ESTABLISH BUY RECORD                         
         L     R6,AREC                                                          
         CLC   BDEND,WORK          LINE END BEFORE BUMGDATE?                    
         BL    C53EX                YES, LEAVE BUMGDATE ALONE                   
         CLC   BDSTART,WORK        LINE START ON/BEFORE BUMGDATE?               
         BNH   C53EX                YES, LEAVE BUMGDATE ALONE                   
*                                                                               
* HAVE A BUMGDATE THAT IS BEFORE BOTH LINE END AND LINE START                   
* SO LETS ADD 1 YEAR TO BUMGDATE.                                               
* NOTE: WE MAY END UP BEYOND LINE-END BUT BEFORE EST-END, BUT SAME-DIFF         
*                                                                               
         GOTO1 VDATCON,DMCB,(3,WORK),(0,DUB)                                    
         GOTO1 VADDAY,DMCB,(C'Y',DUB),(0,DUB),F'1'                              
         GOTO1 VDATCON,DMCB,(0,DUB),(2,BUMGDATE)                                
*                                                                               
C53EX    MVC   AREC,FULL                                                        
         B     XIT                                                              
         DROP  R6                                                               
*=================================================================              
* EXTRACT INFO FROM MAKEGOOD RECORD                                             
*=================================================================              
                                                                                
GETINFO  NTR1                                                                   
         L     R6,AREC             THIS IS THE MAKEGOOD LINE                    
         MVC   BUMGLINE,BUYKBUY    SET BUY LINE NUMBER                          
         MVI   CDTYPE,C'I'         INPUT CODE - C,MG=A0                         
         OC    BUMGCODE,BUMGCODE   IS C,MG=A0                                   
         BNZ   GI40                 YES                                         
*                                                                               
         CLI   BDMGDATE,X'C0'      IS THE CODE SET IN RECORD                    
         BH    GI30                 YES                                         
*                                                                               
* GET NEW MG CODE                                                               
*                                                                               
         MVI   CDTYPE,C'N'         SET TYPE TO NEW CODE                         
         MVI   BYTE,MGAQCOD        GET NEXT CODE                                
         OC    BUEXPKEY,BUEXPKEY   TEST CANAD EXPLODED STA                      
         BZ    GI10                                                             
         OC    SVCUTLST+20(3),SVCUTLST+20  AVOID A CALAMITY                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   MGECODE,SVCUTLST+20   USE NETWORK VALUES                         
         MVC   MGAECOD,SVCUTLST+22                                              
         B     GI20                                                             
*                                                                               
GI10     BAS   RE,CALLMGA                                                       
         BE    GI20                                                             
         MVC   NERRCD,=AL2(TABLFULL)                                            
         CLI   BYTE,MGAQTFUL       TEST TOO MANY TABLE ENTRIES                  
         BE    BUYERR1                                                          
         MVC   NERRCD,=AL2(NOMGAV)                                              
         B     BUYERR1             NO MORE CODES AVAILABLE                      
*                                                                               
GI20     MVC   BUMGCODE,MGECODE    SAVE ALPHA MGCODE                            
         MVC   BUMGBCOD,MGEBCODE   AND BINARY MGCODE                            
         B     GIX                                                              
*                                                                               
* EXTRACT INFO FROM MAKEGOOD RECORD                                             
*                                                                               
GI30     MVI   CDTYPE,C'E'         SET TYPE TO EXISTING CODE                    
         MVC   BUMGCODE,BDMGDATE   SET MG CODE FROM RECALLED RECORD             
         L     R5,ADBLOCK                                                       
         USING MGABLKD,R5                                                       
         MVI   BYTE,MGAQBIN        NEED TO GET HEX VALUE                        
         MVC   MGQCODE,BDMGDATE    2-BYTE ALPHA IN                              
         BRAS  RE,CALLMGA                                                       
         MVC   BUMGBCOD,MGBCODE+1  SAVE 1 BYTE ONLY!                            
         DROP  R5                                                               
*                                                                               
GI40     CLI   CDTYPE,C'I'         IF C,MG=A0                                   
         BE    GI45                                                             
         CLI   BUMGFLG,BUMGCLR     IF C,MG=0                                    
         BNE   GI50                                                             
GI45     OC    BUMGSTA,BUMGSTA     DO NOT ENTER STATION!                        
         BNZ   STAINPER                                                         
*                                                                               
GI50     XC    GETNUM,GETNUM       SET FOR FULL SEARCH                          
         MVC   GETCODE,BUMGCODE    SET SEARCH CODE                              
         MVI   GETTYPE,1           SET ENTRY TYPE = MAKEGOOD                    
         MVC   GETSTA,=X'FFFFFF'   IGNORE STATION (FOR CABLE)                   
*                                                                               
         BAS   RE,GETENTRY                                                      
         BNE   GIX                                                              
*                                                                               
GI60     MVC   BUMGPRD1,MGEPRD1    EXTRACT INFO FROM EXISTING CODE              
         MVC   BUMGSLN1,MGESLN1                                                 
         MVC   BUMGPRD2,MGEPRD2                                                 
         MVC   BUMGSLN2,MGESLN2                                                 
*                                                                               
GIX      B     XIT                                                              
         EJECT                                                                  
*===================================================================*           
*  VALIDATE THAT THE INPUT IS OK TO DO                              *           
*  EG, DO NOT ALLOW C,MG=A1 IF LINE IS LAST MG FOR CODE A0          *           
*===================================================================*           
         SPACE 1                                                                
VALINPUT NTR1                                                                   
         CLI   BUMGFLG,BUMGNEG     IF C,MG=-1JAN1                               
         BE    VI20                                                             
         CLI   CDTYPE,C'I'         IF C,MG=A0                                   
         BNE   VIX                                                              
*                                                                               
         OC    BUMGSTA,BUMGSTA     TEST STATION INPUT                           
         BNZ   STAINPER                                                         
*                                                                               
         XC    GETNUM,GETNUM       SET FOR FULL SEARCH                          
         MVC   GETCODE,BUMGCODE    SET SEARCH CODE                              
         MVI   GETTYPE,1           SET ENTRY TYPE = MAKEGOOD                    
         MVC   GETSTA,=X'FFFFFF'   IGNORE STATION (FOR CABLE)                   
*                                                                               
         BAS   RE,GETENTRY         MAKE SURE CODE EXISTS IN TABLE               
         BNE   MGCDNF                                                           
*                                                                               
         L     R6,AREC1                                                         
         CLI   BDMGDATE,X'C0'      IS THERE AN EXISTING CODE                    
         BNH   VIX                                                              
         CLC   BDMGDATE,BUMGCODE   CANNOT CHANGE TO ITSELF                      
         BE    SAMECD                                                           
*                                                                               
* LINE ALREADY HAS CODE, MAKE SURE NOT LAST MG CODE                             
*                                                                               
         XC    GETNUM,GETNUM       SET FOR FULL SEARCH                          
         MVC   GETCODE,BDMGDATE    SET SEARCH CODE                              
         MVI   GETTYPE,1           SET ENTRY TYPE = MAKEGOOD                    
         MVC   GETSTA,=X'FFFFFF'   IGNORE STATION (FOR CABLE)                   
*                                                                               
         BAS   RE,GETENTRY                                                      
         JNE   *+2                                                              
         CLI   QSTA,C'0'           TEST CABLE                                   
         BL    VI05                                                             
         CLC   MGESTA,BUYKSTA      YES, MATCH ON MKGD STATION?                  
         BNE   VIX                                                              
VI05     CLC   MGELINE,BUYKBUY                                                  
         BNE   VIX                 DIFFERENT LINE NUMBER - OK                   
*                                                                               
VI10     BAS   RE,GETENTRY         ELSE GET NEXT                                
         BNE   LSTERR                                                           
         CLI   QSTA,C'0'           TEST CABLE                                   
         BL    VI15                                                             
         CLC   MGESTA,BUYKSTA      YES, MATCH ON MKGD STATION?                  
         BNE   VIX                                                              
VI15     CLC   MGELINE,BUYKBUY     LINE NUMBER                                  
         BNE   VIX                 DIFFERENT LINE NUMBER - OK                   
         B     VI10                                                             
         EJECT                                                                  
*=====================================================================*         
* THIS CODE REMOVES A SPOT FROM A MG GROUP (EXCEPT THE LAST)          *         
* THE STATION IN THE HEADLINES IS THE MISSED STATION                  *         
*=====================================================================*         
         SPACE 1                                                                
VI20     DS    0H                  MAKE SURE IT'S NOT LAST MISSED SPOT          
         CLI   BUMGSPOT,0                                                       
         BNE   *+8                                                              
         MVI   BUMGSPOT,1                                                       
*                                                                               
         OC    BUMGSTA,BUMGSTA     FOR A DIFFERENT STATION                      
         BZ    VI25                 NO                                          
         MVC   KEY+4(2),SVKEY+4     YES, STILL HAVE TO CHECK THAT               
         MVC   KEY+6(3),BUMGSTA      DIFFERENT STATION                          
*                                                                               
VI25     BAS   RE,FNDSPOT          TEST SPOT IN TABLE                           
         BNE   SPTNTFN             NO - ERROR                                   
         MVC   BUMGCODE,GETCODE    SAVE MG CODE FROM SPOT                       
                                                                                
* NOW SEE IF THERE ARE ANY OTHER MISSED SPOTS (FNDSPOT SET GETCODE)             
                                                                                
         XC    GETNUM,GETNUM       SET FOR FULL SEARCH                          
         MVC   GETSTA,=X'FFFFFF'   IGNORE STATION (FOR CABLE)                   
         MVI   GETTYPE,0           SET ENTRY TYPE = MISSED                      
         MVI   COUNTER,0                                                        
*                                                                               
VI30     BAS   RE,GETENTRY                                                      
         BNE   VI40                                                             
         LLC   R1,COUNTER                                                       
         LA    R1,1(R1)                                                         
         STC   R1,COUNTER                                                       
         B     VI30                                                             
*                                                                               
VI40     CLI   COUNTER,1           IF NOT AT LEAST 2 SPOTS, ERROR               
         BNH   NOMOREMG                                                         
*                                                                               
VIX      B     XIT                                                              
*                                                                               
SAMECD   MVC   NERRCD,=AL2(NOCHMGCD)    CANNOT CHANGE TO ITSELF                 
         B     BUYERR1                                                          
*                                                                               
MGCDNF   MVC   NERRCD,=AL2(MGNTFND)     MAKEGOOD CODE NOT FOUND                 
         B     BUYERR1                                                          
*                                                                               
LSTERR   MVC   NERRCD,=AL2(LSTMGGRP)    LAST MG CODE OF GROUP                   
         B     BUYERR1                                                          
*                                                                               
SPTNTFN  MVC   NERRCD,=AL2(SPTNOTMG)    SPOT HAS NOT BEEN MADEGOOD              
         B     BUYERR1                                                          
*                                                                               
NOMOREMG MVC   NERRCD,=AL2(UNMGLAST)    CANT UNDO LAST MISSED SPOT              
         B     BUYERR1                                                          
STAINPER MVC   NERRCD,=AL2(NOSTAINP)    DO NOT INPUT STATION                    
         B     BUYERR1                                                          
         EJECT                                                                  
*=============================================================                  
* HANDLE MISSED SPOT                                                            
* ON EXIT, MISSED LINE WAS WRITTEN, BUT SAVED IN AREC2                          
*=============================================================                  
         SPACE 1                                                                
MISPOT   NTR1                                                                   
         XC    WORK2+24(20),WORK2+24  CLEAR SAVE AREA                           
         MVI   ERRCD,0                                                          
         CLI   BUMGFLG,BUMGCLR     IF C,MG=0                                    
         BE    MSX                                                              
         CLI   CDTYPE,C'I'         TEST FOR C,MG=A4                             
         BE    MSX                 YES - NO MISSED INVOLVED                     
*                                                                               
         CLI   BUMGFLG,BUMGNEG     TEST C,MG=-1JAN8                             
         BE    MS4                                                              
* MAKE SURE LINE DOES NOT REFERENCE ITSELF (INPUT ON LINE 2 C,MG=2JAN1)         
         OC    BUMGSTA,BUMGSTA     TEST OTHER-STATION MG                        
         BNZ   *+14                YES - DO NOT CHECK                           
         CLC   BUREFMAS,SVKEY+11                                                
         BE    SAMECD                                                           
*                                                                               
MS4      MVC   DUB(4),AREC1        FROM                                         
         MVC   DUB+4(4),AREC2      TO                                           
         GOTO1 MOVEREC             SAVE MAKEGOOD LINE IN REC2                   
         MVC   WORK2+24(20),KEY    SAVE MAKEGOOD KEY/DA                         
         MVC   THISLINE,BUREFMAS                                                
*                                                                               
         CLI   BUMGFLG,BUMGNEG     IF C,MG=-1JAN1                               
         BE    MS50                 YES, PROCESS UN-MISS LOGIC                  
*                                                                               
         OC    BUMGSTA,BUMGSTA     FOR A DIFFERENT STATION                      
         BZ    MS10                 NO                                          
         MVC   KEY+4(2),SVKEY+4     YES, STILL HAVE TO CHECK THAT               
         MVC   KEY+6(3),BUMGSTA      DIFFERENT STATION                          
*                                                                               
MS10     BAS   RE,FNDSPOT          SEE IF SPOT IS IN THE TABLE                  
         BNE   MS15                                                             
         CLC   GETCODE,=C'PR'      IF IT IS CODE PR (PREEMPT) THEN OK           
         BE    *+14                                                             
         CLC   GETCODE,=C'*P'                                                   
         BNE   MSERR                                                            
***      MVC   BUMGSTA,MGESTA      SET THE MISSED STATION                       
***      MVC   THISLINE,MGELINE    AND THE MISSED LINE                          
*                                  READ MISSED LINE                             
MS15     MVC   KEY+11(2),THISLINE                                               
         GOTO1 HIGH                                                             
         MVI   ERRCD,NOTFOUND                                                   
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BUYERR2                                                          
         GOTO1 GETREC                                                           
         BRAS  RE,GETPKGEL                                                      
*                                                                               
         OC    BUYMSTA(2),BUYMSTA  TEST CAN NETWORK BUY                         
         BNZ   *+8                 NO                                           
         BRAS  RE,STAMTCH          MATCH STATION LISTS                          
*                                                                               
MS16     MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=AL2(MGNOSMRP)     MG LINES MUST HAVE SAME REP            
         CLC   BUREP,BDREP         TEST MSTR/SLV LINES HAVE SAME REP            
         BNE   BUYERR2                                                          
*                                                                               
         MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(MGPURPNE)                                              
         LHI   RF,SVB0PROF-BUYSAVE                                              
         AR    RF,RA                                                            
         CLI   9(RF),C'O'          OPTIONAL PURPOSE CODES                       
         JE    MS17                YES                                          
         CLI   9(RF),C'Y'          TEST PURPOSE CODES                           
         BNE   MS17                NO                                           
* MISSED/MAKEGOOD MUST HAVE SAME PURPOSE CODE                                   
         L     R6,AREC2            POINT TO MAKEGOOD REC                        
         LA    R6,24(R6)                                                        
         MVI   ELCDLO,X'70'                                                     
         MVI   ELCDHI,X'70'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   BUYERR2                                                          
         LR    R7,R6               SAVE POINTER                                 
*                                                                               
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   BUYERR2                                                          
         SR    RE,RE                                                            
         IC    RE,1(R6)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R6),0(R7)                                                    
         BNE   BUYERR2                                                          
*                                                                               
MS17     MVI   ERRCD,NEWERRS                                                    
         MVC   NERRCD,=Y(BADSPOT2)                                              
         MVI   SPOTNUM,0                                                        
*                                                                               
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
         USING REGELEM,R6                                                       
         SR    R7,R7               CLEAR SAVE REGISTER                          
*                                                                               
MS20     BRAS  RE,NEXTEL                                                        
         BNE   MS28                                                             
         CLC   BUMGDATE,RDATE      IS THIS THE DATE                             
         BNE   MS20                                                             
         TM    RSTATUS,X'80'       IF MINUS                                     
         BO    MS20                SKIP                                         
*                                                                               
         TM    WRKRUPSW,WRKRUPSW_NEWDRMG    SKIP THIS FOR NEW DARE              
         BO    MS20A                                                            
         TM    WRKRUPSW,WRKRUPSW_ISDRMG     IS THIS A DARE MAKEGOOD             
         BZ    *+12                         NO                                  
         TM    RSTATUS,X'10'                YUP..  MUST BE MKGD PENDING         
         BZ    MS20                                                             
*                                                                               
MS20A    LLC   R1,SPOTNUM                                                       
         LA    R1,1(R1)            ELSE INC SPOT NUMBER                         
         STC   R1,SPOTNUM                                                       
*                                                                               
         CLI   BUMGSPOT,0          TEST GENERIC SEARCH                          
         BNE   MS24                                                             
*                                                                               
         TM    RSTATUS,X'02'       TEST ALREADY MADE GOOD                       
         BO    MS20                YES - SKIP                                   
         CLI   1(R6),10            TEST SPOT ALLOCATED                          
         BNH   MS20                         NO - DO NOT USE                     
         TM    WRKRUPSW,WRKRUPSW_ISDRMG     IS THIS A DARE MAKEGOOD             
         BO    MS20B                        YES                                 
         TM    RSTATUS,X'10'                TEST DARE MG PENDING                
         BO    MS20                         YES - IGNORE                        
         B     MS22                         AS IF ...                           
* 00/PRD IS THE NORMAL SEQUENCE SO TEST SVMGBPR2                                
MS20B    L     RE,AMGWORK                                                       
         CLI   SVMGBPR2-MGWORK(RE),0        DO WE KNOW THE PRODUCTS             
         BE    MS22                         NO - JUST USE THIS SPOT             
MS21     SR    R0,R0                                                            
         SR    R1,R1                                                            
         IC    R0,10(R6)           BPRD1                                        
         CLI   1(R6),14                                                         
         BNH   *+8                                                              
         IC    R1,14(R6)           BPRD2                                        
         STC   R0,HALF                                                          
         STC   R1,HALF+1                                                        
         CR    R0,R1                                                            
         BH    *+12                                                             
         STC   R1,HALF                                                          
         STC   R0,HALF+1                                                        
         L     RE,AMGWORK                                                       
         CLC   HALF,SVMGBPR1-MGWORK(RE)  SAME PRDS ? (HIGH CODE FIRST)          
         BNE   MS20                ...AS IF....                                 
*                                                                               
MS22     LR    R7,R6               SAVE ELEMENT ADDRESS                         
         ICM   R7,8,SPOTNUM        SAVE SPOTNUM IN HOB                          
         B     MS20                                                             
*                                                                               
MS24     CLC   BUMGSPOT,SPOTNUM             IS THIS THE SPOT                    
         BNE   MS20                                                             
         TM    WRKRUPSW,WRKRUPSW_ISDRMG     IS THIS A DARE MAKEGOOD             
         BO    MS26                         YES                                 
         MVC   NERRCD,=AL2(DRMGPNDG)                                            
         TM    RSTATUS,X'10'                TEST DARE MG PENDING                
         BO    BUYERR1                                                          
*                                                                               
MS26     MVC   NERRCD,=AL2(SPTALLOC)        SPOT MUST BE ALLOCATED              
         CLI   1(R6),10                                                         
         BNH   BUYERR1                                                          
         B     MS30                                                             
*                                                                               
MS28     CLI   BUMGSPOT,0          TEST GENERIC SEARCH                          
         BNE   BUYERR2             NO - DIDN'T FIND IT                          
         LTR   R7,R7               TEST ANY SPOT FOUND                          
         BZ    BUYERR2             NO - ERROR                                   
         LR    R6,R7               RESTORE ELEMENT POINTER                      
         STCM  R7,8,BUMGSPOT       SET SPOT NUMBER                              
*                                                                               
MS30     TM    WRKRUPSW,WRKRUPSW_ISDRMG     IS THIS A DARE MAKEGOOD             
         BZ    MS35                         NOPE...                             
         NI    6(R6),X'EF'                  TURN OFF MKGD PENDING NOW           
*                                                                               
MS35     BRAS  RE,ADD0C            ADD X'0C' ELEMENT                            
         BNE   BUYERR2             NEQ IS ERROR RETURN                          
         BAS   RE,UPD0B            UPDATE THE X'0B' ELEMENT                     
         BAS   RE,ADD19            ADD X'19' ELEMENT                            
*                                                                               
MS40     GOTO1 SETCHGDT                                                         
         BRAS  RE,SETPKGEL                                                      
         GOTO1 PUTREC                                                           
*                                                                               
         TM    SVAFLAG1,X'22'      TEST CTA ACTIVE                              
         BZ    MS42                                                             
         GOTO1 GOGETCTA,DMCB,('CIBCHGQ',AREC)                                   
*                                                                               
MS42     B     MSX                                                              
*===============                                                                
* UN-MISS LOGIC                                                                 
*===============                                                                
MS50     XC    ENTRY,ENTRY         GET SPOT ENTRY IN TABLE                      
         MVC   MGECODE,GETCODE     SET UP AN ENTRY                              
         MVC   MGELINE,BUREFMAS                                                 
         MVC   MGEDATE,BUMGDATE                                                 
         MVC   MGESPNUM,BUMGSPOT                                                
         OC    BUMGSTA,BUMGSTA                                                  
         BZ    *+10                                                             
         MVC   MGESTA,BUMGSTA                                                   
         BRAS  RE,UNMGREC          FIX THE MISSED RECORD                        
*                                                                               
MSX      MVC   DUB(4),AREC1        MOVE MISSED LINE TO REC2                     
         MVC   DUB+4(4),AREC2      SO CAN COPY DEMOS                            
         GOTO1 MOVEREC                                                          
*                                                                               
         B     XIT                                                              
*                                                                               
MSERR    MVC   NERRCD,=AL2(SPOTMG) SPOT ALREADY MADEGOOD                        
         B     BUYERR1             NO - INVALID                                 
         EJECT                                                                  
*==========================================================*                    
* HANDLE MAKEGOOD RECORD                                   *                    
* UPDATE REGELS WITH MAKEGOOD CODE AND PRD ALLOCATIONS     *                    
* INSERT MAKEGOOD REF IN BDMGDATE                                               
*==========================================================*                    
         SPACE 1                                                                
MGSPOT   NTR1                                                                   
         CLI   BUMGFLG,BUMGNEG     IF C,MG=-1JAN1                               
         BE    MGSX                DON'T SO ANYTHING TO MAKEGOOD                
         OC    BUREFMAS,BUREFMAS   IF C,MG=0/A0                                 
         BZ    MGS10               ALREADY HAVE MAKEGOOD                        
         CLI   CDTYPE,C'E'         IF WE ARE USING AN EXISTING CODE             
         BE    MGSX                   WE ARE ADDING A SPOT                      
*                                                                               
MGS05    XC    LASTDATE,LASTDATE                                                
         MVC   KEY(20),WORK2+24    SET MAKEGOOD LINE KEY/DA                     
         GOTO1 GETREC                                                           
         BRAS  RE,GETPKGEL                                                      
*                                                                               
MGS10    CLI   BUMGFLG,BUMGCLR     IF C,MG=0                                    
         BNE   MGS20               NO                                           
         BAS   RE,REMMG            REMOVE MG                                    
         XC    BUMGDATA,BUMGDATA   CLEAR DATA                                   
         B     MGSX                                                             
*                                                                               
MGS20    CLI   BDMGDATE,0          IS THE BUY A MAKEGOOD NOW                    
         BNE   MGS30               YES                                          
         OC    BUMGSTA,BUMGSTA     TEST OTHER STATION MAKEGOOD                  
         BNZ   MGS30                                                            
*                                                                               
         LHI   RF,SVB0PROF-BUYSAVE                                              
         AR    RF,RA                                                            
         CLI   8(RF),C'Y'                                                       
         BE    *+12                                                             
         CLI   8(RF),C'O'          TEST OVERWRITE                               
         BNE   MGS30                                                            
         BRAS  RE,MGDEM            GO COPY DEMOS                                
*                                                                               
MGS30    BAS   RE,ALLOCMG          ALLOCATE MAKEGOOD SPOTS IF NECESSARY         
*                                                                               
         CLI   CDTYPE,C'I'         TEST CHANGING MG REF                         
         BNE   MGS50                NO, TYPE IS NEW MKGD CODE                   
         CLI   BDMGDATE,0           YES, IS LINE A MAKEGOOD NOW                 
         BE    MGS50                 NO                                         
                                                                                
* FOR CHANGE OF REFERENCE NEED TO DELETE OLD TABLE ENTRY                        
                                                                                
MGS40    XC    GETNUM,GETNUM       SET FOR FULL SEARCH                          
         MVC   GETCODE,BDMGDATE    ASK GETETNRY FOR OLD CODE                    
         MVC   GETSTA,BUYKSTA      FOR CABLE, MATCH ON MKGD STATION             
         MVI   GETTYPE,1           SET ENTRY TYPE = MAKEGOOD                    
*                                                                               
MGS44    BAS   RE,GETENTRY         GETENTRY RETURNS VALUES IN ENTRY             
         BNE   MGS50               NO MORE ENTRIES                              
*                                                                               
         CLC   MGELINE,BUMGLINE    MATCH LINE NUMBER                            
         BNE   MGS44               NO                                           
*                                                                               
MGS46    MVI   BYTE,MGAQDEL        DELETE THE ENTRY                             
         BAS   RE,CALLMGA                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MGS50    MVC   BDMGDATE,BUMGCODE   SET NEW MG CODE                              
*                                                                               
MGS100   BRAS  RE,PRREASON                                                      
         GOTO1 SETCHGDT                                                         
         LA    RE,PRDLIST          SET TO ADD PASSIVES FOR NEW PRDS             
         ST    RE,DMCB+20                                                       
         BRAS  RE,SETPKGEL                                                      
         GOTO1 PUTREC                                                           
*                                                                               
MGSX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        UPDATE THE 0B ELEMENT                                                  
*                                                                               
UPD0B    NTR1                                                                   
         USING REGELEM,R6                                                       
         OI    RSTATUS,X'42'       MARK SPOT MINUSED/MADEGOOD                   
*                                                                               
         MVC   13(1,R6),BUMGBCOD                                                
         CLI   13(R6),0                                                         
         JE    *+2                                                              
         MVC   BUMGPRD1,10(R6)                                                  
         MVC   BUMGSLN1,11(R6)                                                  
         CLI   1(R6),X'0E'                                                      
         BE    U0BX                                                             
         MVC   BUMGPRD2,14(R6)                                                  
         MVC   BUMGSLN2,15(R6)                                                  
*                                                                               
U0BX     B     XIT                                                              
         EJECT                                                                  
*=====================================================================*         
* IF SPOTS IN MAKEGOOD LINE ARE UNALLOCATED, COPY THE ALLOCATION FROM *         
* THE MISSED SPOTS                                                    *         
* FOR DIY TRADE BUYS, CHANGE MAKEGOOD SO TRADE/TRADE OR CASH/CASH     *         
*=====================================================================*         
         SPACE                                                                  
ALLOCMG  NTR1                                                                   
         XC    PRDLIST,PRDLIST     CLEAR ADDED PRDLIST                          
         MVI   SPOTNUM,0                                                        
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
         L     R6,AREC                                                          
         LA    R6,24(R6)                                                        
*                                                                               
AMG2     BRAS  RE,NEXTEL                                                        
         BNE   XIT                                                              
*                                                                               
         CLC   LASTDATE,RDATE      SAME DATE                                    
         BE    AMG4                                                             
         MVC   LASTDATE,RDATE                                                   
         MVI   SPOTNUM,0                                                        
*                                                                               
AMG4     TM    RSTATUS,X'80'       IF MINUS                                     
         BNZ   AMG2                SKIP                                         
         ZIC   R1,SPOTNUM                                                       
         LA    R1,1(R1)            INC SPOT NUMBER                              
         STC   R1,SPOTNUM                                                       
*                                                                               
         TM    RSTATUS,X'40'       IF MINUSSED                                  
         BO    AMG2                SKIP                                         
*                                                                               
         TM    BDSTAT2,X'10'       SET DIY TRADE BUY                            
         BZ    AMG6                NO                                           
         CLI   1(R6),14            MUST BE SINGLE PRD ALLOCATION                
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   10(1,R6),BUMGPRD1   MATCH ALLOCATION                             
         BE    XIT                                                              
         MVC   10(1,R6),BUMGPRD1   SET NEW PRODUCT                              
         MVC   PRDLIST(1),BUMGPRD1 PUT ENTRY IN ADDED PRD LIST                  
         B     AMG2                                                             
*****                                                                           
AMG6     CLI   1(R6),10            TEST SPOT ALLOCATED                          
         BH    AMG2                YES - DO NOT REALLOCATE                      
*                                                                               
         ZIC   R1,1(R6)            SAVE ELEMENT                                 
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R6)                                                    
*                                                                               
         MVC   PRDLIST(1),BUMGPRD1                                              
         CLI   BUMGPRD2,0          TEST PIGGYBACK                               
         BE    *+10                                                             
         MVC   PRDLIST+1(1),BUMGPRD2                                            
*                                                                               
         BRAS  RE,DELEL            DELETE OLD ELEMENT                           
* NEED TO CHECK IF NEW/OLD SLNS ARE EQUAL                                       
         MVI   ERRCD,SLNERR                                                     
         ZIC   RE,BUMGSLN1                                                      
         ZIC   RF,BUMGSLN2                                                      
         AR    RE,RF               RE = OLD SLN                                 
         LTR   RE,RE                                                            
         JE    BUYERR                                                           
         CLM   RE,1,BDSEC          TEST SAME AS NEW                             
         BE    AMG10               YUP                                          
*NEED TO ADJUST SLNS                                                            
         ZIC   RF,BDSEC            GET NEW SLN                                  
*                                                                               
         ZIC   R1,BUMGSLN1                                                      
         MR    R0,RF               X NEW LEN                                    
         DR    R0,RE               / OLD LEN                                    
         LTR   R0,R0                                                            
         JNZ   BUYERR                                                           
         STC   R1,BUMGSLN1                                                      
         LTR   R1,R1                                                            
         JZ    BUYERR                                                           
*                                                                               
         ZIC   R1,BUMGSLN2                                                      
         MR    R0,RF               X NEW LEN                                    
         DR    R0,RE               / OLD LEN                                    
         LTR   R0,R0                                                            
         JNZ   BUYERR                                                           
         STC   R1,BUMGSLN2                                                      
* MAKE SURE LENGTHS ARE IN TABLE                                                
         MVC   BYTE,BUMGSLN1                                                    
         GOTO1 VCHKSLN                                                          
         MVC   BYTE,BUMGSLN2                                                    
         CLI   BYTE,0                                                           
         BE    AMG10                                                            
         GOTO1 VCHKSLN                                                          
*                                                                               
AMG10    LA    R1,ELEM                                                          
         MVI   ELEM+1,X'0E'        SET LENGTH                                   
         MVC   10(1,R1),BUMGPRD1                                                
         MVC   11(1,R1),BUMGSLN1                                                
*                                                                               
         CLI   BUMGPRD2,0                                                       
         BE    AMG12                                                            
         MVI   ELEM+1,X'12'                                                     
         MVC   14(1,R1),BUMGPRD2                                                
         MVC   15(1,R1),BUMGSLN2                                                
*                                                                               
AMG12    CLI   SVCXTRA+8,C'P'      TEST P&G                                     
         BNE   AMG20                                                            
*                                                                               
         TM    SVXFRCTL,SVXFR_MAK  TEST CALLED BY MATCHMAKER                    
         BO    *+8                                                              
         MVI   ERRAREA,X'FE'       SET TO UNWIND IN NOT MM                      
         GOTO1 TESTGLS,DMCB,ELEM                                                
         MVI   ERRAREA,0           CLEAR ERROR FLAG                             
*                                                                               
AMG20    BRAS  RE,ADDEL            ADD BACK THE ELEMENT                         
         B     AMG2                                                             
         EJECT                                                                  
*        ADD X'19' ELEMENT IF NEEDED                                            
*                                                                               
ADD19    NTR1                                                                   
         CLI   BUMGBCOD,X'FF'      OTHERWISE, ONLY IF CODE>240                  
         BNE   A19X                                                             
*                                                                               
         LLC   R1,1(R6)            BUMP PAST 0B ELEMENT                         
         AR    R6,R1                                                            
         LLC   R1,1(R6)            BUMP PAST 0C ELEMENT                         
         AR    R6,R1                                                            
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'1904'                                                 
         MVC   ELEM+2(2),BUMGCODE  SET 2-BYTE ALPHA VALUE!                      
*                                                                               
         BRAS  RE,ADDEL                                                         
*                                                                               
A19X     B     XIT                                                              
         EJECT                                                                  
*===========================================================*                   
*        REMOVE THE MAKEGOOD                                *                   
*===========================================================*                   
         SPACE 1                                                                
REMMG    NTR1                                                                   
         XC    GETNUM,GETNUM       SET FOR FULL SEARCH                          
         MVC   GETCODE,BDMGDATE                                                 
         MVC   GETSTA,BUYKSTA      FOR CABLE, MATCH ON MKGD STATION             
         MVI   GETTYPE,1           SET ENTRY TYPE = MAKEGOOD                    
*                                                                               
         MVI   LASTMG,C'N'                                                      
         MVI   ERRCD,NOMGUNAL      NO MAKEGOOD TO UNDO                          
*                                                                               
REM10    BAS   RE,GETENTRY                                                      
         BNE   BUYERR2                                                          
         CLC   MGELINE,SVKEY+11    MATCH LINE NUMBER                            
         BNE   REM10                                                            
*                                                                               
REM30    MVI   BYTE,MGAQDEL        DELETE THE ENTRY                             
         BAS   RE,CALLMGA                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    GETNUM,GETNUM       THEN START AGAIN                             
REM40    BAS   RE,GETENTRY         IS THERE ANOTHER ENTRY                       
         BNE   REM60                                                            
         CLC   MGELINE,SVKEY+11    MATCH LINE NUMBER                            
         BE    REM30               YES - GO DELETE IT                           
         B     REM40                                                            
*                                                                               
REM60    XC    GETNUM,GETNUM       START AT BEGINNING OF TABLE                  
         MVC   GETSTA,=X'FFFFFF'   IGNORE STATION (FOR CABLE)                   
         BAS   RE,GETENTRY         TO TEST THIS CODE STILL THERE                
         BE    *+8                                                              
         MVI   LASTMG,C'Y'         ELSE SET FLAG THAT IT IS GONE                
*                                                                               
         XC    BDMGDATE,BDMGDATE   CLEAR MG DATE                                
*                                                                               
         BRAS  RE,PRREASON                                                      
         GOTO1 SETCHGDT            WRITE MG LINE BACK                           
         BRAS  RE,SETPKGEL                                                      
         GOTO1 PUTREC                                                           
*                                                                               
         CLI   LASTMG,C'Y'         WAS IT THE LAST MG FOR THIS CODE             
         BNE   RMX                 NO - UNMAKEGOOD ALL MISSED SPOTS             
         BAS   RE,FIXMISS                                                       
*                                                                               
RMX      B     XIT                                                              
         EJECT                                                                  
*============================================================*                  
* SEARCH THE TSAR TABLE FOR GETCODE/GETSTA/GETTYPE           *                  
*============================================================*                  
         SPACE 1                                                                
GETENTRY NTR1                                                                   
         XC    ENTRY,ENTRY                                                      
*                                                                               
         MVI   BYTE,MGAQRDH        READ HIGH                                    
         OC    GETNUM,GETNUM       TEST FIRST TIME                              
         BZ    GETENT4                                                          
*                                                                               
GETENT2  MVI   BYTE,MGAQNXT                                                     
*                                                                               
GETENT4  BAS   RE,CALLMGA                                                       
*                                                                               
         CLI   BYTE,MGAQEOF        END OF FILE                                  
         BE    NO                                                               
         CLC   MGECODE,GETCODE     MATCH ALPHA MG CODE                          
         BNE   GETENT2                                                          
*                                                                               
         CLI   MGECODE,X'F0'       TEST FOR SPECIAL                             
         BH    GETENT2                                                          
*                                                                               
         CLI   QSTA,C'0'           TEST CABLE                                   
         BL    GETENT8              NO - ALWAYS IGNORE STATION                  
         CLC   GETSTA,=X'FFFFFF'   TEST IGNORE STATION                          
         BE    GETENT8                                                          
         CLC   MGESTA,GETSTA       MAKEGOOD STATION                             
         BNE   GETENT2                                                          
*                                                                               
GETENT8  CLI   GETTYPE,X'FF'       TEST REQUEST FOR ALL TYPES                   
         BE    GETENT10                                                         
         CLC   MGETYPE,GETTYPE     ENTRY TYPE                                   
         BNE   GETENT2                                                          
*                                                                               
GETENT10 B     YES                                                              
         EJECT                                                                  
*==============================================================*                
* FIX ALL THE MISSED SPOTS FOR CODE - BUMGCODE                 *                
*==============================================================*                
         SPACE 1                                                                
FIXMISS  NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(13),SVKEY                                                    
*                                                                               
         MVC   GETCODE,BUMGCODE                                                 
         MVI   GETTYPE,X'FF'       SET TO GET ALL REFERENCES                    
         MVC   GETSTA,=X'FFFFFF'   IGNORE STATION (FOR CABLE)                   
*                                                                               
FM10     XC    GETNUM,GETNUM       SET FOR FULL SEARCH                          
*                                                                               
         BAS   RE,GETENTRY                                                      
         BNE   FMX                                                              
*                                                                               
         CLI   MGETYPE,0           IS THIS THE MISSED SPOT                      
         BNE   *+8                                                              
         BRAS  RE,UNMGREC          FIX THE MISSED RECORD                        
*                                                                               
         MVI   BYTE,MGAQDEL        DELETE THE SPOT                              
         BAS   RE,CALLMGA                                                       
         BNE   BUYERR2                                                          
*                                                                               
         B     FM10                                                             
*                                                                               
FMX      B     XIT                                                              
         EJECT                                                                  
*                                                                               
*        SEE IF SPOT IS IN THE TABLE                                            
*                                                                               
FNDSPOT  NTR1                                                                   
         XC    ENTRY,ENTRY                                                      
         MVI   BYTE,MGAQRDH        READ HIGH                                    
         XC    ELEM,ELEM           CLEAR ENTRY SAVE AREA                        
*                                                                               
FS10     BAS   RE,CALLMGA                                                       
         CLI   BYTE,MGAQEOF         END OF FILE                                 
         BE    FS20                                                             
         MVI   BYTE,MGAQNXT                                                     
         OC    MGESTA,MGESTA        DO WE HAVE MG STATION?                      
         BZ    FS12                 -NO                                         
         CLC   MGESTA,KEY+6         -YES, MATCH ON MISSED STATION?              
         BNE   FS10                                                             
FS12     CLC   BUREFMAS,MGELINE     LINE NUMBER                                 
         BNE   FS10                                                             
         CLC   BUMGDATE,MGEDATE     DATE                                        
         BNE   FS10                                                             
         CLI   BUMGSPOT,0           TEST GENERIC SEARCH                         
         BNE   FS14                 NO - MATCH SPOT NUM                         
         CLI   MGETYPE,0            TEST TYPE = MISSED                          
         BNE   FS10                 NO                                          
         CLC   MGECODE,=C'PR'       TEST TYPE = PREEMPT                         
         BE    *+14                                                             
         CLC   MGECODE,=C'*P'                                                   
         BNE   FS10                 NO - DON'T USE                              
         B     FS16                                                             
*                                                                               
FS14     CLC   BUMGSPOT,MGESPNUM        SPOT NUMBER                             
         BNE   FS10                                                             
         CLI   MGETYPE,0                TYPE = MISSED                           
         BNE   FS10                                                             
*                                                                               
FS16     MVC   ELEM(L'ENTRY),ENTRY      SAVE THE ENTRY                          
         B     FS10                                                             
*                                                                               
FS20     OC    ELEM,ELEM                TEST FOUND ANY SPOT                     
         BZ    NO                                                               
         MVC   ENTRY,ELEM               RESTORE THE ENTRY                       
         MVC   GETCODE,MGECODE          SAVE ALPHA MGCODE                       
         B     YES                                                              
         EJECT                                                                  
*                                                                               
*        CALL BLDMGA TO BUILD TABLE OF MAKEGOODS IN TMPSTR                      
*                                                                               
SETMGAD  NTR1                                                                   
         XC    TEMPWK(TEMPWKLN),TEMPWK  CLEAR MGA WORK AREA                     
         L     R5,ADBLOCK          CALL MGABLD TO BUILD TABLE                   
         USING MGABLKD,R5                                                       
         XC    0(MGALNQ,R5),0(R5)                                               
         MVC   MGAACOM,VCOMFACS    SET A(COMFACS)                               
         MVC   MGGETBUY,VGETBUY    SET A(GETBUY)                                
         MVC   MG1OR2,SV1OR2                                                    
         MVC   MGATSAR,VTSAR       A(TSAR)                                      
         MVC   MGAIO,AREC4         A(IO AREA)                                   
         LA    R1,SVDEMOS          A(SVDEMOS)                                   
         ST    R1,MGADEM                                                        
         LA    R1,SVBRDEMS         A(SVBRDEMS)                                  
         ST    R1,MGABRDEM                                                      
*                                                                               
         TM    WRKRUPSW,WRKRUPSW_NEWDRMG    THIS FOR NEW DARE                   
         BZ    MGAD10                                                           
         OI    MGAOPT2,MGAOPT2_MINB1   SET USE MINIO BUFFER1                    
         B     MGAD20                                                           
*                                                                               
MGAD10   OI    MGAOPT2,MGAOPT2_NODSK   SET DO NOT WRITE TO DISK                 
         MVI   MGATSRPGS,40        REQUEST 40 PAGES (18432 BYTES/PAGE)          
         OI    MGATSRPGS,X'80'     SET FLAG FOR BOTH TSAR BUFFERS               
*                                                                               
MGAD20   MVC   MGAAGMD,SVAGYMD     SET AGY/MED                                  
         MVC   MGACLT,SVCLT            CLIENT                                   
         MVC   MGAPRD,SVPRD            PRODUCT                                  
         MVC   MGASTA,SVMKT            MKT/STA                                  
         OC    SVNDEF(16),SVNDEF   CANADIAN?                                    
         BZ    MGAD30                                                           
         MVC   MGASTA,SVCUTLST                                                  
*                                                                               
MGAD30   MVC   MGAEST,SVEST            ESTIMATE                                 
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000AC2'  BLDMGN                                   
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         ST    RF,VBLDMGA                                                       
         B     XIT                                                              
*                                                                               
CALLMGA  NTR1                                                                   
         L     R5,ADBLOCK          CALL MGABLD TO BUILD TABLE                   
         USING MGABLKD,R5                                                       
         MVC   MGATSNUM,GETNUM     LAST TSAR RECORD NUMBER                      
         MVC   MGAACT,BYTE         SET ACTION CODE                              
         MVC   MGAENTRY,ENTRY      ENTRY                                        
*                                                                               
         OI    MGAOPT,MGONONC      SKIP NO CHARGE                               
         OI    MGAOPT,MGONOPR      SKIP PRE-EMPT                                
*                                                                               
         GOTO1 VBLDMGA,MGABLKD                                                  
         MVC   GETNUM,MGATSNUM                                                  
         MVC   ENTRY,MGAENTRY                                                   
         MVC   BYTE,MGAERR                                                      
*                                                                               
         CLI   BYTE,0              SET CC ON EXIT                               
         B     XIT                                                              
         EJECT                                                                  
GETMKTCD NTR1                                                                   
         L     R4,AREC4                                                         
         ST    R4,AREC                                                          
         XC    0(200,R4),0(R4)                                                  
*                                                                               
GM10     MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),BUYMD                                                   
         MVC   KEY+2(5),WORK       STATION                                      
         MVC   KEY+7(2),AGYALPHA                                                
         USING STARECD,R4                                                       
         GOTO1 RDSTA                                                            
*                                                                               
GM20     PACK  DUB,SMKT                                                         
         CVB   R0,DUB                                                           
         STCM  R0,3,GETMKT                                                      
         MVC   AREC,AREC1                                                       
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
BUYERR1  DS    0H                                                               
         MVI   ERRCD,NEWERRS                                                    
*                                                                               
* SPBUY31/32 SHARE HAVE OVERLAPPING TEMPSTR PAGES                               
* SO WE CAN NEVER WRITE TSAR BUFFER IF IN CALLBASE MODE                         
* ON THE OTHER HAND, IT SHOULD NEVER BE NEEDED.                                 
*                                                                               
BUYERR2  DS    0H                                                               
         OC    SVNDEF,SVNDEF       TEST CANAD NTWK                              
         BZ    BUYERRX                                                          
         MVI   ERRAREA,X'FE'       SET FOR DC H'0',$ABEND                       
         GOTO1 STAPACK,DMCB,(C'U',KEY+4),WORK,WORK+4                            
         MVC   ERRTEXT(4),WORK+4    MOVE STATION                                
BUYERRX  GOTO1 ERROR                                                            
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        FIND THE MISSED RECORD - 'UNMISS' IT                                   
***********************************************************************         
                                                                                
UNMGREC  NTR1  BASE=*,LABEL=*                                                   
         L     R5,ADBLOCK                                                       
         USING MGABLKD,R5                                                       
         MVC   KEY+4(5),SVKEY+4    SET MARKET AND STATION                       
         OC    MGESTA,MGESTA       TEST STATION EXCEPTION                       
         BZ    *+10                                                             
         MVC   KEY+6(3),MGESTA     SET STATION FROM TABLE ENTRY                 
*                                                                               
         OC    BUEXPKEY,BUEXPKEY                                                
         BZ    *+10                                                             
         MVC   KEY(13),BUEXPKEY                                                 
         DROP  R5                                                               
*                                                                               
UM05     MVC   KEY+11(2),MGELINE   SET LINE NUMBER                              
         GOTO1 HIGH                READ MISSED LINE                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
UM07     MVI   SPOTNUM,0                                                        
         L     R6,AREC1                                                         
         USING REGELEM,R6                                                       
         LA    R6,24(R6)                                                        
         MVI   ELCDLO,X'0B'        FIND THE SPOT                                
         MVI   ELCDHI,X'0C'                                                     
*                                                                               
UM10     BRAS  RE,NEXTEL                                                        
         BNE   UMX                                                              
         CLC   RDATE,MGEDATE       IS THIS THE RIGHT SPOT                       
         BNE   UM10                                                             
         TM    RSTATUS,X'80'       IF MINUSSED                                  
         BO    UM10                SKIP                                         
         LLC   R1,SPOTNUM                                                       
         LA    R1,1(R1)            ELSE INC SPOT NUMBER                         
         STC   R1,SPOTNUM                                                       
         CLC   SPOTNUM,MGESPNUM    SPOT NUMBER                                  
         BNE   UM10                                                             
*                                                                               
         MVI   13(R6),0            CLEAR OLD MG CODE                            
         NI    RSTATUS,X'FF'-X'02' REMOVE MG ON NEW LINE                        
*                                                                               
         L     R1,ASVDARE                                                       
         USING SVDARED,R1                                                       
         OI    SVDRFLG2,SVDRFLG2_MRK   TO GIVE SPCL MESSAGE                     
         DROP  R1                                                               
*                                                                               
         XC    HALF2,HALF2                                                      
         MVC   HALF2(1),10(R6)     SAVE PRODUCT CODE(S) FOR ORDER LKUP          
         CLI   1(R6),X'0E'         DO WE HAVE A PIGGY FOR THIS MKGD?            
         BNH   *+10                                                             
         MVC   HALF2+1(1),14(R6)   PIGGYBACK                                    
*                                                                               
         LR    R5,R6               SAVE LOCATION OF ELEMENT                     
         OC    RPAY,RPAY           IF THE SPOT HASN'T BEEN PAID                 
         BNZ   UM30                                                             
         NI    RSTATUS,X'FF'-X'40' REMOVE SPOT HAS BEEN MINUSED                 
         MVI   ELCDLO,X'0C'        AND X'0C' ELEMENT                            
         MVI   ELCDHI,X'0C'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   UM30                                                             
         BRAS  RE,DELEL                                                         
*                                                                               
UM30     LR    R6,R5               RESTORE A(X'0B' ELEMENT)                     
         MVI   ELCDLO,X'19'        MISSED EXCEPTION                             
         MVI   ELCDHI,X'19'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   UM40                                                             
         BRAS  RE,DELEL                                                         
*                                                                               
UM40     GOTO1 SETCHGDT            WRITE MISSED LINE BACK                       
         BRAS  RE,SETPKGEL                                                      
         GOTO1 PUTREC                                                           
*                                                                               
UM50     L     R1,ASVDARE                                                       
         USING SVDARED,R1                                                       
         TM    SVDRFLG2,SVDRFLG2_MRK   SPCL MESSAGE NEEDED?                     
         BZ    UMX                 NO                                           
         DROP  R1                                                               
*                                                                               
         L     R5,ADBLOCK                                                       
         USING MGABLKD,R5                                                       
         XC    KEY,KEY             SETUP THE KEY AS MUCH AS WE CAN              
         LA    R1,KEY                                                           
         USING DOKEY,R1                                                         
         MVI   DCKTYPE,DCKTYPQ     X'0DB5'                                      
         MVI   DCKSUBTY,DCKSTYPQ                                                
         MVC   DCKAGMD,MGAAGMD                                                  
         MVC   DCKCLT,MGACLT                                                    
         MVC   DCKPRD,HALF2                                                     
         MVC   DCKEST,MGAEST                                                    
         MVC   DCKSTA,MGASTA+2     MGASTA IS XL5                                
         CLI   DCKSTA,X'E8'        IS THIS FOR A CABLE STATION?                 
         BL    *+8                                                              
         NI    DCKSTA+2,X'80'      YES, WE ONLY WANT SYSCODE LEVEL              
         MVC   DCKPRD2,HALF2+1                                                  
         DROP  R1                                                               
********                                                                        
         CLC   SVDARPRF+6(3),=C'000'  TRADE SPECIAL REP DEFINED?                
         BNH   UM54                   NO, NO NEED TO LOOK FOR TRADE             
         MVC   DMCB+4(4),=X'D9000ABC'  RCPACK                                   
         GOTO1 VCALLOV,DMCB                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         GOTO1 (RF),DMCB,(C'P',SVDARPRF+6),HALF2                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,AREC1                                                         
         USING BUYKEY,R1                                                        
         CLC   HALF2,BDREP         MATCH ON THE REP FOR TRADE                   
         BNE   UM54                                                             
         DROP  R1                                                               
         OI    DCKFLAG-DOKEY+KEY,DCKFTRDE                                       
********                                                                        
*                                                                               
UM54     L     RF,ASVDARE                                                       
         CLI   0(RF),C'Y'          ESTIMATE USES FLIGHTS?                       
         BNE   UM70                NO,                                          
         LA    R1,0                FLIGHT NUMBER                                
         LA    RE,72(RF)           A(AFTER ALL FLIGHT DATES)                    
         LA    RF,8(RF)            A(FIRST FLIGHT DATE)                         
*                                                                               
UM55     CR    RF,RE               PAST ALL THE FLIGHT DATES?                   
         BL    UM57                                                             
         LA    R1,1(R1)            THIS IS THE CURRENT FLIGHT NUMBER            
         B     UM60                YES, USE THE LAST FLIGHT NUMBER              
*                                                                               
UM57     OC    0(2,RF),0(RF)       ANY DATE HERE?                               
         BZ    UM60                NO, USE THE LAST FLIGHT NUMBER               
         CLC   MGEDATE,0(RF)                                                    
         BL    UM60                                                             
         LA    R1,1(R1)            THIS IS THE CURRENT FLIGHT NUMBER            
         LA    RF,4(RF)                                                         
         B     UM55                                                             
*                                                                               
UM60     STC   R1,DCKFLTNM-DOKEY+KEY                                            
*                                                                               
UM70     GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'DOKEY),KEYSAVE   NO DARE ORDER TO MODIFY                   
         BNE   UM100                                                            
         MVC   AREC,AREC3                                                       
         GOTO1 GETREC                                                           
         L     R6,AREC3                                                         
         USING DOKEY,R6                                                         
         LA    R6,24(R6)                                                        
         MVI   ELCDLO,DOSPELQ                                                   
         MVI   ELCDHI,DOSPELQ                                                   
         BRAS  RE,NEXTEL                                                        
         BNE   UM100               THERE SHOULD BE ONE                          
         USING DOSPELD,R6                                                       
         OI    DOSPFLG1,DOSPUWMG   USER UNWOUND A MKGD IN THE PAST              
         DROP  R6                                                               
         BRAS  RE,SETPKGEL                                                      
         GOTO1 PUTREC                                                           
*                                                                               
UM100    XC    KEY,KEY             NEED TO RESTORE BUYKEY WE WERE ON            
         MVC   KEY(BUYKBUY-BUYKEY),BUYKEY                                       
         MVI   KEY+10,0                                                         
         MVC   KEY+11(2),BUYKEY+10                                              
         GOTO1 HIGH                                                             
         MVC   AREC,AREC1                                                       
         GOTO1 GETREC                                                           
         BRAS  RE,GETPKGEL                                                      
*                                                                               
UMX      XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*===============================================================                
* COPY DEMOS FROM MISSED BUYLINE TO MAKEGOOD BUYLINE                            
* MAKEGOOD LINE IS IN AREC1, MISSED LINE IS IN AREC2                            
*===============================================================                
         SPACE 1                                                                
MGDEM    NTR1  BASE=*,LABEL=*                                                   
         L     R6,AREC1                                                         
         LA    R6,BDELEM                                                        
*                                                                               
MGDEM2   SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    MGDEMX                                                           
         CLI   0(R6),2                                                          
         BE    MGDEM10                                                          
         CLI   0(R6),3                                                          
         BNE   MGDEM2                                                           
*                                                                               
MGDEM10  L     R7,AREC2                                                         
         LA    R7,BDELEM-BUYREC(R7)                                             
*                                                                               
MGDEM12  SR    R0,R0                                                            
         IC    R0,1(R7)                                                         
         AR    R7,R0                                                            
         CLI   0(R7),0             NOT FOUND - IGNORE                           
         BE    MGDEM2                                                           
         CLC   0(1,R7),0(R6)       MATCH ELCODES                                
         BNE   MGDEM12                                                          
         CLI   0(R6),2             TEST ORIG DEMO                               
         BE    MGDEM20             YES                                          
         CLC   4(2,R6),4(R7)       SPILL - MATCH AGY MKT NUMS                   
         BNE   MGDEM12                                                          
*                                                                               
MGDEM20  MVC   4(20,R6),4(R7)      MOVE BOOKS/PROGS/ETC.                        
         SR    R0,R0               SET TO FIND THE VALUES                       
         IC    R0,1(R6)                                                         
         AHI   R0,-24                                                           
         BNP   MGDEMX                                                           
         SRL   R0,3                                                             
         LA    R1,24(R6)                                                        
*                                                                               
MGDEM22  LHI   RF,SVB0PROF-BUYSAVE                                              
         AR    RF,RA                                                            
*                                                                               
         CLI   8(RF),C'O'          TEST OVERWRITE OVERRIDES                     
         BE    MGDEM24                                                          
*                                                                               
         TM    4(R1),X'80'         DEMO OVERRIDE?                               
         BO    *+8                 YES                                          
MGDEM24  BAS   RE,MGDGET                                                        
*                                                                               
         LA    R1,8(R1)                                                         
         BCT   R0,MGDEM22                                                       
*                                                                               
         B     MGDEM2              CHECK FOR MORE ELEMENTS                      
*                                                                               
MGDEMX   XIT1                                                                   
*                                                                               
MGDGET   NTR1                                                                   
         SR    RE,RE                                                            
         IC    RE,1(R7)                                                         
         AHI   RE,-24                                                           
         BNP   MGDEMX                                                           
         SRL   RE,3                                                             
         LA    RF,24(R7)                                                        
*                                                                               
MGDGET2  CLC   0(3,R1),0(RF)       MATCH DEMO CODE                              
         BE    MGDGET4                                                          
         LA    RF,8(RF)                                                         
         BCT   RE,MGDGET2                                                       
         B     MGDEMX                                                           
*                                                                               
MGDGET4  MVC   3(5,R1),3(RF)       MOVE OVER OLD DEMO                           
         B     MGDEMX                                                           
         LTORG                                                                  
         EJECT                                                                  
*===================================================================*           
*        IF THIS A MAKEGOOD FOR A CANADIAN NETOWRK BUY              *           
*        MAKE SURE THE LOCAL STATIONS IN THE MAKEGOOD LINE AND THE  *           
*        MISSED LINE ARE THE SAME                                   *           
*                                                                   *           
*        NEED TO MATCH MKT/STATION IN NTWK ELMS                     *           
*                                                                   *           
*NTRY    AREC1 MISSED LINE                                          *           
*        AREC2 MAKEGOOD LINE                                        *           
*===================================================================*           
         SPACE 2                                                                
         DS    0D                                                               
STAMTCH  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    SVNDEF(16),SVNDEF   SKIP IF NOT A CANADIAN NTWK BUY              
         BZ    STAMTCHX                                                         
*                                                                               
         L     R6,AREC1            POINT TO MISSED BUYLINE                      
*                                                                               
         USING BUYREC,R6           ESTABLISH BUY RECORD                         
*                                                                               
         OC    BUYMSTA(2),BUYMSTA  SKIP IF NOT MARKET ZERO BUY                  
         BNZ   STAMTCHX                                                         
*                                                                               
         LA    R6,24(R6)           POINT TO FIRST ELEMENT IN RECORD             
         USING NTWKELEM,R6         ESTABLISH AS NETWORK ELEMENT                 
*                                                                               
         SR    R0,R0               INIT ELEMENT COUNTER                         
         SR    RF,RF                                                            
*                                                                               
STMT1LP  DS    0H                                                               
*                                                                               
         CLI   NTWKCODE,0          DONE IF NO MORE ELEMENTS IN RECORD           
         BE    STMT1DN                                                          
*                                                                               
         CLI   NTWKCODE,X'68'      SKIP IF NOT A NETWORK ELEMENT                
         BNE   STMT1CN                                                          
*                                                                               
         AHI   R0,1                BUMP ELEMENT COUNTER                         
*                                                                               
*        MATCH TO A NETWORK ELEMENT IN MAKEGOOD LINE                            
*                                                                               
         L     R5,AREC2            POINT TO MAKEGOOD BUYLINE                    
*                                  MUST BE MARKET ZERO BUYLINE                  
         OC    BUYMSTA-BUYKEY(2,R5),BUYMSTA-BUYKEY(R5)                          
         BNZ   STAMTCHX                                                         
*                                                                               
         LA    R5,24(R5)           POINT TO FIRST ELEMENT IN RECORD             
*                                                                               
STMT2LP  DS    0H                                                               
*                                                                               
         CLI   NTWKCODE-NTWKELEM(R5),0    DONE IF NO MORE ELMS IN REC           
         BE    STMT2DN                                                          
*                                                                               
         CLI   NTWKCODE-NTWKELEM(R5),X'68'  SKIP IF NOT A NTWK ELEMENT          
         BNE   STMT2CN                                                          
*                                                                               
         CLC   NTWKMKST,NTWKMKST-NTWKELEM(R5)   MATCH MKT/STA                   
         BE    STMT2FD                                                          
*                                                                               
STMT2CN  DS    0H                                                               
*                                                                               
         IC    RF,NTWKLEN-NTWKELEM(R5)  BUMP TO NEXT ELEMENT                    
         LA    R5,0(RF,R5)                                                      
*                                                                               
         B     STMT2LP                                                          
*                                                                               
STMT2DN  DS    0H                  NO MATCH FOUND                               
*                                                                               
         B     STAMTCHE                                                         
*                                                                               
STMT2FD  DS    0H                  ELEMENT MATCH                                
*                                                                               
STMT1CN  DS    0H                                                               
*                                                                               
         IC    RF,NTWKLEN          BUMP TO NEXT ELEMENT                         
         LA    R6,NTWKELEM(RF)                                                  
*                                                                               
         B     STMT1LP                                                          
*                                                                               
STMT1DN  DS    0H                                                               
*                                                                               
*        NUMBER OF NTWK ELMS IN BOTH RECORDS MUST BE THE SAME                   
*                                                                               
*        COUNT NTWK ELEMENTS IN MAKEGOOD BUYLINE                                
*                                                                               
         L     R5,AREC2            POINT TO MAKEGOOD BUYLINE                    
         LA    R5,24(R5)           POINT TO FIRST ELEMENT IN RECORD             
*                                                                               
         SR    R1,R1               INIT ELEMENT COUNTER                         
*                                                                               
STMT3LP  DS    0H                                                               
*                                                                               
         CLI   NTWKCODE-NTWKELEM(R5),0    DONE IF NO MORE ELMS IN REC           
         BE    STMT3DN                                                          
*                                                                               
         CLI   NTWKCODE-NTWKELEM(R5),X'68'  SKIP IF NOT A NTWK ELEMENT          
         BNE   STMT3CN                                                          
*                                                                               
         AHI   R1,1                BUMP ELEMENT COUNTER                         
*                                                                               
STMT3CN  DS    0H                                                               
*                                                                               
         IC    RF,NTWKLEN-NTWKELEM(R5)  BUMP TO NEXT ELEMENT                    
         LA    R5,0(RF,R5)                                                      
*                                                                               
         B     STMT3LP                                                          
*                                                                               
STMT3DN  DS    0H                  NO MATCH FOUND                               
*                                                                               
         CR    R0,R1               NUMBER OF ELEMENTS MUST MATCH                
         BNE   STAMTCHE                                                         
*                                                                               
STAMTCHX DS    0H                                                               
         XIT1                                                                   
*                                                                               
STAMTCHE DS    0H                  NOT SAME LIST OF STAS IN BOTH RECS           
         LHI   RF,MSMGSTAS         SET ERROR CODE                               
*                                                                               
         STCM  RF,3,NERRCD                                                      
         MVI   ERRCD,NEWERRS       INDICATE LARGE ERROR CODE NUMBER             
*                                                                               
         GOTO1 ERROR                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*===================================================================*           
*        DELETE ALL ELEMENTS BETWEEN X'10' AND X'19'                *           
*        ADD MINUS OTO ELEMENT                                      *           
*===================================================================*           
         SPACE 1                                                                
ADD0C    NTR1  BASE=*,LABEL=*                                                   
         LR    R5,R6                                                            
*                                                                               
ADC10    ZIC   R0,1(R5)            BUMP TO NEXT ELEMENT                         
         AR    R5,R0                                                            
         CLI   0(R5),X'11'         TEST INTG                                    
         BL    *+12                                                             
         CLI   0(R5),X'19'                                                      
         BNH   ADC10                                                            
         CLI   0(R5),X'0C'         TEST OTO                                     
         BNE   *+12                NO                                           
         TM    6(R5),X'80'         TEST MINUS                                   
         BO    ADCEQX              YES - DONT NEED ANOTHER                      
*                                                                               
         TM    SVPWFLG,X'01'       TEST PW EST                                  
         BZ    ADC20                                                            
         TM    WRKRUPSW,WRKRUPSW_ISDRMG     IS THIS A DARE MAKEGOOD             
         BO    ADC20                        YES - LET IT GO                     
         OC    4(2,R6),4(R6)       TEST MISSED SPOT PAID                        
         BZ    ADC30                                                            
         MVI   ERRCD,PAIDOTO       PW CLIENT CANNOT -OTO PAID SPOT              
         B     ADCNEQX                                                          
*                                                                               
ADC20    TM    SVOPT1,SVOPT1_PAIDOTO  TEST SPCL CODE REQD IF PAID               
         BZ    ADC30                  NO                                        
         TM    WRKRUPSW,WRKRUPSW_ISDRMG     IS THIS A DARE MAKEGOOD             
         BO    ADC30               YES - HOPELESS                               
         OC    4(2,R6),4(R6)       TEST MISSED SPOT PAID                        
         BZ    ADC22               NO                                           
* YES PAID - SEE IF SPECIAL CODE ENTERED                                        
         CLC   SVOTOCHR,8+4(R2)    C,MG<X>=                                     
         BE    ADC30               YES                                          
         MVI   ERRCD,PAIDOTO                                                    
         B     ADCNEQX                                                          
* NOT PAID - SPECIAL CODE MUST NOT BE ENTERED                                   
ADC22    CLC   SVOTOCHR,8+4(R2)                                                 
         BNE   ADC30                                                            
         MVI   ERRCD,UNPDOTO                                                    
         B     ADCNEQX                                                          
*                                                                               
ADC30    XC    ELEM,ELEM           ADD MINUS OTO                                
         MVC   ELEM(18),0(R6)                                                   
         XC    ELEM+4(2),ELEM+4    CLEAR PAY DATE                               
         XC    ELEM+12(2),ELEM+12  AND PRODUCT BILL DATES                       
         XC    ELEM+16(2),ELEM+16                                               
         OI    ELEM+6,X'80'        SET MINUS SPOT                               
         NI    ELEM+6,X'BD'        UNSET MINUSSED/MADE GOOD                     
         MVI   ELEM,X'0C'          SET OTO EL CODE                              
*                                                                               
ADC40    ZIC   R0,1(R6)            BUMP PAST ORIGINAL 0B ELEMENT                
         AR    R6,R0                                                            
*                                                                               
ADC42    CLI   0(R6),X'10'                                                      
         BL    ADC46                                                            
         CLI   0(R6),X'19'                                                      
         BH    ADC46                                                            
         LA    R6,0(R6)            CLEAR HOB                                    
         GOTO1 VRECUP,DMCB,BUYREC,(R6)                                          
         B     ADC42                                                            
*                                                                               
ADC46    GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
*                                                                               
ADCEQX   CR    RB,RB               SET CC EQU                                   
         B     ADCX                                                             
*                                                                               
ADCNEQX  LTR   RB,RB               SET CC NEQ                                   
*                                                                               
ADCX     XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*==================================================================*            
*        ADD A REASON CODE ELEMENT                                 *            
*==================================================================*            
         SPACE 1                                                                
PRREASON NTR1  BASE=*,LABEL=*                                                   
         L     R1,AREC                                                          
         SR    R0,R0               FIND ELEMENT IN RECORD                       
         SR    R1,R1               SET SWITCH THAT X'65' NOT FOUND              
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'65'                                                     
         MVI   ELCDHI,X'65'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   PRR10                                                            
         BRAS  RE,DELEL                                                         
         LA    R1,1                                                             
*                                                                               
PRR10    OC    BUREFMAS,BUREFMAS   IF C,MG=0                                    
         BNZ   PRR20                                                            
         LTR   R1,R1               IF A X'65' WASN'T FOUND                      
         BZ    PRX                    DON'T DELETE COMMENT                      
         SR    R0,R0               ELSE FIND COMMENT ELEMENT                    
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'66'                                                     
         MVI   ELCDHI,X'66'                                                     
*                                                                               
PRR15    BRAS  RE,NEXTEL                                                        
         BNE   PRX                                                              
         CLI   2(R6),1             IF IT'S COMMENT #1                           
         BNE   PRR15                                                            
         BRAS  RE,DELEL            DELETE IT                                    
         B     PRX                                                              
*                                                                               
PRR20    CLC   BUMGRSCD,SPACES     IF THERE IS NO REASON CODE                   
         BNH   PRX                 DONE                                         
         XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'6509'    ELEMENT CODE/LENGTH                          
         MVC   ELEM+2(6),BUMGRSCD  REASON CODE                                  
         MVC   ELEM+8(1),BUMGRSOF         OFFICE                                
         BRAS  RE,ADDEL                                                         
*                                                                               
         SR    R0,R0               FIND COMMENT ELEMENT                         
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'66'                                                     
         MVI   ELCDHI,X'66'                                                     
*                                                                               
PRR25    BRAS  RE,NEXTEL                                                        
         BNE   PRR30                                                            
         CLI   2(R6),1             REPLACE 1ST COMMENT WITH REASON CODE         
         BNE   PRR25                                                            
         BAS   RE,RENUM1           RENUMBER COMMENTS                            
*                                                                               
PRR30    LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'66'                                                     
         MVI   ELCDHI,X'66'                                                     
         BRAS  RE,NEXTEL                                                        
         XC    ELEM,ELEM                                                        
         MVC   ELEM(2),=X'6635'    ELEMENT CODE/LENGTH                          
         MVI   ELEM+2,1            COMMENT NUMBER                               
         MVC   ELEM+3(L'BUMGCOM),BUMGCOM  COMMENT                               
         BRAS  RE,ADDEL                                                         
*                                                                               
PRX      XIT1                                                                   
         EJECT                                                                  
*        RE-NUMBER THE COMMENTS                                                 
*                                                                               
RENUM1   NTR1                                                                   
         SR    R1,R1               COUNT N'COMMENTS                             
         LA    R6,BDELEM                                                        
*                                                                               
RN110    BRAS  RE,NEXTEL                                                        
         BNE   RN120                                                            
         LA    R1,1(R1)            INC N'COMMENTS                               
         B     RN110                                                            
*                                                                               
RN120    LA    R6,BDELEM                                                        
         LR    R2,R1                                                            
*                                                                               
RN130    BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         BCT   R2,RN130                                                         
         LR    R3,R1                                                            
         LA    R3,1(R3)                                                         
         STC   R3,2(R6)                                                         
         BCT   R1,RN120                                                         
*                                                                               
RN1X     B     PRX                                                              
         LTORG                                                                  
         EJECT                                                                  
*====================================================================           
* FIND PACKAGE ELEMENT AND CONVERT TO 2-BYTE FORMAT IF NECESSARY                
* IF FOUND R6 HAS A(PKGEL) ON EXIT                                              
*====================================================================           
                                                                                
GETPKGEL NTR1  BASE=*,LABEL=*                                                   
                                                                                
         MVI   ELCDLO,5                                                         
         MVI   ELCDHI,5                                                         
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         JNE   NEQXIT                                                           
*                                                                               
         USING PKGELEM,R6                                                       
         TM    PKGIND,X'10'        TEST 2-BYTE LINENUMS IN ELEMENT              
         JO    GETPKGX                                                          
* CONVERSION REQUIRED                                                           
X        USING PKGELEM,ELEM                                                     
         XC    ELEM,ELEM                                                        
         MVC   ELEM(3),0(R6)       MOVE ELEM CODE/LEN/IND                       
         OI    X.PKGIND,X'10'      SET 2-BYTE FLAG                              
         LLC   R0,PKGLEN                                                        
         AHI   R0,-3                                                            
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    RE,PKGLINES         AND POINT TO FIRST                           
         LA    R4,X.PKGLINES                                                    
*                                                                               
GETPKG2  MVC   1(1,R4),0(RE)       MOVE 1-BYTE LINE TO 2-BYTE FIELD             
         LA    RE,1(RE)                                                         
         LA    R4,2(R4)                                                         
         BCT   R0,GETPKG2                                                       
*                                                                               
         LA    R0,ELEM                                                          
         SR    R4,R0               GET NEW ELEMENT LENGTH                       
         STC   R4,X.PKGLEN                                                      
*                                                                               
         BRAS  RE,DELEL            DELETE OLD PKGEL                             
         BRAS  RE,ADDEL            INSERT NEW PKGEL                             
*                                                                               
GETPKGX  CR    R6,R6               SET CC EQ                                    
         XIT1  REGS=(R6)                                                        
         DROP  X                                                                
         LTORG                                                                  
         EJECT                                                                  
*====================================================================           
* PROGRAM HAS BUILT 2-BYTE LINE NUMBERS IN NEW PKGEL                            
* IF NECESSARY, CONVERT BACK TO 1-BYTE LINE NUMBERS AND CHANGE IND              
*====================================================================           
                                                                                
SETPKGEL NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   SV1OR2,2                                                         
         JE    EQXIT                                                            
*                                                                               
         MVI   ELCDLO,5                                                         
         MVI   ELCDHI,5                                                         
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         JNE   NEQXIT                                                           
*                                                                               
         USING PKGELEM,R6                                                       
X        USING PKGELEM,ELEM                                                     
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM(3),0(R6)         MOVE ELEM CODE/LEN/IND                     
         NI    X.PKGIND,X'FF'-X'10'  UNSET 2-BYTE FLAG                          
         LLC   R0,PKGLEN                                                        
         AHI   R0,-3                                                            
         SRL   R0,1                SET R0 FOR NUMBER OF LINES                   
         LTR   R0,R0                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         LA    RE,PKGLINES         AND POINT TO FIRST                           
         LA    R4,X.PKGLINES                                                    
*                                                                               
SETPKG2  MVC   0(1,R4),1(RE)       MOVE 2-BYTE LINE TO 1-BYTE FIELD             
         LA    R4,1(R4)                                                         
         LA    RE,2(RE)                                                         
         BCT   R0,SETPKG2                                                       
*                                                                               
         LA    R0,ELEM                                                          
         SR    R4,R0               GET NEW ELEMENT LENGTH                       
         STC   R4,X.PKGLEN                                                      
*                                                                               
         BRAS  RE,DELEL            DELETE OLD PKGEL                             
         BRAS  RE,ADDEL            INSERT NEW PKGEL                             
         J     EQXIT                                                            
         DROP  X                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPMGADN                                                        
         EJECT                                                                  
         PRINT OFF                                                              
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE DDTSARD                                                        
         EJECT                                                                  
       ++INCLUDE SPGENDOV                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENDRORD                                                     
         EJECT                                                                  
       ++INCLUDE SPBUYWORK                                                      
         EJECT                                                                  
         PRINT ON                                                               
SPBUYWKD DSECT                                                                  
         ORG   MSTRBUY                                                          
TEMPWK   DS    0D                                                               
TSARBLK  DS    CL48                                                             
ENTRY    DS    XL64                                                             
THISLINE DS    XL2                                                              
COUNTER  DS    XL1                                                              
CDTYPE   DS    CL1                 USING AN EXISTING CODE                       
LASTMG   DS    CL1                                                              
*SPTFND   DS    CL1                                                             
SPOTNUM  DS    XL1                                                              
LASTDATE DS    XL2                                                              
*MYCLT    DS    CL3                                                             
*RATING   DS    CL10                                                            
TEMPWKLN EQU   *-TEMPWK                                                         
*                                                                               
VBLDMGA  DS    A                                                                
GETNUM   DS    H                   LAST TSAR RECORD NUMBER                      
GETCODE  DS    CL2                                                              
GETMKT   DS    XL2                                                              
GETSTA   DS    XL3                                                              
GETTYPE  DS    XL1                                                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'065SPBUY13   10/17/19'                                      
         END                                                                    
