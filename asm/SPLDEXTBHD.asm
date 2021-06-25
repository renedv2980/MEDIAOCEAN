*          DATA SET SPLDEXTBHD AT LEVEL 046 AS OF 03/15/04                      
*PHASE SP@XTBHD                                                                 
*INCLUDE PRNTBL                                                                 
         TITLE 'CONVERT OLD BILL HEADERS TO NEW FORMAT'                         
*                                                                               
********************************************************************            
*                                                                               
* THIS EXTERN IS TO BE USED TO CONVERT "OLD"-STYLE BILL HEADER                  
* RECORDS TO THE LATEST FORMAT.                                                 
*                                                                               
*   1. THE CANADIAN PROVINCIAL VAT "ELEMENTS" HAVE BEEN MOVED FURTHER           
*      DOWN IN THE RECORD.                                                      
*   2. PL6 ACCUMULATORS HAVE BEEN ADDED FOR GROSS, NET, AND ACTUAL              
*      (BOTH VENDOR $ AND COS2 $).                                              
*   3. FIELD BLMGR AND BLMKT HAVE BEEN MOVED OUT OF BRETACCT AND GIVEN          
*      THEIR OWN HOME.                                                          
*   4. FIELDS BILCTYP AND BLPKGNM HAVE BEEN ORG'D INTO BRETACCT                 
*      (FOR SPACE EFFICIENCY: THESE ARE NETPAK-ONLY FIELDS).                    
*                                                                               
* THIS EXTERN SHOULD ONLY BE NEEDED WHEN CONVERTING OLD DATA WHICH              
* MUST BE RESTORED BY AGENCY REQUEST.                                           
*                                                                               
********************************************************************            
*                                                                               
*                                                                               
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 20,DMLDEXT                                                       
         USING WORKD,RC                                                         
         EJECT                                                                  
*                                                                               
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         L     R9,VLDDEFN                                                       
         USING LDDEFND,R9                                                       
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         SPACE 2                                                                
*                                                                               
* INITIALIZE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R6,AREC                                                          
         USING BILLHDRD,R6                                                      
         CLI   BKEYTYPE,0          MUST BE BILL HEADER RECORD                   
         BNE   DMXKEEP                                                          
         CLI   BKEYAM,0                                                         
         BE    DMXKEEP                                                          
         OC    BKEYYSRV(5),BKEYYSRV                                             
         BZ    DMXKEEP                                                          
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,3,BLEN           R4 = RECORD LENGTH                           
*                                                                               
         CHI   R4,BILSTAT3-BILLREC+1  REC LONG ENOUGH TO HAVE BILSTAT3?         
         BL    *+12                NO                                           
         TM    BILSTAT3,BSTCNV1Q+BSTCNV2Q   RECORD ALREADY CONVERTED?           
         BO    DMXKEEP                      YES: NO CHANGES NECESSARY           
*                                                                               
         CLI   TRACE,C'Y'                                                       
         BNE   SKIP1                                                            
         GOTO1 =V(PRNTBL),DMCB,=C'OLD BILL HEADER RECORD',(R6),        +        
               C'DUMP',(R4),=C'1D',(C'P',LPRINT)                                
*                                                                               
SKIP1    DS    0H                                                               
         CHI   R4,BILSTAT3-BILLREC+1  REC LONG ENOUGH TO HAVE BILSTAT3?         
         BL    *+12                NO                                           
         TM    BILSTAT3,BSTCNV1Q   RECORD NEEDS CONVERSION "1"?                 
         BO    NETFLDS             NO                                           
*                                                                               
         CHI   R4,BVATS_OLD-BILLREC  IF L'RECORD > 160...                       
         BH    DOVATS                ...THEN HANDLE PROVINCIAL VAT(S)           
*                                                                               
         LHI   RE,BILFRLEN         NEW RECORD LENGTH WITHOUT VAT DATA           
         SR    RE,R4               RE = NUMBER OF NEW BYTES TO ADD              
         LA    R1,BILLREC(R4)      R1 POINTS JUST PAST END OF RECORD            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    0(0,R1),0(R1)       CLEAR THE NEW BYTES                          
*                                                                               
         MVC   BLEN,=Y(BILFRLEN)   UPDATE RECORD LENGTH                         
*                                                                               
         B     PACCUMS                                                          
*                                                                               
DOVATS   DS    0H                                                               
         SR    R3,R3                                                            
         ICM   R3,1,BILNPVTS       COUNT OF VAT ELEMS                           
         BZ    PACCUMS             NO VAT ELEMENTS TO MOVE                      
*                                                                               
         MHI   R3,BILPVLEN         R3 = TOTAL LENGTH OF ELEM SET                
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   BILPVELD(0),BVATS_OLD  COPY VAT ELEMENTS TO NEW LOCATION         
         XC    BVATS_OLD,BVATS_OLD    CLEAR THE OLD VAT LOCATION                
*                                                                               
         AHI   R3,1                RESTORE R3 TO LENGTH OF ELEMENT SET          
         AHI   R3,BILFRLEN         PLUS FIXED PORTION = RECORD LENGTH           
         STCM  R3,3,BLEN           UPDATE RECORD LENGTH                         
*                                                                               
PACCUMS  DS    0H                                                               
         MVC   WORK(10),BAMT       GROSS IS IN EBCDIC                           
         ZIC   R0,WORK+9           GROSS: EXAMINE SIGN NIBBLE                   
         SRL   R0,4                                                             
         CHI   R0,X'0C'            POSITIVE VALUE: OKAY                         
         BE    DOGROSS                                                          
         CHI   R0,X'0D'            NEGATIVE VALUE: OKAY                         
         BE    DOGROSS                                                          
         CHI   R0,X'0F'                                                         
         BE    *+6                                                              
         DC    H'0'                WHAT KIND OF SIGN NIBBLE IS THIS?            
         NI    WORK+9,X'CF'        CHANGE SIGN NIBBLE X'F' TO X'C'              
DOGROSS  PACK  BGRSP,WORK(10)      GROSS                                        
*                                                                               
         ICM   R0,15,BNET          NET                                          
         CVD   R0,DUB                                                           
         MVC   BNETP,DUB+2                                                      
*                                                                               
         ICM   R0,15,BACTUAL       ACTUAL                                       
         CVD   R0,DUB                                                           
         MVC   BACTP,DUB+2                                                      
*                                                                               
         ICM   R0,15,BCCGRS        GROSS (COS2)                                 
         CVD   R0,DUB                                                           
         MVC   BGRS2P,DUB+2                                                     
*                                                                               
         ICM   R0,15,BCCNET        NET (COS2)                                   
         CVD   R0,DUB                                                           
         MVC   BNET2P,DUB+2                                                     
*                                                                               
         ICM   R0,15,BCCACT        ACTUAL (COS2)                                
         CVD   R0,DUB                                                           
         MVC   BACT2P,DUB+2                                                     
*                                                                               
         MVC   BLMGR,SPACES        INITIALIZE MKTGRP AND MKT TO SPACES          
         MVC   BLMKT,SPACES                                                     
         CLI   BRETAIL,0           RETAIL BILL?                                 
         BNE   WRITEIT             YES                                          
*                                                                               
         MVC   BLMGR,BLMGR_OLD     NO: COPY MKTGRP AND MKT...                   
         MVC   BLMKT,BLMKT_OLD     ... TO NEW LOCATION                          
         XC    BLMGR_OLD,BLMGR_OLD                                              
         XC    BLMKT_OLD,BLMKT_OLD                                              
*                                                                               
WRITEIT  DS    0H                                                               
         OI    BILSTAT3,BSTCNV1Q   FLAG RECORD: "CONVERSION 1"                  
*                                                                               
NETFLDS  DS    0H                                                               
         CHI   R4,BILSTAT3-BILLREC+1  REC LONG ENOUGH TO HAVE BILSTAT3?         
         BL    *+12                NO                                           
         TM    BILSTAT3,BSTCNV2Q   RECORD NEEDS CONVERSION "2"?                 
         BO    DONE                NO                                           
*                                                                               
         XC    BRETACCT(3),BRETACCT   CLEAR FIRST 3 BYTES OF BRETACCT           
         MVC   BILCTYP,BLCTYP_OLD     COPY COST TYPE TO NEW LOCATION            
         MVC   BLPKGNM,BLPKGNM_OLD    COPY PACKAGE NAME TO NEW LOCATION         
         XC    BLCTYP_OLD,BLCTYP_OLD  CLEAR OLD LOCATIONS                       
         XC    BLPKGNM_OLD,BLPKGNM_OLD                                          
         OI    BILSTAT3,BSTCNV2Q      FLAG RECORD: CONVERSION "2"               
*                                                                               
DONE     DS    0H                                                               
         CLI   TRACE,C'Y'                                                       
         BNE   SKIP2                                                            
         SR    R0,R0                                                            
         ICM   R0,3,BLEN                                                        
         GOTO1 =V(PRNTBL),DMCB,=C'NEW BILL HEADER RECORD',(R6),        +        
               C'DUMP',(R0),=C'1D',(C'P',LPRINT)                                
*                                                                               
SKIP2    DS    0H                                                               
         L     R0,TOTCNT                                                        
         AHI   R0,1                                                             
         ST    R0,TOTCNT                                                        
*                                                                               
         B     DMXKEEP                                                          
         DROP  R6                                                               
                                                                                
DMXEOF   DS    0H                                                               
         MVC   P(34),=C'TOTAL NO. CONVERTED BILL HEADERS: '                     
         EDIT  TOTCNT,(8,P+34),ZERO=NOBLANK                                     
         GOTO1 VPRINTER                                                         
*                                                                               
         B     DMXIT                                                            
         EJECT                                                                  
TOTCNT   DC    F'0'                TOTAL NO. RECS CHANGED                       
TRACE    DC    C'Y'                'Y' = DO PRNTBL TRACE                        
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
WORK     DS    CL64                                                             
DMCB     DS    6F                                                               
APARM    DS    A                                                                
         DS    0F                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
         SPACE 3                                                                
*DMLDDEFN                                                                       
*DDDPRINT                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
         PRINT ON                                                               
         EJECT                                                                  
BILLHDRD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
         ORG   BRETACCT                                                         
BLMGR_OLD DS   CL5                 MGR CODE                                     
BLMKT_OLD DS   CL4                 MKT CODE                                     
         ORG   BILSTAT2+1                                                       
BLCTYP_OLD DS  CL1                 NETPAK COST TYPE                             
         ORG   BLREVINO+2                                                       
BLPKGNM_OLD DS CL5                 NETPAK PACKAGE NAME                          
         ORG   BILPVELD-52                                                      
BVATS_OLD DS   XL52                OLD VAT 'ELEMENT' LOCATION                   
         ORG                                                                    
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'046SPLDEXTBHD03/15/04'                                      
         END                                                                    
