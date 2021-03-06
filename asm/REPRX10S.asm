*          DATA SET REPRX10S   AT LEVEL 021 AS OF 08/02/99                      
*PHASE T81A10A                                                                  
         TITLE 'REPRP10(T81A10) - REP PC SELLERS BLACK BOX ROUTINES'            
*********************************************************************           
* ALL ROUTINES SHOULD FOLLOW USE THE FOLLOWING OUTPUT FORMAT:                   
*                                                                               
*   CONDITION CODE NOT EQUAL                                                    
*                                                                               
*       - INDICATES THE ROUTINE FAILED                                          
*            COULD BE BECAUSE OF INVALID DATA, NO DATA ON FILE, ETC.            
*       - REASON/ERROR CODE IN FIRST HALF WORD OF OUTPUT AREA                   
*                                                                               
*   CONDITION CODE EQUAL                                                        
*                                                                               
*       - INDICATES THE ROUTINE SUCCEEDED                                       
*       - OUPUT AREA HAS THE FOLLOWING FORMAT:                                  
*          BYTE 0-1    LENGTH OF DATA FOR A SINGLE ITEM (I)                     
*          BYTE 2      NUMBER OF SUBDIVISIONS IN A SINGLE DATA ITEM (N)         
*          BYTE 3-(3+2N) 1 BYTE LENGTH OF EACH DATA SUBDIVISION                 
*                        1 BYTE DATA TYPE CODE                                  
*          BYTE (4+2N)-(4+2N+JI)   DATA (J = 0 - NUMBER OF ITEMS)               
*          BYTE (5+2N+JI)          00  END OF DATA                              
*-------------------------------------------------------------------*           
* HISTORY:                                                          *           
*                                                                   *           
* 05/05/1999 (JRD) FIX INFINITE LOOP ON SINGLE OFFICE AGENCY        *           
* 06/08/1999 (JRD) FIX COMPARE IN MULTI OFFICE AGENCY CHECK         *           
*                  NEW SALESPERSON DATA: OFFICE CODE                *           
*                                        OFFICE NAME                *           
*                                        MANAGER FLAG               *           
*                                        EMAIL ADDRESS              *           
*                                                                   *           
* 06/15/1999 (JRD) NEW SALESPERSON DATA: OFFICE ADDRESS             *           
* 06/25/1999 (JRD) DEV SALESPERSON AND DEV TYPE                     *           
*                                                                   *           
*********************************************************************           
T81A10   CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL 0,T81A10,RR=R2,CLEAR=YES                                         
         L     RC,0(R1)                                                         
         USING WORKD,RC                                                         
         MVC   OVPARMS,0(R1)                                                    
         LA    R1,4(R1)            SUBROUTINES DON'T WANT RC                    
         ST    R2,OVRELO                                                        
*                                                                               
         LR    R7,RB                                                            
         AH    R7,=Y(COMMON-T81A10)                                             
         USING COMMON,R7                                                        
*                                                                               
         SRL   RF,32-8                                                          
         CLM   RF,1,=AL1(ROUTSN)                                                
         BNH   *+6                                                              
         DC    H'0'                                                             
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         BNM   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SLL   RF,2                                                             
         L     RF,ROUTS(RF)                                                     
         A     RF,OVRELO                                                        
         BASR  RE,RF                                                            
         B     EXIT                                                             
*                                                                               
ROUTS    DS    0F                                                               
         DC    A(VSTA)                                                          
         DC    A(VAGY)                                                          
         DC    A(VADV)                                                          
         DC    A(VPRD)                                                          
         DC    A(VSAL)                                                          
         DC    A(GDPLIST)                                                       
         DC    A(GMSLIST)                                                       
         DC    A(GCTLIST)                                                       
         DC    A(GSALSTX)                                                       
         DC    A(VFLIGHT)                                                       
         DC    A(AGYADDR)                                                       
         DC    A(IBKLIST)                                                       
         DC    A(VDEVSAL)                                                       
         DC    A(VDEVTYP)                                                       
         DC    A(GDCTLST)                                                       
ROUTSN   EQU   (*-ROUTS)/4                                                      
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
********************************************************************            
COMMON   DS    0D                                                               
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITNO   LTR   RB,RB               SET CC NOT EQUAL                             
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
EXIT     DS    0H                  JUST EXIT                                    
         MVC   AIOREC,AIO1         ALWAYS RESTORE TO AIO1                       
         XIT1                                                                   
********************************************************************            
* LITERALS AND CONSTANTS                                                        
********************************************************************            
FF       EQU   X'FF'                                                            
         EJECT                                                                  
         DS    0D                                                               
********************************************************************            
* INPUT  :  P1       A(CL5 STATION CALL LETTERS)                                
*           P2       A(OUTPUT AREA)                                             
*                                                                               
* OUTPUT :  CC =     STATION VALID                                              
*           CC !=    STATION INVALID                                            
*                                                                               
* VALID OUTPUT AREA FORMAT:                                                     
*           STATION CALL LETTERS                                                
*           STATION MARKET NAME                                                 
*           STATION CHANNEL                                                     
*           STATION AFFILIATE                                                   
*                                                                               
********************************************************************            
VSTA     NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*VSTA*'                                                      
*                                                                               
         LM    R2,R3,0(R1)                                                      
*                                                                               
K        USING RSTAKEY,KEY                                                      
         XC    K.RSTAKEY,K.RSTAKEY                                              
         MVI   K.RSTAKTYP,X'02'                                                 
         MVC   K.RSTAKREP,REPALPHA     REP POWER CODE                           
         MVC   K.RSTAKSTA,0(R2)        STATION CALL LETTERS                     
         OC    K.RSTAKSTA,SPACES                                                
         CLI   K.RSTAKSTA+4,C'1'   SATELLITE #1?                                
         BE    VSTA0010                                                         
         CLI   K.RSTAKSTA+4,C'2'   SATELLITE #2?                                
         BE    VSTA0010                                                         
         CLI   K.RSTAKSTA+4,C'T'   TV?                                          
         BNE   VSTA0020                                                         
VSTA0010 DS    0H                                                               
         MVI   K.RSTAKSTA+4,C' '   YES - TV HAS NO BAND                         
         DROP  K                                                                
*                                                                               
VSTA0020 DS    0H                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(L'RSTAKEY),KEYSAVE                                           
         BE    *+14                                                             
         MVC   0(2,R3),=Y(150)                                                  
         B     EXITNO                                                           
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
*&&DO                                                                           
         L     R6,AIOREC           CHECK FOR RECIEVING ID                       
         LA    R6,RSTAELEM-RSTAREC(R6)                                          
VSTA0022 DS    0H                                                               
         CLI   0(R6),0             END OF RECORD?                               
         BNE   *+14                                                             
         MVC   0(2,R3),=Y(275)     YES - REQUIRES REVIEVING ID                  
         B     EXITNO                                                           
         CLI   0(R6),X'05'                                                      
         BE    VSTA0024                                                         
         ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         B     VSTA0022                                                         
*&&                                                                             
VSTA0024 DS    0H                                                               
         LR    RE,R3                                                            
         SR    RF,RF                                                            
         LA    R3,3(R3)                                                         
         MVI   0(R3),L'RSTAKSTA    SUBDIVISION LENGTH                           
         MVI   1(R3),QSTAKSTA                                                   
         LA    RF,L'RSTAKSTA(RF)                                                
         LA    R3,2(R3)                                                         
         MVI   0(R3),L'RSTAMKT     SUBDIVISION LENGTH                           
         MVI   1(R3),QSTAMKT                                                    
         LA    RF,L'RSTAMKT(RF)                                                 
         LA    R3,2(R3)                                                         
         MVI   0(R3),L'RSTAAFFL    SUBDIVISION LENGTH                           
         MVI   1(R3),QSTAAFFL                                                   
         LA    RF,L'RSTAAFFL(RF)                                                
         LA    R3,2(R3)                                                         
         MVI   0(R3),3             SUBDIVISION LENGTH                           
         MVI   1(R3),QSTACHAN                                                   
         LA    RF,3(RF)                                                         
         LA    R3,2(R3)                                                         
*                                                                               
         STCM  RF,3,0(RE)           DATA LENGTH                                 
         LA    RE,2(RE)                                                         
         LR    RF,R3                                                            
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         SRL   RF,1                                                             
         STC   RF,0(RE)            NUMBER OF SUBDIVISIONS                       
*                                                                               
         L     R6,AIOREC                                                        
         USING RSTAREC,R6                                                       
         MVC   0(L'RSTAKSTA,R3),RSTAKSTA                                        
         LA    R3,L'RSTAKSTA(R3)                                                
         MVC   0(L'RSTAMKT,R3),RSTAMKT                                          
         LA    R3,L'RSTAMKT(R3)                                                 
         EDIT  (B2,RSTACHAN),(3,0(R3)),ALIGN=LEFT                               
         LA    R3,3(R3)                                                         
         MVC   0(L'RSTAAFFL,R3),RSTAAFFL                                        
         LA    R3,L'RSTAAFFL(R3)                                                
         DROP  R6                                                               
*                                                                               
         MVI   0(R3),0             END OF OUTPUT                                
         LA    R3,1(R3)                                                         
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
********************************************************************            
* INPUT  :  P1       A(CL4 AGENCY CODE)                                         
*           P2       A(CL2 AGENCY OFFICE)                                       
*           P3       A(OUTPUT AREA)                                             
*                                                                               
* OUTPUT :  CC =     AGENCY VALID                                               
*           CC !=    AGENCY INVALID                                             
*                                                                               
* VALID OUTPUT AREA FORMAT:                                                     
*           AGENCY CODE                                                         
*           AGENCY OFFICE                                                       
*           AGENCY NAME (RAGYNAM2)                                              
********************************************************************            
VAGY     NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*VAGY*'                                                      
*                                                                               
         LM    R2,R4,0(R1)                                                      
*                                                                               
K        USING RAGYKEY,KEY                                                      
         XC    K.RAGYKEY,K.RAGYKEY                                              
         MVI   K.RAGYKTYP,X'0A'                                                 
         OC    0(L'RAGYKAGY,R2),SPACES                                          
         MVC   K.RAGYKAGY,0(R2)                                                 
         OC    0(L'RAGYKAOF,R3),SPACES                                          
         MVC   K.RAGYKAOF,0(R3)                                                 
         MVC   K.RAGYKREP,REPALPHA                                              
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(L'RAGYKEY),KEYSAVE                                           
         BE    *+14                                                             
         MVC   0(2,R4),=Y(152)                                                  
         B     EXITNO                                                           
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
*                                                                               
         CLC   0(L'RAGYKAOF,R3),SPACES                                          
         BH    VAGY012             AGENCY OFFICE WAS GIVEN - CONTINUE           
*                                                                               
VAGY010  DS    0H                                                               
         GOTO1 VSEQ                                                             
*                                                                               
K        USING RAGYKEY,KEY                                                      
         CLI   K.RAGYKTYP,X'0A'    AGENCY RECORD?                               
         BNE   VAGY012             NO - OFFICE NOT REQUIRED                     
*                                                                               
         CLC   K.RAGYKREP,REPALPHA SAME REP?                                    
         BNE   VAGY010             NO                                           
*                                                                               
         CLC   K.RAGYKAGY,0(R2)                                                 
         BNE   *+14                                                             
         MVC   0(2,R4),=Y(94)      YES- NEED OFFICE                             
         B     EXITNO                                                           
         DROP  K                                                                
*                                                                               
VAGY012  DS    0H                                                               
         LR    RE,R4                                                            
         SR    RF,RF                                                            
         LA    R4,3(R4)                                                         
         MVI   0(R4),L'RAGYKAGY    SUBDIVISION LENGTH                           
         MVI   1(R4),QAGYKAGY                                                   
         LA    RF,L'RAGYKAGY(RF)                                                
         LA    R4,2(R4)                                                         
         MVI   0(R4),L'RAGYKAOF    SUBDIVISION LENGTH                           
         MVI   1(R4),QAGYKAOF                                                   
         LA    RF,L'RAGYKAOF(RF)                                                
         LA    R4,2(R4)                                                         
         MVI   0(R4),L'RAGYNAM2    SUBDIVISION LENGTH                           
         MVI   1(R4),QAGYNAME                                                   
         LA    RF,L'RAGYNAM2(RF)                                                
         LA    R4,2(R4)                                                         
*                                                                               
         STCM  RF,3,0(RE)           DATA LENGTH                                 
         LA    RE,2(RE)                                                         
         LR    RF,R4                                                            
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         SRL   RF,1                                                             
         STC   RF,0(RE)            NUMBER OF SUBDIVISIONS                       
*                                                                               
         L     R6,AIOREC                                                        
         USING RAGYREC,R6                                                       
         MVC   0(L'RAGYKAGY,R4),RAGYKAGY                                        
         LA    R4,L'RAGYKAGY(R4)                                                
         MVC   0(L'RAGYKAOF,R4),RAGYKAOF                                        
         LA    R4,L'RAGYKAOF(R4)                                                
         MVC   0(L'RAGYNAM2,R4),RAGYNAM2                                        
         LA    R4,L'RAGYNAM2(R4)                                                
         DROP  R6                                                               
*                                                                               
         MVI   0(R4),0             END OF OUTPUT                                
         LA    R4,1(R4)                                                         
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
********************************************************************            
* INPUT  :  P1       A(CL4 ADVERTISE CODE)                                      
*           P2       A(OUTPUT AREA FOR ADVERTISER NAME)                         
*                                                                               
* OUTPUT :  CC =     ADVERTISER VALID                                           
*           CC !=    ADVERTISER INVALID                                         
*                                                                               
* VALID OUTPUT AREA FORMAT:                                                     
*           ADVERTISER CODE                                                     
*           ADVERTISER NAME                                                     
********************************************************************            
VADV     NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*VADV*'                                                      
*                                                                               
         LM    R2,R3,0(R1)                                                      
*                                                                               
K        USING RADVKEY,KEY                                                      
         XC    K.RADVKEY,K.RADVKEY                                              
         MVI   K.RADVKTYP,X'08'                                                 
         MVC   K.RADVKREP,REPALPHA                                              
         MVC   K.RADVKADV,0(R2)                                                 
         OC    K.RADVKADV,SPACES                                                
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(L'RADVKEY),KEYSAVE                                           
         BE    *+14                                                             
         MVC   0(2,R3),=Y(153)                                                  
         B     EXITNO                                                           
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
*                                                                               
         LR    RE,R3                                                            
         SR    RF,RF                                                            
         LA    R3,3(R3)                                                         
         MVI   0(R3),L'RADVKADV    SUBDIVISION LENGTH                           
         MVI   1(R3),QADVKADV                                                   
         LA    RF,L'RADVKADV(RF)                                                
         LA    R3,2(R3)                                                         
         MVI   0(R3),L'RADVNAME    SUBDIVISION LENGTH                           
         MVI   1(R3),QADVNAME                                                   
         LA    RF,L'RADVNAME(RF)                                                
         LA    R3,2(R3)                                                         
*                                                                               
         STCM  RF,3,0(RE)           DATA LENGTH                                 
         LA    RE,2(RE)                                                         
         LR    RF,R3                                                            
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         SRL   RF,1                                                             
         STC   RF,0(RE)            NUMBER OF SUBDIVISIONS                       
*                                                                               
         L     R6,AIOREC                                                        
         USING RADVREC,R6                                                       
         MVC   0(L'RADVKADV,R3),RADVKADV                                        
         LA    R3,L'RADVKADV(R3)                                                
         MVC   0(L'RADVNAME,R3),RADVNAME                                        
         LA    R3,L'RADVNAME(R3)                                                
         DROP  R6                                                               
*                                                                               
         MVI   0(R3),0             END OF OUTPUT                                
         LA    R3,1(R3)                                                         
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
********************************************************************            
* INPUT  :  P1       A(CL3 PRODUCT CODE)                                        
*           P2       A(CL4 ADVERTISER CODE)                                     
*           P3       A(OUTPUT AREA FOR PRODUCT NAME)                            
*                                                                               
* OUTPUT :  CC =     PRODUCT VALID                                              
*           CC !=    PRODUCT INVALID                                            
*                                                                               
* VALID OUTPUT AREA FORMAT:                                                     
*           PRODUCT CODE                                                        
*           PRODUCT NAME                                                        
********************************************************************            
VPRD     NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*VPRD*'                                                      
*                                                                               
         LM    R2,R4,0(R1)                                                      
*                                                                               
K        USING RPRDKEY,KEY                                                      
         XC    K.RPRDKEY,K.RPRDKEY                                              
         MVI   K.RPRDKTYP,X'09'                                                 
         MVC   K.RPRDKREP,REPALPHA                                              
         MVC   K.RPRDKADV,0(R3)                                                 
         OC    K.RPRDKADV,SPACES                                                
         MVC   K.RPRDKPRD,0(R2)                                                 
         OC    K.RPRDKPRD,SPACES                                                
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(L'RPRDKEY),KEYSAVE                                           
         BE    *+14                                                             
         MVC   0(2,R4),=Y(109)                                                  
         B     EXITNO                                                           
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
*                                                                               
         LR    RE,R4                                                            
         SR    RF,RF                                                            
         LA    R4,3(R4)                                                         
         MVI   0(R4),L'RPRDKPRD    SUBDIVISION LENGTH                           
         MVI   1(R4),QPRDKPRD                                                   
         LA    RF,L'RPRDKPRD(RF)                                                
         LA    R4,2(R4)                                                         
         MVI   0(R4),L'RPRDNAME    SUBDIVISION LENGTH                           
         MVI   1(R4),QPRDNAME                                                   
         LA    RF,L'RPRDNAME(RF)                                                
         LA    R4,2(R4)                                                         
*                                                                               
         STCM  RF,3,0(RE)           DATA LENGTH                                 
         LA    RE,2(RE)                                                         
         LR    RF,R4                                                            
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         SRL   RF,1                                                             
         STC   RF,0(RE)            NUMBER OF SUBDIVISIONS                       
*                                                                               
         L     R6,AIOREC                                                        
         USING RPRDREC,R6                                                       
         MVC   0(L'RPRDKPRD,R4),RPRDKPRD                                        
         LA    R4,L'RPRDKPRD(R4)                                                
         MVC   0(L'RPRDNAME,R4),RPRDNAME                                        
         LA    R4,L'RPRDNAME(R4)                                                
         DROP  R6                                                               
*                                                                               
         MVI   0(R4),0             END OF OUTPUT                                
         LA    R4,1(R4)                                                         
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
********************************************************************            
* INPUT  :  P1       A(CL3 DEV SALESPERSON CODE)                                
*           P2       A(OUTPUT AREA FOR DEV SALESPERSON)                         
*                                                                               
* OUTPUT :  CC =     DEVSAL VALID                                               
*           CC !=    DEVSAL INVALID                                             
*                                                                               
* VALID OUTPUT AREA FORMAT:                                                     
*           DEVSAL CODE                                                         
*           DEVSAL NAME                                                         
*           DEVSAL TELEPHONE #                                                  
*           DEVSAL FAX #                                                        
********************************************************************            
VDEVSAL  NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*VDSAL*'                                                     
*                                                                               
         LM    R2,R3,0(R1)                                                      
*                                                                               
K        USING RDSPKEY,KEY                                                      
         XC    K.RDSPKEY,K.RDSPKEY                                              
         MVI   K.RDSPKTYP,X'3A'                                                 
         MVC   K.RDSPKSAL,0(R2)                                                 
         OC    K.RDSPKSAL,SPACES                                                
         MVC   K.RDSPKREP,REPALPHA                                              
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(L'RDSPKEY),KEYSAVE                                           
         BE    *+14                                                             
         MVC   0(2,R3),=Y(415)                                                  
         B     EXITNO                                                           
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
*                                                                               
         L     R6,AIOREC                                                        
         USING RDSPREC,R6                                                       
*                                                                               
         OC    RDSPLEAV,RDSPLEAV                                                
         BZ    VDSP0002                                                         
*                                                                               
         GOTO1 VDATCON,DMCB,(5,0),(2,WORK)                                      
         CLC   RDSPLEAV,WORK                                                    
         BH    VDSP0002                                                         
         MVC   0(2,R3),=Y(459)                                                  
         B     EXITNO                                                           
         DROP  R6                                                               
*                                                                               
VDSP0002 DS    0H                                                               
         LR    RE,R3                                                            
         SR    RF,RF                                                            
         LA    R3,3(R3)                                                         
         MVI   0(R3),L'RDSPKSAL    SUBDIVISION LENGTH                           
         MVI   1(R3),QDSPKSAL                                                   
         LA    RF,L'RDSPKSAL(RF)                                                
         LA    R3,2(R3)                                                         
         MVI   0(R3),L'RDSPNAME    SUBDIVISION LENGTH                           
         MVI   1(R3),QDSPNAME                                                   
         LA    RF,L'RDSPNAME(RF)                                                
         LA    R3,2(R3)                                                         
         MVI   0(R3),L'RDSPTEL     SUBDIVISION LENGTH                           
         MVI   1(R3),QDSPTEL                                                    
         LA    RF,L'RDSPTEL(RF)                                                 
         LA    R3,2(R3)                                                         
         MVI   0(R3),L'RDSPFAX     SUBDIVISION LENGTH                           
         MVI   1(R3),QDSPFAX                                                    
         LA    RF,L'RDSPFAX(RF)                                                 
         LA    R3,2(R3)                                                         
*                                                                               
         STCM  RF,3,0(RE)           DATA LENGTH                                 
         LA    RE,2(RE)                                                         
         LR    RF,R3                                                            
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         SRL   RF,1                                                             
         STC   RF,0(RE)            NUMBER OF SUBDIVISIONS                       
*                                                                               
         L     R6,AIOREC                                                        
         USING RDSPREC,R6                                                       
         MVC   0(L'RDSPKSAL,R3),RDSPKSAL                                        
         LA    R3,L'RDSPKSAL(R3)                                                
         MVC   0(L'RDSPNAME,R3),RDSPNAME                                        
         LA    R3,L'RDSPNAME(R3)                                                
         MVC   0(L'RDSPTEL,R3),RDSPTEL                                          
         LA    R3,L'RDSPTEL(R3)                                                 
         MVC   0(L'RDSPFAX,R3),RDSPFAX                                          
         LA    R3,L'RDSPFAX(R3)                                                 
         DROP  R6                                                               
*                                                                               
         MVI   0(R3),0             END OF OUTPUT                                
         LA    R3,1(R3)                                                         
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
********************************************************************            
* INPUT  :  P1       A(CL3 DEV CON TYPE CODE)                                   
*           P2       A(OUTPUT AREA FOR DEV CON TYPE)                            
*                                                                               
* OUTPUT :  CC =     DEVSAL VALID                                               
*           CC !=    DEVSAL INVALID                                             
*                                                                               
* VALID OUTPUT AREA FORMAT:                                                     
*           DEV CON TYPE CODE                                                   
*           DEV CON TYPE NAME                                                   
********************************************************************            
VDEVTYP  NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*VDTYP*'                                                     
*                                                                               
         LM    R2,R3,0(R1)                                                      
*                                                                               
K        USING RDCTKEY,KEY                                                      
         XC    K.RDCTKEY,K.RDCTKEY                                              
         MVI   K.RDCTKTYP,X'3B'                                                 
         MVC   K.RDCTKCTY,0(R2)                                                 
         OC    K.RDCTKCTY,SPACES                                                
         MVC   K.RDCTKREP,REPALPHA                                              
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(L'RDCTKEY),KEYSAVE                                           
         BE    *+14                                                             
         MVC   0(2,R3),=Y(416)                                                  
         B     EXITNO                                                           
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
*                                                                               
         LR    RE,R3                                                            
         SR    RF,RF                                                            
         LA    R3,3(R3)                                                         
         MVI   0(R3),L'RDCTKCTY    SUBDIVISION LENGTH                           
         MVI   1(R3),QDCTKCTY                                                   
         LA    RF,L'RDCTKCTY(RF)                                                
         LA    R3,2(R3)                                                         
         MVI   0(R3),L'RDCTDESC    SUBDIVISION LENGTH                           
         MVI   1(R3),QDCTNAME                                                   
         LA    RF,L'RDCTDESC(RF)                                                
         LA    R3,2(R3)                                                         
*                                                                               
         STCM  RF,3,0(RE)           DATA LENGTH                                 
         LA    RE,2(RE)                                                         
         LR    RF,R3                                                            
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         SRL   RF,1                                                             
         STC   RF,0(RE)            NUMBER OF SUBDIVISIONS                       
*                                                                               
         L     R6,AIOREC                                                        
         USING RDCTREC,R6                                                       
         MVC   0(L'RDCTKCTY,R3),RDCTKCTY                                        
         LA    R3,L'RDCTKCTY(R3)                                                
         MVC   0(L'RDCTDESC,R3),RDCTDESC                                        
         LA    R3,L'RDCTDESC(R3)                                                
         DROP  R6                                                               
*                                                                               
         MVI   0(R3),0             END OF OUTPUT                                
         LA    R3,1(R3)                                                         
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
********************************************************************            
* INPUT  :  P1       A(CL3 SALESPERSON CODE)                                    
*           P2       A(OUTPUT AREA)                                             
*                                                                               
* OUTPUT :  CC =     SALESPERSON VALID                                          
*           CC !=    SALESPERSON INVALID                                        
*                                                                               
* VALID OUTPUT AREA FORMAT:                                                     
*                                  SALESMAN CODE                                
*                                  SALESMAN NAME                                
*                                  SALESMAN TELEPHONE                           
*                                  SALESMAN FAX                                 
*                                  SALESMAN TEAM                                
*                                  SALESMAN OFFICE                              
*                                  SALESMAN IS MANAGER                          
*                                  DIVISION NAME                                
*                                  TEAM NAME                                    
*                                  SALESMAN EMAIL                               
*                                  OFFICE NAME                                  
*                                  OFFICE ADDR 1                                
*                                  OFFICE ADDR2 - STATE - ZIP                   
********************************************************************            
VSAL     NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*VSAL*'                                                      
*                                                                               
         LM    R2,R3,0(R1)                                                      
*                                                                               
K        USING RSALKEY,KEY                                                      
         XC    K.RSALKEY,K.RSALKEY                                              
         MVI   K.RSALKTYP,X'06'                                                 
         MVC   K.RSALKREP,REPALPHA                                              
         MVC   K.RSALKSAL,0(R2)                                                 
         OC    K.RSALKSAL,SPACES                                                
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(L'RSALKEY),KEYSAVE                                           
         BE    *+14                                                             
         MVC   0(2,R3),=Y(154)                                                  
         B     EXITNO                                                           
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
*                                                                               
         LR    RE,R3                                                            
         SR    RF,RF                                                            
         LA    R3,3(R3)                                                         
         MVI   0(R3),L'RSALKSAL    SUBDIVISION LENGTH                           
         MVI   1(R3),QSALKSAL                                                   
         LA    RF,L'RSALKSAL(RF)                                                
         LA    R3,2(R3)                                                         
         MVI   0(R3),L'RSALNAME    SUBDIVISION LENGTH                           
         MVI   1(R3),QSALNAME                                                   
         LA    RF,L'RSALNAME(RF)                                                
         LA    R3,2(R3)                                                         
         MVI   0(R3),L'RSALTEL     SUBDIVISION LENGTH                           
         MVI   1(R3),QSALTEL                                                    
         LA    RF,L'RSALTEL(RF)                                                 
         LA    R3,2(R3)                                                         
         MVI   0(R3),L'RSALFAX     SUBDIVISION LENGTH                           
         MVI   1(R3),QSALFAX                                                    
         LA    RF,L'RSALFAX(RF)                                                 
         LA    R3,2(R3)                                                         
         MVI   0(R3),L'RSALTEAM    SUBDIVISION LENGTH                           
         MVI   1(R3),QSALTEAM                                                   
         LA    RF,L'RSALTEAM(RF)                                                
         LA    R3,2(R3)                                                         
         MVI   0(R3),L'RSALOFF     SUBDIVISION LENGTH                           
         MVI   1(R3),QSALOFF                                                    
         LA    RF,L'RSALOFF(RF)                                                 
         LA    R3,2(R3)                                                         
         MVI   0(R3),L'RSALMGR     SUBDIVISION LENGTH                           
         MVI   1(R3),QSALMGR                                                    
         LA    RF,L'RSALMGR(RF)                                                 
         LA    R3,2(R3)                                                         
         MVI   0(R3),L'RTEMDVNM    SUBDIVISION LENGTH                           
         MVI   1(R3),QTEMDVNM                                                   
         LA    RF,L'RTEMDVNM(RF)                                                
         LA    R3,2(R3)                                                         
         MVI   0(R3),L'RTEMNAME    SUBDIVISION LENGTH                           
         MVI   1(R3),QTEMNAME                                                   
         LA    RF,L'RTEMNAME(RF)                                                
         LA    R3,2(R3)                                                         
         MVI   0(R3),40            SUBDIVISION LENGTH                           
         MVI   1(R3),QSALEMAL                                                   
         LA    RF,40(RF)                                                        
         LA    R3,2(R3)                                                         
         MVI   0(R3),L'ROFFNAME    SUBDIVISION LENGTH                           
         MVI   1(R3),QSALOFFN                                                   
         LA    RF,L'ROFFNAME(RF)                                                
         LA    R3,2(R3)                                                         
         MVI   0(R3),L'ROFFADD1    SUBDIVISION LENGTH                           
         MVI   1(R3),QSALADD0                                                   
         LA    RF,L'ROFFADD1(RF)                                                
         LA    R3,2(R3)                                                         
         MVI   0(R3),L'ROFFADD2+2+L'ROFFSTT+L'ROFFZIP                           
         MVI   1(R3),QSALADD1                                                   
         LA    RF,L'ROFFADD2+2+L'ROFFSTT+L'ROFFZIP(RF)                          
         LA    R3,2(R3)                                                         
*                                                                               
         STCM  RF,3,0(RE)           DATA LENGTH                                 
         LA    RE,2(RE)                                                         
         LR    RF,R3                                                            
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         SRL   RF,1                                                             
         STC   RF,0(RE)            NUMBER OF SUBDIVISIONS                       
*                                                                               
         L     R6,AIOREC                                                        
         USING RSALREC,R6                                                       
         MVC   0(L'RSALKSAL,R3),RSALKSAL                                        
         LA    R3,L'RSALKSAL(R3)                                                
         MVC   0(L'RSALNAME,R3),RSALNAME                                        
         LA    R3,L'RSALNAME(R3)                                                
         MVC   0(L'RSALTEL,R3),RSALTEL                                          
         LA    R3,L'RSALTEL(R3)                                                 
         MVC   0(L'RSALFAX,R3),RSALFAX                                          
         LA    R3,L'RSALFAX(R3)                                                 
         MVC   0(L'RSALTEAM,R3),RSALTEAM                                        
         LA    R3,L'RSALTEAM(R3)                                                
         MVC   0(L'RSALOFF,R3),RSALOFF                                          
         LR    R4,R3               SAVE POSITION OF OFF TO READ NAME            
         LA    R3,L'RSALOFF(R3)                                                 
         MVC   0(L'RSALMGR,R3),RSALMGR                                          
         LA    R3,L'RSALMGR(R3)                                                 
*                                                                               
K        USING RTEMKEY,KEY                                                      
         XC    K.RTEMKEY,K.RTEMKEY                                              
         MVI   K.RTEMKTYP,X'05'                                                 
         MVC   K.RTEMKREP,REPALPHA                                              
         MVC   K.RTEMKTEM,RSALTEAM                                              
         DROP  K,R6                                                             
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(L'RTEMKEY),KEYSAVE                                           
         BE    *+6                 MISSING SALES DIV/TEAM RECORD                
         DC    H'0'                                                             
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
         L     R6,AIOREC                                                        
         USING RTEMREC,R6                                                       
*                                                                               
         MVC   0(L'RTEMDVNM,R3),RTEMDVNM                                        
         LA    R3,L'RTEMDVNM(R3)                                                
         MVC   0(L'RTEMNAME,R3),RTEMNAME                                        
         LA    R3,L'RTEMNAME(R3)                                                
         DROP  R6                                                               
*                                                                               
K        USING RSA2KEY,KEY                                                      
         XC    K.RSA2KEY,K.RSA2KEY                                              
         MVI   K.RSA2KTYP,X'46'                                                 
         MVC   K.RSA2KREP,REPALPHA                                              
         MVC   K.RSA2KSAL,0(R2)                                                 
         OC    K.RSA2KSAL,SPACES                                                
         DROP  K                                                                
*                                                                               
         MVC   0(40,R3),SPACES     PRE CLEAR FIELD                              
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(L'RSA2KEY),KEYSAVE                                           
         BNE   VSAL0052            MISSING PERSON 2 RECORD                      
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
*                                                                               
         L     R6,AIOREC                                                        
         LA    R6,RSA2ELEM-RSA2REC(R6)                                          
VSAL0050 DS    0H                                                               
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0             EOR?                                         
         BE    VSAL0052            YES                                          
         CLI   0(R6),X'20'         EMAIL ADDRESS ELEMENT?                       
         BNE   VSAL0050            NO                                           
*                                                                               
         CLI   1(R6),2             CHECK FOR BAD ELEMENTS                       
         BNH   VSAL0052                                                         
*                                                                               
         ZIC   RE,1(R6)                                                         
         AHI   RE,-3                                                            
         EX    RE,*+4                                                           
         MVC   0(0,R3),2(R6)                                                    
VSAL0052 DS    0H                                                               
         LA    R3,40(R3)                                                        
*                                                                               
K        USING ROFFKEY,KEY                                                      
         XC    K.ROFFKEY,K.ROFFKEY                                              
         MVI   K.ROFFKTYP,X'04'                                                 
         MVC   K.ROFFKREP,REPALPHA                                              
         MVC   K.ROFFKOFF,0(R4)                                                 
         OC    K.ROFFKOFF,SPACES                                                
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(L'ROFFKEY),KEYSAVE                                           
         BE    *+6                 MISSING OFFICE RECORD                        
         DC    H'0'                                                             
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
*                                                                               
         L     R6,AIOREC                                                        
         USING ROFFREC,R6                                                       
*                                                                               
         MVC   0(L'ROFFNAME,R3),ROFFNAME                                        
         LA    R3,L'ROFFNAME(R3)                                                
         MVC   0(L'ROFFADD1,R3),ROFFADD1                                        
         OC    0(L'ROFFADD1,R3),SPACES                                          
         LA    R3,L'ROFFADD1(R3)                                                
*                                          2ND ADDRESS LINE COMBINES            
         MVC   0(L'ROFFADD2,R3),ROFFADD2      ADDRESS STATE AND ZIP             
         LA    RE,1+L'ROFFADD2(R3)                                              
         MVC   0(L'ROFFSTT,RE),ROFFSTT                                          
         LA    RE,1+L'ROFFSTT(RE)                                               
         MVC   0(L'ROFFZIP,RE),ROFFZIP                                          
         OC    0(L'ROFFADD2+2+L'ROFFSTT+L'ROFFZIP,R3),SPACES                    
         LA    R3,L'ROFFADD2+2+L'ROFFSTT+L'ROFFZIP(R3)                          
*                                                                               
         DROP  R6                                                               
*                                                                               
         MVI   0(R3),0             END OF OUTPUT                                
         LA    R3,1(R3)                                                         
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
********************************************************************            
* INPUT  :  P1       A(START DATE)                                              
*           P2       A(END DATE)                                                
*           P3       A(STATION)                                                 
*           P4       A(OUTPUT AREA)                                             
*                                                                               
* OUTPUT :  CC !=    INVALID FLIGHT                                             
*           CC =     VALID FLIGHT                                               
*                                                                               
********************************************************************            
VFLIGHT  NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*VFLIGHT'                                                    
*                                                                               
         LM    R2,R5,0(R1)                                                      
         GOTO1 VDATCON,DMCB,(8,(R2)),(0,WORK+44)                                
         GOTO1 VDATCON,DMCB,(8,(R3)),(0,WORK+50)                                
*                                                                               
* CHECK IF K DATES EXCEED 1 CALENDAR YR (13 BROADCAST MONTHS MAX)               
*                                                                               
         GOTO1 VGTBROAD,DMCB,(1,WORK+44),WORK,VGETDAY,VADDAY                    
         GOTO1 VGTBROAD,DMCB,(1,WORK+50),WORK+12,VGETDAY,VADDAY                 
*                                                                               
         GOTO1 VADDAY,DMCB,(C'Y',WORK+6),(0,WORK+6),1                           
*                                                                               
         CLC   WORK+18(4),WORK+6                                                
         BNH   *+14                ERROR - DATES CANT EXCEED 1 CAL YR           
         MVC   0(2,R5),=AL2(49)                                                 
         B     EXITL                                                            
*                                                                               
         LA    RE,KEY                                                           
         USING RSTAKEY,RE                                                       
         XC    RSTAKEY,RSTAKEY                                                  
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,REPALPHA                                                
         MVC   RSTAKSTA,0(R4)                                                   
         DROP  RE                                                               
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(L'RSTAKEY),KEYSAVE                                           
         BE    *+14                                                             
         MVC   0(2,R5),=Y(150)                                                  
         B     EXITL                                                            
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
         GOTO1 VDATCON,DMCB,(8,(R2)),(3,WORK)                                   
*                                                                               
         L     R6,AIOREC                                                        
         USING RSTAREC,R6                                                       
         CLC   RSTASTRT,WORK                                                    
         BNH   *+14                                                             
         MVC   0(2,R5),=AL2(287)                                                
         B     EXITL                                                            
         DROP  R6                                                               
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
********************************************************************            
* INPUT  :  P1       A(OUTPUT AREA)                                             
*                                                                               
* OUTPUT :  CC !=    NO DAPART LIST                                             
*           CC =     DAYPART LIST IN OUTPUT AREA                                
*                                                                               
********************************************************************            
GDPLIST  NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*GDPLST*'                                                    
*                                                                               
         L     R2,0(R1)                                                         
*                                                                               
         LR    RE,R2                                                            
         SR    RF,RF                                                            
         LA    R2,3(R2)                                                         
         MVI   0(R2),1             SUBDIVISION LENGTH                           
         MVI   1(R2),QDPTCODE                                                   
         LA    RF,1(RF)                                                         
         LA    R2,2(R2)                                                         
         MVI   0(R2),L'RRDPSNAM    SUBDIVISION LENGTH                           
         MVI   1(R2),QDPTSNAM                                                   
         LA    RF,L'RRDPSNAM(RF)                                                
         LA    R2,2(R2)                                                         
         MVI   0(R2),L'RRDPLNAM    SUBDIVISION LENGTH                           
         MVI   1(R2),QDPTLNAM                                                   
         LA    RF,L'RRDPLNAM(RF)                                                
         LA    R2,2(R2)                                                         
*                                                                               
         STCM  RF,3,0(RE)           DATA LENGTH                                 
         LA    RE,2(RE)                                                         
         LR    RF,R2                                                            
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         SRL   RF,1                                                             
         STC   RF,0(RE)            NUMBER OF SUBDIVISIONS                       
*                                                                               
K        USING RSETREC,KEY                                                      
         XC    K.RSETKEY,K.RSETKEY                                              
         MVI   K.RSETKTYP,RSETKTYQ                                              
         MVC   K.RSETKREP,PARALPHA    PARENT REP POWER CODE                     
         MVC   K.RSETKSET,=C'DP'                                                
         MVC   K.RSETKID,=C'ALL '                                               
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(L'RSETKEY),KEYSAVE                                           
         BNE   EXITNO              NOT FOUND - GET DEMO MARKET                  
*                                                                               
         GOTO1 VGETREC,AIO2                                                     
*                                                                               
         L     R6,AIO2                                                          
         LA    R6,RSETELEM-RSETREC(R6)                                          
GDPL010  DS    0H                                                               
         CLI   0(R6),0             EOR?                                         
         BE    GDPLX               YES                                          
         CLI   0(R6),RSETMCDQ      MEMBER ELEMENT?                              
         BE    GDPL015             YES                                          
*                                                                               
         ZIC   R0,1(R6)            NO - NEXT ELEMENT                            
         AR    R6,R0                                                            
         B     GDPL010                                                          
*                                                                               
GDPL015  DS    0H                  NEXT ELEMENT                                 
         USING RSETMEMD,R6                                                      
         XC    HALF,HALF                                                        
         MVC   HALF+1,RSETMLEN     MEMBER LENGTH                                
         DROP  R6                                                               
*                                                                               
         ZIC   R3,1(R6)                                                         
         AR    R3,R6               END OF ELEMENT                               
*                                                                               
         LA    R6,RSETMTOV(R6)     MEMBER LIST                                  
GDPL020  CR    R6,R3               END OF ELMEMENT?                             
         BNL   GDPL010             YES - NEXT ELEMENT                           
*                                                                               
         MVC   0(1,R2),0(R6)       DAYPART CODE                                 
*                                                                               
K        USING RRDPRECD,KEY                                                     
         XC    K.RRDPKEY,K.RRDPKEY                                              
         MVI   K.RRDPKTYP,RRDPKIDQ                                              
         MVC   K.RRDPKREP,PARALPHA   PARENT REP POWER CODE                      
         MVC   K.RRDPKDPT,0(R6)                                                 
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(L'RRDPRECD),KEYSAVE                                          
         BE    GDPL030                                                          
*                                                                               
         LH    R1,HALF             KEY NOT FOUND                                
         BCTR  R1,0                 - CLEAR INVALID CODE                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,R2),0(R2)                                                    
         B     GDPL050              - SKIP TO NEXT CODE                         
*                                                                               
GDPL030  DS    0H                                                               
         GOTO1 VGETREC,AIO1                                                     
*                                                                               
         L     R5,AIO1             COPY SHORT AND LONG NAMES                    
         USING RRDPRECD,R5          INTO OUTPUT AREA                            
         MVC   1(L'RRDPSNAM,R2),RRDPSNAM                                        
         MVC   1+L'RRDPSNAM(L'RRDPLNAM,R2),RRDPLNAM                             
*                                                                               
         LA    R2,1+L'RRDPSNAM+L'RRDPLNAM(R2)                                   
         DROP  R5                                                               
*                                                                               
GDPL050  DS    0H                                                               
         AH    R6,HALF             NEXT MEMBER                                  
         B     GDPL020                                                          
*                                                                               
GDPLX    DS    0H                                                               
         XC    0(1+L'RRDPSNAM+L'RRDPLNAM,R2),0(R2)                              
         MVC   AIOREC,AIO1                                                      
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
********************************************************************            
* INPUT  :  P1       A(CL5 STATION CALL LETTERS)                                
*           P2       A(OUTPUT AREA)                                             
*                                                                               
* OUTPUT :  CC !=    NO STATION/MARKET LIST                                     
*           CC =     STATION/MARKET LIST IN OUTPUT AREA                         
*                                                                               
********************************************************************            
GMSLIST  NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*GMSLST*'                                                    
*                                                                               
         LM    R2,R3,0(R1)                                                      
*                                                                               
         LR    RE,R3                                                            
         SR    RF,RF                                                            
         LA    R3,3(R3)                                                         
         MVI   0(R3),5             SUBDIVISION LENGTH                           
         MVI   1(R3),QMKTSTA                                                    
         LA    RF,5(RF)                                                         
         LA    R3,2(R3)                                                         
*                                                                               
         STCM  RF,3,0(RE)           DATA LENGTH                                 
         LA    RE,2(RE)                                                         
         LR    RF,R3                                                            
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         SRL   RF,1                                                             
         STC   RF,0(RE)            NUMBER OF SUBDIVISIONS                       
*                                                                               
K        USING RSETREC,KEY                                                      
         XC    KEY,KEY                                                          
         MVI   K.RSETKTYP,RSETKTYQ                                              
         MVC   K.RSETKREP,REPALPHA REP POWER CODE                               
         MVC   K.RSETKSET,=C'MS'                                                
         MVC   K.RSETKID,0(R2)                                                  
         OC    K.RSETKID,SPACES                                                 
         CLI   K.RSETKID+4,C'T'    TV?                                          
         BNE   *+8                                                              
         MVI   K.RSETKID+4,C' '    YES - TV HAS NO BAND                         
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(L'RSETKEY),KEYSAVE                                           
         BNE   EXITNO              NOT FOUND - GET DEMO MARKET                  
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
*                                                                               
         L     R6,AIOREC                                                        
         LA    R6,RSETELEM-RSETREC(R6)                                          
GMSL010  DS    0H                                                               
         CLI   0(R6),0             EOR?                                         
         BE    GMSLX               YES                                          
         CLI   0(R6),RSETMCDQ      MEMBER ELEMENT?                              
         BE    GMSL015             YES                                          
*                                                                               
         ZIC   R0,1(R6)            NO - NEXT ELEMENT                            
         AR    R6,R0                                                            
         B     GMSL010                                                          
*                                                                               
GMSL015  DS    0H                  NEXT ELEMENT                                 
         USING RSETMEMD,R6                                                      
         XC    HALF,HALF                                                        
         MVC   HALF+1,RSETMLEN     MEMBER LENGTH                                
         DROP  R6                                                               
*                                                                               
         ZIC   RE,1(R6)                                                         
         AR    RE,R6               END OF ELEMENT                               
*                                                                               
         LA    R6,RSETMTOV(R6)     MEMBER LIST                                  
GMSL020  CR    R6,RE               END OF ELMEMENT?                             
         BNL   GMSL010             YES - NEXT ELEMENT                           
*                                                                               
         MVC   0(5,R3),0(R6)       SAVE STATION CALL LETTERS                    
*                                                                               
         CLI   4(R3),C'T'          REMOVE BAND FOR TV                           
         BNE   *+8                                                              
         MVI   4(R3),C' '                                                       
*                                                                               
         LA    R3,5(R3)               NEXT SLOT IN OUTPUT AREA                  
*                                                                               
         AH    R6,HALF             NEXT MEMBER                                  
         B     GMSL020                                                          
*                                                                               
GMSLX    DS    0H                                                               
         XC    0(5,R3),0(R3)       END OF OUTPUT                                
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
********************************************************************            
* INPUT  :  P1       A(OUTPUT AREA)                                             
*                                                                               
* OUTPUT :  CC =     CONTRACT TYPE LIST IN OUTPUT AREA                          
*                                                                               
********************************************************************            
GCTLIST  NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*GCTLST*'                                                    
*                                                                               
         L     R3,0(R1)                                                         
*                                                                               
         LR    RE,R3                                                            
         SR    RF,RF                                                            
         LA    R3,3(R3)                                                         
         MVI   0(R3),L'RCTYKCTY    SUBDIVISION LENGTH                           
         MVI   1(R3),QCTYKCTY                                                   
         LA    RF,L'RCTYKCTY(RF)                                                
         LA    R3,2(R3)                                                         
         MVI   0(R3),L'RCTYDESC    SUBDIVISION LENGTH                           
         MVI   1(R3),QCTYDESC                                                   
         LA    RF,L'RCTYDESC(RF)                                                
         LA    R3,2(R3)                                                         
*                                                                               
         STCM  RF,3,0(RE)           DATA LENGTH                                 
         LA    RE,2(RE)                                                         
         LR    RF,R3                                                            
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         SRL   RF,1                                                             
         STC   RF,0(RE)            NUMBER OF SUBDIVISIONS                       
*                                                                               
K        USING RCTYREC,KEY                                                      
         XC    K.RCTYKEY,K.RCTYKEY                                              
         MVI   K.RCTYKTYP,RCTYKTYQ                                              
         MVC   K.RCTYKREP,REPALPHA REP POWER CODE                               
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
GCTL010  DS    0H                                                               
         CLC   KEY(RCTYKCTY-RCTYKEY),KEYSAVE                                    
         BNE   GCTLX               NOT FOUND                                    
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
         L     R6,AIOREC                                                        
         USING RCTYREC,R6                                                       
*                                                                               
         MVC   0(L'RCTYKCTY,R3),RCTYKCTY                                        
         LA    R3,L'RCTYKCTY(R3)                                                
         MVC   0(L'RCTYDESC,R3),RCTYDESC                                        
         LA    R3,L'RCTYDESC(R3)                                                
*                                                                               
         GOTO1 VSEQ                                                             
         B     GCTL010                                                          
*                                                                               
GCTLX    DS    0H                                                               
         XC    0(L'RCTYDESC+L'RCTYKCTY,R3),0(R3)                                
         LA    R3,L'RCTYDESC+L'RCTYKCTY(R3)                                     
         B     EXITOK                                                           
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
********************************************************************            
* INPUT  :  P1       A(OUTPUT AREA)                                             
*                                                                               
* OUTPUT :  CC =     DEV CONTRACT TYPE LIST IN OUTPUT AREA                      
*                                                                               
********************************************************************            
GDCTLST  NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*GDCTL*'                                                     
*                                                                               
         L     R3,0(R1)                                                         
*                                                                               
         LR    RE,R3                                                            
         SR    RF,RF                                                            
         LA    R3,3(R3)                                                         
         MVI   0(R3),L'RDCTKCTY    SUBDIVISION LENGTH                           
         MVI   1(R3),QDCTKCTY                                                   
         LA    RF,L'RDCTKCTY(RF)                                                
         LA    R3,2(R3)                                                         
         MVI   0(R3),L'RDCTDESC    SUBDIVISION LENGTH                           
         MVI   1(R3),QDCTNAME                                                   
         LA    RF,L'RDCTDESC(RF)                                                
         LA    R3,2(R3)                                                         
*                                                                               
         STCM  RF,3,0(RE)           DATA LENGTH                                 
         LA    RE,2(RE)                                                         
         LR    RF,R3                                                            
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         SRL   RF,1                                                             
         STC   RF,0(RE)            NUMBER OF SUBDIVISIONS                       
*                                                                               
K        USING RDCTREC,KEY                                                      
         XC    K.RDCTKEY,K.RDCTKEY                                              
         MVI   K.RDCTKTYP,X'3B'                                                 
         MVC   K.RDCTKREP,REPALPHA REP POWER CODE                               
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
GDCTL10  DS    0H                                                               
         CLC   KEY(RDCTKCTY-RDCTKEY),KEYSAVE                                    
         BNE   GDCTLX              NOT FOUND                                    
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
         L     R6,AIOREC                                                        
         USING RDCTREC,R6                                                       
*                                                                               
         MVC   0(L'RDCTKCTY,R3),RDCTKCTY                                        
         LA    R3,L'RDCTKCTY(R3)                                                
         MVC   0(L'RDCTDESC,R3),RDCTDESC                                        
         LA    R3,L'RDCTDESC(R3)                                                
*                                                                               
         GOTO1 VSEQ                                                             
         B     GDCTL10                                                          
*                                                                               
GDCTLX   DS    0H                                                               
         XC    0(L'RDCTDESC+L'RDCTKCTY,R3),0(R3)                                
         LA    R3,L'RDCTDESC+L'RDCTKCTY(R3)                                     
         B     EXITOK                                                           
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
********************************************************************            
* INPUT  :  P1       A(CL3 SALESPERSON CODE)                                    
*           P2       A(OUTPUT AREA)                                             
*                                                                               
* OUTPUT :  CC !=    NO STATION EXCLUSION LIST                                  
*           CC =     STATION EXCLUSION LIST IN OUTPUT AREA                      
*                                                                               
********************************************************************            
GSALSTX  NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*GSALSTX'                                                    
*                                                                               
         LM    R2,R3,0(R1)                                                      
*                                                                               
         LR    RE,R3                                                            
         SR    RF,RF                                                            
         LA    R3,3(R3)                                                         
         MVI   0(R3),5             SUBDIVISION LENGTH                           
         MVI   1(R3),QSALSTAX                                                   
         LA    RF,5(RF)                                                         
         LA    R3,2(R3)                                                         
*                                                                               
         STCM  RF,3,0(RE)           DATA LENGTH                                 
         LA    RE,2(RE)                                                         
         LR    RF,R3                                                            
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         SRL   RF,1                                                             
         STC   RF,0(RE)            NUMBER OF SUBDIVISIONS                       
*                                                                               
K        USING RSA2KEY,KEY                                                      
         XC    KEY,KEY                                                          
         MVI   K.RSA2KTYP,X'46'                                                 
         MVC   K.RSA2KREP,REPALPHA REP POWER CODE                               
         MVC   K.RSA2KSAL,0(R2)                                                 
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(L'RSA2KEY),KEYSAVE                                           
         BNE   EXITNO              NOT FOUND                                    
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
*                                                                               
         L     R6,AIOREC                                                        
         LA    R6,RSA2ELEM-RSA2REC(R6)                                          
GSTX010  DS    0H                                                               
         CLI   0(R6),0             EOR?                                         
         BE    GSTXX               YES                                          
         CLI   0(R6),X'10'         MEMBER ELEMENT?                              
         BE    GSTX020             YES                                          
*                                                                               
         ZIC   R0,1(R6)            NO - NEXT ELEMENT                            
         AR    R6,R0                                                            
         B     GSTX010                                                          
*                                                                               
GSTX020  DS    0H                                                               
         ZIC   R0,1(R6)                                                         
         AR    R0,R6               END OF ELEM                                  
*                                                                               
         LA    R6,2(R6)                                                         
GSTX030  DS    0H                                                               
         CR    R6,R0               END FOUND?                                   
         BNL   GSTXX               YES                                          
*                                                                               
         MVC   0(5,R3),0(R6)                                                    
         LA    R6,5(R6)                                                         
         LA    R3,5(R3)                                                         
         B     GSTX030                                                          
*                                                                               
GSTXX    DS    0H                                                               
         XC    0(5,R3),0(R3)       END OF OUTPUT                                
         LA    R3,5(R3)                                                         
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
********************************************************************            
* INPUT  :  P1       A(CL4 AGENCY CODE)                                         
*           P2       A(CL2 AGENCY OFFICE)                                       
*           P3       A(CL4 ADVERTISER CODE)                                     
*           P4       A(CL1 CONTRACT TYPE)                                       
*           P5       A(OUTPUT AREA)                                             
*                                                                               
* OUTPUT :  ??                                                                  
*                                                                               
*                                                                               
********************************************************************            
AGYADDR  NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*AGYADDR'                                                    
*                                                                               
         LM    R2,R6,0(R1)                                                      
         XC    WORK,WORK                                                        
W        USING AGADDRD,WORK                                                     
*                                                                               
         LR    RE,R6                                                            
         SR    RF,RF                                                            
         LA    R6,3(R6)                                                         
         MVI   0(R6),L'ADDRLN0     SUBDIVISION LENGTH                           
         MVI   1(R6),QAGADLN0                                                   
         LA    RF,L'ADDRLN0(RF)                                                 
         LA    R6,2(R6)                                                         
         MVI   0(R6),L'ADDRLN1     SUBDIVISION LENGTH                           
         MVI   1(R6),QAGADLN1                                                   
         LA    RF,L'ADDRLN1(RF)                                                 
         LA    R6,2(R6)                                                         
         MVI   0(R6),L'ADDRLN2     SUBDIVISION LENGTH                           
         MVI   1(R6),QAGADLN2                                                   
         LA    RF,L'ADDRLN2(RF)                                                 
         LA    R6,2(R6)                                                         
         MVI   0(R6),L'ADDRLN3     SUBDIVISION LENGTH                           
         MVI   1(R6),QAGADLN3                                                   
         LA    RF,L'ADDRLN3(RF)                                                 
         LA    R6,2(R6)                                                         
         MVI   0(R6),L'ADDRNAM     SUBDIVISION LENGTH                           
         MVI   1(R6),QAGADNAM                                                   
         LA    RF,L'ADDRNAM(RF)                                                 
         LA    R6,2(R6)                                                         
*                                                                               
         STCM  RF,3,0(RE)           DATA LENGTH                                 
         LA    RE,2(RE)                                                         
         LR    RF,R6                                                            
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         SRL   RF,1                                                             
         STC   RF,0(RE)            NUMBER OF SUBDIVISIONS                       
*                                                                               
         XC    KEY,KEY                                                          
K        USING RAGYKEY,KEY                                                      
         MVI   K.RAGYKTYP,X'0A'                                                 
         MVC   K.RAGYKAGY,0(R2)      AGENCY                                     
         OC    K.RAGYKAGY,SPACES                                                
         MVC   K.RAGYKAOF,0(R3)      OFFICE                                     
         OC    K.RAGYKAOF,SPACES                                                
         MVC   K.RAGYKREP,REPALPHA                                              
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RAGYKEY),KEYSAVE                                           
         BNE   EXITL                                                            
*                                                                               
         GOTO1 VGETREC,AIO1                                                     
         L     R8,AIO1                                                          
         USING RAGYREC,R8                                                       
         MVC   W.ADDRLN1(20),RAGYADD1                                           
         MVC   W.ADDRLN3(20),RAGYCITY                                           
         MVC   W.ADDRZIP,RAGYZIP                                                
         MVC   W.ADDRST,RAGYSTAT                                                
*                                                                               
         TM    RAGYFLAG,X'80'      EXPANDED ADDRESS?                            
         BZ    AGADD020            NO                                           
         DROP  R8                                                               
*                                                                               
         XC    KEY,KEY                                                          
K        USING RAGY2KEY,KEY                                                     
         MVI   K.RAGK2TYP,RAGK2TYQ                                              
         MVC   K.RAGK2AGY,0(R2)        AGENCY                                   
         OC    K.RAGK2AGY,SPACES                                                
         MVC   K.RAGK2AOF,0(R3)        OFFICE                                   
         OC    K.RAGK2AOF,SPACES                                                
         MVC   K.RAGK2REP,REPALPHA     REPALPHA                                 
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RAGY2KEY),KEYSAVE                                          
         BNE   AGADD020                                                         
*                                                                               
         GOTO1 VGETREC,AIO2                                                     
*                                                                               
         L     RE,AIO2                                                          
         LA    RE,RAGY2FXE-RAGY2REC(RE)                                         
AGADD010 DS    0H                                                               
         ZIC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0             EOR?                                         
         BE    AGADDRX             YES                                          
         CLI   0(RE),X'20'         EXPANDED ADDRESS ELEMENT?                    
         BNE   AGADD010            NO                                           
*                                                                               
         USING RAGY2AE1,RE                                                      
         MVC   W.ADDRLN1(34),RAGY2AD1                                           
         MVC   W.ADDRLN2(34),RAGY2AD2                                           
         DROP  RE                                                               
*                                                                               
* NOTE: THE NEXT 3 LINES ARE TO FIX THE PROBLEM OF THE CITY BEING               
*       DUPLICATED IN THE 2ND ADDRESS FIELD IN PETRY CONVERTED RECS             
         CLC   W.ADDRLN3(20),W.ADDRLN2                                          
         BNE   *+10                                                             
         XC    W.ADDRLN3,W.ADDRLN3                                              
*                                                                               
AGADD020 DS    0H                                                               
         LA    RE,W.ADDRLN3        FLOAT STATE AND ZIP                          
         LA    RF,L'ADDRLN3-1(RE)                                               
AGADD022 DS    0H                                                               
         OI    0(RF),X'40'                                                      
         CLI   0(RF),X'40'                                                      
         BH    *+12                                                             
         BCTR  RF,0                                                             
         CR    RF,RE                                                            
         BH    AGADD022                                                         
*                                                                               
         MVC   3(2,RF),W.ADDRST                                                 
         MVC   7(10,RF),W.ADDRZIP                                               
*                                                                               
         OC    W.ADDRLN2,SPACES                                                 
         CLC   W.ADDRLN2,SPACES                                                 
         BH    *+16                                                             
         MVC   W.ADDRLN2,W.ADDRLN3                                              
         XC    W.ADDRLN3,W.ADDRLN3                                              
*                                                                               
*  LOOKUP CONTYPE RECORD TO GET FORMAT INFO                                     
*                                                                               
         XC    KEY,KEY                                                          
K        USING RCTYREC,KEY                                                      
         MVI   K.RCTYKTYP,RCTYKTYQ     REC TYPE                                 
         MVC   K.RCTYKREP,REPALPHA     REP CODE                                 
         MVC   K.RCTYKCTY,0(R5)        CON TYPE                                 
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
         CLC   KEY(L'RCTYKEY),KEYSAVE                                           
         BNE   AGADDRX                                                          
         GOTO1 VGETREC,AIO2                                                     
*                                                                               
         L     R8,AIO2                                                          
         LA    R8,RCTYELEM-RCTYREC(R8)                                          
AGADD030 DS    0H                                                               
         ZIC   R0,1(R8)                                                         
         AR    R8,R0                                                            
         CLI   0(R8),0             EOR?                                         
         BE    AGADDRX             YES                                          
         CLI   0(R8),X'10'         FORMAT ELEMENT?                              
         BNE   AGADD030            NO                                           
*                                                                               
         USING RCTYFEL,R8                                                       
         TM    RCTYFPRA,X'08'           CARE OF AGENCY OVERRIDE?                
         BNZ   AGADD040                 YES                                     
*                                                                               
         L     RE,AIO1                                                          
R        USING RAGYREC,RE                                                       
         TM    R.RAGYFLAG,X'20'    CARE OF AGENCY?                              
         BZ    AGADD040            NO                                           
         DROP  R                                                                
*                                                                               
         XC    KEY,KEY                                                          
K        USING RADVKEY,KEY                                                      
         MVI   K.RADVKTYP,X'08'                                                 
         MVC   K.RADVKREP,REPALPHA                                              
         MVC   K.RADVKADV,0(R4)                                                 
         OC    K.RADVKADV,SPACES                                                
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(L'RADVKEY),KEYSAVE                                           
         BNE   AGADD040                                                         
*                                                                               
         GOTO1 VGETREC,AIO1                                                     
         L     RE,AIO1                                                          
         USING RADVREC,RE                                                       
         MVC   W.ADDRLN0(L'RADVNAME),RADVNAME                                   
         DROP  RE                                                               
*                                                                               
         OC    W.ADDRLN0,SPACES                                                 
         CLC   W.ADDRLN0,SPACES                                                 
         BNH   AGADD080                                                         
*                                                                               
         LA    RE,W.ADDRLN0+L'RADVNAME-1                                        
         CLI   0(RE),C' '                                                       
         BH    *+10                                                             
         BCTR  RE,0                                                             
         B     *-10                                                             
*                                                                               
         MVC   2(9,RE),=C'-CARE OF:'                                            
         B     AGADD080                                                         
*                                                                               
AGADD040 DS    0H                                                               
         TM    RCTYFA1S,X'80'           REPLACE AGY ADDRESS 1?                  
         BNO   AGADD050                 NO - NEXT FIELD                         
*                                                                               
         XC    W.ADDRLN1,W.ADDRLN1                                              
         MVI   BYTE,C'G'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   AGADD050                                                         
*                                                                               
         L     RE,FULL                                                          
         ZIC   R1,1(RE)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   W.ADDRLN1(0),3(RE)                                               
*                                                                               
AGADD050 DS    0H                                                               
         TM    RCTYFA2S,X'80'           REPLACE AGY ADDRESS 2?                  
         BNO   AGADD060                 NO - NEXT FIELD                         
*                                                                               
         XC    W.ADDRLN2,W.ADDRLN2                                              
         MVI   BYTE,C'H'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   AGADD060                                                         
*                                                                               
         L     RE,FULL                                                          
         ZIC   R1,1(RE)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   W.ADDRLN2(0),3(RE)                                               
*                                                                               
AGADD060 DS    0H                                                               
         TM    RCTYFA3S,X'80'           REPLACE AGY ADDRESS 3?                  
         BNO   AGADD070                 NO - NEXT FIELD                         
*                                                                               
         XC    W.ADDRLN3,W.ADDRLN3                                              
         MVI   BYTE,C'I'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   AGADD070                                                         
*                                                                               
         L     RE,FULL                                                          
         ZIC   R1,1(RE)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   W.ADDRLN3(0),3(RE)                                               
*                                                                               
AGADD070 DS    0H                                                               
         TM    RCTYFANS,X'80'           REPLACE AGY NAME?                       
         BNO   AGADD080                 NO - NEXT FIELD                         
*                                                                               
         XC    W.ADDRNAM,W.ADDRNAM                                              
         MVI   BYTE,C'E'                                                        
         BAS   RE,TXTSEEK                                                       
         BNE   AGADD080                                                         
*                                                                               
         L     RE,FULL                                                          
         ZIC   R1,1(RE)                                                         
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   W.ADDRNAM(0),3(RE)                                               
*                                                                               
AGADD080 DS    0H                                                               
*                                                                               
AGADDRX  DS    0H                                                               
         OC    WORK,SPACES                                                      
         MVC   0(ADDRST-AGADDRD,R6),WORK                                        
         B     EXITOK                                                           
*----------------------------                                                   
TXTSEEK  NTR1                                                                   
         L     R8,AIO2                                                          
         LA    R8,RCTYELEM-RCTYREC(R8)                                          
TS010    DS    0H                                                               
         ZIC   R0,1(R8)                                                         
         AR    R8,R0                                                            
         CLI   0(R8),0             EOR?                                         
         BE    EXITL               YES                                          
         CLI   0(R8),X'12'         FORMAT ELEMENT?                              
         BNE   TS010               NO                                           
         CLC   BYTE,2(R8)                                                       
         BNE   TS010                                                            
*                                                                               
         ST    R8,FULL                                                          
         B     EXITOK                                                           
         DROP  R8                                                               
*----------------------------                                                   
         LTORG                                                                  
         EJECT                                                                  
********************************************************************            
* INPUT  :  P1       A(CL5 STATION CALL LETTERS)                                
*           P2       A(OUTPUT AREA)                                             
*                                                                               
* OUTPUT :  CC !=    NO INVENTORY BOOK LIST                                     
*           CC =     INVENTORY BOOK LIST IN OUTPUT AREA                         
*                                                                               
********************************************************************            
         DS    0D                                                               
IBKLIST  NTR1  BASE=*                                                           
         B     *+12                                                             
         DC    CL8'*IBKLST*'                                                    
*                                                                               
         LM    R2,R3,0(R1)                                                      
*                                                                               
         LR    RE,R3                                                            
         SR    RF,RF                                                            
         LA    R3,3(R3)                                                         
         MVI   0(R3),1             TYPE SUBDIVISION                             
         MVI   1(R3),QIBKLTYP                                                   
         LA    RF,1(RF)                                                         
         LA    R3,2(R3)                                                         
*                                                                               
         MVI   0(R3),8             BOOK SUBDIVISION                             
         MVI   1(R3),QIBKLBK                                                    
         LA    RF,8(RF)                                                         
         LA    R3,2(R3)                                                         
*                                                                               
         MVI   0(R3),8             ALIAS SUBDIVISION                            
         MVI   1(R3),QIBKLONM                                                   
         LA    RF,8(RF)                                                         
         LA    R3,2(R3)                                                         
*                                                                               
         STCM  RF,3,0(RE)           DATA LENGTH                                 
         LA    RE,2(RE)                                                         
         LR    RF,R3                                                            
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         SRL   RF,1                                                             
         STC   RF,0(RE)            NUMBER OF SUBDIVISIONS                       
*                                                                               
K        USING RSTAKEY,KEY                                                      
         XC    K.RSTAKEY,K.RSTAKEY                                              
         MVI   K.RSTAKTYP,X'02'                                                 
         MVC   K.RSTAKREP,REPALPHA     REP POWER CODE                           
         MVC   K.RSTAKSTA,0(R2)        STATION CALL LETTERS                     
         OC    K.RSTAKSTA,SPACES                                                
         CLI   K.RSTAKSTA+4,C'1'   SATELLITE #1?                                
         BE    IBKL0010                                                         
         CLI   K.RSTAKSTA+4,C'2'   SATELLITE #2?                                
         BE    IBKL0010                                                         
         CLI   K.RSTAKSTA+4,C'T'   TV?                                          
         BNE   IBKL0020                                                         
IBKL0010 DS    0H                                                               
         MVI   K.RSTAKSTA+4,C' '   YES - TV HAS NO BAND                         
         DROP  K                                                                
*                                                                               
IBKL0020 DS    0H                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(L'RSTAKEY),KEYSAVE                                           
         BE    *+14                                                             
         MVC   0(2,R3),=Y(150)                                                  
         B     EXITNO                                                           
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
*                                                                               
         L     R6,AIOREC           CHECK FOR IVENTORY BOOK LIST ELEM            
         LA    R6,RSTAELEM-RSTAREC(R6)                                          
IBKL0022 DS    0H                                                               
         CLI   0(R6),0             END OF RECORD?                               
         BE    EXITNO                                                           
         CLI   0(R6),X'48'                                                      
         BE    IBKL0024                                                         
         ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         B     IBKL0022                                                         
*                                                                               
IBKL0024 DS    0H                                                               
         USING RSTABLEL,R6         READ BOOK LIST RECORD                        
         XC    KEY,KEY                                                          
K        USING RIBLKEY,KEY                                                      
         MVI   K.RIBLKTYP,RIBLKTYQ                                              
         MVI   K.RIBLKSTY,RIBLKSTQ                                              
         MVC   K.RIBLKREP,REPALPHA                                              
         MVC   K.RIBLKTAG,RSTABLTG                                              
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(L'KEY),KEY     SAVE THE KEY FOR BOOK TYPE READ              
         MVC   WORK+50(L'RSTABLTS),RSTABLTS  SAVE THE BOOK TYPES                
         DROP  R6                                                               
*                                                                               
         MVI   BYTE,C'N'           SET NO DATA                                  
*                                                                               
         LA    R4,WORK+49                                                       
         B     *+10                                                             
IBKL0030 DS    0H                                                               
         MVC   K.RIBLKBTY,0(R4)                                                 
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(L'RIBLKEY),KEYSAVE                                           
         BNE   IBKL0060                                                         
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
*                                                                               
         L     R6,AIOREC           CHECK FOR IVENTORY BOOK LIST ELEM            
         LA    R6,RIBL1ST-RIBLREC(R6)                                           
*                                                                               
IBKL0040 DS    0H                                                               
         CLI   0(R6),0             END OF RECORD?                               
         BE    IBKL0060                                                         
         CLI   0(R6),RIBLBKEQ                                                   
         BNE   IBKL0050                                                         
*                                                                               
         MVI   BYTE,C'Y'           SET DATA                                     
*                                                                               
         USING RIBLBOOK,R6                                                      
         MVC   0(1,R3),K.RIBLKBTY                                               
         MVC   1(8,R3),RIBLBKBK                                                 
*                                                                               
         XC    WORK+100(30),WORK+100     BUILD FAKE FIELD FOR BOOK NAME         
         MVI   WORK+100,16+8                                                    
         GOTOX VUNBOOK,DMCB,(1,RIBLBKBK),WORK+100,0,(C'+',=CL6' ')              
         MVC   1(8,R3),WORK+100+8                                               
*                                                                               
         MVC   1+8(8,R3),RIBLBKOV                                               
         LA    R3,1+8+8(R3)                                                     
         DROP  R6,K                                                             
*                                                                               
IBKL0050 DS    0H                                                               
         ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         B     IBKL0040                                                         
*                                                                               
IBKL0060 DS    0H                  READ NEXT TYPE                               
         LA    R4,1(R4)                                                         
         MVC   KEY,WORK                                                         
         CLI   0(R4),C' '                                                       
         BH    IBKL0030                                                         
*                                                                               
         CLI   BYTE,C'Y'           DATA?                                        
         BNE   EXITNO              NO                                           
*                                                                               
         XC    0(10,R3),0(R3)                                                   
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
********************************************************************            
AGADDRD  DSECT                                                                  
ADDRLN0  DS    CL36                                                             
ADDRLN1  DS    CL36                                                             
ADDRLN2  DS    CL36                                                             
ADDRLN3  DS    CL36                                                             
ADDRNAM  DS    CL(L'RAGYNAM2)                                                   
ADDRST   DS    CL(L'RAGYSTAT)                                                   
ADDRZIP  DS    CL(L'RAGYZIP)                                                    
********************************************************************            
       ++INCLUDE REPRPWORKD                                                     
       ++INCLUDE REGENSAL2                                                      
       ++INCLUDE REGENAGY2                                                      
       ++INCLUDE REGENIBKL                                                      
       ++INCLUDE REGENDSP                                                       
       ++INCLUDE REGENDCT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021REPRX10S  08/02/99'                                      
         END                                                                    
