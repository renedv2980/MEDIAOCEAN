*          DATA SET REPRO24    AT LEVEL 011 AS OF 11/02/98                      
*&&      SET   NOP=N                                                            
*PHASE T80A24B                                                                  
*                                                                               
T80A24   TITLE 'REPRO24 - PROPOSAL FETCH OVERLAY'                               
**********************************************************************          
* PARAMETER 1 - ADDRESS OF THE NEWFILE WORKING STORAGE                          
*                                                                               
* USES AIOREC FOR THE FETCH BLOCK                                               
*                                                                               
* MODULE ASSUMES MINIO HAS BEEN INITIALIZED IN AIO7                             
*                                                                               
* PASSES AIO1-4 AND AIO6 TO FETCH                                               
**********************************************************************          
         EJECT                                                                  
PRO24    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 OVERWRKQ,REPRO24*,R7,RR=RE,CLEAR=YES                             
         USING OVERWRKD,RC                                                      
*                                                                               
         L     R9,0(R1)    <========+                                           
         USING WORKD,R9             º                                           
*                                   º                                           
         L     RA,ATWA              º                                           
         USING TWAD,RA              º                                           
         L     R8,AGWORK            º                                           
         USING GWORKD,R8            º                                           
*                                   º                                           
         L     R2,AIOREC       <----+--------SAVE SOME STORAGE                  
         LA    R3,RFTBLKL           º                                           
         SR    R4,R4                º                                           
         SR    R5,R5                º                                           
         MVCL  R2,R4                º                                           
*                                   º                                           
         ST    RE,SVRELO            SO WE DON'T MESS WITH 23'S SVRELO           
         MVC   SVPARMS,0(R1)                                                    
         ST    R1,CALLR1                                                        
*                                                                               
         GOTOX ('SAVVALQ',AREPRO01)                                             
         EJECT                                                                  
*                                                                               
         GOTOX (GETPROFQ,AREPRO01),BODMCB,('RREPQSEL',SELPROFS)                 
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
***************                                                                 
* DESCRIPTION ELEMENT                                                           
***************                                                                 
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRDSELQ    GET THE DESCRIPTION ELEMENT                  
         BAS   RE,MINIOHI                                                       
         BE    *+6                                                              
         DC    H'0'                BETTER HAVE ONE                              
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRDSELD,R6                                                      
         XC    SAVSLNS,SAVSLNS                                                  
         MVC   SAVSLNS,RPRDSSEC    COPY THE SPOT LENGTHS                        
*                                                                               
         LA    RE,FTMTAB           GET THE FETCH METHOD                         
INITDS5  CLI   0(RE),FF            END OF TABLE?                                
         BE    INITDS10            YES - ASSUMPTION MADE                        
         CLC   RPRDSFTM,0(RE)      MATCH FOUND                                  
         BE    INITDS10            YES - CONVERT AND SAVE                       
         LA    RE,L'FTMTAB(RE)                                                  
         B     INITDS5                                                          
INITDS10 MVC   SAVFTM,1(RE)        SAVE THE FETCH METHOD                        
         MVI   SAVFTM,RFTCDC1Q     ALWAYS FIRST FOR NOW                         
*                                                                               
***************                                                                 
* DAYPART ELEMENT(S)                                                            
***************                                                                 
         XC    SAVDPTS,SAVDPTS                                                  
         XC    SAVDYTMS,SAVDYTMS                                                
         XC    SAVSEDTS,SAVSEDTS                                                
         LA    R2,SAVDPTS                                                       
         LA    R3,SAVDYTMS                                                      
         LA    R4,SAVSEDTS                                                      
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRDPELQ    GET THE DAYPART ELEMENT                      
         BAS   RE,MINIOHI                                                       
         BNE   EXITL                                                            
*                                                                               
INITDP10 L     R6,MINELEM                                                       
         USING RPRDPELD,R6                                                      
*                                                                               
         MVC   0(1,R2),RPRDPFLG                                                 
         MVC   1(L'RPRDPDPT,R2),RPRDPDPT                                        
         MVC   L'RPRDPDPT+1(4,R2),RPRDPTAB                                      
         MVC   0(L'RPRDPDAY,R3),RPRDPDAY                                        
         MVC   L'RPRDPDAY(L'RPRDPSTM+L'RPRDPETM,R3),RPRDPSTM                    
         MVC   0(L'RPRDPSDT+L'RPRDPEDT,R4),RPRDPSDT                             
         OI    RPRDPFLG,RPRDPFFT      SET TO FETCHED                            
         BAS   RE,MINIOWRT                                                      
*                                                                               
         LA    R2,L'SAVDPT(R2)                                                  
         LA    R3,L'SAVDYTM(R3)                                                 
         LA    R4,L'SAVSEDT(R4)                                                 
*                                                                               
INITDP15 BAS   RE,MINIOSEQ                                                      
         BNE   INITDPX                                                          
*                                                                               
         L     R6,MINELEM                                                       
         CLI   0(R6),RPRDPELQ      DAYPART ELEMENT STILL?                       
         BE    INITDP10                                                         
*                                                                               
INITDPX  DS    0H                                                               
         DROP  R6                                                               
***************                                                                 
* BOOK EXTENSION ELEMENT(S)                                                     
***************                                                                 
         XC    SAVXBKS,SAVXBKS                                                  
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRBXELQ    GET THE BOOK EXTENSION ELEMENT               
         BAS   RE,MINIOHI                                                       
         BNE   INITBXX                                                          
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRBXELD,R6                                                      
*                                                                               
INITBX10 CLI   0(R6),RPRBXELQ                                                   
         BNE   INITBX20                                                         
*                                                                               
         ZIC   R1,RPRBXIOR         INTERNAL ORDER NUMBER                        
         BCTR  R1,0                                                             
         MH    R1,=Y(L'SAVXBK)                                                  
         LA    R1,SAVXBKS(R1)                                                   
         USING XBOKLIN,R1                                                       
         MVC   XBLNDPT,RPRBXDPT                                                 
         MVC   XBLNFLG,RPRBXFLG                                                 
         DROP  R1                                                               
*                                                                               
INITBX16 BAS   RE,MINIOSEQ                                                      
         BE    INITBX10                                                         
*                                                                               
INITBX20 DS    0H                                                               
*                                                                               
INITBXX  DS    0H                                                               
***************                                                                 
* COST ELEMENTS - FOR RATE CARDS                                                
***************                                                                 
INITRT   DS    0H                                                               
         LA    R3,1                                                             
         XC    SAVRATCS,SAVRATCS                                                
*                                                                               
INITRT5  DS    0H                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRCHELQ                                                 
         STC   R3,MINEKEY+1                                                     
         BAS   RE,MINIOHI                                                       
         L     R6,MINELEM                                                       
         CLI   0(R6),RPRCHELQ                                                   
         BNE   INITRT15                                                         
         USING RPRCHELD,R6                                                      
         CLM   R3,1,RPRCHSEQ                                                    
         BNE   INITRT15                                                         
*                                                                               
         LR    RE,R3                                                            
         BCTR  RE,0                                                             
         MH    RE,=Y(L'SAVRATC)                                                 
         LA    RE,SAVRATCS(RE)                                                  
         USING RFTCRTES,RE                                                      
*                                                                               
         MVC   RFTCRTCD(4),RPRCHRTC                                             
         MVC   RFTCRTCD+4(4),RPRCHRT2                                           
         MVC   RFTCQQTR,RPRCHQTR                                                
         MVC   RFTCQYR,RPRCHYR                                                  
         NI    RFTCQYR,X'7F'                                                    
         MVC   RFTCSLN,RPRCHSLN                                                 
         DROP  RE                                                               
*                                                                               
         LR    RE,R3                                                            
         BCTR  RE,0                                                             
         MH    RE,=Y(L'SAVCOST)                                                 
         LA    RE,SAVCOSTS(RE)                                                  
         USING CSTLIN,RE                                                        
         ZIC   R1,RPRCHLEN                                                      
         BCTR  R1,0                                                             
         CH    R1,=Y(20-1)         MAX LABEL EX LEN                             
         BNH   *+8                                                              
         LA    R1,(20-1)                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CSLNLBL,RPRCHLBL                                                 
         MVC   CSLNLBK,RPRCHBK                                                  
         MVC   CSLNIORD,RPRCHSEQ                                                
         MVC   CSLNPBC,RPRCHPBC                                                 
         DROP  RE                                                               
*                                                                               
INITRT15 DS    0H                                                               
         LA    R3,1(R3)                                                         
         CH    R3,=H'4'                                                         
         BNH   INITRT5                                                          
*                                                                               
INITRTX  DS    0H                                                               
         EJECT                                                                  
************************************************                                
* DO INVENTORY FETCH FOR HEADER DATA                                            
************************************************                                
******************************************************************              
* PROPOSAL STATION ELEMENTS - DO AN APPROPRIATE FETCH FOR EACH ONE              
******************************************************************              
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRSTELQ    GET THE STATION ELEMENT                      
         USING RPRSTELD,R6                                                      
         BAS   RE,MINIOHI                                                       
         BE    *+6                 NEED AT LEAST ONE                            
         DC    H'0'                                                             
*                                                                               
DFSTA    DS    0H                                                               
         CLI   RPRSTEL,RPRSTELQ                       STATION ELEMENT?          
         BNE   DFSTAX                                 NO - ALL DONE             
*                                                                               
         LA    R0,FETCHBLK         CLEAR THE BLOCK                              
         LH    R1,=Y(RFTBLKL)                                                   
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R4,FETCHBLK                                                      
         USING RFTBLKD,R4                                                       
         MVC   RFTACOM,ACOM                    A(COMFACS)                       
         MVC   RFTAIO1,AIO1                    A(2K IO AREA)                    
         MVC   RFTAIO2,AIO6                    A(2K IO AREA)                    
         MVC   RFTAWRK,AIO2                    A(6K WORK AREA)                  
*                                              USES AIO2,AIO3, & AIO4           
         MVC   RFTCREP,CUAALF                           REP CODE                
         LA    RE,RFTCRTES                                                      
         LA    RF,SAVRATCS                                                      
         LA    R0,SAVRATCS+L'SAVRATCS                                           
DFSTARC1 DS    0H                                                               
         OC    0(L'SAVRATC,RF),0(RF)           ANY RATE CARD?                   
         BZ    *+14                            NO                               
         MVC   0(RFTCRTSL,RE),0(RF)                                             
         LA    RE,RFTCRTSL(RE)                                                  
         LA    RF,RFTCRTSL(RF)                                                  
         CR    RF,R0                                                            
         BL    DFSTARC1                                                         
*                                                                               
         MVC   SAVSTICD,RPRSTICD              SAVE THE INTETRNAL CODE           
         MVC   SAVSTFLG,RPRSTFLG              SAVE THE STATION FLAGS            
         MVC   RFTCSTAT,RPRSTSTA              STATION CALL LETTERS              
         CLI   RFTCSTAT+4,C' '                NEED 'T' SET?                     
         BNE   *+8                            NO                                
         MVI   RFTCSTAT+4,C'T'                                                  
         TM    RPRSTFLG,RPRSTSTL              SATELLITE REQUEST?                
         BZ    *+8                            NO                                
         MVI   RFTCSTAT+4,C'1'                                                  
*                                                                               
         TM    SAVSTFLG,RPRSTFFT   STATION FETCHED?                             
         BNZ   DFSTN0            YES                                            
         OI    RPRSTFLG,RPRSTFFT   SET TO FETCHED                               
         BAS   RE,MINIOWRT                                                      
*======================================================                         
         EJECT                                                                  
********************************************************                        
* FETCH FOR UNFETCHED DAYPARTS                                                  
********************************************************                        
DFSTN0   DS    0H                                                               
         MVI   RFTAMODE,RFTAINVQ                        FETCH MODE              
         MVI   RFTCNTL,RFTCHDRQ+RFTCRTEQ                DATA FLAGS              
         LA    RE,FTCHHOOK                                                      
         STCM  RE,15,RFTHOOKA                           HOOK ROUTINE            
*                                                                               
*--------------------------------------------------------                       
*            FETCH METHOD & DEF EFF START AND END DATES(FLIGHT DATES)           
*--------------------------------------------------------                       
         MVC   RFTCDCTL,SAVFTM     FETCH METHOD                                 
         GOTO1 VDATCON,DMCB,(3,CCONDAT),(2,RFTCEFST)                            
         GOTO1 VDATCON,DMCB,(3,CCONDAT+3),(2,RFTCEFEN)                          
*                                                                               
*--------------------------------------------------------                       
*            DAYPART, DAY/TIME AND EFFECTIVE DATES BLOCK                        
*--------------------------------------------------------                       
         LA    RF,SAVDPTS                                                       
         LA    RE,SAVDYTMS                                                      
         LA    R2,SAVSEDTS                                                      
         LA    R3,RFTCDTMS         FETCH DPT, DAY/TIME, EFF DATE BLOCK          
         USING RFTCDTMS,R3                                                      
         LA    R0,NUMLINS                                                       
*                                                                               
DFSTN5   DS    0H                                                               
         CLI   1(RF),C' '          ANY DAYPART?                                 
         BNH   DFSTN10             NO - ALL DONE                                
         TM    SAVSTFLG,RPRSTFFT   STATION FETCHED?                             
         BZ    *+12                NO                                           
         TM    0(RF),RPRDPFFT      DAYPART FETCHED?                             
         BNZ   DFSTN8              YES - SKIP IT                                
*                                                                               
         MVC   RFTCDTDP,1(RF)      DAYPART                                      
         MVC   RFTCDTDY,0(RE)      DAY                                          
         MVC   RFTCDTST,1(RE)      START TIME                                   
         MVC   RFTCDTEN,3(RE)      END TIME                                     
         OC    0(3,R2),0(R2)                                                    
         BZ    DFSTN6                                                           
         GOTO1 VDATCON,DMCB,(8,0(R2)),(2,RFTCDTES)     EFF START &              
DFSTN6   OC    3(3,R2),3(R2)                                                    
         BZ    DFSTN7                                                           
         GOTO1 VDATCON,DMCB,(8,3(R2)),(2,RFTCDTEE)     END DATES                
DFSTN7   LA    R3,RFTCDTLQ(R3)                                                  
DFSTN8   LA    RF,L'SAVDPT(RF)                                                  
         LA    RE,L'SAVDYTM(RE)                                                 
         LA    R2,L'SAVSEDT(R2)                                                 
         BCT   R0,DFSTN5                                                        
*                                                                               
         DROP  R3                                                               
         OC    RFTCDTDP,RFTCDTDP       ANY DAYPARTS?                            
         BZ    DFSTNXT                 NO - ALL DONE                            
*                                                                               
DFSTN10  DS    0H                                                               
*                                                                               
*--------------------------------------------------------                       
*            FETCH CALL - FOR ADDING NEW DETAIL CLUSTERS                        
*--------------------------------------------------------                       
         NI    MISCFLG1,X'FF'-MF1MNADD    NO ADD YET                            
         L     R0,MINELEM          CLEAR OUT MINELEM                            
         LH    R1,MINMAXEL                                                      
         SR    RF,RF                                                            
         SR    RE,RE                                                            
         MVCL  R0,RE                                                            
         GOTO1 VFETCH,DMCB,FETCHBLK                                             
*                                                                               
         L     R5,AIO7                                                          
         TM    MISCFLG1,MF1MNADD   NEED ADD?                                    
         BZ    DFFTRX              NO                                           
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
DFFTR10  BAS   RE,MINIOADD         ADD THE ELEMENT                              
         BE    DFFTRX                                                           
         BL    *+6                 DUPLICATE KEY ON ADD                         
         DC    H'0'                                                             
         ZIC   RE,RPRDTSEQ         DETAIL SEQUENCE #                            
         LA    RE,1(RE)            ADD 1                                        
         STC   RE,RPRDTSEQ                                                      
         CLI   RPRDTSEQ,X'00'      END OF THE LINE?                             
         BNE   DFFTR10             NO - TRY AGAIN                               
         DC    H'0'                                                             
*                                                                               
DFFTRX   DS    0H                                                               
         NI    MISCFLG1,FF-MF1MNADD                                             
         B     DFSTNXT             ALL DONE                                     
*                                                                               
*===================================================                            
         EJECT                                                                  
*************************                                                       
* PROCESS NEXT STATION                                                          
*************************                                                       
DFSTNXT  XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRSTELQ                                                 
         ZIC   RE,SAVSTICD                                                      
         LA    RE,1(RE)                                                         
         STC   RE,MINEKEY+1                                                     
         BAS   RE,MINIOHI                                                       
         BNE   DFSTAX                                                           
         L     R6,MINELEM                                                       
         B     DFSTA                                                            
*                                                                               
*===================================================                            
*                                                                               
DFSTAX   DS    0H                                                               
         BAS   RE,MINIOCLS         CALLER SHOULD DO THIS                        
         GOTOX ('RESVALQ',AREPRO01)                                             
*                                                                               
         B     EXITOK                                                           
         DROP  R6                                                               
         DROP  R4,R5                                                            
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
* HOOK FOR THE FETCH ROUTINE TO ADD NEW DETAIL CLUSTERS                         
***********************************************************************         
FTCHHOOK NTR1                                                                   
         LA    R4,FETCHBLK                                                      
         USING RFTBLKD,R4                                                       
         OC    RFTERR,RFTERR                                                    
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         TM    RFTMODE,RFTNHDRQ    NEW HEADER?                                  
         BZ    FHOOKX              NO                                           
*                                                                               
*===================================================                            
         EJECT                                                                  
*********                                                                       
* CHECK IF WE NEED TO ADD AN ELEMENT FIRST                                      
*********                                                                       
         TM    MISCFLG1,MF1MNADD                                                
         BZ    FHIP0                  NO                                        
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
FHMNADD5 BAS   RE,MINIOADD         ADD THE ELEMENT                              
         BE    FHMNX                                                            
         BL    *+6                 DUPLICATE KEY ON ADD                         
         DC    H'0'                                                             
         ZIC   RE,RPRDTSEQ         DETAIL SEQUENCE #                            
         LA    RE,1(RE)            ADD 1                                        
         STC   RE,RPRDTSEQ                                                      
         CLI   RPRDTSEQ,X'00'      END OF THE LINE?                             
         BNE   FHMNADD5            NO - TRY AGAIN                               
         DC    H'0'                                                             
*                                                                               
FHMNX    NI    MISCFLG1,X'FF'-MF1MNADD    DON'T NEED ADD ANYMORE                
         L     R0,MINELEM          CLEAR OUT MINELEM                            
         LH    R1,MINMAXEL                                                      
         SR    RF,RF                                                            
         SR    RE,RE                                                            
         MVCL  R0,RE                                                            
*                                                                               
         DROP  R6                                                               
*                                                                               
*===================================================                            
*                                                                               
*********                                                                       
* PROCESS PROGRAM NAMES                                                         
********                                                                        
FHIP0    OI    MISCFLG1,MF1MNADD   NEED ADD                                     
         LA    R3,RFTFPGMS                                                      
         LA    R2,SAVPRGNS                                                      
         XC    SAVPRGNS,SAVPRGNS                                                
         OC    RFTFPGMS,RFTFPGMS                                                
         BZ    FHIPX               NOT GOOD!!!                                  
*                                                                               
         USING SAVPRGND,R2                                                      
         SR    RE,RE               PROGRAM NAME COUNT                           
FHIP5    CLI   0(R3),0             ANY NAME ?                                   
         BE    FHIP10              NO                                           
         CH    RE,=H'8'            LAST                                         
         BE    FHIP10              YES                                          
         LA    RE,1(RE)                                                         
         LA    RF,L'RFTFPGMS-1(R3) LAST CHARACTER OF NAME                       
         CLI   0(RF),C' '          SIGN. CHAR?                                  
         BH    *+8                 YES                                          
         BCT   RF,*-8                                                           
*                                                                               
         SR    RF,R3               LENGTH OF STRING                             
         LA    RF,1(RF)            CORRECTION FACTOR                            
         STC   RF,SAVPRGLN         SAVE IT IN THE TABLE                         
         LA    R3,L'RFTFPGMS(R3)                                                
         LA    R2,SAVPRGNQ(R2)                                                  
         B     FHIP5               CHECK NEXT NAME                              
         DROP  R2                                                               
*                                                                               
FHIP10   STC   RE,PRGCOUNT                                                      
         STC   RE,BYTE1          PROGRAMS LEFT TO MATCH                         
         XC    HALF1,HALF1     CLEAR THE SAVED SEQUENCE #                       
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRTXELQ    TEXT ELEMENT                                 
         MVI   MINEKEY+1,RPRTXPRQ  PROGRAM TYPE TEXT                            
         BAS   RE,MINIOHI                                                       
         BNE   FHIP30              ADD ALL THE PROGRAM NAMES                    
         L     R6,MINELEM                                                       
         USING RPRTXELD,R6                                                      
FHIP12   CLI   0(R6),RPRTXELQ      TEXT ELEMENT?                                
         BNE   FHIP30              NO - ADD THE NAMES                           
         CLI   2(R6),RPRTXPRQ      PROGRAM TEXT?                                
         BNE   FHIP30              NO - ADD THE NAMES                           
*                                                                               
         MVC   HALF1,RPRTXSEQ    SAVE THE SEQUENCE NUMBER FOR ADDING            
         ZIC   R1,RPRTXLEN         ELEMENT LENGTH                               
         LA    RE,RPRTXOVQ         ELEMENT OVERHEAD                             
         SR    R1,RE               TEXT LENGTH                                  
         LA    R3,RFTFPGMS                                                      
         LA    R2,SAVPRGNS                                                      
         USING SAVPRGND,R2                                                      
*                                                                               
         ZIC   R0,PRGCOUNT         NUMBER OF PROGRAM NAMES                      
FHIP14   ZIC   RE,SAVPRGLN         PROGRAM NAME LENGTH                          
         OC    SAVPRGCD,SAVPRGCD   ALREADY MATCHED?                             
         BNZ   FHIP16              YES - SKIP                                   
*                                                                               
         CR    RE,R1               LENGTH MATCH?                                
         BNE   FHIP16              NO                                           
         BCTR  R1,0                GET EX TEXT LENGTH                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),RPRTXTXT    TEXT MATCH?                                  
         BNE   FHIP16              NO                                           
*                                                                               
         ZIC   R1,BYTE1          CORRECT MATCHES LEFT COUNT                     
         BCTR  R1,0                                                             
         STC   R1,BYTE1                                                         
         MVC   SAVPRGCD,RPRTXSEQ   SAVE THE TEXT SEQUENCE #                     
         LTR   R1,R1               ANY LEFT TO MATCH?                           
         BZ    FHIPX               NO                                           
*                                                                               
FHIP16   LA    R2,SAVPRGNQ(R2)     NEXT PROGRAM NAME                            
         LA    R3,L'RFTFPGMS(R3)                                                
         BCT   R0,FHIP14                                                        
*                                                                               
FHIP18   BAS   RE,MINIOSEQ         NO MATCHES FOR THIS ELEMENT                  
         BNE   FHIP30              EOR - ADD PROGRAM NAMES                      
         B     FHIP12                                                           
         DROP  R2,R6                                                            
*                                                                               
FHIP30   DS    0H                  ADD REMAINING PROGRAM NAMES                  
         LA    R3,RFTFPGMS                                                      
         LA    R2,SAVPRGNS                                                      
         USING SAVPRGND,R2                                                      
         ZIC   R0,BYTE1                                                         
*                                                                               
FHIP33   OC    SAVPRGCD,SAVPRGCD   ALREADY MATCHED?                             
         BZ    FHIP35              NO                                           
         LA    R2,SAVPRGNQ(R2)     NEXT PROGRAM NAME                            
         LA    R3,L'RFTFPGMS(R3)                                                
         B     FHIP33                                                           
*                                                                               
FHIP35   L     R6,MINELEM                                                       
         USING RPRTXELD,R6                                                      
         XC    0(256,R6),0(R6)                                                  
         MVI   RPRTXEL,RPRTXELQ                                                 
         ZIC   RE,SAVPRGLN         TEXT LENGTH                                  
         LA    R1,RPRTXOVQ(RE)     ELEMENT LENGTH                               
         STC   R1,RPRTXLEN                                                      
         MVI   RPRTXTYP,RPRTXPRQ PROGRAM TYPE                                   
         LH    R1,HALF1          LOAD SAVED SEQ. #                              
         LA    R1,1(R1)                                                         
         STCM  R1,3,RPRTXSEQ       NEW SEQUENCE #                               
         STCM  R1,3,SAVPRGCD                                                    
         STH   R1,HALF1                                                         
         BCTR  RE,0                EX TEXT LENGTH                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   RPRTXTXT(0),0(R3)   TEXT                                         
         BAS   RE,MINIOADD         ADD THE ELEMENT                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
FHIP37   LA    R2,SAVPRGNQ(R2)     NEXT PROGRAM NAME                            
         LA    R3,L'RFTFPGMS(R3)                                                
         BCT   R0,FHIP33                                                        
         DROP  R2,R6                                                            
*                                                                               
FHIPX    DS    0H                                                               
*                                                                               
*===================================================                            
         EJECT                                                                  
**************************************                                          
* ADD DETAIL ELEMENT CLUSTER                                                    
**************************************                                          
*    DETAIL ELEMENT                                                             
********                                                                        
         L     R0,MINELEM          CLEAR OUT MINELEM                            
         LH    R1,MINMAXEL                                                      
         SR    RF,RF                                                            
         SR    RE,RE                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
         MVI   RPRDTEL,RPRDTELQ                                                 
         MVI   RPRDTLEN,RPRDTOVQ   LENGTH W/O WEEKS                             
         LA    RE,RFTFDTMS                                                      
         USING RFTFDTDY,RE                                                      
         MVC   RPRDTDAY,RFTFDTDY   DAYS                                         
         MVC   RPRDTSTM,RFTFDTST   START TIME                                   
         MVC   RPRDTETM,RFTFDTEN   END TIME                                     
         DROP  RE                                                               
         GOTOX (PCKTIMQ,AREPRO01),DMCB,RFTFDTST,RFTFDTEN,RPRDTTIM               
         MVC   RPRDTSTA,SAVSTICD   INTERNAL STATION CODE                        
         GOTO1 VDATCON,DMCB,(2,RFTFEFST),(19,RPRDTEFF)                          
         GOTO1 VDATCON,DMCB,(2,RFTFEFEN),(19,RPRDTEEF)                          
*                                                                               
         TM    SELPROF+SELPDPB,SELPDPA                                          
         BNO   FHIDT04              USE MATCHED DAYPART                         
*                                                                               
         LA    R1,SAVDPTS                                                       
         LA    RE,SAVDPTS+L'SAVDPTS                                             
FHIDT01  CLC   1(1,R1),RFTFDPTS    MATCH ON PRINCIPLE DAYPART?                  
         BNE   *+14                NO                                           
         MVC   RPRDTDPT,1(R1)                                                   
         B     FHIDT10             ALREADY ON CORRECT LINE                      
*                                                                               
         LA    R1,L'SAVDPT(R1)                                                  
         CR    R1,RE                                                            
         BL    FHIDT01                                                          
*                                                                               
FHIDT04  MVC   RPRDTDPT,RFTFDTM    SHOULD USE DAYPART MATCHED                   
         LA    R1,SAVDPTS          FIND THE CPP                                 
         LA    RE,SAVDPTS+L'SAVDPTS                                             
FHIDT05  CLC   1(1,R1),RPRDTDPT    HAS TO BE THERE - WELL NOT REALLY            
         BE    FHIDT10                                                          
         LA    R1,L'SAVDPT(R1)     NEXT DAY PART                                
         CR    R1,RE                                                            
         BL    FHIDT05                                                          
         B     *+10                                                             
FHIDT10  MVC   RPRDTTAB,2(R1)      TAB CPP                                      
*                                                                               
         LA    R1,1                                                             
         LA    R3,SAVXBKS          SET SUPRESSED BOOKS                          
         USING XBOKLIN,R3                                                       
FHIDT12  DS    0H                                                               
         OC    XBLNDPT,XBLNDPT     ANY DAYPARTS?                                
         BZ    FHIDT16             NO - NOTHING TO DO                           
*                                                                               
         LA    RE,XBLNDPT                                                       
         LA    RF,L'XBLNDPT(RE)                                                 
*                                                                               
FHIDT14  DS    0H                                                               
         CLC   RPRDTDPT,0(RE)      DAYPART IN MATCH?                            
         BE    FHIDT15             YES - SUPPRESS                               
         LA    RE,1(RE)                                                         
         CR    RE,RF                                                            
         BL    FHIDT14                                                          
         B     FHIDT16             NOT FOUND, DON'T SUPPRESS                    
*                                                                               
FHIDT15  LA    RF,X'80'                                                         
         LR    RE,R1                                                            
         SRL   RF,0(RE)                                                         
         STC   RF,BOBYTE1          GET CORRECT MASK FOR BOOK                    
         OC    RPRDTBKS,BOBYTE1                                                 
*                                                                               
FHIDT16  DS    0H                                                               
         LA    R1,1(R1)                                                         
         LA    R3,L'SAVXBK(R3)                                                  
         LA    R0,SAVXBKS+L'SAVXBKS                                             
         CR    R3,R0                                                            
         BL    FHIDT12                                                          
         DROP  R3                                                               
*                                                                               
         MVC   RPRDTINM,RFTFINV    INVENTORY NUMBER                             
         MVC   RPRDTSTM,RFTFDTST   FIRST START TIME IN DETAIL                   
         MVC   RPRDTETM,RFTFDTEN   FIRST END TIME IN DETAIL                     
         MVC   RPRDTPRG,SAVPRGNS+(SAVPRGCD-SAVPRGND)                            
         MVC   RPRDTSLN,SAVSLNS    FIRST LENGTH IS DEFAULT                      
*                                                                               
* CHECK FOR FIRST COST                                                          
*                                                                               
         OC    SAVRATC,SAVRATC     ANY RATE CARD?                               
         BZ    FHIDT30             NO - NO COST THEN                            
*                                                                               
         LA    RE,RFTFRTES                                                      
         LA    R0,RFTFRTEN                                                      
FHIDT20  DS    0H                                                               
         OC    0(RFTFRTSL,RE),0(RE)                                             
         BZ    FHIDT30                                                          
         CLC   SAVRATC,0(RE)       MATCH?                                       
         BE    FHIDT22             YES                                          
         LA    RE,RFTFRTSL(RE)                                                  
         BCT   R0,FHIDT20                                                       
         B     FHIDT30                                                          
*                                                                               
FHIDT22  DS    0H                                                               
         MVC   RPRDTNC1,RFTFRRTE-RFTFRTES(RE)                                   
         NI    RPRDTNC1,X'7F'       CLEAR N/A FLAG ON LARGE COST                
*                                                                               
FHIDT30  DS    0H                                                               
         ZIC   RE,1(R6)            SKIP OVER ELEM                               
         AR    R6,RE                                                            
*                                                                               
*===================================================                            
         SPACE 2                                                                
********                                                                        
*    SUPLEMENTAL DAY/TIME ELEMENTS                                              
********                                                                        
         LA    R3,RFTFDTMS+L'RFTFDTMS                                           
         USING RPRDYELD,R6                                                      
         OC    0(L'RFTFDTMS,R3),0(R3)                                           
         BZ    FHISDXX             NO DATA - DONE                               
*                                                                               
         MVI   RPRDYEL,RPRDYELQ    ELEMENT CODE                                 
*                                                                               
         LA    R0,7                MAX OF 7 EXTRA DAY TIMES                     
         LA    R2,RPRDYDTM         DAY/TIME LIST                                
*                                                                               
FHISD5   MVC   0(L'RPRDYDTM,R2),0(R3)                                           
         LA    R2,L'RPRDYDTM(R2)                                                
         LA    R3,L'RFTFDTMS(R3)                                                
         OC    0(L'RFTFDTMS,R3),0(R3)                                           
         BZ    FHISDX              NO DATA - DONE                               
         BCT   R0,FHISD5                                                        
*                                                                               
FHISDX   DS    0H                                                               
         SR    R2,R6               ELEMENT LENGTH                               
         STC   R2,RPRDYLEN                                                      
         ZIC   RE,1(R6)            SKIP OVER ELEM                               
         AR    R6,RE                                                            
FHISDXX  DS    0H                                                               
*                                                                               
*===================================================                            
         SPACE 2                                                                
********                                                                        
*    SUPLEMENTAL PROGRAM ELEMENTS                                               
********                                                                        
         LA    R2,SAVPRGNS+L'SAVPRGN         SECOND NAME                        
         USING SAVPRGND,R2                                                      
         USING RPRPRELD,R6                                                      
*                                                                               
         OC    SAVPRGCD,SAVPRGCD   ANY TEXT ATTACHED?                           
         BZ    FHISPXX             NO                                           
*                                                                               
         MVI   RPRPREL,RPRPRELQ    ELEMENT CODE                                 
*                                                                               
         LA    R3,RPRPRPRG         PROGRAM LINKS LIST                           
         LA    R0,7                MAX OF 7 EXTRA NAMES                         
FHISP5   MVC   0(L'RPRPRPRG,R3),SAVPRGCD                                        
         LA    R2,SAVPRGNQ(R2)                                                  
         LA    R3,L'RPRPRPRG(R3)                                                
         OC    SAVPRGCD,SAVPRGCD   ANY TEXT ATTACHED?                           
         BZ    FHISPX              NO                                           
         BCT   R0,FHISP5                                                        
*                                                                               
FHISPX   DS    0H                                                               
         SR    R3,R6               ELEMENT LENGTH                               
         STC   R3,RPRPRLEN                                                      
         DROP  R2                                                               
         ZIC   RE,1(R6)            SKIP OVER ELEM                               
         AR    R6,RE                                                            
FHISPXX  DS    0H                                                               
*                                                                               
*===================================================                            
         SPACE 2                                                                
********                                                                        
*    SUPPLEMENTAL COSTS                                                         
********                                                                        
         OC    RFTFRTES(RFTFRTEN*RFTFRTSL),RFTFRTES                             
         BZ    FHICSX                                                           
*                                                                               
         LA    R2,SAVRATCS+L'SAVRATC        SUPPLEMENTAL RATE CARDS?            
         OC    0(3*L'SAVRATC,R2),0(R2)                                          
         BZ    FHICS30                      NO                                  
*                                                                               
         USING RPRCSELD,R6                                                      
         MVI   RPRCSEL,RPRCSELQ                                                 
         MVI   RPRCSLEN,RPRCSLNQ                                                
         LA    R3,RPRCSNC2                                                      
         DROP  R6                                                               
*                                                                               
FHICS08  DS    0H                                                               
         OC    0(L'SAVRATC,R2),0(R2)        ANY RATE CARD?                      
         BZ    FHICS20                      NO                                  
*                                                                               
         LA    RE,RFTFRTES                                                      
         LA    R0,RFTFRTEN                                                      
FHICS10  DS    0H                                                               
         OC    0(RFTFRTSL,RE),0(RE)                                             
         BZ    FHICS20                                                          
         CLC   0(L'SAVRATC,R2),0(RE)        MATCH?                              
         BE    FHICS12                      YES                                 
         LA    RE,RFTFRTSL(RE)                                                  
         BCT   R0,FHICS10                                                       
         B     FHICS20                                                          
*                                                                               
FHICS12  DS    0H                                                               
         MVC   0(L'RPRCSNC2,R3),RFTFRRTE-RFTFRTES(RE)                           
         NI    0(R3),X'7F'           CLEAR N/A FLAG ON LARGE COST               
*                                                                               
FHICS20  DS    0H                                                               
         LA    R2,L'SAVRATC(R2)                                                 
         LA    R3,RPRCSNC3-RPRCSNC2(R3)                                         
         LA    R0,SAVRATCS+L'SAVRATCS                                           
         CR    R2,R0                                                            
         BL    FHICS08                                                          
*                                                                               
FHICS30  DS    0H                  NOW DO PERCENTAGES                           
         L     RE,MINELEM                                                       
         USING RPRDTELD,RE                                                      
         MVC   BOFULL1,RPRDTNC1                                                 
         OC    BOFULL1,BOFULL1                                                  
         BZ    FHICSX              NO COST SKIP LINE                            
         TM    BOFULL1,X'80'       N/A?                                         
         BNZ   FHICSX              YES SKIP LINE                                
         DROP  RE                                                               
*                                                                               
         USING RPRCSELD,R6                                                      
         MVI   RPRCSEL,RPRCSELQ                                                 
         MVI   RPRCSLEN,RPRCSLNQ                                                
         LA    R3,RPRCSNC2                                                      
         DROP  R6                                                               
*                                                                               
         LA    R0,NUMCSTS-1                                                     
         LA    RE,SAVCOSTS+L'SAVCOST                                            
         USING CSTLIN,RE                                                        
*                                                                               
FHICS036 DS    0H                                                               
         CLI   CSLNPBC,0           % OF BASE COST?                              
         BE    FHICS038            NO                                           
*                                                                               
         L     R1,BOFULL1                                                       
         CVD   R1,BODUB1                                                        
         ZAP   BOWORK1(16),BODUB1                                               
         MP    BOWORK1(16),=P'100'                                              
         ZIC   R1,CSLNPBC                                                       
         CVD   R1,BODUB1                                                        
         MP    BOWORK1(16),BODUB1                                               
         DP    BOWORK1(16),=PL8'100'                                            
         MVC   BODUB1,BOWORK1                                                   
         SRP   BODUB1,64-2,5                                                    
*                                                                               
         SRP   BODUB1,64-2,5                                                    
         CVB   R1,BODUB1                                                        
         MH    R1,=H'100'          MAKE IT DOLLARS                              
         ST    R1,0(R3)                                                         
*                                                                               
FHICS038 DS    0H                                                               
         LA    RE,L'SAVCOST(RE)                                                 
         LA    R3,RPRCSNC3-RPRCSNC2(R3)                                         
         BCT   R0,FHICS036                                                      
         DROP  RE                                                               
*                                                                               
FHICSX   DS    0H                                                               
         ZIC   RE,1(R6)            SKIP OVER ELEM                               
         AR    R6,RE                                                            
*                                                                               
*===================================================                            
         SPACE 2                                                                
********                                                                        
*    AVAIL DAY/TIMES                                                            
********                                                                        
FHIAV    DS    0H                                                               
         OC    RFTFAVLS(RFTFAVNQ*L'RFTFAVLS),RFTFAVLS                           
         BZ    FHIAVX              NONE - ADD INV DAYTIMES??                    
*                                                                               
         LA    R2,RFTFAVLS                                                      
         LA    R0,RFTFAVLS+(RFTFAVNQ*L'RFTFAVLS)                                
FHIAV1   CLI   0(R2),C'/'          PARSE CHARACTER?                             
         BNE   *+8                                                              
         MVI   0(R2),C'\'          YES - REPLACE IT                             
         LA    R2,1(R2)                                                         
         CR    R2,R0                                                            
         BL    FHIAV1                                                           
*                                                                               
         LA    R2,RFTFAVLS                                                      
         USING RPRAVELD,R6                                                      
         LA    R3,RPRAVALS                                                      
         MVI   RPRAVEL,RPRAVELQ                                                 
*                                                                               
FHIAV2   OC    0(L'RFTFAVLS,R2),0(R2)                                           
         BZ    FHIAV4                                                           
*                                                                               
         MVC   0(L'RPRAVALS,R3),0(R2)                                           
         LA    R3,L'RPRAVALS(R3)                                                
         LA    R2,L'RFTFAVLS(R2)                                                
         CR    R2,R0                                                            
         BL    FHIAV2                                                           
*                                                                               
FHIAV4   DS    0H                                                               
         SR    R3,R6               ELEMENT LENGTH                               
         STC   R3,RPRAVLEN                                                      
         ZIC   RE,1(R6)            SKIP OVER ELEM                               
         AR    R6,RE                                                            
FHIAVX   DS    0H                                                               
*                                                                               
*===================================================                            
         SPACE 2                                                                
FHIX     DS    0H                                                               
         DROP  R6                                                               
FHOOKX   B     EXIT                                                             
         DROP  R4,R5                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE READS FOR A MINIO ELEMENT                                        
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
*              MINEKEY             MINIO ELEMENT KEY SET BY CALLER              
***********************************************************************         
MINIORD  NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         GOTO1 VMINIO,DMCB,('MINRD',(R5))                                       
         CLI   MINERR,0                                                         
         BE    EXITOK                                                           
         DC    H'0'                DIE ON ANY ERROR                             
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE READS HIGH FOR A MINIO ELEMENT.                                  
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
*              MINEKEY             MINIO ELEMENT KEY SET BY CALLER              
***********************************************************************         
MINIOHI  NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         GOTO1 VMINIO,DMCB,('MINHI',(R5))                                       
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    EXITOK                                                           
         B     EXITL               OTHERWISE RETURN 'NO'                        
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE READS SEQUENTIAL FOR A MINIO ELEMENT.                            
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
***********************************************************************         
MINIOSEQ NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         GOTO1 VMINIO,DMCB,('MINSEQ',(R5))                                      
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    EXITOK                                                           
         CLI   MINERR,MINEEOF      RETURN 'NO' IF END-OF-FILE                   
         BE    EXITL                                                            
         DC    H'0'                DIE ON ANY OTHER ERROR                       
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE WRITES OUT A MINIO ELEMENT.                                      
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
*              MINELEM             CONTAINS THE MINIO ELEMENT                   
***********************************************************************         
MINIOWRT NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         L     RF,MINELEM                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(1),0(RF)                                                 
         MVC   MINEKEY+1(L'RPROKMEL-1),2(RF)                                    
*                                                                               
         OI    MNIOFLAG,MNIOCLSQ   REMEMBER TO CLOSE MINIO FILE                 
         GOTO1 VMINIO,DMCB,('MINWRT',(R5))                                      
         CLI   MINERR,0                                                         
         BE    EXITOK                                                           
         DC    H'0'                DIE ON ANY ERROR                             
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE ADDS A MINIO ELEMENT.                                            
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
*              MINELEM             CONTAINS THE MINIO ELEMENT                   
***********************************************************************         
MINIOADD NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         L     RF,MINELEM                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(1),0(RF)                                                 
         MVC   MINEKEY+1(L'RPROKMEL-1),2(RF)                                    
*                                                                               
         OI    MNIOFLAG,MNIOCLSQ   REMEMBER TO CLOSE MINIO FILE                 
         GOTO1 VMINIO,DMCB,('MINADD',(R5))                                      
*                                                                               
         CLI   MINERR,0                                                         
         BE    EXITOK                                                           
         CLI   MINERR,MINEDUP      DUPLICATE KEY?                               
         BE    EXITL               YES, RETURN A NO                             
         DC    H'0'                DIE ON ANY ERROR                             
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE DELETES A MINIO ELEMENT.  CALLER IS RESPONSIBLE FOR              
* POINTING TO ELEMENT FIRST.                                                    
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
***********************************************************************         
MINIODEL NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         OI    MNIOFLAG,MNIOCLSQ   REMEMBER TO CLOSE MINIO FILE                 
         GOTO1 VMINIO,DMCB,('MINDEL',(R5))                                      
         CLI   MINERR,0                                                         
         BE    EXITOK                                                           
         DC    H'0'                DIE ON ANY ERROR                             
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE CLOSES MINIO AND FLUSHES OUT THE BUFFERS TO THE MINIO            
* RECORDS.                                                                      
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
***********************************************************************         
MINIOCLS NTR1                                                                   
         TM    MNIOFLAG,MNIOCLSQ   DO WE NEED TO?                               
         BZ    EXITOK              NO                                           
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         GOTO1 VMINIO,DMCB,('MINCLS',(R5))                                      
         CLI   MINERR,0                                                         
         BE    EXITOK                                                           
         DC    H'0'                DIE ON ANY ERROR                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* EXITS                                                                         
***********************************************************************         
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         SPACE 1                                                                
EXIT     L     R1,CALLR1           RETURN PARAMETERS                            
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
         XIT1  ,                   EXIT WITH CC SET                             
DIE      DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                          
***********************************************************************         
         LTORG                                                                  
*                                                                               
         SPACE 2                                                                
FTMTAB   DS    0XL2                FETCH METHOD CONVERSION TABLE                
         DC    AL1(RPRDSFAQ,0)               ALL DAYPARTS                       
         DC    AL1(RPRDSFFQ,RFTCDC1Q)        FIRST DAYPART                      
         DC    AL1(RPRDSFPQ,RFTCDCPQ)        PRIMARY DAYPART                    
         DC    X'FF',X'0'                    UNKNOWN(WALTER LIKES ALL)          
*                                                                               
         SPACE 1                                                                
NUMLINS  EQU   8                   NUMBER OF DAYPART LINES                      
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
         EJECT                                                                  
***********************************************************************         
* OVERLAY WORKING STORAGE                                                       
***********************************************************************         
OVERWRKD DSECT                                                                  
DMCB     DS    6F                                                               
SVRELO   DS    A                                                                
CALLR1   DS    A                                                                
SVPARMS  DS    0XL24                                                            
SVPARMS1 DS    A                                                                
SVPARMS2 DS    A                                                                
SVPARMS3 DS    A                                                                
SVPARMS4 DS    A                                                                
SVPARMS5 DS    A                                                                
SVPARMS6 DS    A                                                                
*                                                                               
HALF1    DS    H                                                                
HALF2    DS    H                                                                
BYTE1    DS    X                                                                
BYTE2    DS    X                                                                
*                                                                               
SELPROFS DS    0CL10                CONTRACT PROFILES                           
         DS    CL1                 PROGRAM #                                    
         DS    CL1                 SPARE                                        
SELPROF  DS    CL8                 PROFILE BITS                                 
*                                                                               
MISCFLG1 DS    XL1                 MISCELLANEOUS FLAGS                          
MF1MNADD EQU   X'80'                - NEED MINO ADD ON FETCH                    
MF1MNWRT EQU   X'40'                - NEED MINO WRITE ON FETCH                  
MF1TMPBT EQU   X'01'                - TEMPORARY BIT (USED BY ANYONE)            
*                                                                               
MNIOFLAG DS    XL1                 MINIO FLAG                                   
MNIOCLSQ EQU   X'80'               - A CHANGE WAS MADE, CLOSE MINIO             
*                                                                               
ELCODE   DS    X                                                                
*                                                                               
SAVFTM   DS    X                   SAVED FETCH METHOD                           
*                                                                               
SAVXBKS  DS    0XL(NUMBKS*XBLNLENQ)   BOOK EXTENSION                            
SAVXBK   DS    (NUMBKS)XL(XBLNLENQ)                                             
*                                                                               
SAVSLNS  DS    0CL(6*1)            SAVED 1-BYTE SPOT LENGTHS                    
SAVSLN   DS    6XL1                                                             
*                                                                               
SAVDPTS  DS    0CL((NUMDPTS+1)*(1+5)) 1BYTE FLAG, 1BYTE DPT/4BYTE CPP           
SAVDPT   DS    (NUMDPTS+1)XL(1+5)                                               
*                                                                               
SAVDYTMS DS    0CL((NUMDPTS+1)*5)   SAVED 1-BYTE DAY/4-BYTE TIMES               
SAVDYTM  DS    (NUMDPTS+1)XL5                                                   
*                                                                               
SAVSEDTS DS    0CL((NUMDPTS+1)*6)   SAVED PWOS JULIAN START & END DATES         
SAVSEDT  DS    (NUMDPTS+1)XL6                                                   
*                                                                               
SAVCOSTS DS    0XL(NUMCSTS*CSLNLENQ)                                            
SAVCOST  DS    (NUMCSTS)XL(CSLNLENQ)                                            
*                                                                               
SAVRATCS DS    0XL((NUMCSTS)*RFTCRTSL)    RATE CARD                             
SAVRATC  DS    (NUMCSTS)XL(RFTCRTSL)                                            
*                                                                               
SAVSTICD DS    C                   SAVED STATION INTERNAL CODE                  
SAVSTFLG DS    C                   SAVED STATION FLAGS                          
*                                                                               
SAVPRGNS DS    0CL(8*SAVPRGNQ)     SAVED PROGRAM NAMES                          
SAVPRGN  DS    8CL(SAVPRGNQ)                                                    
PRGCOUNT DS    X                   COUNT OF PROGRAM NAMES                       
*                                                                               
FTCHUPGD DS    XL((7*(11+14)))   FETCH UPGRADE EXPR BLOCK                       
         DS    XL1                                                              
*                                                                               
FETCHBLK DS    CL(RFTBLKL)         FETCH BLOCK                                  
*                                                                               
OVERWRKQ EQU   *-OVERWRKD          LENGTH OF WORKING STORAGE                    
         DS    XL(IOAREALN-RFTBLKL)                                             
         SPACE 2                                                                
SAVPRGND DSECT                     SAVED PROGRAM NAME DSECT                     
SAVPRGLN DS    X                   LENGTH OF STRING                             
SAVPRGCD DS    XL2                 TEXT ELEMENT CODE                            
SAVPRGNQ EQU   *-SAVPRGND                                                       
         EJECT                                                                  
*                                                                               
* REPROLN                                                                       
         PRINT OFF                                                              
       ++INCLUDE REPROLN                                                        
         EJECT                                                                  
         PRINT ON                                                               
* REPROWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE REPROWORK                                                      
         PRINT ON                                                               
* REFETCHD                                                                      
         PRINT OFF                                                              
       ++INCLUDE REFETCHD                                                       
         PRINT ON                                                               
* FAFACTS                                                                       
**********RINT OFF                                                              
**********NCLUDE FAFACTS                                                        
**********RINT ON                                                               
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
* DDDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDDEQUS                                                       
         PRINT ON                                                               
* CTMSGEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTMSGEQUS                                                      
         PRINT ON                                                               
* FASELIST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASELIST                                                       
         PRINT ON                                                               
* FASYSFAC                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSFAC                                                       
         PRINT ON                                                               
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* DDFLDHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011REPRO24   11/02/98'                                      
         END                                                                    
*                                                                               
