*          DATA SET SPSFM75    AT LEVEL 059 AS OF 12/05/12                      
*PHASE T21775A                                                                  
*INCLUDE BINSR31                                                                
T21775   TITLE 'SPSFM75 - DARE ORDER MOVE'                                      
*                                                                               
T21775   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21775*,R7,RR=R3                                              
*                                                                               
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE         R5 = A(OVERLAY STORAGE AREA)                 
         USING SAVED,R5                                                         
         ST    R3,RELO                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(0,WORK)                                       
         GOTO1 ADDAY,DMCB,WORK,DUB,-14                                          
         GOTO1 DATCON,DMCB,(0,DUB),(2,TDYLSS2W)                                 
         XC    TDYLSS2W,=X'FFFF'    **MUST COMPLEMENT!!                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
         B     XIT                                                              
***********************************************************************         
* VALIDATE THE KEY                                                              
***********************************************************************         
VK       DS    0H                                                               
         MVI   MISCFLG1,0                                                       
         LA    R2,CONWHENH                                                      
         CLI   CONWHENH+5,0        MUST HAVE REPORT ID                          
         BE    MISSFLD                                                          
***************                                                                 
* VALIDATE THE MEDIA                                                            
***************                                                                 
VKMED00  DS    0H                                                               
         LA    R2,MOVMEDH                                                       
         TM    4(R2),X'20'         IF THIS KEY FIELD CHANGED                    
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   THEN INDICATE IT                             
*                                                                               
         CLI   5(R2),0             LENGTH OF INPUT = 0?                         
         BNE   VKMED10                                                          
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'GETF',(R2),,GLVSPMD                                 
         CLI   8(R1),0                                                          
         BE    VKMED10                                                          
         B     NEEDFLDS                                                         
*                                                                               
VKMED10  GOTO1 VALIMED             VALIDATE MEDIA                               
*                                                                               
VKMEDX   OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'         TRANSMIT                                     
***************                                                                 
* VALIDATE THE BUYER                                                            
***************                                                                 
VKBUYR00 DS    0H                                                               
         LA    R2,MOVFBYRH         BUYER                                        
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   NO, KEY CHANGED                              
*                                                                               
         XC    MOVFBYN,MOVFBYN                                                  
         OI    MOVFBYNH+6,X'80'                                                 
*                                                                               
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         GOTO1 VALIBUYR,DMCB,8(R2)  YES, VALIDATE IT                            
         BNE   INVLFLD                                                          
*                                                                               
         OC    QBUYER,SPACES                                                    
         MVC   MOVFBYN,QBUYER                                                   
         OI    MOVFBYNH+6,X'80'                                                 
*                                                                               
         MVC   FROMBYR,MOVFBYR                                                  
         OC    FROMBYR,SPACES                                                   
         MVC   FRBYRNME,QBUYER                                                  
*                                                                               
VKBUYRX  OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'         TRANSMIT                                     
***************                                                                 
* VALIDATE THE CLIENT                                                           
***************                                                                 
VKCLT00  DS    0H                                                               
         XC    BCLT,BCLT                                                        
         LA    R2,MOVCLTH          CLIENT                                       
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   NO, KEY CHANGED                              
*&&DO                                                                           
         CLC   =C'NE',AGENCY                                                    
         BNE   VKCLT10                                                          
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*&&                                                                             
VKCLT10  NI    FLTRFLG1,X'FF'-(FF1CLT+FF1CLST)                                  
         CLI   5(R2),0             ANY CLIENT FILTER?                           
         BE    VKCLTX                                                           
*                                                                               
         BAS   RE,CLTOFC           BY CLIENT OFFICE?                            
         BE    VKCLTX              YES - DONE                                   
         BAS   RE,CLTGRP           BY CLIENT GROUP?                             
         BE    VKCLTX              YES - DONE                                   
*                                                                               
         GOTO1 VALICLT                                                          
         OI    FLTRFLG1,FF1CLT                                                  
*                                                                               
VKCLTX   OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'         TRANSMIT                                     
***************                                                                 
* VALIDATE THE MARKET(S)                                                        
***************                                                                 
VKMKT00  DS    0H                                                               
         XC    SVMKT1(L'SVMKT1*MAXMKTS),SVMKT1                                  
         LA    R2,MOVMKT1H                                                      
         LA    R3,SVMKT1                                                        
         NI    FLTRFLG1,X'FF'-FF1MKT                                            
*&&DO                                                                           
         CLC   =C'NE',AGENCY                                                    
         BNE   VKMKT10                                                          
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
*&&                                                                             
VKMKT10  TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   NO, KEY CHANGED                              
*                                                                               
         CLI   5(R2),0             ANY MARKET HERE?                             
         BNE   VKMKT15             NO, CHECK THE NEXT ONE                       
         XC    0(L'SVMKT1,R3),0(R3)                                             
         B     VKMKT20                                                          
*                                                                               
VKMKT15  TM    4(R2),X'08'         NUMERIC?                                     
         BZ    INVLFLD                                                          
         LLC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R0,DUB                                                           
         EDIT  (R0),(4,8(R2)),FILL=0                                            
         MVI   5(R2),4                                                          
         MVC   0(L'SVMKT1,R3),8(R2)                                             
*                                                                               
         OI    FLTRFLG1,FF1MKT                                                  
         LA    R3,L'SVMKT1(R3)                                                  
*                                                                               
VKMKT20  OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R0,MOVMKT3H                                                      
         CR    R2,R0                                                            
         BNH   VKMKT10                                                          
*                                                                               
VKMKTX   DS    0H                                                               
***************                                                                 
* VALIDATE THE DARE PERIOD                                                      
***************                                                                 
VKPER00  XC    JSTDATE,JSTDATE                                                  
         XC    JNDDATE,JNDDATE                                                  
         LA    R2,MOVPERDH                                                      
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   NO, KEY CHANGED                              
*                                                                               
*&&DO                                                                           
         NI    FLTRFLG1,X'FF'-FF1EST                                            
         CLC   =C'NE',AGENCY                                                    
         BNE   VKPER50                                                          
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK),0                                    
         CLI   DMCB+4,1                                                         
         BH    INVLFLD                                                          
         CLC   =C'EST ',BLOCK+12                                                
         BNE   INVLFLD                                                          
         CLI   BLOCK+1,0                                                        
         BE    INVLFLD                                                          
         TM    BLOCK+3,X'80'       AT LEAST NUMERIC                             
         BZ    INVLFLD                                                          
         ICM   RF,15,BLOCK+8                                                    
         CHI   RF,255                                                           
         BH    INVLFLD                                                          
         LTR   RF,RF                                                            
         BZ    INVLFLD                                                          
         STC   RF,FILTREST                                                      
         OI    FLTRFLG1,FF1EST     FILTERING ON ESTIMATE                        
         B     VKPERX                                                           
*&&                                                                             
VKPER50  NI    FLTRFLG1,X'FF'-FF1DPERS-FF1DPERI                                 
         CLI   5(R2),0             ANY DARE PERIOD FILTER?                      
         BE    VKPERX              NONE                                         
         CLI   5(R2),2             DON'T ACCEPT 1 CHARACTER                     
         BNH   INVLFLD             INVALID                                      
*                                                                               
         GOTO1 PERVAL,DMCB,(5(R2),8(R2)),PERVALST                               
         TM    4(R1),X'03'         ANY INVALID DATES?                           
         BNZ   INVLFLD             YES                                          
         TM    4(R1),X'04'         ONLY 1 INPUT DATE?                           
         BO    INVLFLD                                                          
*                                                                               
         LA    R2,MOVWITHH                                                      
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         CLI   8(R2),C'Y'                                                       
         BNE   VKPER55                                                          
         OI    FLTRFLG1,FF1DPERI                                                
         B     VKPERX2                                                          
VKPER55  CLI   8(R2),C'N'                                                       
         BNE   INVLFLD                                                          
         OI    FLTRFLG1,FF1DPERS                                                
         B     VKPERX2                                                          
*                                                                               
VKPERX   XC    MOVWITH,MOVWITH                                                  
VKPERX2  OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'         TRANSMIT                                     
         OI    MOVPERDH+4,X'20'                                                 
         OI    MOVPERDH+6,X'80'    TRANSMIT                                     
***************                                                                 
* VALIDATE THE TO BUYER                                                         
***************                                                                 
VKBYR00  DS    0H                                                               
         LA    R2,MOVTBYRH                                                      
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         GOTO1 VALIBUYR,DMCB,8(R2)                                              
         BNE   INVLFLD                                                          
*                                                                               
         OC    QBUYER,SPACES                                                    
         MVC   MOVTBYN,QBUYER                                                   
         OI    MOVTBYNH+6,X'80'                                                 
         MVC   TOBYR,8(R2)                                                      
         OC    TOBYR,SPACES                                                     
         MVC   TOBYRNME,QBUYER                                                  
*                                                                               
         CLC   TOBYR,FROMBYR       CAN'T COPY TO SAME BUYER - DUH               
         BE    INVLFLD                                                          
*                                                                               
VKBYRX   DS    0H                                                               
***************                                                                 
* VALIDATE OPTIONS - TEST RUN - Y/N                                             
***************                                                                 
VKTST00  DS    0H                                                               
         LA    R2,MOVTESTH                                                      
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         CLI   MOVTEST,C'Y'                                                     
         BE    VKTSTX                                                           
         CLI   MOVTEST,C'N'                                                     
         BNE   INVLFLD                                                          
VKTSTX   DS    0H                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD                                                               
***********************************************************************         
PR       DS    0H                                                               
*                                                                               
         XC    AESTTAB,AESTTAB                                                  
*                                                                               
         LA    R1,HDSPECS                                                       
         ST    R1,SPECS                                                         
         LA    R1,HDHOOK                                                        
         ST    R1,HEADHOOK                                                      
         ZAP   ORDCOUNT,=P'0'                                                   
***************                                                                 
* LET'S DO THE MOVE  --  BY BUYER OR BUYER/CLT-USE PASSIVE BY BUYER             
***************                                                                 
         TM    FLTRFLG1,FF1MKT     ANY MKT FILTER                               
         BO    PR200                                                            
         USING DOKEY,R4                                                         
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   DBKTYPE,DBKTYPQ     X'0D'                                        
         MVI   DBKSUBTY,DBKSTYPQ   X'B4'                                        
         MVC   DBKAGMD,BAGYMD                                                   
         MVC   DBKBYR,FROMBYR                                                   
         MVC   SAVEKEY,KEY                                                      
         GOTO1 HIGH                                                             
PR20     CLC   KEY(DBKORD-DOKEY),SAVEKEY   SAME THRU BUYER                      
         BNE   PRX                                                              
         MVC   SAVEKEY,KEY                                                      
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO                                                           
         TM    15(R4),X'80'        HAS THIS RECORD BEEN DELETED?                
         BO    PR30                YES, SKIP IT                                 
         LA    R4,24(R4)                                                        
         USING DOIDELD,R4          ID ELEM                                      
*                                                                               
         TM    FLTRFLG1,FF1CLST    CLIENT LIST FILTER?                          
         BNO   PR25                NO                                           
*                                                                               
         L     RE,ATIA             CLIENT TABLE                                 
*                                                                               
PR21     CLC   =X'FFFF',0(RE)      END OF TABLE?                                
         BE    PR30                YES - NO MATCH                               
         CLC   DOIDCLT,0(RE)       MATCH ON CLIENT?                             
         BE    PR26                YES                                          
         LA    RE,5(RE)            BUMP TO NEXT CLIENT ENTRY                    
         B     PR21                CHECK NEXT ENTRY                             
*                                                                               
PR25     TM    FLTRFLG1,FF1CLT     CLT FILTER                                   
         BNO   PR26                                                             
         CLC   DOIDCLT,BCLT        MATCH ON CLIENT FILTER                       
         BNE   PR30                                                             
*                                                                               
PR26     TM    FLTRFLG1,FF1EST     EST FILTER                                   
         BNO   PR28                                                             
         CLC   DOIDEST,FILTREST    MATCH ON ESTIMATE FILTER                     
         BNE   PR30                                                             
*                                                                               
PR28     TM    FLTRFLG1,FF1DPERS+FF1DPERI  ANY DATE FILTER                      
         BZ    PR29                                                             
         BAS   RE,CHKEDATE                                                      
         BNE   PR30                                                             
PR29     BAS   RE,MOVEORD          MOVE THE ORDER                               
*                                                                               
PR30     MVC   KEY,SAVEKEY                                                      
         OI    DMINBTS,X'08'       READ FOR DELETES                             
         GOTO1 HIGH                RESTORE SEQUENCE(EVEN IF DELETED)            
PR32     GOTO1 SEQ                                                              
         TM    KEY+13,X'80'        DELETED                                      
         BO    PR32                GO UNTIL NOT DELETED                         
         NI    DMINBTS,X'FF'-X'08' TURN OFF READ FOR DELETES                    
         B     PR20                                                             
*                                                                               
***************                                                                 
* LET'S DO THE MOVE  --  BY MARKET OR MKT/CLT - USE PASSIVE BY STATION          
***************                                                                 
PR200    TM    FLTRFLG1,FF1MKT     MKT FILTER                                   
         BNO   PRX                                                              
         XC    LASTSTA,LASTSTA                                                  
         USING DOKEY,R4                                                         
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   DSKTYPE,DSKTYPQ     X'0D'                                        
         MVI   DSKSUBTY,DSKSTYPQ   X'B7'                                        
         MVC   DSKAGMD,BAGYMD                                                   
         MVC   DSKBYR,FROMBYR                                                   
         MVC   SAVEKEY,KEY                                                      
         GOTO1 HIGH                                                             
PR220    CLC   KEY(DSKSTA-DOKEY),SAVEKEY   SAME THRU BUYER                      
         BNE   PRX                                                              
         MVC   SAVEKEY,KEY                                                      
         LA    R4,KEY                                                           
         CLC   LASTSTA,DSKSTA      SAME STATION AS LAST REC?                    
         BE    PR230                                                            
         MVC   BSTA,DSKSTA                                                      
         MVC   LASTSTA,DSKSTA                                                   
         BAS   RE,GETSTAS          GET LIST OF ALL STATION RECS                 
         MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                RESTORE SEQUENCE                             
*                                                                               
PR230    MVC   SAVEKEY,KEY                                                      
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         TM    15(R4),X'80'        HAS THIS RECORD BEEN DELETED?                
         BO    PR250               YES, SKIP IT                                 
         LA    R4,24(R4)                                                        
         USING DOIDELD,R4          ID ELEM                                      
         MVC   BCHKCLT,DOIDCLT                                                  
*                                                                               
         TM    FLTRFLG1,FF1CLST    CLIENT LIST FILTER?                          
         BNO   PR235               NO                                           
*                                                                               
         L     RE,ATIA             CLIENT TABLE                                 
*                                                                               
PR231    CLC   =X'FFFF',0(RE)      END OF TABLE?                                
         BE    PR250               YES - NO MATCH                               
         CLC   BCHKCLT,0(RE)       MATCH ON CLIENT?                             
         BE    PR240               YES                                          
         LA    RE,5(RE)            BUMP TO NEXT CLIENT ENTRY                    
         B     PR231               CHECK NEXT ENTRY                             
*                                                                               
PR235    TM    FLTRFLG1,FF1CLT     CLT FILTER                                   
         BNO   PR240                                                            
         CLC   BCHKCLT,BCLT        MATCH ON CLIENT FILTER                       
         BNE   PR250                                                            
*                                                                               
PR240    TM    FLTRFLG1,FF1EST     EST FILTER                                   
         BNO   PR245                                                            
         CLC   DOIDEST,FILTREST    MATCH ON ESTIMATE FILTER                     
         BNE   PR250                                                            
*                                                                               
PR245    BAS   RE,CHKSTA           IS STATION/CLT IN MKT TO BE MOVED            
         BNE   PR250                                                            
         TM    FLTRFLG1,FF1DPERS+FF1DPERI   ANY DATE FILTER                     
         BZ    *+12                                                             
         BAS   RE,CHKEDATE                                                      
         BNE   PR250                                                            
*                                                                               
         BAS   RE,MOVEORD          MOVE THE ORDER                               
*                                                                               
PR250    MVC   KEY,SAVEKEY                                                      
         OI    DMINBTS,X'08'       READ FOR DELETES                             
         GOTO1 HIGH                RESTORE SEQUENCE(EVEN IF DELETED)            
PR252    GOTO1 SEQ                                                              
         TM    KEY+13,X'80'        DELETED                                      
         BO    PR252               GO UNTIL NOT DELETED                         
         NI    DMINBTS,X'FF'-X'08' TURN OFF READ FOR DELETES                    
         B     PR220                                                            
*                                                                               
PRX      GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P+1(14),=C'ORDERS MOVED ='                                       
         EDIT  (P8,ORDCOUNT),(10,P+16),ALIGN=LEFT                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         OC    AESTTAB,AESTTAB                                                  
         BZ    XIT                                                              
*                                                                               
         L     R0,=A(ESTLEN)                                                    
         L     R1,AESTTAB                                                       
         FREEMAIN RC,A=(1),LV=(0)                                               
*                                                                               
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* VALIBUYR                                                                      
*        THIS ROUTINE VALIDATES THE BUYER AND RETURNS THE OFFICE CODE.          
*                                                                               
* CAUTION:     SVKEY GETS CLOBBERED.                                            
*                                                                               
* ON ENTRY:    P1                   A(3 BYTE CHARACTER BUYER)                   
*                                                                               
* ON EXIT:     QBUYER               BUYER'S FULL NAME                           
*              QOFFICE              OFFICE CODE OF THE BUYER                    
*              QPHONE               PHONE NUMBER OF THE BUYER                   
*              QPHONEXT             PHONE EXTENSION                             
***********************************************************************         
VALIBUYR NTR1                                                                   
         NI    OMBASFL2,X'FF'-OM2DSTOV   RESET BUYER DEACTIVATED FLAG           
         MVC   SVKEY,KEY           SAVE KEY USED                                
*                                                                               
         L     R2,0(R1)                                                         
         OC    0(3,R2),SPACES                                                   
*****                                                                           
         L     R1,SYSPARMS                                                      
         L     R1,8(R1)                                                         
         XC    DMCB,DMCB                                                        
         USING COMFACSD,R1                                                      
         L     RF,CCALLOV                                                       
         DROP  R1                                                               
         LA    R1,DMCB                                                          
         MVC   DMCB+4(4),=X'D9000ABC'                                           
         GOTO1 (RF),(R1),0                                                      
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'P',(R2)),(X'80',KEY)                                
*****                                                                           
***      GOTO1 RCPACK,DMCB,(C'P',(R2)),(X'80',KEY)                              
         BNE   VBYR10              INVALID BUYER                                
*                                                                               
         LA    R4,KEY              SET UP THE KEY                               
         USING BYRRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVI   BYRKTYP,BYRKTYPQ                                                 
         MVI   BYRKSUB,BYRKSUBQ                                                 
         MVC   BYRKAM,BAGYMD                                                    
         MVC   BYRKBYR,0(R2)                                                    
         OC    BYRKBYR,SPACES                                                   
***      MVC   QBYR,BYRKBYR                                                     
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(L'BYRKEY),KEYSAVE                                            
         BE    *+14                                                             
VBYR10   MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY BEING USED                       
         B     NO                  RETURN 'NO' TO CALLER                        
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
***  CHECK TO SEE IF THE BUYER IS DEACTIVATED                                   
         L     R6,AIO                                                           
         MVI   ELCODE,BYRDCD2Q                                                  
         BAS   RE,GETEL                                                         
         BNE   VBYR20                                                           
*                                                                               
         USING BYRDSCD2,R6                                                      
         TM    BYRMFLG1,BYRMFDAC   IS THIS DEACTIVATED?                         
         BZ    VBYR20               - NOPE                                      
         OI    OMBASFL2,OM2DACT     - TURN ON THE FLAG                          
*                                                                               
VBYR20   L     R6,AIO                                                           
         MVI   ELCODE,BYRDCDQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING BYRDSCD,R6                                                       
         MVC   QBUYER,BYRFNAME     GET BUYER'S FULL NAME                        
         MVC   QOFFICE,BYROFFID        OFFICE ID                                
         MVC   QPHONE,BYRPHONE         PHONE NUMBER                             
         MVC   QPHONEXT,BYRPHEXT       AND PHONE EXTENSION                      
*                                                                               
         XC    KEY,KEY             RESTORE KEY BEING USED                       
         MVC   KEY(L'SVKEY),SVKEY                                               
*                                                                               
         B     YES                 RETURN 'YES' TO CALLER                       
         EJECT                                                                  
***********************************************************************         
* BUILDS TABLE OF ALL RECS FOR A STATION                                        
*                                                                               
* ON ENTRY:    BSTA                STATION                                      
* ON EXIT:     STATABLE            LIST OF STATIONS/CLT/STATUS                  
* WARNING:     AIO2 WILL BE CLOBBERED                                           
***********************************************************************         
GETSTAS  NTR1                                                                   
         XC    MKTSTAT,MKTSTAT                                                  
         MVC   STATION,BSTA        BINARY STATION WE'RE LOOKING FOR             
         GOTO1 MSUNPK,DMCB,MKTSTAT,WORK,WORK+4                                  
         CLI   WORK+8,C' '                                                      
         BNE   *+10                                                             
         MVC   WORK+8(1),QMED                                                   
*                                                                               
         USING STABLED,R2                                                       
         LA    R2,STATABLE         TABLE OF STATION/CLT/STATUS                  
*                                                                               
         LA    R4,KEY                                                           
         USING STARECD,R4                                                       
         XC    KEY,KEY             BUILD KEY                                    
         MVI   STAKTYPE,C'S'                                                    
         MVC   STAKMED,QMED        MEDIA                                        
         MVC   STAKCALL,WORK+4     STATION CALL LETTERS                         
         MVC   STAKAGY,AGENCY                                                   
         MVC   SVSTAKEY,KEY                                                     
         DROP  R4                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'STATION',(R4),AIO2                
         B     GSTA12                                                           
GSTA10   GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'STATION',(R4),AIO2                
GSTA12   CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO2             STILL ONE OF OUR RECORDS?                    
         USING STAREC,R6                                                        
         CLC   STAKEY(STAKCLT-STAKEY),SVSTAKEY   SAME UP TO CLT                 
         BNE   GSTAX               NO                                           
*                                                                               
         XC    MKTSTAT,MKTSTAT                                                  
         XC    WORK,WORK                                                        
         MVC   WORK(L'STAKCALL),STAKCALL                                        
         GOTO1 MSPACK,DMCB,SMKT,WORK,MKTSTAT                                    
*                                                                               
         CLC   STAKCLT,=C'000'     DEFAULT STA REC (NO EXCEPTION)               
         BE    GSTA15                                                           
*                                                                               
         TM    FLTRFLG1,FF1CLST    CLIENT LIST FILTER?                          
         BNO   GSTA13              NO                                           
*                                                                               
         L     RE,ATIA             CLIENT TABLE                                 
*                                                                               
GSTA12A  CLC   =X'FFFF',0(RE)      END OF TABLE?                                
         BE    GSTA10              YES - NO MATCH                               
         CLC   STAKCLT,2(RE)       MATCH ON CLIENT?                             
         BE    GSTA15              YES                                          
         LA    RE,5(RE)            BUMP TO NEXT CLIENT ENTRY                    
         B     GSTA12A             CHECK NEXT ENTRY                             
*                                                                               
GSTA13   TM    FLTRFLG1,FF1CLT     ARE WE FILTERING BY CLIENT?                  
         BZ    GSTA15                                                           
         CLC   STAKCLT,QCLT        YES, ACCEPT FOR THIS QCLT AND C'000'         
         BNE   GSTA10                                                           
*                                                                               
GSTA15   MVC   STSTATN,STATION     BINARY STATION                               
         MVC   STCLT,STAKCLT       CLIENT                                       
         MVI   STSTATUS,0          CLEAR STATUS                                 
*                                                                               
         LA    R3,SVMKT1           CHECK IF MATCHES ANY MARKETS                 
         LA    R1,MAXMKTS          NUMBER OF MARKETS                            
GSTA20   OC    0(L'SVMKT1,R3),0(R3)                                             
         BZ    GSTA25                                                           
         CLC   SMKT,0(R3)                                                       
         BE    GSTA30                                                           
GSTA25   LA    R3,L'SVMKT1(R3)                                                  
         BCT   R1,GSTA20                                                        
         OI    STSTATUS,STSTNOT    STATION/CLT NOT IN ANY REQUESTED MKT         
*                                                                               
GSTA30   LA    R2,STABLNQ(R2)                                                   
         LA    R1,STATABND         OUT OF ROOM?                                 
         CR    R2,R1                                                            
         BL    GSTA10                                                           
         DC    H'0'                STATABLE IS FULL                             
*                                                                               
GSTAX    MVC   0(3,R2),=3X'FF'     EOT                                          
         B     XIT                                                              
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* CHECK IF STATION IS IN A MARKET TO BE MOVED                                   
* ON ENTRY:    BSTA                STATION                                      
*              BCHKCLT             BINARY CLIENT                                
* ON EXIT:     NEQ/EQ              DO NOT MOVE ORDER/MOVE ORDER                 
***********************************************************************         
CHKSTA   NTR1                                                                   
         GOTO1 CLUNPK,DMCB,BCHKCLT,CHKCLT                                       
         USING STABLED,R2                                                       
         LA    R2,STATABLE         TABLE OF STATION/CLT/STATUS                  
         MVI   MKTMATCH,0                                                       
         CLC   BSTA,STSTATN                                                     
         BE    *+6                                                              
         DC    H'0'                WRONG                                        
         USING DOKEY,R6                                                         
         L     R6,AIO                                                           
         CLC   DOKSTA,STSTATN                                                   
         BE    *+6                                                              
         DC    H'0'                WRONG                                        
*                                                                               
CSTA10   CLC   CHKCLT,STCLT        ANY CLIENT OVERRIDE                          
         BNE   CSTA30                                                           
         MVI   MKTMATCH,C'Y'                                                    
         TM    STSTATUS,STSTNOT    DOES DEFAULT STATION REC MATCH MKT           
         BNO   *+8                                                              
         MVI   MKTMATCH,C'N'                                                    
         B     CSTAX                                                            
*                                                                               
CSTA30   CLC   STCLT,=C'000'       DEFAULT WILL BE LAST REC                     
         BNE   CSTA40              SO HAVEN'T FOUND CLT -                       
         MVI   MKTMATCH,C'Y'                                                    
         TM    STSTATUS,STSTNOT    DOES DEFAULT STATION REC MATCH MKT           
         BNO   *+8                                                              
         MVI   MKTMATCH,C'N'                                                    
         B     CSTAX                                                            
*                                                                               
CSTA40   LA    R2,STABLNQ(R2)                                                   
         CLC   0(3,R2),=3X'FF'                                                  
         BNE   CSTA10                                                           
*                                                                               
CSTAX    CLI   MKTMATCH,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   MKTMATCH,C'Y'                                                    
         BE    YES                                                              
         B     NO                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK IF ORDER IS WITHIN REQUESTED PERIOD                                     
* ON ENTRY:    AIO                 ORDER REC                                    
*              JSTDATE-JNDDATE     JULIAN PWOS PERIOD                           
* ON EXIT:     NEQ/EQ              DO NOT MOVE ORDER/MOVE ORDER                 
***********************************************************************         
CHKDATE  NTR1                                                                   
         USING DOSTELD,R6                                                       
         L     R6,AIO                                                           
         LA    R6,24(R6)                                                        
CDATE10  CLI   0(R6),0                                                          
         BE    NO                                                               
         CLI   0(R6),DOSTELQ       X'12' ELEM                                   
         BNE   CDATE20                                                          
*                                                                               
         CLI   DOSTSTAT,DSENT      AM I SENT?                                   
         BE    CDATE30                                                          
         CLI   DOSTSTAT,DFXSENT    AM I FAX SENT?                               
         BE    CDATE30                                                          
         CLI   DOSTSTAT,DEMSENT    AM I EMAIL SENT?                             
         BE    CDATE30                                                          
CDATE20  LLC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     CDATE10                                                          
*                                                                               
CDATE30  CLC   DOSTDATE,JSTDATE    ORDER >= START OF PERIOD                     
         BL    NO                                                               
         CLC   DOSTDATE,JNDDATE    ORDER <= END OF PERIOD                       
         BH    NO                                                               
         B     YES                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK IF ESTIMATE IS WITHIN REQUESTED PERIOD                                  
* ON ENTRY:    AIO                 ORDER REC                                    
*              JSTDATE-JNDDATE     JULIAN PWOS PERIOD                           
* ON EXIT:     NEQ/EQ              DO NOT MOVE ORDER/MOVE ORDER                 
***********************************************************************         
CHKEDATE NTR1                                                                   
*                                                                               
         L     R6,AIO                                                           
         LA    R6,24(R6)                                                        
         USING DOIDELD,R6                                                       
         LA    R3,WORK                                                          
         USING ESTD,R3                                                          
         XC    WORK,WORK                                                        
         MVC   EDCLT,DOIDCLT                                                    
         MVC   EDEST,DOIDEST                                                    
*                                                                               
         OC    AESTTAB,AESTTAB     DID GET STORAGE YET?                         
         BNZ   CED20               YES                                          
*                                                                               
         L     R0,=A(ESTLEN)                                                    
         GETMAIN RU,LV=(0),LOC=(ANY,ANY)                                        
         LTR   RF,RF               ANY ERRORS?                                  
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,AESTTAB                                                       
         B     CED30                                                            
*                                                                               
CED20    GOTO1 ,ESTPAR1,WORK,AESTTAB                                            
         MVI   ESTPAR4,0           FIND A RECORD                                
         L     RF,=V(BINSRCH)                                                   
         O     RF,=X'80000000'     SWITCH TO 31 BIT MODE                        
         BASSM RE,RF                                                            
*                                                                               
         TM    0(R1),X'80'         TEST RECORD NOT FOUND                        
         BZ    CED40               WE FOUND THE RECORD                          
*                                                                               
CED30    XC    KEY,KEY             ADD RECORD TO BINSRCH TABLE                  
         LA    R4,KEY                                                           
         USING ESTHDRD,R4                                                       
         MVI   EKEYTYPE,EKEYTYPQ   X'00'                                        
         MVC   EKEYAM,BAGYMD       AGY/MD                                       
         MVC   EKEYCLT,DOIDCLT     CLIENT                                       
         MVC   EKEYPRD,=C'POL'     POL PRODUCT                                  
         MVC   EKEYEST,DOIDEST     ESTIMATE                                     
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   EKEY,KEYSAVE                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         L     R4,AIO3                                                          
         MVC   EDESTART,ESTART                                                  
         MVC   EDEEND,EEND                                                      
*                                                                               
         GOTO1 ,ESTPAR1,WORK,AESTTAB                                            
         MVI   ESTPAR4,1           INSERT IF NOT FOUND                          
         L     RF,=V(BINSRCH)                                                   
         O     RF,=X'80000000'     SWITCH TO 31 BIT MODE                        
         BASSM RE,RF                                                            
*                                                                               
         OC    0(4,R1),0(R1)       TEST FULL                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
CED40    LA    RF,*+10             DON'T KNOW HOW WE GOT OUT OF 31-BIT          
         O     RF,=X'80000000'                                                  
         BSM   0,RF                GET BACK INTO 31-BIT MODE                    
*                                                                               
         L     R2,0(R1)                                                         
         MVC   WORK(EDRECLEN),0(R2)                                             
*                                                                               
         LA    RE,*+6              SWITCH BACK TO 24 BIT MODE                   
         BSM   0,RE                                                             
*                                                                               
         LA    R2,PERVALST                                                      
         USING PERVALD,R2                                                       
         TM    FLTRFLG1,FF1DPERI   WITHIN FLIGHT == Y?                          
         BO    CED50               YES                                          
*                                                                               
         CLC   EDESTART,PVALESTA   EST START DATE >= START OF PERIOD            
         BL    NO                                                               
         CLC   EDEEND,PVALEEND     EST END DATE <= END OF PERIOD                
         BH    NO                                                               
         B     YES                                                              
*                                                                               
CED50    CLC   EDESTART,PVALEEND   EST START DATE >= END OF PERIOD              
         BH    NO                                                               
         CLC   EDEEND,PVALESTA     EST END DATE <= START OF PERIOD              
         BL    NO                                                               
         B     YES                                                              
                                                                                
         DROP  R4,R3,R2                                                         
*                                                                               
         DC    C'ESTPAR'                                                        
ESTPAR1  DC    A(0)                                                             
ESTPAR2  DC    A(0)                A(TABLE)                                     
ESTPAR3  DC    F'0'                RECORD COUNT                                 
ESTPAR4  DC    A(EDRECLEN)         RECORD LENGTH                                
ESTPAR5  DC    A(EDKEYLEN)         KEYDSPL/KEYLEN                               
ESTPAR6  DC    A(ESTMAX)           MAX NUMBER OF RECORDS                        
*                                                                               
***********************************************************************         
* MOVE ORDER AND ALL ASSOCIATED POINTERS AND NOTICE RECS                        
* ON ENTRY:    ORDER RECORD IN AIO                                              
* ON EXIT:     ORDER MOVED TO NEW BUYER                                         
***********************************************************************         
MOVEORD  NTR1                                                                   
***************                                                                 
* CHANGE ORDER REC AND DEL/CHANGE ALL PASSIVES                                  
***************                                                                 
         L     R6,AIO                                                           
         MVI   ELCODE,DOIDELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING DOIDELD,R6                                                       
         MVC   SVCLT,DOIDCLT                                                    
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DOSPELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING DOSPELD,R6                                                       
         MVC   SVMKT,DOSPMKT                                                    
*                                                                               
         XC    SVCLRDAT,SVCLRDAT   CLEAR COLOR AND DATE FIELD                   
         L     R6,AIO                                                           
         MVI   ELCODE,COLELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   MO05                                                             
         USING COLOREL,R6                                                       
         MVC   SVCOLCOL,COLCOL     SAVE THIS FOR LATER                          
         MVC   SVCOLDAT,COLDATE    SAVE THIS FOR LATER                          
         DROP  R6                                                               
*                                                                               
MO05     L     R6,AIO                                                           
         MVC   SVDOKEY,0(R6)       SAVE ACTIVE KEY                              
         L     R6,AIO                                                           
         LA    R6,24(R6)                                                        
         USING DOIDELD,R6                                                       
         MVC   DOIDBYR,TOBYR                                                    
         CLI   MOVTEST,C'Y'                                                     
         BE    MO10                                                             
         GOTO1 PUTREC              CHANGE ORDER REC                             
*                                                                               
* DEL 'FROM' BUYER PASSIVE POINTER AND ADD 'TO' BUYER PASSIVE                   
*                                                                               
MO10     OC    SVCLRDAT,SVCLRDAT   ANY 'FROM' COLOR OR COLOR DATE               
         BZ    MO20                NONE                                         
*                                                                               
         LA    R4,KEY                                                           
KY       USING DAREORDD,R4                                                      
         LA    R6,SVDOKEY          USE SAVE ACT KEY (AIO GETS TRASHED)          
FR       USING DAREORDD,R6                                                      
         XC    KEY,KEY                                                          
         MVI   KY.DBKTYPE,DBKTYPQ                                               
         MVI   KY.DBKSUBTY,DBKSTYPQ                                             
         MVC   KY.DBKAGMD,FR.DOKAGMD                                            
         MVC   KY.DBKORD,FR.DOKORDER                                            
         MVC   KY.DBKSTA,FR.DOKSTA                                              
         MVC   KY.DBKBYR,FROMBYR                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'DOKEY),KEYSAVE                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    KEY+13,X'80'        DELETE KEY                                   
         CLI   MOVTEST,C'Y'                                                     
         BE    MO20                                                             
         GOTO1 WRITE                                                            
         DROP  FR                                                               
         MVC   KY.DBKBYR,TOBYR                                                  
*****                                                                           
         MVC   FULL,DMDSKADD       COPY DISK ADDRESS                            
         OI    DMINBTS,X'08'       READ FOR DELETED                             
         GOTO1 HIGH                                                             
         MVC   KEY+14(4),FULL      DISK ADDRESS                                 
         MVC   KEY+13(1),SVDOKEY+DORSTAT-DOKEY                                  
         CLC   KEY(L'DOKEY),KEYSAVE     SAME KEY, BUT DELETED?                  
         BE    MO15                                                             
         MVC   KEY(L'DOKEY),KEYSAVE                                             
         GOTO1 ADD                                                              
         B     MO20                                                             
*                                                                               
MO15     GOTO1 WRITE                                                            
*                                                                               
* DEL FROM BUYER STATION PASSIVE POINTER AND ADD TO BUYER                       
*                                                                               
MO20     LA    R6,SVDOKEY          USE SAVE ACT KEY (AIO GETS TRASHED)          
FR       USING DAREORDD,R6                                                      
         LA    R4,KEY                                                           
KY       USING DAREORDD,R4                                                      
         XC    KEY,KEY                                                          
         MVI   KY.DSKTYPE,DSKTYPQ                                               
         MVI   KY.DSKSUBTY,DSKSTYPQ                                             
         MVC   KY.DSKAGMD,FR.DOKAGMD                                            
         MVC   KY.DSKSTA,FR.DOKSTA                                              
         MVC   KY.DSKORD,FR.DOKORDER                                            
         MVC   KY.DSKBYR,FROMBYR                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'DOKEY),KEYSAVE                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    KEY+13,X'80'        DELETE KEY                                   
         CLI   MOVTEST,C'Y'                                                     
         BE    MO30                                                             
         GOTO1 WRITE                                                            
         DROP  FR                                                               
         MVC   KY.DSKBYR,TOBYR                                                  
*****                                                                           
         MVC   FULL,DMDSKADD       COPY DISK ADDRESS                            
         OI    DMINBTS,X'08'       READ FOR DELETED                             
         GOTO1 HIGH                                                             
         MVC   KEY+14(4),FULL      DISK ADDRESS                                 
         MVC   KEY+13(1),SVDOKEY+DORSTAT-DOKEY                                  
         CLC   KEY(L'DOKEY),KEYSAVE     SAME KEY, BUT DELETED?                  
         BE    MO25                                                             
         MVC   KEY(L'DOKEY),KEYSAVE                                             
         GOTO1 ADD                                                              
         B     MO30                                                             
*                                                                               
MO25     GOTO1 WRITE                                                            
*                                                                               
* DEL FROM BUYER COLOR PASSIVE POINTER AND ADD TO BUYER                         
*                                                                               
MO30     CLI   SVCOLCOL,0          DID WE HAVE A COLOR ELEMENT?                 
         BE    MO40                NO                                           
*                                                                               
         LA    R6,SVDOKEY          USE SAVE ACT KEY (AIO GETS TRASHED)          
FR       USING DAREORDD,R6                                                      
         LA    R4,KEY                                                           
KY       USING DAREORDD,R4                                                      
         XC    KEY,KEY                                                          
         MVI   KY.DSCKTYPE,DSCKTYPQ                                             
         MVI   KY.DSCKSTYP,DSCKSTYQ                                             
         MVC   KY.DSCKAGMD,FR.DOKAGMD                                           
         MVC   KY.DSCKBYR,FROMBYR                                               
         MVC   KY.DSCKSTAT,SVCOLCOL                                             
         MVC   KY.DSCKDATE,SVCOLDAT                                             
         MVC   KY.DSCKORDR,FR.DOKORDER                                          
         GOTO1 HIGH                                                             
         CLC   KEY(L'DOKEY),KEYSAVE  COLOR PASSIVE MIGHT NOT EXIST B/C          
         BNE   MO33                   OCT01/01 RULE OR BLACK                    
         OI    KEY+13,X'80'        DELETE KEY                                   
         CLI   MOVTEST,C'Y'                                                     
         BE    MO40                                                             
         GOTO1 WRITE                                                            
         DROP  FR                                                               
*                                                                               
MO33     DS    0H                                                               
         MVC   KY.DSCKBYR,TOBYR                                                 
*****                                                                           
         MVC   FULL,DMDSKADD       COPY DISK ADDRESS                            
         OI    DMINBTS,X'08'       READ FOR DELETED                             
         GOTO1 HIGH                                                             
         MVC   KEY+14(4),FULL      DISK ADDRESS                                 
         MVC   KEY+13(1),SVDOKEY+DORSTAT-DOKEY                                  
         CLC   KEY(L'DOKEY),KEYSAVE     SAME KEY, BUT DELETED?                  
         BE    MO35                                                             
         MVC   KEY(L'DOKEY),KEYSAVE                                             
         CLC   KEY+7(2),=X'34BE'                                                
         BH    MO40                                                             
         CLI   KEY+6,C'K'                                                       
         BNE   MO34                                                             
         CLC   KEY+7(2),TDYLSS2W                                                
         BH    MO40                                                             
MO34     GOTO1 ADD                                                              
         B     MO40                                                             
*                                                                               
MO35     GOTO1 WRITE                                                            
*                                                                               
* DEL FROM BUYER FROM BUYER/CLIENT/MARKET PASSIVE AND TO BUYER                  
*                                                                               
MO40     OC    SVMKT,SVMKT         DO WE HAVE MARKET IN THE RECORD?             
         BZ    MO60                                                             
*                                                                               
         LA    R6,SVDOKEY          USE SAVE ACT KEY (AIO GETS TRASHED)          
FR       USING DAREORDD,R6                                                      
         LA    R4,KEY                                                           
KY       USING DAREORDD,R4                                                      
         XC    KEY,KEY                                                          
         MVI   KY.DBCKTYPE,DBCKTYPQ   X'0D'                                     
         MVI   KY.DBCKSTYP,DBCKSTYQ   X'BE'                                     
         MVC   KY.DBCKAGMD,FR.DOKAGMD                                           
         MVC   DMCB+4(4),=X'D9000ABC'   RCPACK                                  
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         GOTO1 (RF),DMCB,(C'P',FROMBYR),KY.DBCKBYR                              
****     GOTO1 RCPACK,DMCB,(C'P',FROMBYR),KY.DBCKBYR                            
         MVC   KY.DBCKCLT,SVCLT                                                 
         MVC   KY.DBCKMKT,SVMKT                                                 
         MVC   KY.DBCKORDR,FR.DOKORDER                                          
         GOTO1 HIGH                                                             
         CLC   KEY(L'DOKEY),KEYSAVE                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    KEY+13,X'80'        DELETE KEY                                   
         CLI   MOVTEST,C'Y'                                                     
         BE    MO50                                                             
         GOTO1 WRITE                                                            
         DROP  FR                                                               
*                                                                               
         MVC   DMCB+4(4),=X'D9000ABC'   RCPACK                                  
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         GOTO1 (RF),DMCB,(C'P',TOBYR),KY.DBCKBYR                                
****     GOTO1 RCPACK,DMCB,(C'P',TOBYR),KY.DBCKBYR                              
*                                                                               
         MVC   FULL,DMDSKADD       COPY DISK ADDRESS                            
         OI    DMINBTS,X'08'       READ FOR DELETED                             
         GOTO1 HIGH                                                             
         MVC   KEY+14(4),FULL      DISK ADDRESS                                 
         MVC   KEY+13(1),SVDOKEY+DORSTAT-DOKEY                                  
         CLC   KEY(L'DOKEY),KEYSAVE     SAME KEY, BUT DELETED?                  
         BE    MO45                                                             
         MVC   KEY(L'DOKEY),KEYSAVE                                             
         GOTO1 ADD                                                              
         B     MO50                                                             
*                                                                               
MO45     GOTO1 WRITE                                                            
*                                                                               
* DEL FROM BUYER FROM BUYER/MARKET/CLIENT PASSIVE AND TO BUYER                  
*                                                                               
MO50     LA    R6,SVDOKEY          USE SAVE ACT KEY (AIO GETS TRASHED)          
FR       USING DAREORDD,R6                                                      
         LA    R4,KEY                                                           
KY       USING DAREORDD,R4                                                      
         XC    KEY,KEY                                                          
         MVI   KY.DBMKTYPE,DBMKTYPQ   X'0D'                                     
         MVI   KY.DBMKSTYP,DBMKSTYQ   X'BF'                                     
         MVC   KY.DBMKAGMD,FR.DOKAGMD                                           
         MVC   DMCB+4(4),=X'D9000ABC'   RCPACK                                  
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         GOTO1 (RF),DMCB,(C'P',FROMBYR),KY.DBMKBYR                              
****     GOTO1 RCPACK,DMCB,(C'P',FROMBYR),KY.DBMKBYR                            
         MVC   KY.DBMKCLT,SVCLT                                                 
         MVC   KY.DBMKMKT,SVMKT                                                 
         MVC   KY.DBMKORDR,FR.DOKORDER                                          
         GOTO1 HIGH                                                             
         CLC   KEY(L'DOKEY),KEYSAVE  COLOR PASSIVE MIGHT NOT EXIST B/C          
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    KEY+13,X'80'        DELETE KEY                                   
         CLI   MOVTEST,C'Y'                                                     
         BE    MO60                                                             
         GOTO1 WRITE                                                            
         DROP  FR                                                               
*                                                                               
         MVC   DMCB+4(4),=X'D9000ABC'   RCPACK                                  
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)                                                         
         GOTO1 (RF),DMCB,(C'P',TOBYR),KY.DBMKBYR                                
****     GOTO1 RCPACK,DMCB,(C'P',TOBYR),KY.DBMKBYR                              
         DROP  KY                                                               
*                                                                               
         MVC   FULL,DMDSKADD       COPY DISK ADDRESS                            
         OI    DMINBTS,X'08'       READ FOR DELETED                             
         GOTO1 HIGH                                                             
         MVC   KEY+14(4),FULL      DISK ADDRESS                                 
         MVC   KEY+13(1),SVDOKEY+DORSTAT-DOKEY                                  
         CLC   KEY(L'DOKEY),KEYSAVE     SAME KEY, BUT DELETED?                  
         BE    MO55                                                             
         MVC   KEY(L'DOKEY),KEYSAVE                                             
         GOTO1 ADD                                                              
         B     MO60                                                             
*                                                                               
MO55     GOTO1 WRITE                                                            
*                                                                               
MO60     BAS   RE,PRINTORD         PRINT ORDER                                  
         AP    ORDCOUNT,=P'1'                                                   
         EJECT                                                                  
***************                                                                 
* MOVE ANY DARE MAKEGOOD NOTICE RECS FOR THIS ORDER NUMBER                      
***************                                                                 
MO200    LA    R6,SVDOKEY                                                       
         USING DAREORDD,R6                                                      
         LA    R4,KEY                                                           
         USING DAREMGND,R4                                                      
         XC    KEY,KEY                                                          
         MVI   MNKTYPE,MNKTYPQ     X'0D'                                        
         MVI   MNKSUBTY,MNKSTYPQ   X'36'                                        
         MVC   MNKAGMD,DOKAGMD                                                  
         MVC   MNKBYR,FROMBYR                                                   
         MVC   MNKORDER,DOKORDER                                                
         MVC   SVMNKEY,KEY                                                      
         DROP  R4,R6                                                            
         GOTO1 HIGH                                                             
*                                                                               
MO210    CLC   KEY(MNKGROUP-MNKEY),KEYSAVE     SAME THRU ORDER NUM              
         BNE   MOX                                                              
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING DAREMGND,R6                                                      
         MVC   SVMNKEY,0(R6)                                                    
         MVC   SVMGSTAT,MNRSTAT    IN CASE IT WAS MARK FOR DELETION             
         OI    MNRSTAT,X'C0'       X'80' TO DELETE AND X'40' FOR MOVE           
         CLI   MOVTEST,C'Y'                                                     
         BE    MO250                                                            
         GOTO1 PUTREC              DELETE MG NOTICE REC                         
         OI    KEY+13,X'C0'        DELETE ACTIVE KEY                            
         GOTO1 WRITE                                                            
*                                                                               
* ADD NEW MAKEGOOD NOTICE RECORD WITH NEW BUYER                                 
*                                                                               
         L     R6,AIO                                                           
         MVC   MNRSTAT,SVMGSTAT                                                 
         MVC   MNKBYR,TOBYR                                                     
         MVC   KEY(L'MNKEY),0(R6)                                               
*****                                                                           
         OI    DMINBTS,X'08'       READ FOR DELETED                             
         GOTO1 HIGH                                                             
         CLC   KEY(L'MNKEY),KEYSAVE                                             
         BE    MO220                                                            
         MVC   KEY(L'MNKEY),0(R6)                                               
         MVC   KEY+13(1),SVMGSTAT  RESTORE PREVIOUS STATE                       
         GOTO1 ADDREC                                                           
         B     MO250                                                            
*                                                                               
MO220    MVC   KEY+13(1),SVMGSTAT     RESTORE PREVIOUS STATE                    
         GOTO1 WRITE                                                            
*                                                                               
         MVC   AIO,AIO3                                                         
         OI    DMINBTS,X'08'       READ DELETED RECORD                          
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         MVC   MNRSTAT,SVMGSTAT                                                 
         GOTO1 PUTREC                                                           
*****                                                                           
MO250    BAS   RE,PRINTMG          PRINT MAKEGOOD                               
*                                                                               
         MVC   KEY(L'SVMNKEY),SVMNKEY  RESTORE SEQ                              
         OI    DMINBTS,X'08'       READ REC WE JUST DELETED                     
         GOTO1 HIGH                RESTORE SEQUENCE EVEN IF DELETED             
         GOTO1 SEQ                 READ NEXT                                    
         B     MO210                                                            
*                                                                               
MOX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        PROCESS AND OFFICE LIST REQUEST                                        
***********************************************************************         
CLTOFC   NTR1                                                                   
         CLI   8(R2),C'$'          OFFICE LIST?                                 
         BNE   COFC00              NO                                           
         CLI   5(R2),2             INPUT LENGTH OF 2?                           
         BNE   NO                  NO - DONE                                    
         B     COFC01              YES - GO PROCESS                             
*                                                                               
COFC00   CLC   =C'O=',8(R2)        SINGLE OFFICE?                               
         BNE   NO                  NO                                           
         LA    R3,OFCBLK           OFFICER BLOCK                                
         USING OFFICED,R3          OFFICER DSECT                                
         XC    OFCBLK,OFCBLK       CLEAR OFFICER BLOCK                          
         MVI   OFCSYS,C'S'         SPOT                                         
         MVC   OFCAGY,AGENCY       AGENCY                                       
         MVC   OFCOFC2,10(R2)      1 OR 2 CHAR OFFICE                           
         GOTO1 OFFICER,DMCB,(C'2',OFFICED),(0,ACOMFACS)                         
         CLI   0(R1),0             ANY ERROR?                                   
         BNE   INVLFLD             YES                                          
         MVC   WORK+40(1),OFCOFC   SAVE OFF 1 BYTE INTERNAL OFFICE CODE         
         DROP  R3                  DROP USING                                   
         LA    R3,1                PROCESS JUST THE ONE OFFICE                  
         B     COFC05              GO PROCESS IT                                
*                                                                               
COFC01   XC    WORK,WORK           AGENCY LEVEL OFFICE LIST PROFILE             
         MVC   WORK(2),=C'S0'      S0                                           
         MVC   WORK+2(2),8(R2)     OFFICE LIST                                  
         MVC   WORK+4(2),AGENCY    AGENCY                                       
         GOTO1 GETPROF,DMCB,(X'C0',WORK),WORK+40,DATAMGR                        
*                                                                               
         L     R0,ATIA             CLIENT TABLE                                 
         LHI   R1,TWAMXLEN         L'CLIENT TABLE                               
         XR    RE,RE               CLEAR RE                                     
         XR    RF,RF               CLEAR RF                                     
         MVCL  R0,RE               CLEAR CLIENT TABLE                           
*                                                                               
         LA    R3,16               MAX 16 OFFICES                               
COFC05   LA    R4,WORK+40          OFFICE LIST PROFILE                          
         L     R2,ATIA             CLIENT TABLE                                 
         LR    R1,R2               R1 = CLIENT TABLE                            
         AHI   R1,TWAMXLEN-1       END OF CLIENT TABLE                          
         MVI   0(R1),X'FF'         INDICATE END OF TABLE                        
*                                                                               
COFC10   CLI   0(R4),C'0'          END OF OFFICE LIST?                          
         BE    COFC30              YES - DONE                                   
*                                                                               
         LA    R6,KEY              R6 = KEY                                     
         USING CLTPHDR,R6          PASSIVE CLIENT BY OFFICE DSECT               
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   CPKEY(2),=X'0D80'   PASSIVE CLT BY OFFICE                        
         MVC   CPKEYAM,BAGYMD      A/M                                          
         MVC   CPKEYOFF,0(R4)      OFFICE                                       
         GOTO1 HIGH                READ HIGH                                    
         B     COFC20              GO TEST KEY                                  
*                                                                               
COFC15   GOTO1 SEQ                 READ SEQ                                     
*                                                                               
COFC20   CLC   KEY(11),KEYSAVE     HAVE ANY MORE OFFICES?                       
         BNE   COFC25              NO - DONE WITH THIS OFFICE                   
*                                                                               
         CLI   0(R2),X'FF'         END OF TABLE?                                
         BE    CTABFULL            YES - GIVE ERROR MESSAGE                     
         MVC   0(2,R2),CPKEYCLT    ADD BINARY CLIENT TO LIST                    
         GOTO1 CLUNPK,DMCB,CPKEYCLT,2(R2)                                       
         LA    R2,5(R2)            BUMP TABLE                                   
         B     COFC15              READ SEQ                                     
*                                                                               
COFC25   LA    R4,1(R4)            BUMP OFFICE LIST PROFILE                     
         BCT   R3,COFC10           MAX 16 OFFICES                               
         DROP  R6                  DROP CLIENT OFFICE KEY USING                 
*                                                                               
COFC30   L     R1,ATIA             CLIENT TABLE                                 
         CLI   0(R1),0             TABLED ANY CLIENTS?                          
         BE    NOCOFF              NO - GIVE ERROR MSG                          
         MVC   0(2,R2),=X'FFFF'    END OF TABLE                                 
         OI    FLTRFLG1,FF1CLST    YES - TURN ON CLIENT LIST FLAG               
         B     YES                 RETUN CC EQU                                 
***********************************************************************         
*        PROCESS A CLIENT GROUP REQUEST                                         
***********************************************************************         
CLTGRP   NTR1                                                                   
         CLC   =C'G=',8(R2)        CLIENT GROUP REQUEST?                        
         BNE   NO                  NO - DONE                                    
         CLI   5(R2),4             INPUT LENGTH OF AT LEAST 4?                  
         BL    NO                  NO - DONE                                    
*                                                                               
         MVC   HALF,10(R2)         CLIENT GROUP ID                              
         CLI   HALF+1,C'0'         2-CHAR CLIENT GROUP?                         
         BL    *+8                 YES                                          
         MVI   HALF+1,C' '         SPACE IN 2ND POS FOR TABLE LOOKUP            
*                                                                               
         LA    R1,SPCGRTAB         CLIENT GROUP TABLE                           
         LHI   R0,(SPCGRTBX-SPCGRTAB)/3                                         
*                                                                               
CG10     CLC   HALF,0(R1)          FOUND A MATCH?                               
         BE    CG20                YES                                          
         LA    R1,3(R1)            BUMP                                         
         BCT   R0,CG10             TEST NEXT IN TABLE                           
         B     INVLFLD             INVALID FIELD                                
*                                                                               
CG20     XC    KEY,KEY             CLEAR THE KEY                                
         LA    R4,KEY              READ GROUP DEFINITION RECORD                 
         USING GRPRECD,R4          CLIENT GROUP DSECT                           
         MVI   GRPKTYP,GRPKTYPQ    X'0D'                                        
         MVI   GRPKSTYP,GRPKCTYQ   X'04'                                        
         MVC   GRPKAGMD,BAGYMD     A/M                                          
         MVC   GRPKID,2(R1)        GROUP ID                                     
*                                                                               
         GOTO1 HIGH                READ HIGH                                    
*                                                                               
         CLC   KEY(13),KEYSAVE     FOUND GROUP ID?                              
         BNE   RECNOTF             NO - REC NOT FOUND                           
*                                                                               
         GOTO1 GETREC              GET THE RECORD                               
*                                                                               
         L     R6,AIO              A(CLIENT GROUP DEF RECORD)                   
         MVI   ELCODE,GRPBRKCQ     BREAK DESCRIPTION ELEMENT                    
         BAS   RE,GETEL            HAVE X'10' ELEMENT?                          
         BE    *+6                 YES                                          
         DC    H'0'                NO - DEATH                                   
*                                                                               
         USING GRPBRKD,R6          BREAK DESCRIPTION DSECT                      
         LLC   R0,GRPBK1LN         BREAK LENGTH 1                               
         LLC   R1,GRPBK2LN         BREAK LENGTH 2                               
         AR    R0,R1               TOTAL BREAK LENGTH NUMBER                    
         DROP  R6                  DROP USING                                   
*                                                                               
         LLC   R1,5(R2)            INPUT LENGTH                                 
         SHI   R1,3                SUBTRACT "G=" AND 1 CHAR CLT GROUP           
         LA    R3,11(R2)           CLIENT GROUP NUMBER                          
         CLI   0(R3),C'0'          2-CHAR CLIENT GROUP?                         
         BNL   *+10                NO                                           
         LA    R3,1(R3)            YES - BUMP R3 BY 1                           
         BCTR  R1,0                SUBTRACT 1 FROM TOTAL LENGTH                 
*                                                                               
         CR    R1,R0               BREAK LENGTH = INPUT LENGTH?                 
         BNE   INVLFLD             NO - ERROR                                   
*                                                                               
         MVC   FULL,0(R3)          GROUP CODES ARE LEFT-JUSTIFIED, PWOS         
         OC    FULL,=C'0000'       ZERO PAD                                     
*                                                                               
         LA    R1,FULL             MAKE SURE IT'S ALL NUMERIC                   
         LA    R3,4                ALL 4 BYTES                                  
*                                                                               
CG21     CLI   0(R1),C'0'          NUMERIC?                                     
         BL    INVLFLD             NO - INVALID                                 
         CLI   0(R1),C'9'          NUMERIC?                                     
         BH    INVLFLD             NO - INVALID                                 
         LA    R1,1(R1)            NEXT BYTE                                    
         BCT   R3,CG21             CHECK NEXT BYTE                              
*                                                                               
         PACK  DUB,FULL            PACK THE CLIENT GROUP NUMBER                 
         L     R0,DUB+4            CLIENT GROUP NUMBER                          
         SRA   R0,4                STRIP 4 BITS OFF                             
         BZ    INVLFLD             AND MUST BE NON-ZERO                         
         STCM  R0,3,GRPKCODE       GROUP CODE (KEY ALREADY BUILT)               
         DROP  R4                  DROP KEY USING                               
*                                                                               
         L     R0,ATIA             CLIENT TABLE                                 
         LHI   R1,TWAMXLEN         L'CLIENT TABLE                               
         XR    RE,RE               CLEAR RE                                     
         XR    RF,RF               CLEAR RF                                     
         MVCL  R0,RE               CLEAR CLIENT TABLE                           
*                                                                               
         L     R2,ATIA             CLIENT TABLE                                 
         LR    R1,R2               R1 = CLIENT TABLE                            
         AHI   R1,TWAMXLEN-1       END OF CLIENT TABLE                          
         MVI   0(R1),X'FF'         INDICATE END OF TABLE                        
*                                                                               
         GOTO1 HIGH                READ HIGH                                    
         B     CG30                GO TEST KEY                                  
*                                                                               
CG25     GOTO1 SEQ                 READ SEQ                                     
*                                                                               
CG30     CLC   KEY(6),KEYSAVE      FOUND CLIENT GROUP RECORD?                   
         BNE   CG40                NO - DONE                                    
*                                                                               
         GOTO1 GETREC              GET THE RECORD                               
*                                                                               
         L     R6,AIO              A(CLIENT GROUP RECORD)                       
         MVI   ELCODE,X'30'        FIND X'30' ELEMENTS                          
         BAS   RE,GETEL            HAVE A X'30' ELEMENT?                        
         B     *+8                 GO TEST CC                                   
*                                                                               
CG35     BAS   RE,NEXTEL           HAVE ANOTHER X'30 ELEMENT?                   
         BNE   CG25                NO - GO READ SEQ                             
*                                                                               
         USING GRPVALD,R6          GROUP VALUE DSECT                            
         CLI   0(R2),X'FF'         END OF TABLE?                                
         BE    CTABFULL            YES - GIVE ERROR MESSAGE                     
         GOTO1 CLPACK,DMCB,GRPVALUE,0(R2)                                       
         MVC   2(3,R2),GRPVALUE    EBCEDIC CLIENT                               
         LA    R2,5(R2)            BUMP TABLE                                   
         B     CG35                GET NEXT ELEMENT                             
         DROP  R6                  DROP USING                                   
*                                                                               
CG40     L     R1,ATIA             CLIENT TABLE                                 
         CLI   0(R1),0             TABLED ANY CLIENTS?                          
         BE    NOCGRP              NO - GIVE ERROR MESSAGE                      
         MVC   0(2,R2),=X'FFFF'    END OF TABLE                                 
         OI    FLTRFLG1,FF1CLST    YES - TURN ON CLIENT LIST FLAG               
         B     YES                 RETUN CC EQU                                 
***********************************************************************         
*        PRINT OUT REPORT LINE FOR THIS ORDER REC                               
***********************************************************************         
*                                                                               
PRINTORD NTR1                                                                   
         LA    R6,SVDOKEY                                                       
         USING DAREORDD,R6                                                      
*                                                                               
         MVC   BINORDER,DOKORDER                                                
         GOTO1 SHWORDER,DMCB,PORDER      ORDER NUMBER                           
*                                                                               
         XC    MKTSTAT,MKTSTAT                                                  
         MVC   STATION,DOKSTA      BINARY STATION WE'RE LOOKING FOR             
         GOTO1 MSUNPK,DMCB,MKTSTAT,WORK,WORK+4                                  
         MVC   WORK+8(1),QMED                                                   
         MVC   PSTATION,WORK+4     STATION                                      
         CLI   PSTATION,X'F0'      IS IT CABLE?                                 
         BL    *+8                 STUPID!!!                                    
         MVI   PSTATION+4,C'/'                                                  
*                                                                               
         L     R6,AIO                                                           
         USING DOIDELD,R6                                                       
         MVI   ELCODE,DOIDELQ                                                   
         BAS   RE,GETEL            GET CLIENT                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 CLUNPK,DMCB,DOIDCLT,PCLIENT                                      
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DOSTELQ                                                   
         BAS   RE,GETEL            ORDER DATE                                   
         BNE   PI30                                                             
*                                                                               
         USING DOSTELD,R6                                                       
PI05     CLI   0(R6),0                                                          
         BE    PI30                                                             
         CLI   0(R6),DOSTELQ                                                    
         BE    PI20                                                             
PI15     LLC   R0,1(R6)            BUMP TO NEXT X'12' ELEM                      
         AR    R6,R0                                                            
         B     PI05                                                             
*                                                                               
PI20     CLI   DOSTSTAT,DSENT      AM I SENT?                                   
         BE    PI25                                                             
         CLI   DOSTSTAT,DFXSENT    AM I FAX SENT?                               
         BE    PI25                                                             
         CLI   DOSTSTAT,DEMSENT    AM I EMAIL SENT?                             
         BE    PI25                                                             
         CLI   DOSTSTAT,DFXRSNT    AM I FAX RESENT?                             
         BNE   PI15                                                             
*                                                                               
PI25     GOTO1 DATCON,DMCB,(8,DOSTDATE),(11,PORDATE)                            
*                                                                               
PI30     BAS   RE,PRTSTAT          PRINT STATUS                                 
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
PIX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        PRINT OUT REPORT LINE FOR THIS MAKEGOOD NOTICE REC                     
***********************************************************************         
*                                                                               
PRINTMG  NTR1                                                                   
         LA    R6,SVMNKEY                                                       
         USING DAREMGND,R6                                                      
*                                                                               
         MVC   BINORDER,MNKORDER                                                
         GOTO1 SHWORDER,DMCB,PORDER      ORDER NUMBER                           
*                                                                               
         MVC   PMGOOD,MNKGROUP     MAKEGOOD GROUP                               
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAYS THE ORDER NUMBER                                                     
*                                                                               
* ON ENTRY:    PARAM 1             A(OUTPUT)                                    
*              BINORDER            ORDER NUMBER AS STORED IN RECORD             
***********************************************************************         
SHWORDER L     R2,0(R1)                                                         
         MVC   FULL,BINORDER                                                    
         XC    FULL,=4X'FF'                                                     
*                                                                               
         TM    FULL,X'80'                 NEW STYLE?                            
         BNZ   SHWO20                                                           
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,FULL                  DATE PORTION                          
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(4,R2),DUB                                                      
*                                                                               
         ICM   R1,3,FULL+2                SEQUENCE PORTION                      
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  4(4,R2),DUB                                                      
         BR    RE                                                               
*                                                                               
SHWO20   NI    FULL,X'FF'-X'80'                                                 
         ICM   R1,15,FULL                                                       
         CVD   R1,DUB                                                           
         AP    DUB,=P'04000000'                                                 
         OI    DUB+7,X'0F'                                                      
         UNPK  0(8,R2),DUB                                                      
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAYS THE STATUS OF THE ORDER                                              
*                                                                               
* ON ENTRY:    AIO CONTAINS ORDER REC                                           
* OUTPUT  :    STATUS TO PSTATUS                                                
***********************************************************************         
PRTSTAT  NTR1                                                                   
         MVI   STATFLAG,0                                                       
         L     R6,AIO                                                           
         USING DOI2ELD,R6                                                       
         MVI   ELCODE,DOI2ELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   PS10                                                             
         TM    DOI2FLG1,DOI2FVAR   VAR ORDER YET?                               
         BZ    *+8                                                              
         OI    STATFLAG,STFVAROR   YES                                          
         DROP  R6                                                               
*                                                                               
PS10     L     R6,AIO                                                           
         USING DOSPELD,R6                                                       
         MVI   ELCODE,DOSPELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   PS20                                                             
         MVC   BYTE,DOSPFLG1       SAVE THIS IN BYTE!                           
***      TM    DOSPFLG1,DOSPCFCM   CONFIRM WITH COMMENT?                        
***      BZ    *+8                                                              
***      OI    STATFLAG,STFCNFCM   YES                                          
         CLI   DOSPREVN,0          GOT A REVISION NUMBER?                       
         BE    *+8                                                              
         OI    STATFLAG,STFREVOR   YES, REVISED ORDER                           
*                                                                               
PS20     L     R6,AIO                                                           
********                                                                        
         MVI   ELCODE,DOSTELQ      GET FIRST X'12' ELEMENT?                     
         BAS   RE,GETEL                                                         
         BE    PS25                AM I UNSENT?                                 
         LA    R2,STATTAB                                                       
         B     PS85                                                             
*                                                                               
         USING DOSTELD,R6                                                       
PS25     CLI   DOSTSTAT,DDLVRD     AM I DELIVERED?                              
         BNE   PS30                                                             
         OI    STATFLAG,STFDLVRD   ORDER IS DELIVERED                           
         LR    R1,R6                                                            
         LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   DOSTSTAT,QRECALL    AM I RECALLED DELIVERED?                     
         BE    PS50                                                             
         CLI   DOSTSTAT,DSENT                                                   
         BNE   PS27                                                             
         LR    R6,R1                                                            
         B     PS50                                                             
*                                                                               
PS27     NI    STATFLAG,X'FF'-STFDLVRD                                          
         B     PS50                                                             
*                                                                               
PS30     CLI   DOSTSTAT,QERRORED   AM I IN ERROR?                               
         BNE   PS40                                                             
         OI    STATFLAG,STFERROR   THIS ORDER IS IN ERROR!!                     
PS32     LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   DOSTSTAT,DFXSENT    AM I FAX/ERROR?                              
         BE    PS50                                                             
         CLI   DOSTSTAT,DEMSENT    AM I EMAIL/ERROR?                            
         BE    PS50                                                             
         CLI   DOSTSTAT,DSENT      AM I SENT/ERROR?                             
         BNE   PS32                                                             
         B     PS50                                                             
*                                                                               
PS40     CLI   DOSTSTAT,QCFMD      AM I CONFIRMED?                              
         BNE   PS50                                                             
         CLI   DOSTLEN,DOSTLNQ3    HAVE EXTENDED TYPE FIELD?                    
         BNE   PS42                                                             
         TM    DOSTSTAT,DCNFMCOM   AM I CONFIRMED W/COMMENTS?                   
         BZ    PS50                                                             
         OI    STATFLAG,STFCNFCM   YES!                                         
         B     PS50                                                             
*                                                                               
PS42     TM    BYTE,DOSPCFCM       CONFIRM WITH COMMENT?                        
         BZ    PS50                                                             
         OI    STATFLAG,STFCNFCM   YES!                                         
*                                                                               
PS50     DS    0H                                                               
********                                                                        
         LA    R2,STATTAB                                                       
PS60     CLI   0(R2),X'FF'         ANY MATCHES?                                 
         BE    PS85                NO, PUT OUT ERR999!!!                        
         CLC   DOSTSTAT,0(R2)                                                   
         BE    PS80                                                             
PS65     LA    R2,8(R2)            BUMP TO NEXT                                 
         B     PS60                                                             
*                                                                               
PS80     ZICM  R1,1(R2)                                                         
         BZ    PS85                                                             
         EX    R1,*+8                                                           
         BO    PS85                                                             
         TM    STATFLAG,0                                                       
         B     PS65                                                             
*                                                                               
PS85     MVC   PSTATUS,2(R2)                                                    
PSX      B     XIT                                                              
         DROP  R6                                                               
*                                                                               
STATTAB  DS    0H  !!!*THE ORDER OF THIS TABLE MUST BE PRESERVED*!!!            
* UNSENT                                                                        
         DC    AL1(0,0),CL6'UNSENT'                                             
* *SENT                                                                         
         DC    AL1(DSENT,STFERROR),CL6'ERROR'                                   
         DC    AL1(DSENT,STFREVOR),CL6'*RVSNT'                                  
         DC    AL1(DSENT,STFVAROR),CL6'*VRSNT'                                  
         DC    AL1(DSENT,0),CL6'*SENT'                                          
* SENT/DELIVERED                                                                
         DC    AL1(DDLVRD,STFREVOR),CL6'REVSNT'                                 
         DC    AL1(DDLVRD,STFVAROR),CL6'VARSNT'                                 
         DC    AL1(DDLVRD,0),CL6'SENT'                                          
* REJECTED                                                                      
         DC    AL1(QRJCT,STFREVOR),CL6'REVREJ'                                  
         DC    AL1(QRJCT,STFVAROR),CL6'VARREJ'                                  
         DC    AL1(QRJCT,0),CL6'RJCTED'                                         
* OPENED/APPROVED                                                               
         DC    AL1(QAPP,STFREVOR),CL6'REVOPN'                                   
         DC    AL1(QAPP,STFVAROR),CL6'VAROPN'                                   
         DC    AL1(QAPP,0),CL6'OPENED'                                          
* CONFIRMED PENDING                                                             
         DC    AL1(QCFMDPND,0),CL6'CFMPND'                                      
* PARTIAL CONFIRM                                                               
         DC    AL1(QCFMD,STFREVOR+STFCNFCM),CL6'**RCNF'                         
         DC    AL1(QCFMD,STFVAROR+STFCNFCM),CL6'**VCNF'                         
         DC    AL1(QCFMD,STFCNFCM),CL6'**CFMD'                                  
* CONFIRM                                                                       
         DC    AL1(QCFMD,STFREVOR),CL6'REVCNF'                                  
         DC    AL1(QCFMD,STFVAROR),CL6'VARCNF'                                  
         DC    AL1(QCFMD,0),CL6'CNFRMD'                                         
* BUYER CONFIRMED                                                               
         DC    AL1(QBYRCNFM,0),CL6'BYRCNF'                                      
* FAX SENT                                                                      
         DC    AL1(QFAXCNCL,0),CL6'FAXERR'                                      
         DC    AL1(DFXSENT,STFERROR),CL6'FAXERR'                                
* FAX SENT                                                                      
         DC    AL1(DFXSENT,0),CL6'FAXSNT'                                       
* FAX DELIVERED                                                                 
         DC    AL1(DFXDLVD,0),CL6'FXDLVD'                                       
* EMAIL SENT                                                                    
         DC    AL1(DEMSENT,STFERROR),CL6'EMLERR'                                
         DC    AL1(DEMSENT,0),CL6'EMLSNT'                                       
* EMAIL DELIVERED                                                               
         DC    AL1(DEMDLVD,0),CL6'EMDLVD'                                       
* EMPTY                                                                         
         DC    AL1(QEMPTY,0),CL6'EMPTY'                                         
* UNDARED                                                                       
         DC    AL1(QUNDARE,0),CL6'UNDARD'                                       
* NOTDARED                                                                      
         DC    AL1(QNODARE,0),CL6'NTDARE'                                       
* (*)RECALL                                                                     
         DC    AL1(QRECALL,STFDLVRD),CL6'*RCALL'                                
         DC    AL1(QRECALL,0),CL6'*RCALL'                                       
* VALID RECALL TYPES                                                            
         DC    AL1(QRCLAPPR,0),CL6'RCLAPP'                                      
         DC    AL1(QRCLCONF,0),CL6'RCLCFM'                                      
         DC    AL1(QRCLDELN,0),CL6'RCLDNT'                                      
         DC    AL1(QRCLREJD,0),CL6'RCLREJ'                                      
         DC    AL1(QRCLTRNS,0),CL6'RCLTRN'                                      
         DC    AL1(QRCLWIP,0),CL6'RCLWIP'                                       
* SENT PENDING                                                                  
         DC    AL1(QSNTPNDG,STFREVOR),CL6'*RVSNT'                               
         DC    AL1(QSNTPNDG,STFVAROR),CL6'*VRSNT'                               
         DC    AL1(QSNTPNDG,0),CL6'*SENT'                                       
* SENT CANCELED, PARTIAL CONFIRM                                                
         DC    AL1(QSNTXCNF,STFREVOR),CL6'**RCNF'                               
         DC    AL1(QSNTXCNF,STFVAROR),CL6'**VCNF'                               
         DC    AL1(QSNTXCNF,0),CL6'**PCNF'                                      
* SEND CANCELLED, REJECTED                                                      
         DC    AL1(QSNTXREJ,STFREVOR),CL6'REVREJ'                               
         DC    AL1(QSNTXREJ,STFVAROR),CL6'VARREJ'                               
         DC    AL1(QSNTXREJ,0),CL6'RJCTED'                                      
* TO BE SENT                                                                    
         DC    AL1(QTOBESNT,STFREVOR),CL6'*RVSNT'                               
         DC    AL1(QTOBESNT,STFVAROR),CL6'*VRSNT'                               
         DC    AL1(QTOBESNT,0),CL6'*SENT'                                       
* UNKNOWN, ERR999                                                               
         DC    X'FF,0',CL6'ERR999'                                              
         EJECT                                                                  
***********************************************************************         
*        HEADSPECS                                                              
***********************************************************************         
*                                                                               
HDSPECS  SSPEC H1,50,C'DARE BUYER MOVE'                                         
         SSPEC H2,50,C'---------------'                                         
         SSPEC H1,2,AGYNAME                                                     
         SSPEC H2,2,AGYADD                                                      
         SSPEC H1,113,PAGE                                                      
         SSPEC H2,113,REQUESTOR                                                 
         SSPEC H3,113,RUN                                                       
         SSPEC H4,1,C'   '                                                      
         SSPEC H4,2,C'MEDIA     :'                                              
         SSPEC H5,2,C'FROM BUYER:'                                              
         SSPEC H6,2,C'TO BUYER  :'                                              
*                                                                               
         SSPEC H8,2,C'ORDER #'                                                  
         SSPEC H9,2,C'--------'                                                 
         SSPEC H8,14,C'STATN'                                                   
         SSPEC H9,14,C'-----'                                                   
         SSPEC H8,23,C'CLT'                                                     
         SSPEC H9,23,C'---'                                                     
         SSPEC H8,30,C'ORD DATE'                                                
         SSPEC H9,30,C'--------'                                                
         SSPEC H8,42,C'STATUS'                                                  
         SSPEC H9,42,C'------'                                                  
         SSPEC H8,52,C'MG '                                                     
         SSPEC H9,52,C'---'                                                     
*                                                                               
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
*        HEADHOOKS                                                              
***********************************************************************         
*                                                                               
HDHOOK   NTR1                                                                   
         MVC   H4+13(L'QMED),QMED                                               
         MVC   H4+20(L'MEDNM),MEDNM                                             
         MVC   H5+13(L'FROMBYR),FROMBYR                                         
         MVC   H5+20(L'FRBYRNME),FRBYRNME                                       
         MVC   H6+13(L'TOBYR),TOBYR                                             
         MVC   H6+20(L'TOBYRNME),TOBYRNME                                       
*                                                                               
         TM    FLTRFLG1,FF1CLST    CLIENT LIST FILTER?                          
         BNO   HDHK05              NO                                           
*                                                                               
         MVC   H4+49(8),=C'CLIENTS='                                            
         LA    R2,H4+58            START CLIENT LIST HERE                       
         LA    R3,18               ROOM TO LIST 18 CLTS (132 BYTE HEAD)         
         L     RE,ATIA             CLIENT TABLE                                 
*                                                                               
HDHK01   CLC   =X'FFFF',0(RE)      END OF TABLE?                                
         BE    HDHK02              YES                                          
         MVC   0(3,R2),2(RE)       MOVE CLIENT TO SCREEN                        
         MVI   3(R2),C','          COMMA SEPARATED                              
         LA    R2,4(R2)            BUMP TO NEXT SLOT                            
         LA    RE,5(RE)            BUMP TO NEXT CLIENT ENTRY                    
         BCT   R3,HDHK01           CHECK NEXT ENTRY                             
*                                                                               
HDHK02   BCTR  R2,0                BACK UP 1                                    
         MVI   0(R2),X'40'         REPLACE THE LAST COMMA W/SPACE               
         B     HDHK10              DONE PRINTING CLIENT LIST                    
*                                                                               
HDHK05   OC    BCLT,BCLT           ANY CLIENT FILTER                            
         BZ    HDHK10                                                           
         MVC   H4+49(8),=C'CLIENT ='                                            
         MVC   H4+58(L'QCLT),QCLT                                               
*                                                                               
HDHK10   OC    SVMKT1(L'SVMKT1*MAXMKTS),SVMKT1   ANY MKT FILTERS                
         BZ    HDHK20                                                           
         MVC   H5+49(8),=C'MARKET ='                                            
         LA    R2,H5+58                                                         
         LA    R3,SVMKT1                                                        
         LA    R4,MAXMKTS                                                       
HDHK15   OC    0(L'SVMKT1,R3),0(R3)                                             
         BZ    *+14                                                             
         MVC   0(L'SVMKT1,R2),0(R3)                                             
         LA    R2,L'SVMKT1+1(R2)                                                
         LA    R3,L'SVMKT1(R3)                                                  
         BCT   R4,HDHK15                                                        
*                                                                               
HDHK20   OC    JSTDATE,JSTDATE     ANY PERIOD FILTER                            
         BZ    HDHKX                                                            
         MVC   H6+49(8),=C'PERIOD ='                                            
         GOTO1 DATCON,DMCB,(8,JSTDATE),(11,WORK)                                
         MVC   H6+58(8),WORK                                                    
         MVI   H6+66,C'-'                                                       
         GOTO1 DATCON,DMCB,(8,JNDDATE),(11,WORK)                                
         MVC   H6+67(8),WORK                                                    
*                                                                               
HDHKX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GENERAL ERROR/INFO MESSAGES AND STUFF                                         
***********************************************************************         
MISSFLD  MVI   ERROR,MISSING                                                    
         B     ERREXIT                                                          
INVLFLD  MVI   ERROR,INVALID                                                    
         B     ERREXIT                                                          
RECNOTF  MVI   ERROR,NOTFOUND                                                   
         B     ERREXIT                                                          
NEEDFLDS MVI   ERROR,REQFIELD                                                   
         B     ERREXIT                                                          
*                                                                               
CTABFULL LA    R2,MOVCLTH          CLIENT                                       
         MVC   ERRNUM,=AL2(834)                                                 
         B     SPERREX                                                          
NOCOFF   LA    R2,MOVCLTH          CLIENT                                       
         MVC   ERRNUM,=AL2(1400)                                                
         B     SPERREX                                                          
NOCGRP   LA    R2,MOVCLTH          CLIENT                                       
         MVC   ERRNUM,=AL2(1399)                                                
         B     SPERREX                                                          
*                                                                               
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
         MVC   AIO,AIO1                                                         
         DROP  RF                                                               
*                                                                               
ERREXIT  GOTO1 ERREX                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE  USED FOR THE GETEL OPERATIONS                
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
*                                                                               
XIT      XIT1                                                                   
*                                                                               
RELO     DS    A                                                                
         EJECT                                                                  
         LTORG                                                                  
       ++INCLUDE SPCGRTAB                                                       
         EJECT                                                                  
* DDBIGBOX                                                                      
* FAGETTXTD                                                                     
* DDGLOBEQUS                                                                    
* DDGLVXCTLD                                                                    
* DDCOMFACSD                                                                    
* DMPRTQL                                                                       
* FAFACTS                                                                       
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDPERVALD                                                                     
* SPGENCLT                                                                      
* SPGENMKT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE DDPERVALD                                                      
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENCLTO         PASSIVE CLIENT BY OFFICE                     
MKTHDRD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
*                                                                               
       ++INCLUDE SPSFMFFD          (BASE SCREEN FOR SYSTEM)                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM2FD          (OUR MOVE SCREEN)                            
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD        (SYSTEM AREAS)                               
         EJECT                                                                  
       ++INCLUDE SPGENDRORD        (RECORD DSECTS)                              
         EJECT                                                                  
       ++INCLUDE SPGENDRMKN                                                     
         EJECT                                                                  
       ++INCLUDE SPADBUYER                                                      
         EJECT                                                                  
       ++INCLUDE SPGENGRP                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
       ++INCLUDE DDOFFICED                                                      
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
* SYSSPARE SAVED STORAGE AREA                                                   
***********************************************************************         
SAVED    DSECT                                                                  
*                                                                               
AESTTAB  DS    V                                                                
*                                                                               
MISCFLG1 DS    X                   MISCELLANEOUS FLAGS                          
MF1KYCHG EQU   X'80'                - KEY WAS CHANGED                           
MF1MKCHG EQU   X'40'                - MARKET(S) WAS CHANGED                     
*                                                                               
FLTRFLG1 DS    X                   FILTER FLAGS                                 
FF1CLT   EQU   X'80'                - BY CLIENT                                 
FF1MKT   EQU   X'40'                - BY MARKET(S)                              
FF1DPERS EQU   X'20'                - BY DARE DATE PERIOD SUBSET                
FF1DPERI EQU   X'10'                - BY DARE DATE PERIOD INTERSECT             
FF1EST   EQU   X'08'                - BY DARE DATE PERIOD                       
FF1CLST  EQU   X'04'                - BY LIST OF CLIENTS                        
*                                                                               
STATFLAG DS    X                   STATUS FLAG                                  
STFVAROR EQU   X'80'               VAR ORDER                                    
STFCNFCM EQU   X'40'               CONFIRM WITH COMMENTS                        
STFREVOR EQU   X'20'               REVISED ORDER                                
STFDLVRD EQU   X'10'               ORDER IS DELIVERED                           
STFERROR EQU   X'08'               ORDER IS IN ERROR STATUS                     
*                                                                               
FROMBYR  DS    CL3                                                              
FRBYRNME DS    CL24                                                             
TOBYR    DS    CL3                                                              
TOBYRNME DS    CL24                                                             
ORDERNUM DS    XL4                                                              
SVMKT1   DS    CL4                 MARKET FILTERS                               
SVMKT2   DS    CL4                                                              
SVMKT3   DS    CL4                                                              
JSTDATE  DS    XL3                 START DATE FILTER (JULIAN PWOS)              
JNDDATE  DS    XL3                 END   DATE FILTER (JULIAN PWOS)              
YSTDATE  DS    XL3                 START DATE FILTER (YYMMDD)                   
YNDDATE  DS    XL3                 END   DATE FILTER (YYMMDD)                   
FILTREST DS    XL1                 FILTERED ESTIMATE                            
*                                                                               
LASTSTA  DS    XL3                                                              
BCHKCLT  DS    XL2                 BINARY CLIENT TO CHECK                       
CHKCLT   DS    CL3                 CLIENT TO CHECK                              
MKTMATCH DS    CL1                                                              
*                                                                               
MAXMKTS  EQU   3                                                                
MKTSTAT  DS    0XL5                                                             
MARKET   DS    XL2                                                              
STATION  DS    XL3                                                              
*                                                                               
ORDCOUNT DS    PL8                                                              
BINORDER DS    XL4                 ORDER NUMBER                                 
QORDER   DS    CL8                 EBCDIC ORDER NO.                             
SAVEKEY  DS    XL(L'KEY)           SAVED KEY                                    
SVMNKEY  DS    XL(L'KEY)           SAVED KEY                                    
SVDOKEY  DS    XL(L'KEY)           SAVED KEY                                    
FAKEFLDH DS    XL8                 FAKE HEADER                                  
FAKEFLD  DS    XL80                FAKE FIELD                                   
PERVALST DS    XL56                BLOCK FOR PERVAL                             
SVSTAKEY DS    XL(L'STAKEY)                                                     
SVCLRDAT DS    0CL3                COLOR AND DATE                               
SVCOLCOL DS    CL1                                                              
SVCOLDAT DS    CL2                                                              
TDYLSS2W DS    XL2                 TODAY LESS 2 WEEKS                           
SVCLT    DS    XL2                                                              
SVMKT    DS    XL2                                                              
SVMGSTAT DS    X                                                                
*                                                                               
QBUYER   DS    CL24                BUYER'S FULL NAME                            
QOFFICE  DS    CL2                 OFFICE ID FOR BUYER                          
QPHONE   DS    CL12                PHONE NUMBER FOR BUYER                       
QPHONEXT DS    CL4                 PHONE EXTENSION                              
*                                                                               
OMBASFL2 DS    XL1                 ORDERMANAGER BASE FLAGS                      
OM2DSTOV EQU   X'80'               - CALLED BY DESTOV MAINT                     
OM2DACT  EQU   X'40'               - DEACTIAVED, CAN'T SEND OR MG               
*                                                                               
STATABLE DS    100CL(STABLNQ)                                                   
STATABND DS    X                                                                
ERRNUM   DS    XL2                                                              
*                                                                               
OFCBLK   DS    XL(OFCLENQ)                                                      
***********************************************************************         
* ESTIMATE TABLE DSECT                                                          
***********************************************************************         
ESTD     DSECT                                                                  
EDKEY    DS    0D                                                               
EDCLT    DS    XL2                                                              
EDEST    DS    XL1                                                              
EDKEYLEN EQU   *-EDKEY                                                          
*                                                                               
EDESTART DS    CL6                                                              
EDEEND   DS    CL6                                                              
EDRECLEN EQU   *-EDKEY                                                          
*                                                                               
ESTMAX   EQU   5000                                                             
ESTLEN   EQU   ESTMAX*EDRECLEN                                                  
***********************************************************************         
* STATION TABLE DSECT                                                           
***********************************************************************         
STABLED  DSECT                                                                  
STSTATN  DS    XL3                 BINIARY STATION                              
STCLT    DS    CL3                 CLIENT OVERRIDE                              
STSTATUS DS    XL1                 STATUS                                       
STSTNOT  EQU   X'80'               STATION NOT IN ANY REQUESTED MKTS            
STABLNQ  EQU   *-STABLED                                                        
*                                                                               
***********************************************************************         
* PRINT LINE DSECT                                                              
***********************************************************************         
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL1                                                              
PORDER   DS    CL8                 ORDER NUMBER                                 
         DS    CL4                                                              
PSTATION DS    CL5                 STATION                                      
         DS    CL4                                                              
PCLIENT  DS    CL3                 CLIENT                                       
         DS    CL4                                                              
PORDATE  DS    CL8                 ORDER DATE                                   
         DS    CL4                                                              
PSTATUS  DS    CL6                 STATUS                                       
         DS    CL4                                                              
PMGOOD   DS    CL3                 MAKEGOOD GROUP CODE                          
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'059SPSFM75   12/05/12'                                      
         END                                                                    
