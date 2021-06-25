*          DATA SET DDRECON    AT LEVEL 002 AS OF 08/31/11                      
*PHASE DDRECONA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE DMUTLCT                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE FILTABL                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE LOGIO                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE CLUNPK                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE NUMVAL                                                                 
*INCLUDE TIMBER                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE PERVAL                                                                 
         TITLE 'DDRECON - RECOVERY FILE CONDENSE PROGRAM'                       
                                                                                
***********************************************************************         
* READ RECOVERY FILE DUMP TAPE. NO CODE TO READ DISK FILE INCLUDED.   *         
* CONDENSES UPDATE EVENTS FOR RECORDS WITH THE SAME KEY TO PRODUCE    *         
* RECOVERY RECORDS REPRESENTING AT MOST A SINGLE UPDATE EVENT.        *         
* THESE RECORDS ARE SORTED BACK INTO TIME ORDER AND WRITTEN TO AN     *         
* OUTPUT FILE IN FORMAT AS STANDARD RECOVERY DUMP TAPE.               *         
*                                                                     *         
* OFFLINE CHANGES ONLY HAVE SIN=X'00000000' OR X'FD00JJJJ'            *         
* OFFLINE CPY/CHG PAIR HAVE SIN=X'00000001' OR X'FE00JJJJ'            *         
*                                                                     *         
* PARAMETER CARDS DEFINED VIA CARDTBL. MUST HAVE FILE=XX CARD.        *         
*                                                                     *         
* INPUT FILE: RECOVERY DUMP FILE DDNAME "RCVTAPE"                     *         
*                                                                     *         
* OUTPUT FILE: CONDENSED RECOVERY DUMP FILE DDNAME "OUT"              *         
*                                                                     *         
***********************************************************************         
         PRINT NOGEN                                                            
DDRECON  CSECT                                                                  
         NBASE RECWKX-RECWK,DDRECON,=V(REGSAVE),RA,R9,CLEAR=YES                 
         USING RECWK,RC                                                         
         L     R8,VCPRINT                                                       
         USING DPRINT,R8                                                        
         MVC   TITLE(40),=CL40'RECOVERY FILE CONDENSE PROGRAM'                  
         B     MAIN                                                             
                                                                                
***********************************************************************         
* MAIN CONTROL CODE                                                   *         
***********************************************************************         
MAIN     BAS   RE,GENINIT          GENERAL INITIALISATION                       
*                                                                               
         BAS   RE,VALCARDS         VALIDATE JCL CARD DATA LINE                  
         BNE   MERR                EXIT IF ERROR                                
*                                                                               
         BAS   RE,READRCV          READ RECOVERY INPUT FILE AND                 
         BNE   MERR                SORT IN KEY ORDER                            
*                                                                               
         BAS   RE,GETCON           GET SORTED RECORDS AND CONDENSE              
         BNE   MERR                UPDATE EVENTS AND SORT TIME ORDER            
*                                                                               
         BAS   RE,SORTOUT          GET SORTED RECORDS AND FORMAT AND            
         BNE   MERR                WRITE TO OUTPUT RECOVERY FILE                
*                                                                               
         B     MXIT                                                             
*                                                                               
MERR     MVI   RETCODE,X'0C'       HERE IF PROCESS ERROR                        
         CLI   ERROR,0                                                          
         BNE   *+8                                                              
         MVI   ERROR,UNDEFERQ                                                   
         BAS   RE,ERRPRT                                                        
         B     MXIT                                                             
*                                                                               
MXIT     XBASE RC=RETCODE,RL=1     EXIT HERE IF OK                              
                                                                                
***********************************************************************         
* READ RECOVERY INPUT FILE, BUILD KEY AND PUT TO SORTER               *         
***********************************************************************         
READRCV  NTR1                                                                   
         MVC   P(40),=CL40'READRCV'                                             
         GOTO1 VPRINTER                                                         
*                                                                               
         LA    R3,SORTCRDI                                                      
         LA    RE,L'ROLDKEY+L'RSEQ                                              
         CVD   RE,DUB                                                           
         UNPK  15(2,R3),DUB+6(2)                                                
         OI    16(R3),X'F0'                                                     
         GOTO1 VSORTER,DMCB,SORTCRDI,RECCARD,0                                  
*                                                                               
         BAS   RE,OPENFIL          OPEN SYSTEM FILES                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,ARECORD                                                       
         USING RECD,R3                                                          
*                                                                               
RRCV010  CLI   INTAPE,C'Y'         TEST IF RECOVERY FILE TAPE INPUT             
         BE    RRCV012                                                          
*                                                                               
RRCV012  LA    R1,RCVTAPE                                                       
         LA    R0,RECVHDR-4                                                     
         GET   (R1),(R0)                                                        
*                                                                               
         CLI   RRECTY,X'81'        IGNORE POINTER COPY/CHANGES                  
         BE    RRCV010                                                          
         CLI   RRECTY,X'82'                                                     
         BE    RRCV010                                                          
         CLI   RSIN,X'FF'          IGNORE DELETED RECORDS                       
         BE    RRCV010                                                          
         CLI   RSIN,X'FE'          OFFLINE COPY/CHANGE PAIR                     
         BNE   *+10                                                             
         MVC   RSIN,=F'1'                                                       
         CLI   RSIN,X'FD'          OFFLINE CHANGES ONLY                         
         BNE   *+10                                                             
         MVC   RSIN,=F'0'                                                       
*                                                                               
         CLC   RFILTY,FILNUM       TEST FOR FILE NUMBER                         
         BNE   RRCV010                                                          
*                                                                               
RRCV014  SR    R1,R1               REMOVE TRAILER IF PRESENT                    
         ICM   R1,3,RECVHDR-4                                                   
         LA    R1,RECVHDR-4(R1)    R1=A(END OF RECORD)                          
         TM    RTIME,X'40'         TEST IF THERE IS A TRAILER                   
         BZ    RRCV016                                                          
         NI    RTIME,255-X'40'                                                  
         BCTR  R1,0                R1=A(LAST BYTE OF TRAILER)                   
         SR    RF,RF                                                            
         IC    RF,0(R1)            RF=L'TRAILER                                 
         SR    R1,RF                                                            
         LA    R1,1(R1)            R1=A(END OF RECORD)                          
         LA    RF,RECVHDR-4        CALCULATE NEW RECORD LENGTH                  
         LR    RE,R1                                                            
         SR    RE,RF                                                            
         STH   RE,RECVHDR-4        SET NEW RECORD LENGTH MINUS TRAILER          
*                                                                               
RRCV016  MVI   0(R1),0             R1=A(END OF RECORD)                          
         SR    RE,RE                                                            
         ICM   RE,3,RECVHDR-4                                                   
         LA    RE,L'ROLDKEY+L'RSEQ+L'RSPARE(RE)                                 
         SLL   RE,16                                                            
         ST    RE,RLEN             SET SORT RECORD LENGTH                       
*                                                                               
         L     RE,MYSEQ            SET SEQUENCE NUMBER                          
         AH    RE,=H'1'                                                         
         ST    RE,MYSEQ                                                         
         MVC   RSEQ,MYSEQ                                                       
         XC    RSPARE,RSPARE                                                    
*                                                                               
         CLI   RRECTY,RTCPYQ       COPY                                         
         BE    RRCV110                                                          
         CLI   RRECTY,RTCHGQ       CHANGE                                       
         BE    RRCV120                                                          
         CLI   RRECTY,RTADDQ       ADD                                          
         BE    RRCV130                                                          
         DC    H'0'                                                             
*                                                                               
RRCV110  SR    RF,RF               MOVE COPY KEY FOR OLD/NEW                    
         IC    RF,KEYLEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ROLDKEY(0),RDATA                                                 
         MVC   SAVSIN,RSIN         SAVE THIS SIN                                
         AP    RCVCPY,=P'1'                                                     
         B     RRCV200                                                          
*                                                                               
RRCV120  CLC   SAVSIN,RSIN         CHECK CHANGE IS PAIR OF COPY                 
         BNE   RRCV122                                                          
         AP    RCVCHG,=P'1'                                                     
         B     RRCV200                                                          
*                                                                               
RRCV122  CLI   CHKSIN,C'N'         TEST TO CHECK SINS OF COPY/CHGS              
         BE    RRCV124                                                          
         OC    RSIN,RSIN           SIN=ZERO OFFLINE CHANGE WITH NO COPY         
         BZ    *+6                                                              
         DC    H'0'                                                             
RRCV124  SR    RF,RF                                                            
         IC    RF,KEYLEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ROLDKEY(0),RDATA                                                 
         AP    RCVCHG,=P'1'                                                     
         B     RRCV200                                                          
*                                                                               
RRCV130  SR    RF,RF               SET NEW/NEW REC                              
         IC    RF,KEYLEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ROLDKEY(0),RDATA                                                 
         AP    RCVADD,=P'1'                                                     
         B     RRCV200                                                          
*                                                                               
RRCV200  DS    0H                  PUT TO SORTER                                
         GOTO1 VSORTER,DMCB,=C'PUT',RLEN                                        
         SR    RF,RF                                                            
         IC    RF,KEYLEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   ROLDKEY(0),RDATA                                                 
         BE    RRCV010                                                          
         DC    H'0'                                                             
*                                                                               
         SR    RF,RF                                                            
         IC    RF,KEYLEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ROLDKEY(0),RDATA    THIS TIME WITH NEW KEY                       
         B     RRCV200                                                          
*                                                                               
EODADRCV BAS   RE,CLOSEFIL         END OF RECOVERY FILE                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     RRCVOK                                                           
*                                                                               
RRCVOK   SR    RC,RC                                                            
RRCVNO   LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R3                                                               
                                                                                
***********************************************************************         
* GET KEY SORTED RECORDS AND CONDENSE UPDATE EVENTS AND PUT TO        *         
* SORTER IN TIME ORDER                                                *         
***********************************************************************         
GETCON   NTR1                                                                   
         MVC   P(40),=CL40'GETMRG'                                              
         GOTO1 VPRINTER                                                         
         LA    R5,OUT                                                           
         OPEN  ((R5),OUTPUT)                                                    
*                                                                               
         L     R3,ARECORD                                                       
THISREC  USING RECD,R3                                                          
         GOTO1 CLEAR,ARECORD                                                    
         GOTO1 CLEAR,AFRSTCPY                                                   
         GOTO1 CLEAR,AFRSTUPD                                                   
         GOTO1 CLEAR,ALASTCPY                                                   
         GOTO1 CLEAR,ALASTUPD                                                   
         MVI   FIRSTFLG,C'Y'                                                    
         MVI   FRSTSTAT,0                                                       
         MVI   LASTSTAT,0                                                       
         MVI   UPDSTAT,0                                                        
*                                                                               
GCON010  BAS   RE,GETNEXT                                                       
         BNE   GCON100                                                          
*                                                                               
         GOTO1 CMPREC,DMCB,(R3),AFRSTUPD                                        
         BE    GCON020                                                          
         CLI   FIRSTFLG,C'Y'                                                    
         BE    GCON030                                                          
         BAS   RE,KEYLAST                                                       
         BAS   RE,KEYFRST                                                       
         B     GCON010                                                          
*                                                                               
GCON020  BAS   RE,KEYNEXT                                                       
         B     GCON010                                                          
*                                                                               
GCON030  MVI   FIRSTFLG,C'N'                                                    
         BAS   RE,KEYFRST                                                       
         B     GCON010                                                          
*                                                                               
GCON100  CLI   FIRSTFLG,C'Y'                                                    
         BE    GCON110                                                          
         BAS   RE,KEYLAST                                                       
*                                                                               
GCON110  LA    R5,OUT                                                           
         CLOSE ((R5))                                                           
         GOTO1 VSORTER,DMCB,=C'END'                                             
         MVC   TITLE(7),FILNAME                                                 
         MVC   TITLE+7(12),=CL12' CONDENSE'                                     
         LA    R3,CNTRS                                                         
         LA    R4,18                                                            
         LA    R5,CNTRSX                                                        
*                                                                               
GCON200  MVC   SPACING,=C'BL02'                                                 
         OI    3(R3),X'0F'                                                      
         UNPK  P+1(8),0(4,R3)                                                   
         MVC   P+11(14),4(R3)                                                   
         GOTO1 VPRINTER                                                         
         BXLE  R3,R4,GCON200                                                    
         B     GCONOK                                                           
*                                                                               
GCONOK   SR    RC,RC                                                            
GCONNO   LTR   RC,RC                                                            
         XIT1                                                                   
                                                                                
***********************************************************************         
* GET CONDENSED RECORDS FROM SORTER, FORMAT INOT RECOVERY RECORDS     *         
* AND WRITE TO OUTPUT FILE                                            *         
***********************************************************************         
SORTOUT  NTR1                                                                   
         MVC   P(40),=CL40'SORTOUT'                                             
         GOTO1 VPRINTER                                                         
         GOTO1 VSORTER,DMCB,SORTCRDO,RECCARD,0                                  
*                                                                               
         LA    R5,OUT                                                           
         OPEN  ((R5),INPUT)                                                     
*                                                                               
         L     R3,ARECORD                                                       
         USING RECD,R3                                                          
         GOTO1 CLEAR,ARECORD                                                    
*                                                                               
SOUT010  LA    R1,OUT                                                           
         GET   (R1),(R3)                                                        
*                                                                               
         GOTO1 VSORTER,DMCB,=C'PUT',RLEN                                        
         B     SOUT010                                                          
*                                                                               
EODADOUT LA    R5,OUT                                                           
         CLOSE ((R5))                                                           
*                                                                               
SOUT020  LA    R5,OUT                                                           
         OPEN  ((R5),OUTPUT)                                                    
*                                                                               
SOUT030  BAS   RE,GETNEXT                                                       
         BNE   SOUT100                                                          
*                                                                               
         LA    R1,OUT                                                           
         LA    R0,RECVHDR-4                                                     
         PUT   (R1),(R0)                                                        
*                                                                               
         B     SOUT030                                                          
*                                                                               
SOUT100  LA    R5,OUT                                                           
         CLOSE ((R5))                                                           
         MVC   TITLE(7),FILNAME                                                 
         MVC   TITLE+7(12),=CL12' CONDENSE'                                     
         LA    R3,CNTRS                                                         
         LA    R4,18                                                            
         LA    R5,CNTRSX                                                        
*                                                                               
SOUT200  MVC   SPACING,=C'BL02'                                                 
         OI    3(R3),X'0F'                                                      
         UNPK  P+1(8),0(4,R3)                                                   
         MVC   P+11(14),4(R3)                                                   
         GOTO1 VPRINTER                                                         
         BXLE  R3,R4,SOUT200                                                    
         B     SOUTOK                                                           
*                                                                               
SOUTOK   SR    RC,RC                                                            
SOUTNO   LTR   RC,RC                                                            
         XIT1                                                                   
                                                                                
***********************************************************************         
* GENERAL INITIALISATION                                              *         
***********************************************************************         
GENINIT  NTR1                                                                   
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         TIME                                                                   
         ST    R0,MVSTIME                                                       
         ST    R1,MVSDATE                                                       
         MVC   CENTURY,=C'20'                                                   
         GOTO1 VDATCON,DMCB,(5,0),(0,TODAY+2)                                   
         MVC   TODAY(2),CENTURY                                                 
         GOTO1 VDATCON,DMCB,(5,0),(2,TODAYC)                                    
         MVI   INTAPE,C'Y'                                                      
         MVI   RETCODE,0                                                        
         MVI   SYSCHAR,C'0'                                                     
         L     RF,=A(RECORD)                                                    
         ST    RF,ARECORD                                                       
         L     RF,=A(FRSTCPY)                                                   
         ST    RF,AFRSTCPY                                                      
         L     RF,=A(FRSTUPD)                                                   
         ST    RF,AFRSTUPD                                                      
         L     RF,=A(LASTCPY)                                                   
         ST    RF,ALASTCPY                                                      
         L     RF,=A(LASTUPD)                                                   
         ST    RF,ALASTUPD                                                      
         L     RF,=V(FILTABL)                                                   
         ST    RF,AFILTAB                                                       
*                                                                               
         MVI   KEYLEN,0                                                         
         MVC   FILNAME,=CL7'       '                                            
         MVI   FILNUM,0                                                         
         XIT1                                                                   
                                                                                
**********************************************************************          
* GET NEXT RECORD FROM SORTER                                        *          
**********************************************************************          
GETNEXT  NTR1                                                                   
         GOTO1 VSORTER,DMCB,=C'GET'                                             
         ICM   R2,15,4(R1)                                                      
         BZ    GNEXNO                                                           
         GOTO1 MOVEREC,DMCB,(R2),ARECORD                                        
         B     GNEXOK                                                           
*                                                                               
GNEXOK   SR    RC,RC                                                            
GNEXNO   LTR   RC,RC                                                            
         XIT1                                                                   
                                                                                
**********************************************************************          
* PROCESS NEXT SORTED RECOVERY RECORDS FOR A GIVEN KEY               *          
**********************************************************************          
KEYNEXT  NTR1                                                                   
         GOTO1 CLEAR,ALASTCPY                                                   
         GOTO1 CLEAR,ALASTUPD                                                   
         CLI   THISREC.RRECTY,RTCPYQ                                            
         BNE   KNEX010                                                          
         GOTO1 MOVEREC,DMCB,ARECORD,ALASTCPY                                    
         BAS   RE,GETNEXT                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   THISREC.RRECTY,RTCHGQ                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 MOVEREC,DMCB,ARECORD,ALASTUPD                                    
         B     KNEX020                                                          
*                                                                               
KNEX010  CLI   THISREC.RRECTY,RTADDQ                                            
         BE    KNEX012                                                          
         OC    THISREC.RSIN,THISREC.RSIN                                        
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   THISREC.RRECTY,RTCHGQ                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P(40),=CL40'KEYNEXT CHG RSIN=0'                                  
         GOTO1 VPRINTER                                                         
*                                                                               
KNEX012  GOTO1 MOVEREC,DMCB,ARECORD,ALASTUPD                                    
*                                                                               
KNEX020  GOTO1 SETSTATE,DMCB,ALASTCPY,ALASTUPD,LASTSTAT                         
         XIT1                                                                   
                                                                                
***********************************************************************         
* PROCESS FIRST SORTED RECOVERY RECORDS FOR A GIVEN KEY               *         
***********************************************************************         
KEYFRST  NTR1                                                                   
         GOTO1 CLEAR,AFRSTCPY                                                   
         GOTO1 CLEAR,AFRSTUPD                                                   
         GOTO1 CLEAR,ALASTCPY                                                   
         GOTO1 CLEAR,ALASTUPD                                                   
         MVI   FRSTSTAT,0                                                       
         MVI   LASTSTAT,0                                                       
         CLI   THISREC.RRECTY,RTCPYQ                                            
         BNE   KFRS010                                                          
         GOTO1 MOVEREC,DMCB,ARECORD,AFRSTCPY                                    
         BAS   RE,GETNEXT                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   THISREC.RRECTY,RTCHGQ                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 MOVEREC,DMCB,ARECORD,AFRSTUPD                                    
         B     KFRS020                                                          
*                                                                               
KFRS010  CLI   THISREC.RRECTY,RTADDQ                                            
         BE    KFRS012                                                          
         OC    THISREC.RSIN,THISREC.RSIN                                        
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   THISREC.RRECTY,RTCHGQ                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P(40),=CL40'KEYFRST CHG RSIN=0'                                  
         GOTO1 VPRINTER                                                         
KFRS012  GOTO1 MOVEREC,DMCB,ARECORD,AFRSTUPD                                    
*                                                                               
KFRS020  GOTO1 SETSTATE,DMCB,AFRSTCPY,AFRSTUPD,FRSTSTAT                         
*                                                                               
KFRSX    XIT1                                                                   
                                                                                
***********************************************************************         
* PROCESS LAST SORTED RECOVERY RECORDS FOR A GIVEN KEY                *         
***********************************************************************         
KEYLAST  NTR1                                                                   
         L     R2,ALASTCPY                                                      
         L     R3,ALASTUPD                                                      
RECCPY   USING RECD,R2                                                          
RECUPD   USING RECD,R3                                                          
         OC    RECUPD.RLEN,RECUPD.RLEN                                          
         BNZ   KLAS010                                                          
         GOTO1 MOVEREC,DMCB,AFRSTCPY,ALASTCPY                                   
         GOTO1 MOVEREC,DMCB,AFRSTUPD,ALASTUPD                                   
         MVC   LASTSTAT,FRSTSTAT                                                
*                                                                               
KLAS010  BAS   RE,GETSTATE                                                      
         ICM   RF,15,WRTROUT                                                    
         BASR  RE,RF                                                            
*                                                                               
KLASX    XIT1                                                                   
         DROP  RECCPY,RECUPD                                                    
                                                                                
***********************************************************************         
* GET UPDATE STATE FOR A GROUP OF RECORDS FOR A GIVEN KEY             *         
***********************************************************************         
GETSTATE NTR1                                                                   
         LA    R2,STATES                                                        
*                                                                               
GSTA010  CLI   0(R2),0                                                          
         BE    GSTAER1                                                          
         CLC   0(1,R2),FRSTSTAT                                                 
         BNE   GSTA020                                                          
         CLC   1(1,R2),LASTSTAT                                                 
         BNE   GSTA020                                                          
         B     GSTA030                                                          
*                                                                               
GSTA020  LA    R2,L'STATES(R2)                                                  
         B     GSTA010                                                          
*                                                                               
GSTA030  MVC   UPDSTAT,2(R2)                                                    
         LA    R2,STATACT                                                       
*                                                                               
GSTA110  CLI   0(R2),0                                                          
         BE    GSTAER2                                                          
         CLC   0(1,R2),UPDSTAT                                                  
         BE    GSTA130                                                          
*                                                                               
GSTA120  LA    R2,L'STATACT(R2)                                                 
         B     GSTA110                                                          
*                                                                               
GSTA130  MVC   WRTROUT,1(R2)                                                    
         B     GSTAOK                                                           
*                                                                               
GSTAER1  MVI   ERROR,STATERQ                                                    
         BAS   RE,ERRPRT                                                        
         DC    H'0'                                                             
*                                                                               
GSTAER2  MVI   ERROR,SACTERQ                                                    
         BAS   RE,ERRPRT                                                        
         DC    H'0'                                                             
*                                                                               
GSTAOK   B     YES                                                              
GSTANO   B     YES                                                              
                                                                                
***********************************************************************         
* WRITE ACTION ROUTINES                                               *         
***********************************************************************         
RECCPY   USING RECD,R2                                                          
RECUPD   USING RECD,R3                                                          
*                                                                               
WRTADD   SR    R5,R5                 WRITE ADD UPDATE STATE                     
         IC    R5,KEYLEN                                                        
         LA    R5,2(R5)                                                         
         L     R3,ALASTUPD                                                      
         LA    RF,RECUPD.RDATA(R5)                                              
         NI    0(RF),X'FF'-DELFLGQ                                              
         MVI   RECUPD.RRECTY,RTADDQ                                             
         GOTO1 WRTREC,ALASTUPD                                                  
WADDX    XIT1                                                                   
*                                                                               
WRTCHG   SR    R5,R5                 WRITE CHANGE UPDATE STATE                  
         IC    R5,KEYLEN                                                        
         LA    R5,2(R5)                                                         
         L     R2,AFRSTCPY                                                      
         L     R3,ALASTUPD                                                      
         OC    RECCPY.RLEN,RECCPY.RLEN                                          
         BNZ   WCHG010                                                          
         MVC   P(40),=CL40'WRTCHG CHG RSIN=0'                                   
         GOTO1 VPRINTER                                                         
         OC    RECUPD.RSIN,RECUPD.RSIN                                          
         BZ    WCHG020                                                          
         DC    H'0'                                                             
*                                                                               
WCHG010  LA    RF,RECCPY.RDATA(R5)                                              
         NI    0(RF),X'FF'-DELFLGQ                                              
         ICM   RF,15,RECUPD.RSEQ                                                
         BCTR  RF,0                                                             
         STCM  RF,15,RECCPY.RSEQ                                                
         GOTO1 WRTREC,AFRSTCPY                                                  
*                                                                               
WCHG020  LA    RF,RECUPD.RDATA(R5)                                              
         NI    0(RF),X'FF'-DELFLGQ                                              
         MVI   RECUPD.RRECTY,RTCHGQ                                             
         GOTO1 WRTREC,ALASTUPD                                                  
         B     WCHGX                                                            
*                                                                               
WCHGX    XIT1                                                                   
*                                                                               
WRTDEL   SR    R5,R5                 WRITE DELETE UPDATE STATE                  
         IC    R5,KEYLEN                                                        
         LA    R5,2(R5)                                                         
         L     R2,AFRSTCPY                                                      
         L     R3,ALASTUPD                                                      
         LA    RF,RECCPY.RDATA(R5)                                              
         NI    0(RF),X'FF'-DELFLGQ                                              
         ICM   RF,15,RECUPD.RSEQ                                                
         BCTR  RF,0                                                             
         STCM  RF,15,RECCPY.RSEQ                                                
         GOTO1 WRTREC,AFRSTCPY                                                  
*                                                                               
         LA    RF,RECUPD.RDATA(R5)                                              
         OI    0(RF),DELFLGQ                                                    
         MVI   RECUPD.RRECTY,RTCHGQ                                             
         GOTO1 WRTREC,ALASTUPD                                                  
         B     WDELX                                                            
*                                                                               
WDELX    XIT1                                                                   
*                                                                               
WRTRES   SR    R5,R5                 WRITE RESTORE UPDATE STATE                 
         IC    R5,KEYLEN                                                        
         LA    R5,2(R5)                                                         
         L     R2,AFRSTCPY                                                      
         L     R3,ALASTUPD                                                      
         LA    RF,RECCPY.RDATA(R5)                                              
         OI    0(RF),DELFLGQ                                                    
         ICM   RF,15,RECUPD.RSEQ                                                
         BCTR  RF,0                                                             
         STCM  RF,15,RECCPY.RSEQ                                                
         GOTO1 WRTREC,ALASTCPY                                                  
*                                                                               
         LA    RF,RECUPD.RDATA(R5)                                              
         NI    0(RF),X'FF'-DELFLGQ                                              
         MVI   RECUPD.RRECTY,RTCHGQ                                             
         GOTO1 WRTREC,ALASTUPD                                                  
         B     WRESX                                                            
*                                                                               
WRESX    XIT1                                                                   
*                                                                               
WRTIGN   MVC   P(40),=CL40'WRTIGN'   WRITE IGNORE UPDATE STATE                  
         GOTO1 VPRINTER                                                         
RECCPY   USING RECD,R2                                                          
RECUPD   USING RECD,R3                                                          
         L     R2,AFRSTCPY                                                      
         L     R3,AFRSTUPD                                                      
         MVC   P(40),=CL40'FRSTCPY'                                             
         MVC   P+10(64),RECCPY.RECD                                             
         GOTO1 VPRINTER                                                         
         MVC   P+10(64),RECCPY.RECD+64                                          
         GOTO1 VPRINTER                                                         
         MVC   P(40),=CL40'FRSTUPD'                                             
         MVC   P+10(64),RECUPD.RECD                                             
         GOTO1 VPRINTER                                                         
         MVC   P+10(64),RECUPD.RECD+64                                          
         GOTO1 VPRINTER                                                         
         L     R2,ALASTCPY                                                      
         L     R3,ALASTUPD                                                      
         MVC   P(40),=CL40'LASTCPY'                                             
         MVC   P+10(64),RECCPY.RECD                                             
         GOTO1 VPRINTER                                                         
         MVC   P+10(64),RECCPY.RECD+64                                          
         GOTO1 VPRINTER                                                         
         MVC   P(40),=CL40'LASTUPD'                                             
         MVC   P+10(64),RECUPD.RECD                                             
         GOTO1 VPRINTER                                                         
         MVC   P+10(64),RECUPD.RECD+64                                          
         GOTO1 VPRINTER                                                         
         MVC   P(40),=CL40'STATES'                                              
         MVC   P+10(1),FRSTSTAT                                                 
         MVC   P+12(1),LASTSTAT                                                 
         MVC   P+14(1),UPDSTAT                                                  
         GOTO1 VPRINTER                                                         
         B     WIGNX                                                            
*                                                                               
WIGNX    XIT1                                                                   
         DROP  RECCPY,RECUPD                                                    
*                                                                               
WRTERR   MVC   P(40),=CL40'WRTERR'   WRITE ERROR UPDATE STATE                   
RECCPY   USING RECD,R2                                                          
RECUPD   USING RECD,R3                                                          
         L     R2,AFRSTCPY                                                      
         L     R3,AFRSTUPD                                                      
         MVC   P(40),=CL40'FRSTCPY'                                             
         MVC   P+10(64),RECCPY.RECD                                             
         GOTO1 VPRINTER                                                         
         MVC   P+10(64),RECCPY.RECD+64                                          
         GOTO1 VPRINTER                                                         
         MVC   P(40),=CL40'FRSTUPD'                                             
         MVC   P+10(64),RECUPD.RECD                                             
         GOTO1 VPRINTER                                                         
         MVC   P+10(64),RECUPD.RECD+64                                          
         GOTO1 VPRINTER                                                         
         L     R2,ALASTCPY                                                      
         L     R3,ALASTUPD                                                      
         MVC   P(40),=CL40'LASTCPY'                                             
         MVC   P+10(64),RECCPY.RECD                                             
         GOTO1 VPRINTER                                                         
         MVC   P+10(64),RECCPY.RECD+64                                          
         GOTO1 VPRINTER                                                         
         MVC   P(40),=CL40'LASTUPD'                                             
         MVC   P+10(64),RECUPD.RECD                                             
         GOTO1 VPRINTER                                                         
         MVC   P+10(64),RECUPD.RECD+64                                          
         GOTO1 VPRINTER                                                         
         MVC   P(40),=CL40'STATES'                                              
         MVC   P+10(1),FRSTSTAT                                                 
         MVC   P+12(1),LASTSTAT                                                 
         MVC   P+14(1),UPDSTAT                                                  
         GOTO1 VPRINTER                                                         
         DC    H'0'                                                             
         B     WERRX                                                            
*                                                                               
WERRX    XIT1                                                                   
         DROP  RECCPY,RECUPD                                                    
                                                                                
***********************************************************************         
* WRITE UDPATE SUMMARY RECORD TO TEMPORARY DATA SET                   *         
* P1 - A(RECORD)                                                      *         
***********************************************************************         
WRTREC   NTR1                                                                   
         L     R2,0(R1)                                                         
         LA    R1,OUT                                                           
         PUT   (R1),(R2)                                                        
         XIT1                                                                   
                                                                                
***********************************************************************         
* CLEAR RECORD                                                        *         
* P1 - A(RECORD)                                                      *         
***********************************************************************         
CLEAR    NTR1                                                                   
         L     RE,0(R1)                                                         
         ICM   RF,15,=AL4(RECDL)                                                
         LA    R0,*                                                             
         L     R1,=F'0'                                                         
         MVCL  RE,R0                                                            
         XIT1                                                                   
                                                                                
***********************************************************************         
* MOVE RECORD BETWEEN STROAGE AREAS                                   *         
* P1 - A(SOURCE RECORD)                                               *         
* P2 - A(DESTINATION RECORD)                                          *         
***********************************************************************         
MOVEREC  NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         LR    RE,R3                                                            
         ICM   RF,15,=AL4(RECDL)                                                
         LA    R0,*                                                             
         L     R1,=F'0'                                                         
         MVCL  RE,R0                                                            
         LR    RE,R3                                                            
         LR    R0,R2                                                            
         ICM   RF,15,0(R2)                                                      
         SRL   RF,16                                                            
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         XIT1                                                                   
                                                                                
***********************************************************************         
* COMPARE RECORD KEYS                                                 *         
* P1 - A(FIRST RECORD)                                                *         
* P2 - A(SECOND RECORD)                                               *         
***********************************************************************         
CMPREC   NTR1                                                                   
         LM    R4,R5,0(R1)                                                      
REC1     USING RECD,R4                                                          
REC2     USING RECD,R5                                                          
         SR    RF,RF                                                            
         IC    RF,KEYLEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   REC1.ROLDKEY(0),REC2.ROLDKEY                                     
         XIT1                                                                   
         DROP  REC1,REC2                                                        
                                                                                
***********************************************************************         
* SET UPDATE STATE CODE FOR RECOVERY RECORD PAIR                      *         
* P1 - A(COPY RECORD OR ZEROES)                                       *         
* P2 - A(UPDATE RECORD)                                               *         
* P3 - A(STATE CODE) SET ON RETURN                                    *         
***********************************************************************         
SETSTATE NTR1                                                                   
         LM    R2,R4,0(R1)                                                      
RECCPY   USING RECD,R2                                                          
RECUPD   USING RECD,R3                                                          
         SR    R5,R5                                                            
         IC    R5,KEYLEN                                                        
         LA    R5,2(R5)                                                         
         OC    RECCPY.RLEN,RECCPY.RLEN                                          
         BZ    SSTA100                                                          
         LA    RF,RECCPY.RDATA(R5)                                              
         TM    0(RF),DELFLGQ                                                    
         BNZ   SSTA010                                                          
         LA    RF,RECUPD.RDATA(R5)                                              
         TM    0(RF),DELFLGQ                                                    
         BNZ   SSTA020                                                          
         MVI   0(R4),C'C'                                                       
         B     SSTAOK                                                           
*                                                                               
SSTA010  LA    RF,RECUPD.RDATA(R5)                                              
         TM    0(RF),DELFLGQ                                                    
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVI   0(R4),C'R'                                                       
         B     SSTAOK                                                           
*                                                                               
SSTA020  LA    RF,RECCPY.RDATA(R5)                                              
         TM    0(RF),DELFLGQ                                                    
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVI   0(R4),C'D'                                                       
         B     SSTAOK                                                           
*                                                                               
SSTA100  LA    RF,RECUPD.RDATA(R5)                                              
         TM    0(RF),DELFLGQ                                                    
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   RECUPD.RRECTY,RTADDQ                                             
         BNE   SSTA110                                                          
         MVI   0(R4),C'A'                                                       
         B     SSTAOK                                                           
*                                                                               
SSTA110  OC    RECUPD.RSIN,RECUPD.RSIN                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   RECUPD.RRECTY,RTCHGQ                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P(40),=CL40'SETSTATE CHG RSIN=0'                                 
         GOTO1 VPRINTER                                                         
         MVI   0(R4),C'C'                                                       
         B     SSTAOK                                                           
*                                                                               
SSTAOK   B     YES                                                              
         DROP  RECCPY,RECUPD                                                    
                                                                                
***********************************************************************         
* VALIDATE JCL CARD DATA LINES AS DEFINED IN CARDTBL                  *         
***********************************************************************         
VALCARDS NTR1                                                                   
         MVC   P(40),=CL40'VALCARDS'                                            
         GOTO1 VPRINTER                                                         
*                                                                               
VCLP1    GOTO1 VCARDS,DMCB,P,=C'RE00'                                           
         CLC   =C'/*',P            IF END OF JCL CHK REQ CARDS INPUT            
         BE    VCEND                                                            
*                                                                               
         LA    RE,CARDTBL          INITIALISE TABLE POINTER                     
*                                                                               
VCLP2    CLI   0(RE),0             END OF TABLE                                 
         BE    VCERR1              CARD NOT IN TABLE                            
         SR    RF,RF                                                            
         IC    RF,CLENGTH(RE)                                                   
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   P(0),CSTRING(RE)    COMPARE STRING                               
         BE    VCLP2X                                                           
         LA    RE,L'CARDTBL(RE)    GET NEXT ENTRY                               
         B     VCLP2                                                            
*                                                                               
VCLP2X   SR    RF,RF               MATCH FOUND                                  
         IC    RF,CROUTINE(RE)                                                  
         SLL   RF,2                                                             
         B     *+0(RF)             BRANCH TO PROCESSING ROUTINE                 
*                                                                               
         B     VCSYSTEM                                                         
         B     VCINPUT                                                          
         B     VCPATCH                                                          
         B     VCDDSIO                                                          
         B     VCFILE                                                           
         B     VCDSPACE                                                         
*                                                                               
VCEND    CLI   SYSTEM,0            CHECK SYSTEM ENTERED                         
         NOP   VCERR2                                                           
         CLI   FILNUM,0            CHECK FILE NUMBER ENTERED                    
         BE    VCERR4                                                           
         B     VCOK                                                             
*                                                                               
VCERR1   MVI   ERROR,VCER1Q        INVALID CARD                                 
         B     VCNO                                                             
VCERR2   MVI   ERROR,VCER2Q        MISSING SYSTEM DEFINITION                    
         B     VCNO                                                             
VCERR3   MVI   ERROR,VCER3Q        INVALID SYSTEM NAME                          
         B     VCNO                                                             
VCERR4   MVI   ERROR,VCER4Q        MISSING FILE NUMBER                          
         B     VCNO                                                             
*                                                                               
VCNO     B     NO                  EXIT ERROR CONDITION                         
*                                                                               
VCOK     B     YES                 EXIT OK                                      
                                                                                
***********************************************************************         
* ROUTINES TO PROCESS EACH JCL CARD DATA LINE                         *         
***********************************************************************         
VCSYSTEM MVC   SYSCARD,=CL10'SYS='  SYSTEM=XXXYY OR SYS=XXXYY                   
         MVC   SYSCARD+4(5),P+4                                                 
         CLI   P+3,C'='                                                         
         BE    *+10                                                             
         MVC   SYSCARD+4(5),P+7                                                 
*                                                                               
         GOTO1 =V(DMDDNAME),DMCB,(X'24',DMDDNAME),SYSCARD                       
         TM    8(R1),X'10'                                                      
         BO    VCERR1                                                           
         L     RF,8(R1)                                                         
         USING DDNADATA,RF                                                      
*                                                                               
         MVC   SENUM,DDNASENO      SYSTEM SE NUMBER                             
         MVC   SYSCHAR,DDNASENA+3  SYSTEM ONE OR TWO CHR SUBID                  
         MVC   SYSCODE,DDNASEN3    SYSTEM ONE CHR LETTER                        
         MVC   SYSTEM,DDNASYNO     SYSTEM NUMBER                                
         B     VCLP1                                                            
                                                                                
VCINPUT  CLC   P+6(4),=C'TAPE'     INPUT=                                       
         BNE   VCERR1                                                           
         MVI   INTAPE,C'Y'                                                      
         B     VCLP1                                                            
                                                                                
VCPATCH  XC    FULL,FULL           PATCH=111111 11                              
         GOTO1 VHEXIN,DMCB,P+6,FULL+1,6                                         
         CLC   DMCB+12(4),=F'3'                                                 
         BNE   VCERR1              MUST BE 6 HEX DIGITS                         
         LA    R4,P+71             FIND LENGTH OF PATCH DATA                    
         LH    RE,=H'-1'                                                        
         LA    RF,P+12                                                          
         CLI   0(R4),C' '                                                       
         BNE   *+12                                                             
         BXH   R4,RE,*-8                                                        
         B     VCERR1                                                           
         SR    R4,RF               L'PATCH DATA IN R4                           
         GOTO1 VHEXIN,DMCB,P+13,WORK,(R4)                                       
         ICM   R1,15,DMCB+12       GET L'HEX PATCH DATA IN R1                   
         BZ    VCERR1              ZERO IS NOT ALLOWED                          
         BCTR  R1,0                                                             
         L     RF,FULL             PATCH DISPLACEMENT IN RF                     
         AR    RF,RB               RF = A(AREA TO BE PATCHED)                   
         EX    R1,*+8              MOVE IN THE PATCH DATA                       
         B     VCLP1                                                            
         MVC   0(0,RF),WORK                                                     
                                                                                
VCDDSIO  L     RF,=V(DDSIO)        DDSIO=                                       
         LTR   RF,RF                                                            
         BZ    VCERR1                                                           
         MVC   0(8,RF),P+6                                                      
         B     VCLP1                                                            
                                                                                
VCFILE   GOTO1 VHEXIN,DMCB,P+5,FILNUM,2,0 FILE=                                 
         CLC   12(4,R1),=F'1'                                                   
         BNE   VCERR1                                                           
         L     R3,AFILTAB                                                       
         USING FILTABD,R3                                                       
         LA    R3,DMFLLEN(R3)                                                   
*                                                                               
VCFI010  CLI   DMFLNUM,0                                                        
         BE    VCERR1                                                           
         CLC   DMFLNUM,FILNUM                                                   
         BE    VCFI020                                                          
         LA    R3,DMFLLEN(R3)                                                   
         B     VCFI010                                                          
*                                                                               
VCFI020  MVC   KEYLEN,DMFLKEYL                                                  
         MVC   FILNAME,DMFLNAME                                                 
         B     VCLP1                                                            
                                                                                
VCDSPACE MVC   DSPACE,P+7          DSPACE=                                      
         B     VCLP1                                                            
                                                                                
PACKIN   SR    R1,R1               GET STRING LENGTH                            
         LA    R0,8                                                             
*                                                                               
PIN010   CLI   0(RF),C' '                                                       
         BE    PINOK                                                            
         CLI   0(RF),C'0'                                                       
         BL    PINNO                                                            
         CLI   0(RF),C'9'                                                       
         BH    PINNO                                                            
         LA    RF,1(RF)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,PIN010                                                        
         B     PINNO                                                            
*                                                                               
PINNO    SR    R1,R1                                                            
PINOK    LTR   R1,R1                                                            
         BR    RE                                                               
                                                                                
***********************************************************************         
* OPEN SYSTEM FILES                                                   *         
***********************************************************************         
OPENFIL  NTR1                                                                   
         MVC   P(40),=CL40'OPENFIL'                                             
         GOTO1 VPRINTER                                                         
*                                                                               
         CLI   INTAPE,C'Y'         OPEN TAPE FILE                               
         BNE   OFIL010                                                          
         OPEN  (RCVTAPE,(INPUT))                                                
         LA    RE,RCVTAPE                                                       
         LA    RF,OUT                                                           
         MVC   62(2,RF),62(RE)     SET OUTPUT TAPE BLKSIZE FROM INPUT           
         B     OFIL020                                                          
*                                  SET UTL SENUM                                
OFIL010  L     RE,=V(UTL)                                                       
         MVC   4(1,RE),SENUM                                                    
*                                                                               
         GOTO1 VDMOD000,DMCB,A(DMODOSYS),(SENUM,IOL)                            
*                                                                               
OFIL020  B     OFILOK                                                           
*                                                                               
OFILNO   MVI   ERROR,OFILERQ                                                    
         B     NO                                                               
OFILOK   B     YES                                                              
                                                                                
***********************************************************************         
* CLOSE SYSTEM FILES                                                  *         
***********************************************************************         
CLOSEFIL NTR1                                                                   
         MVC   P(40),=CL40'CLOSEFIL'                                            
         GOTO1 VPRINTER                                                         
*                                                                               
         CLI   INTAPE,C'Y'         CLOSE TAPE FILE                              
         BNE   CFIL010                                                          
         CLOSE (RCVTAPE)                                                        
         B     CFIL020                                                          
*                                                                               
CFIL010  L     RE,=V(UTL)          SET UTL SENUM                                
         MVC   4(1,RE),SENUM                                                    
*                                                                               
         GOTO1 VDMOD000,DMCB,A(DMODCSYS),(SENUM,IOL)                            
*                                                                               
CFIL020  B     CFILOK                                                           
*                                                                               
CFIL030  B     CFILOK                                                           
*                                                                               
CFIL040  B     CFILOK                                                           
*                                                                               
CFILNO   MVI   ERROR,CFILERQ                                                    
         B     NO                                                               
CFILOK   B     YES                                                              
                                                                                
***********************************************************************         
* PRINT ERROR MESSAGE                                                 *         
***********************************************************************         
ERRPRT   NTR1                                                                   
         LA    R3,ERRTAB                                                        
         SR    RF,RF                                                            
         ICM   RF,1,ERROR                                                       
         MH    RF,=H'40'                                                        
         AR    R3,RF                                                            
         MVC   P,SPACES                                                         
         GOTO1 VPRINTER                                                         
         MVC   P+13(10),=C'ERROR CODE'                                          
         MVC   P+23(L'ERRTAB),0(R3)                                             
         GOTO1 VPRINTER                                                         
         DC    H'00'                                                            
         XIT1                                                                   
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
         XIT1                                                                   
                                                                                
         LTORG                                                                  
                                                                                
         DS    0H                                                               
ERRTAB   DS    0CL40               ERROR REPORT STRINGS                         
         DC    CL40'000 UNDEFINED ERROR'                                        
         DC    CL40'001 INVALID CONTROL CARD INPUT'                             
         DC    CL40'002 MISSING SYSTEM DEFINITION'                              
         DC    CL40'003 INVALID SYSTEM NAME'                                    
         DC    CL40'004 MISSING FILE NUMBER'                                    
         DC    CL40'005 UNDEFINED ERROR'                                        
         DC    CL40'006 OPEN FILES ERROR'                                       
         DC    CL40'007 CLOSE FILES ERROR'                                      
         DC    CL40'008 STATES TABLE ERROR'                                     
         DC    CL40'009 STATACT TABLE ERROR'                                    
VCER1Q   EQU   1                                                                
VCER2Q   EQU   2                                                                
VCER3Q   EQU   3                                                                
VCER4Q   EQU   4                                                                
UNDEFERQ EQU   5                                                                
OFILERQ  EQU   6                                                                
CFILERQ  EQU   7                                                                
STATERQ  EQU   8                                                                
SACTERQ  EQU   9                                                                
*                                                                               
SORTCRDI DC    CL80'SORT FIELDS=(5,64,A),FORMAT=BI,WORK=1'                      
SORTCRDO DC    CL80'SORT FIELDS=(65,4,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=V,LENGTH=(8300,,,,)'                            
*                                                                               
CNTRS    DS    0D                                                               
RCVADD   DC    PL4'0',CL14'RECOVERY ADDS'                                       
RCVCPY   DC    PL4'0',CL14'RECOVERY CPYS'                                       
RCVCHG   DC    PL4'0',CL14'RECOVERY CHGS'                                       
CHGKEY   DC    PL4'0',CL14'CHANGED KEYS'                                        
REUSEKEY DC    PL4'0',CL14'RE-USED KEYS'                                        
FILIN    DC    PL4'0',CL14'FILE IN'                                             
FILDEL   DC    PL4'0',CL14'FILE DELETES'                                        
FILOUT   DC    PL4'0',CL14'FILE OUT'                                            
CNTRSX   EQU   *-1                                                              
*                                                                               
DMDDNAME DC    CL8'DMDDNAME'                                                    
VDATAMGR DC    V(DATAMGR)                                                       
VDMOD000 DC    V(DMOD000)                                                       
VLOGIO   DC    V(LOGIO)                                                         
VCPRINT  DC    V(CPRINT)                                                        
VPRINT   DC    V(PRINT)                                                         
VPRINTER DC    V(PRINTER)                                                       
VSORTER  DC    V(SORTER)                                                        
VHEXOUT  DC    V(HEXOUT)                                                        
VHEXIN   DC    V(HEXIN)                                                         
VCARDS   DC    V(CARDS)                                                         
VNUMVAL  DC    V(NUMVAL)                                                        
VTIMBER  DC    V(TIMBER)                                                        
VDATVAL  DC    V(DATVAL)                                                        
VDATCON  DC    V(DATCON)                                                        
VPERVAL  DC    V(PERVAL)                                                        
*                                                                               
SAVSIN   DC    F'0'                                                             
MYSEQ    DC    F'0'                                                             
CHKSIN   DC    C'Y'                                                             
*                                                                               
RTCPYQ   EQU   X'01'               COPY RECOVERY TRANSACTION CODE               
RTCHGQ   EQU   X'02'               CHANGE RECOVERY TRANSACTION CODE             
RTADDQ   EQU   X'03'               ADD RECOVERY TRANSACTION CODE                
DELFLGQ  EQU   X'80'               LOGICAL RECORD STATUS DELETE FLAG            
                                                                                
* UPDATE STATE TRANSITION MATRIX                                                
* THREE RELATED UPDATE STATE CODE FIELDS - FIRST, LAST AND RESULT               
* WHERE POSSIBLE UPDATE STATE CODES ARE:                                        
* C'A' = ADD                                                                    
* C'C' = CHANGE                                                                 
* C'D' = DELETE                                                                 
* C'R' = RESTORE                                                                
* C'I' = IGNORE                                                                 
* C'E' = ERROR                                                                  
*                                                                               
         DS    0D                                                               
STATES   DS    0CL3                                                             
*                                                                               
         DC    C'A',C'A',C'A'                                                   
         DC    C'A',C'C',C'A'                                                   
         DC    C'A',C'D',C'I'                                                   
         DC    C'A',C'R',C'A'                                                   
*                                                                               
         DC    C'C',C'C',C'C'                                                   
         DC    C'C',C'D',C'D'                                                   
         DC    C'C',C'R',C'C'                                                   
         DC    C'C',C'A',C'E'                                                   
*                                                                               
         DC    C'D',C'D',C'D'                                                   
         DC    C'D',C'C',C'C'                                                   
         DC    C'D',C'R',C'C'                                                   
         DC    C'D',C'A',C'E'                                                   
*                                                                               
         DC    C'R',C'R',C'R'                                                   
         DC    C'R',C'C',C'R'                                                   
         DC    C'R',C'D',C'I'                                                   
         DC    C'R',C'A',C'E'                                                   
*                                                                               
         DC    X'00'                                                            
                                                                                
* UPDATE STATE WRITE ACTION TABLE                                               
* STATE CODE + A(WRITE ACTION ROUTINE)                                          
*                                                                               
         DS    0D                                                               
STATACT  DS    0CL5                                                             
*                                                                               
         DC    C'A',AL4(WRTADD)                                                 
         DC    C'C',AL4(WRTCHG)                                                 
         DC    C'D',AL4(WRTDEL)                                                 
         DC    C'R',AL4(WRTRES)                                                 
         DC    C'I',AL4(WRTIGN)                                                 
         DC    C'E',AL4(WRTERR)                                                 
*                                                                               
         DC    X'00'                                                            
                                                                                
SNUMLIST DC    C' 123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ',X'00'                    
                                                                                
* CARDTBL DEFINES JOB CONTROL INPUT PARAMETER CARDS                             
* AL1 LENGTH OF CARD NAME                                                       
* AL1 ACTION ROUTINE NUMBER                                                     
* XL1 ACTION FLAGS                                                              
* CL8 CARD NAME                                                                 
*                                                                               
CARDTBL  DS    0CL14                                                            
         DC    AL1(07,01),X'00',CL11'SYSTEM='                                   
         DC    AL1(04,01),X'00',CL11'SYS='                                      
         DC    AL1(06,02),X'00',CL11'INPUT='                                    
         DC    AL1(06,03),X'00',CL11'PATCH='                                    
         DC    AL1(06,04),X'00',CL11'DDSIO='                                    
         DC    AL1(05,05),X'00',CL11'FILE='                                     
         DC    AL1(07,06),X'00',CL11'DSPACE='                                   
CARDTBLX DC    AL1(00)                                                          
CLENGTH  EQU   0                                                                
CROUTINE EQU   1                                                                
CFLAG    EQU   2                                                                
CSTRING  EQU   3                                                                
                                                                                
RCVTAPE  DCB   DDNAME=RCVTAPE,DSORG=PS,MACRF=(GM),RECFM=VB,            X        
               BUFNO=2,EODAD=EODADRCV                                           
*                                                                               
OUT      DCB   DDNAME=OUT,DSORG=PS,MACRF=(PM,GM),RECFM=VB,             X        
               BUFNO=2,LRECL=8200,EODAD=EODADOUT                                
                                                                                
RECWK    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
*                                                                               
FILEFLAG DS    XL1                 RECOVERY FILE FLAG                           
*                                                                               
MVSPARM  DS    XL1                 MVS INPUT PARAMETERS                         
MVSTIME  DS    F                   IBM TIME BINARY 100S SECS                    
MVSDATE  DS    F                   IBM DATE JULIAN                              
CENTURY  DS    CL2                 CURRENT CENTURY EBCDIC VALUE                 
TODAY    DS    CL8                 TODAYS DATE EBCDIC                           
TODAYC   DS    XL2                 TODAYS DATE COMPRESSED                       
TIMENOW  DS    CL8                 TIME NOW EBCDIC                              
*                                                                               
SYSCARD  DS    CL10                SYS=CARD                                     
SYSTEM   DS    XL1                 SYSTEM BINARY CODE                           
SYSCODE  DS    CL1                 SYSTEM CHARACTER CODE                        
SYSCHAR  DS    CL2                 SUB SYSTEM CHARACTER CODE                    
DSPACE   DS    CL1                 DSPACE=CARD                                  
*                                                                               
SENUM    DS    XL1                 SYSTEM SE NUMBER                             
INTAPE   DS    CL1                 RECOVERY FILE ON TAPE FLAG                   
MSGAREA  DS    CL10                                                             
RETCODE  DS    XL1                                                              
ERROR    DS    XL1                                                              
*                                                                               
KEYLENQ  EQU   60                  MAXIMUM ALLOWED LENGTH FOR KEY               
KEYLEN   DS    XL1                                                              
FILNUM   DS    XL1                                                              
FILNAME  DS    CL7                                                              
*                                                                               
FIRSTFLG DS    CL1                                                              
FRSTSTAT DS    CL1                                                              
LASTSTAT DS    CL1                                                              
CURRSTAT DS    CL1                                                              
UPDSTAT  DS    CL1                                                              
*                                                                               
WRTROUT  DS    A                                                                
*                                                                               
AFILTAB  DS    A                                                                
ARECORD  DS    A                                                                
AFRSTCPY DS    A                                                                
AFRSTUPD DS    A                                                                
ALASTCPY DS    A                                                                
ALASTUPD DS    A                                                                
*                                                                               
WORK     DS    XL256               GENERAL WORK AREA                            
ELEMENT  DS    XL256               RECORD ELEMENT WORK AREA                     
IOKEYSV  DS    CL32                SAVE RECORD KEY BUFFER                       
IOKEY    DS    CL32                RECORD KEY BUFFER                            
IOL      DS    F                   GENERAL IO AREA                              
IO       DS    2048X                                                            
*                                                                               
RECWKX   EQU   *                                                                
                                                                                
RECORD   CSECT                                                                  
         DC    (RECDL)X'00'                                                     
*                                                                               
FRSTCPY  CSECT                                                                  
         DC    (RECDL)X'00'                                                     
*                                                                               
FRSTUPD  CSECT                                                                  
         DC    (RECDL)X'00'                                                     
*                                                                               
LASTCPY  CSECT                                                                  
         DC    (RECDL)X'00'                                                     
*                                                                               
LASTUPD  CSECT                                                                  
         DC    (RECDL)X'00'                                                     
                                                                                
RECD     DS    0D                  RECOVERY RECORD                              
RLEN     DS    H                   LOGICAL IOCS LEN                             
         DS    H                                                                
ROLDKEY  DS    CL(KEYLENQ)                                                      
RSEQ     DS    XL4                                                              
RSPARE   DS    XL4                 4 FOR ORIGINAL LENGTH                        
       ++INCLUDE DMRCVRHDR                                                      
RDATA    DS    8192C               8K MAX LOGICAL RECORD                        
       ++INCLUDE DMRCVREXT                                                      
RECDX    EQU   *                                                                
RECDL    EQU   *-RECD                                                           
                                                                                
* DDDPRINT                                                                      
       ++INCLUDE DDDPRINT                                                       
* DMGREQUS                                                                      
       ++INCLUDE DMGREQUS                                                       
* DMDDNAMED                                                                     
       ++INCLUDE DMDDNAMED                                                      
* DMFILTABD                                                                     
       ++INCLUDE DMFILTABD                                                      
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002DDRECON   08/31/11'                                      
         END                                                                    
