*          DATA SET REPRX30X   AT LEVEL 211 AS OF 08/31/00                      
*PHASE T81A30C                                                                  
*                                                                               
T81A30   TITLE 'REPRP30 - REACH AND FREQUENCY ROUTINES'                         
*********************************************************************           
*                                                                     *         
*                                                                     *         
*---------------------------------------------------------------------*         
*  HISTORY AS OF 10/7/99                                              *         
*                                                                     *         
* 03/08/2000   JRD   DON'T DUMP WHEN THERE ARE NO BOOKS ON FILE       *         
*                                                                     *         
***********************************************************************         
         PRINT NOGEN                                                            
T81A30   CSECT                                                                  
         NMOD1 OVERWRKQ,*T81A30*,RR=RE,CLEAR=YES                                
         LR    R9,RC                                                            
         USING OVERWRKD,R9                                                      
         L     RC,0(R1)                                                         
         USING WORKD,RC                                                         
         L     RA,AMAPTAB                                                       
         USING MAPTABD,RA                                                       
DB       USING DBLOCK,MYDBLOCK                                                  
*                                                                               
         MVC   OVPARMS,0(R1)                                                    
         ST    RE,OVRELO                                                        
*                                                                               
         L     RE,ACOMFACS                                                      
         L     R8,CDEMAND-COMFACSD(RE)                                          
         ST    R8,VDEMAND                                                       
         L     R8,CDEMOUT-COMFACSD(RE)                                          
         ST    R8,VDEMOUT                                                       
*                                                                               
         LR    R8,RB                                                            
         AH    R8,=Y(COMMON-T81A30)                                             
         USING COMMON,R8                                                        
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
         DC    A(HARRIS)                                                        
ROUTSN   EQU   (*-ROUTS)/4                                                      
         EJECT                                                                  
COMMON   DS    0D                                                               
***********************************************************************         
* EXITS                                                                         
***********************************************************************         
ETOOBIG  MVC   ERROR,=Y(804)                                                    
         B     EXITL                                                            
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         SPACE 1                                                                
EXIT     DS    0H                                                               
         XIT1  ,                   EXIT WITH CC SET                             
DIE      DC    H'0'                                                             
***********************************************************************         
* CHECK FOR A HISPANIC STATION CALL LETTER SWITCH                               
*                                                                               
*  R1 A(STATION CALL LETTERS)                                                   
*                                                                               
*  TEL-H --> TELE-T ON EJOR(B3) AND TELEMUNDO(B1)                               
*  KOB1 --> KOB 1 ON PETRY(PV)                                                  
*                                                                               
* IMPORTANT!!!! MUST MATCH SWITCH IN REPRP00!!!!!!!!!!!!!!!                     
*                                                                               
***********************************************************************         
SWHISP   DS    0H                                                               
         CLC   REPALPHA,=C'B3'     TELE-H                                       
         BE    *+14                                                             
         CLC   REPALPHA,=C'B1'                                                  
         BNE   SWH010                                                           
*                                                                               
         CLC   =C'TEL H',0(R1)                                                  
         BNE   SWH010                                                           
         MVC   0(5,R1),=C'TELE '                                                
         B     SWHISPX                                                          
*                                                                               
SWH010   DS    0H                                                               
         CLC   REPALPHA,=C'PV'                                                  
         BNE   SWH020                                                           
*                                                                               
         CLC   =C'KOB 1',0(R1)                                                  
         BNE   SWH020                                                           
         MVC   0(5,R1),=C'KOB1 '                                                
         B     SWHISPX                                                          
*                                                                               
SWH020   DS    0H                                                               
*                                                                               
SWHISPX  DS    0H                                                               
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                          
***********************************************************************         
         LTORG                                                                  
*                                                                               
QTRS     DC    X'0205070B0205070B'                                              
*                                                                               
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
*                                                                               
*                                                                               
STALENQ  EQU   5                                                                
DPTLENQ  EQU   5                                                                
DEMLENQ  EQU   3                                                                
BKLENQ   EQU   5                                                                
UPGLENQ  EQU   11+14+1                                                          
RCDLENQ  EQU   8+1+1                                                            
FLTLENQ  EQU   6                                                                
         EJECT                                                                  
*********************************************************************           
* R/F FOR THE HARRIS MODEL                                                      
*********************************************************************           
HARRIS   NTR1  BASE=*,LABEL=*                                                   
         L     R2,OVPARMS+4        SAVE OFF PARAMETER INFO                      
         LA    R2,VHPARMLQ(R2)                                                  
*                                                                               
         ZIC   RE,0(R2)            SKIP BOOKS                                   
         MHI   RE,BKLENQ                                                        
         LA    R2,1(RE,R2)                                                      
*                                                                               
         CLI   0(R2),0                                                          
         BE    HARRISX             NO DEMOS QUIT                                
*                                                                               
         MVC   NUMDEMS,0(R2)       NUMBER OF DEMOS                              
         LA    R2,1(R2)                                                         
         LR    RF,R2                                                            
         S     RF,OVPARMS+4        DISP. TO 1ST DEMO                            
         ST    RF,FRSTDEM                                                       
         ZIC   RF,NUMDEMS                                                       
         MH    RF,=Y(DEMLENQ)      LENGTH OF DEMOS                              
         BCTR  RF,0                -1 FOR EX                                    
         EX    RF,*+4                                                           
         MVC   DEMOS(0),0(R2)                                                   
         LA    R2,1(RF,R2)         BUMP TO RATECARDS                            
         LA    RF,DEMOS+1(RF)      POINT TO END OF DEMO LIST                    
         MVI   0(RF),X'FF'                                                      
*                                                                               
         MVC   CUMES,DEMOS                                                      
         LA    RF,CUMES            CONVERT RATINGS TO CUMES                     
HR0010   DS    0H                                                               
         CLI   0(RF),X'FF'                                                      
         BE    HR0012                                                           
         CLI   1(RF),C'R'                                                       
         BNE   *+8                                                              
         MVI   1(RF),C'C'                                                       
         LA    RF,3(RF)                                                         
         B     HR0010                                                           
*                                                                               
HR0012   DS    0H                                                               
         ZIC   RE,0(R2)            SKIP DAYPARTS                                
         MHI   RE,DPTLENQ                                                       
         LA    R2,1(RE,R2)                                                      
         ZIC   RE,0(R2)            SKIP COMMENTS                                
         MHI   RE,60                                                            
         LA    R2,1(RE,R2)                                                      
*                                                                               
         MVC   NUMSTAS,0(R2)       NUMBER OF STATIONS                           
         LA    R2,1(R2)                                                         
         LR    RF,R2                                                            
         S     RF,OVPARMS+4        DISP. TO 1ST STATION                         
         ST    RF,FRSTSTA                                                       
         ZIC   RF,NUMSTAS                                                       
         MH    RF,=Y(STALENQ)                                                   
         AR    R2,RF                                                            
*                                                                               
         L     R2,OVPARMS+4        DETERMINE BROADCAST QUARTERS                 
         USING VHPARMD,R2                                                       
         GOTO1 VDATCON,DMCB,(8,VHPFLS),(0,WORK+6)                               
         GOTO1 VGTBROAD,DMCB,(1,WORK+6),WORK+12,VGETDAY,VADDAY                  
         GOTO1 VDATCON,DMCB,(0,WORK+18),(3,WORK)                                
*                                                                               
         GOTO1 VDATCON,DMCB,(8,VHPFLE),(0,WORK+6)                               
         GOTO1 VGTBROAD,DMCB,(1,WORK+6),WORK+12,VGETDAY,VADDAY                  
         GOTO1 VDATCON,DMCB,(0,WORK+18),(3,WORK+3)                              
         DROP  R2                                                               
*                                                                               
         MVC   STYEAR,WORK         SAVE START YEAR                              
         MVC   ENYEAR,WORK+3       SAVE END YEAR                                
*                                                                               
         ZIC   RF,WORK+1                                                        
         SR    RE,RE                                                            
         LA    R0,3                                                             
         DR    RE,R0                                                            
         LTR   RE,RE               ADD 1 IF THERES A REMAINDER                  
         BZ    *+8                                                              
         LA    RF,1(RF)                                                         
         STC   RF,STQTR            STORE STARTING QUARTER                       
*                                                                               
         ZIC   RF,WORK+4                                                        
         SR    RE,RE                                                            
         LA    R0,3                                                             
         DR    RE,R0                                                            
         LTR   RE,RE               ADD 1 IF THERES A REMAINDER                  
         BZ    *+8                                                              
         LA    RF,1(RF)                                                         
         STC   RF,ENQTR            STORE END QUARTER                            
*                                                                               
         ZIC   RE,STQTR            DETERMINE NUMBER OF QUARTERS                 
         BCTR  RE,0                INCLUSIVE                                    
         SR    RF,RE                                                            
         CLC   STYEAR,ENYEAR                                                    
         BE    *+8                                                              
         AHI   RF,4                                                             
         STC   RF,NUMQTRS                                                       
         EJECT                                                                  
*------------------------------------------------------------                   
* BUILD LIST OF VALID STATIONS                                                  
*------------------------------------------------------------                   
         L     RE,AIO2                                                          
         LHI   RF,LENIO             IN THE MARKET IN AIO2                       
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         XC    DB.DBLOCK,DB.DBLOCK                                              
         MVC   DB.DBAREC,AIO1                                                   
         MVC   DB.DBSELAGY,REPALPHA                                             
         MVC   DB.DBFILE,=C'TP '                                                
         MVC   DB.DBCOMFCS,ACOMFACS                                             
         MVI   DB.DBSELMED,C'T'                                                 
         MVC   DB.DBSELSRC,RTSRVC                                               
*                                                                               
         L     RE,OVPARMS+4                                                     
         USING VHPARMD,RE                                                       
         MVC   DB.DBSELSTA,VHPSTA                                               
         DROP  RE                                                               
         CLI   DB.DBSELSTA+4,C' '                                               
         BH    *+8                                                              
         MVI   DB.DBSELSTA+4,C'T'                                               
*                                                                               
         MVI   DB.DBFUNCT,DBGETMK  ELSE,GET THE RATING SERVICE MARKET           
         GOTO1 VDEMAND,DMCB,DB.DBLOCK,0,0                                       
*                                                                               
         OC    DB.DBACTRMK,DB.DBACTRMK                                          
         BZ    HARRISX             NO DEMO INFO AVAILABLE                       
*                                                                               
         MVC   MARKET,DB.DBACTRMK                                               
*                                                                               
*        TEMPORARY KLUGE TO PROTECT NIELSEN MARKET DATA FROM                    
*          LOCAL STATIONS USING AN UMBRELLA AGENCY CODE                         
                                                                                
         BAS   RE,VAL4MKT          IS THIS USERID/MKT ALLOWED?                  
         BNE   HARRISX             NO, DO NOT RETRIEVE DEMOS                    
*                                                                               
         XC    DB.DBLOCK,DB.DBLOCK                                              
         MVC   DB.DBSELAGY,REPALPHA                                             
         MVC   DB.DBAREC,AIO1                                                   
         MVC   DB.DBFILE,=C'TP '                                                
         MVC   DB.DBCOMFCS,ACOMFACS                                             
         MVI   DB.DBSELMED,C'T'                                                 
         MVC   DB.DBSELSRC,RTSRVC                                               
         MVI   DB.DBFUNCT,DBGETTLB    GET THE LATEST BOOK                       
         EDIT  (B2,MARKET),(4,DB.DBSELSTA),FILL=0                               
         MVI   DB.DBSELSTA+4,C'T'                                               
*                                                                               
         GOTO1 VDEMAND,DMCB,DB.DBLOCK,BOOKHOOK,0                                
*                                                                               
         XC    DB.DBLOCK,DB.DBLOCK                                              
         MVC   DB.DBSELAGY,REPALPHA                                             
         MVC   DB.DBAREC,AIO1                                                   
         MVC   DB.DBFILE,=C'TP '                                                
         MVC   DB.DBCOMFCS,ACOMFACS                                             
         MVI   DB.DBSELMED,C'T'                                                 
         MVC   DB.DBSELSRC,RTSRVC                                               
         MVI   DB.DBFUNCT,DBGETMS     GET THE STATIONS                          
         MVC   DB.DBSELRMK,MARKET                                               
         MVC   DB.DBSELBK,LATESTBK                                              
*                                                                               
         CLI   NUMSTAS,0                                                        
         BE    HR0100              NO COMPETITVE STATIONS IN REQUEST            
*                                                                               
         GOTO1 VDEMAND,DMCB,DB.DBLOCK,LISTHOOK,0                                
*                                                                               
         L     RE,AIO2             POINT TO VALIDATED STATIONS                  
         MVC   NUMSTAS,0(RE)       FOR THE REMAINDER OF ROUTINE                 
         LA    RE,1(RE)                                                         
         ST    RE,FRSTSTA                                                       
*                                                                               
HR0100   DS    0H                                                               
         ZIC   R3,NUMQTRS          BUILD LIST OF BOOKS                          
         ZIC   R2,STQTR                                                         
         BCTR  R2,0                                                             
         LA    RE,QTRS(R2)         INDEX INTO MONTHS TABLE                      
         MVC   BYTE,0(RE)                                                       
*                                                                               
         XC    DB.DBLOCK,DB.DBLOCK                                              
         B     HR0104                                                           
*                                                                               
HR0102   DS    0H                                                               
         XC    DB.DBLOCK,DB.DBLOCK                                              
*                                                                               
         LA    RE,QTRS(R2)         INDEX INTO MONTHS TABLE                      
         MVC   DB.DBSELBK(1),ENYEAR                                             
         CLC   BYTE,0(RE)                CURRENT QTR BEFORE START?              
         BNL   *+10                      NO USE END YEAR                        
HR0104   MVC   DB.DBSELBK(1),STYEAR                                             
         MVC   DB.DBSELBK+1(1),0(RE)                                            
*                                                                               
         MVC   DB.DBAREC,AIO1                                                   
         MVC   DB.DBSELAGY,REPALPHA                                             
         MVC   DB.DBFILE,=C'TP '                                                
         MVC   DB.DBCOMFCS,ACOMFACS                                             
         MVI   DB.DBSELMED,C'T'                                                 
         MVI   DB.DBTPTT,C'T'                                                   
         MVI   DB.DBFUNCT,DBVLSTBK                                              
         MVC   DB.DBSELSRC,RTSRVC                                               
         L     RE,OVPARMS+4                                                     
         USING VHPARMD,RE                                                       
         MVC   DB.DBSELSTA,VHPSTA                                               
         DROP  RE                                                               
         CLI   DB.DBSELSTA+4,C' '                                               
         BH    *+8                                                              
         MVI   DB.DBSELSTA+4,C'T'                                               
*                                                                               
         MVC   DB.DBSELDAY,=X'40'               M                               
         MVC   DB.DBSELTIM,=X'07D007DF'         8-815P                          
*                                                                               
*        SEE IF THE BOOK IS LOADED                                              
*                                                                               
         MVI   WORK+4,1            TRY COUNT                                    
HR0106   DS    0H                                                               
         GOTO1 VDEMAND,DMCB,DB.DBLOCK,0,0                                       
         CLI   DB.DBERROR,0        ERROR?                                       
         BE    HR0108              NO                                           
*                                                                               
         CLI   DB.DBERROR,16       NOT FOUND?                                   
         BE    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
         CLI   WORK+4,5            ONLY ALLOW 5 TRIES                           
         BNL   HR0108              JUST USE THIS BOOK                           
***      BL    *+6                                                              
***      DC    H'0'                                                             
*                                                                               
         ZIC   RE,WORK+4                                                        
         LA    RE,1(RE)                                                         
         STC   RE,WORK+4                                                        
*                                                                               
         MVC   WORK(1),DB.DBSELBK                                               
         MVC   WORK+1(2),=X'0101'                                               
         GOTO1 VDATCON,DMCB,(3,WORK),(0,WORK+6)                                 
         GOTO1 VADDAY,DMCB,(C'Y',WORK+6),(0,WORK+6),-1                          
         GOTO1 VDATCON,DMCB,(0,WORK+6),(3,WORK)                                 
         MVC   DB.DBSELBK(1),WORK                                               
         B     HR0106                                                           
*                                                                               
HR0108   DS    0H                                                               
         ZIC   RE,NUMQTRS          INDEX INTO BOOKS LIST                        
         SR    RE,R3                                                            
         MHI   RE,2                                                             
         LA    RE,BOOKS(RE)                                                     
         MVC   0(2,RE),DB.DBSELBK                                               
*                                                                               
         LA    R2,1(R2)                                                         
         BCT   R3,HR0102                                                        
         EJECT                                                                  
*------------------------------------------------------------                   
* GET MARKET CUMES                                                              
*------------------------------------------------------------                   
         ZIC   R3,NUMQTRS                                                       
         SR    R2,R2                                                            
*                                                                               
HR0160   DS    0H                                                               
         LR    RE,R2                                                            
         MHI   RE,2                INDEX INTO BOOKS                             
         LA    RE,BOOKS(RE)                                                     
*                                                                               
         XC    DB.DBLOCK,DB.DBLOCK                                              
         MVC   DB.DBSELBK,0(RE)                                                 
         MVC   DB.DBAREC,AIO1                                                   
         MVC   DB.DBSELAGY,REPALPHA                                             
         MVC   DB.DBFILE,=C'TP '                                                
         MVC   DB.DBCOMFCS,ACOMFACS                                             
         MVI   DB.DBSELMED,C'D'                                                 
         MVI   DB.DBFUNCT,DBGETDEM                                              
         MVC   DB.DBSELSRC,RTSRVC                                               
         EDIT  (B2,MARKET),(4,DB.DBSELSTA),FILL=0                               
         MVI   DB.DBSELSTA+4,C'T'                                               
         MVC   DB.DBSELDAY,=X'7F'               M-SU                            
         MVC   DB.DBSELTIM,=X'0258000C8'        6A-2A                           
*                                                                               
         STC   R2,BYTE2            STORE QTR INDEX FOR THE HOOK                 
*                                                                               
*        GET CUMES FOR THE MARKET(BASIS CUMES)                                  
*                                                                               
         GOTO1 VDEMAND,DMCB,DB.DBLOCK,MKTHOOK,0                                 
         CLI   DB.DBERROR,0        ERROR?                                       
         B     *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,1(R2)                                                         
         BCT   R3,HR0160                                                        
         EJECT                                                                  
*------------------------------------------------------------                   
* GET STATION CUMES                                                             
*------------------------------------------------------------                   
         MVC   REMSTAS,NUMSTAS                                                  
         MVC   CURSTA,FRSTSTA                                                   
         L     RE,OVPARMS+4                                                     
         USING VHPARMD,RE                                                       
         LA    R4,VHPSTA                                                        
         DROP  RE                                                               
*                                                                               
HR0200   DS    0H                                                               
         ZIC   R3,NUMQTRS                                                       
         SR    R2,R2                                                            
*                                                                               
HR0210   DS    0H                                                               
         LR    RE,R2                                                            
         MHI   RE,2                INDEX INTO BOOKS                             
         LA    RE,BOOKS(RE)                                                     
*                                                                               
         XC    DB.DBLOCK,DB.DBLOCK                                              
         MVC   DB.DBSELBK,0(RE)                                                 
         MVC   DB.DBAREC,AIO1                                                   
         MVC   DB.DBSELAGY,REPALPHA                                             
         MVC   DB.DBFILE,=C'TP '                                                
         MVC   DB.DBCOMFCS,ACOMFACS                                             
         MVI   DB.DBSELMED,C'D'                 DPT?                            
         MVI   DB.DBFUNCT,DBGETDEM              GET DEMO?                       
         MVC   DB.DBSELSRC,RTSRVC                                               
         MVC   DB.DBSELSTA(5),0(R4)                                             
         CLI   DB.DBSELSTA+4,C' '                                               
         BH    *+8                                                              
         MVI   DB.DBSELSTA+4,C'T'                                               
*                                                                               
         STC   R2,BYTE2            STORE QTR INDEX FOR THE HOOK                 
*                                                                               
*        GET CUMES FOR THE STATION FOR ALL DAYPARTS/DEMOS                       
*                                                                               
         GOTO1 VDEMAND,DMCB,DB.DBLOCK,STAHOOK,0                                 
         CLI   DB.DBERROR,0        ERROR?                                       
         B     *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,1(R2)                                                         
         BCT   R3,HR0210                                                        
*------------------------------------------------------------                   
*        SEND DATA FROM TABLE TO THE PC                                         
*------------------------------------------------------------                   
         LA    R3,1                                                             
HR0280   DS    0H                                                               
         LR    RE,R3                                                            
         BCTR  RE,0                                                             
         MHI   RE,3                                                             
         LA    RE,DEMOS(RE)                                                     
         CLI   1(RE),C'R'          RATING?                                      
         BNE   HR0300              NO                                           
*                                  R/F INFO DATA ELEMENT                        
         GOTO1 ASETELEM,DMCB,AFABLK,RFIDATA,0                                   
         OI    MISCFLG1,MF1DATA    SET DATA IN BUFFER                           
*                                                                               
*                                  STATION CALL LETTERS                         
         GOTO1 AADDDATA,DMCB,AFABLK,RFISTAEL,(R4),0                             
*                                                                               
         STC   R3,BYTE             DEMO SEQUENCE NUMBER                         
         GOTO1 AADDDATA,DMCB,AFABLK,RFIDEMEL,BYTE,0                             
*                                                                               
         CLI   ISCMT,C'Y'                                                       
         BNE   HR0282                                                           
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,RFICMTEL,0,0                                
*                                                                               
HR0282   DS    0H                                                               
         LR    R2,R3                                                            
         BCTR  R2,0                                                             
         MHI   R2,CTDLENQ          INDEX TO DEMO                                
         LA    R2,CUMETAB(R2)                                                   
         USING CUMETAB,R2                                                       
*                                                                               
         LA    R5,1                INTIAL QUARTER                               
         ZIC   RE,STQTR                                                         
         MVC   BYTE2,STQTR                                                      
         B     HR0288                                                           
*                                                                               
HR0286   DS    0H                                                               
         ZIC   RE,STQTR                                                         
         AR    RE,R5                                                            
         BCTR  RE,0                                                             
         CHI   RE,4                                                             
         BNH   *+8                                                              
         AHI   RE,-4                                                            
         STC   RE,BYTE2                                                         
*                                                                               
         MVC   BYTE,ENYEAR                                                      
         CLC   STQTR,BYTE2         CURRENT QTR BEFORE START?                    
         BNL   *+10                NO USE END YEAR                              
HR0288   MVC   BYTE,STYEAR                                                      
*                                  R/F INFO QUARTER DATA ELEMENT                
         GOTO1 ASETELEM,DMCB,AFABLK,RFQDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
*                                  ADD YEAR                                     
         GOTO1 AADDDATA,DMCB,AFABLK,RFQYREL,BYTE,0                              
*                                  ADD QUARTER                                  
         GOTO1 AADDDATA,DMCB,AFABLK,RFQQTREL,BYTE2,0                            
*                                                                               
         L     RE,ATWA                                                          
         USING T81AFFD,RE                                                       
         CLC   VERSION,=X'01100011'    1.16.17+ GETS BOOK USED                  
         BL    HR0289                                                           
         DROP  RE                                                               
*                                                                               
         LR    RE,R5                                                            
         BCTR  RE,0                                                             
         MHI   RE,2                                                             
         LA    RE,BOOKS(RE)                                                     
*                                                                               
         XC    WORK,WORK                                                        
         MVI   WORK,16+8           FAKE 16 BYTE FIELD                           
         MVC   WORK+16+8+1(2),0(RE)                                             
         GOTO1 VUNBOOK,DMCB,(1,WORK+16+8),WORK,0,                      +        
               (C'+',=CL6' ')                                                   
*                                                                               
         ZIC   RF,WORK                                                          
         BCTR  RF,0                                                             
         LA    RF,WORK(RF)                                                      
         CLI   0(RF),C' '          REMOVE SPACES                                
         BH    *+8                                                              
         BCT   RF,*-8                                                           
*                                                                               
         LA    RE,WORK                                                          
         SR    RF,RE                                                            
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,RFQSWPEL,WORK+8,(RF)                        
*                                                                               
HR0289   DS    0H                                                               
         LR    RE,R5                                                            
         BCTR  RE,0                                                             
         MHI   RE,4                                                             
         LA    RE,MKTBASIS(RE)                                                  
         LR    R0,RE               ADD MARKET BASIS                             
         GOTO1 AADDDATA,DMCB,AFABLK,RFQBCUEL,(R0),0                             
*                                                                               
*                                  ADD MARKET CUME                              
         GOTO1 AADDDATA,DMCB,AFABLK,RFQMCUEL,CTMKTC,0                           
*                                                                               
*                                  ADD STATION CUME                             
         GOTO1 AADDDATA,DMCB,AFABLK,RFQSCUEL,CTSTAC,0                           
*                                                                               
*                                  ADD STATION RATING                           
         GOTO1 AADDDATA,DMCB,AFABLK,RFQSRAEL,CTSTAR,0                           
*                                                                               
         LA    R0,13                                                            
         LA    R6,CTDPTC                                                        
HR0290   DS    0H                                                               
*                                  ADD DAYPART CUME                             
         GOTO1 AADDDATA,DMCB,AFABLK,RFQDCUEL,(R6),0                             
         LA    R6,4(R6)                                                         
         BCT   R0,HR0290                                                        
*                                                                               
                                                                                
         XC    CTSTAC(CTSLENQ),CTSTAC         CLEAR STATION INFO                
         LA    R2,CTQLENQ(R2)                 BUMP TO NEXT QUARTER              
*                                                                               
         CLM   R5,1,NUMQTRS        END OF QUARTERS?                             
         BNL   *+12                YES                                          
         LA    R5,1(R5)                                                         
         B     HR0286              LOOP FOR NEXT QUARTER                        
*                                                                               
HR0300   DS    0H                                                               
         CLM   R3,1,NUMDEMS        END OF DEMOS?                                
         BNL   *+12                YES                                          
         LA    R3,1(R3)                                                         
         B     HR0280              LOOP FOR NEXT DEMO                           
         DROP  R2                                                               
*                                                                               
*        LOOP BACK FOR NEXT STATION                                             
*                                                                               
         L     R4,CURSTA                                                        
         LA    RE,STALENQ(R4)                                                   
         ST    RE,CURSTA                                                        
*                                                                               
         ZIC   RE,REMSTAS                                                       
         BCTR  RE,0                                                             
         STC   RE,REMSTAS                                                       
         LTR   RE,RE                                                            
         BNM   HR0200                                                           
*                                                                               
HARRISX  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
*                                                                               
*   TEMPORARY VALIDATION ROUTINE TO DETERMINE IF USERID ALLOWED ACCESS          
*     TO NIELSEN DATA FOR REQUESTED MARKET                                      
*                                                                               
***********************************************************************         
VAL4MKT  NTR1                                                                   
         LA    R1,USRMKTAB                                                      
VMKT010  CLI   0(R1),X'FF'       END OF TABLE?                                  
         BE    MKTOKAY           YES, NO RESTRICTIONS FOUND                     
         CLC   REPALPHA,0(R1)    MATCH ON REPCODE?                              
         BE    VMKT050           YES, BRANCH TO NEXT STEP                       
         ZICM  R2,2(R1),2        ELSE, PUT ENTRY LENGTH IN R2                   
         LA    R1,0(R2,R1)       BUMP R1 TO NEXT TABLE ENTRY                    
         B     VMKT010                                                          
                                                                                
*        THIS REPALPHA HAS RESTRICTIONS, CHK IF 1 FOR THIS ID                   
VMKT050  L     RE,ATWA           POINT RE AT TWA                                
         USING TWAD,RE                                                          
         ZICM  R2,2(R1),2        ENTRY LENGTH IN R2                             
         LA    R2,0(R2,R1)       POINT R2 AT END OF THIS ENTRY                  
         LA    R1,4(R1)          POINT R1 TO 1ST ID/MKT SUB ENTRY               
         SR    R3,R3             INITIALIZE SUB-ENTRY LENGTH HOLDER             
                                                                                
VMKT100  LA    R1,0(R3,R1)       POINT R1 TO NEXT SUB ENTRY                     
         CR    R1,R2             HAVE WE REACHED END OF MAIN ENTRY?             
         BNL   MKTOKAY           YES, NO RESTRICTION FOUND                      
         ZIC   R3,2(R1)          PUT SUB-ENTRY LENGTH IN R3                     
         CLC   TWAUSRID,0(R1)    MATCH ON USER ID?                              
         BNE   VMKT100           NO, CHECK NEXT                                 
                                                                                
         LA    R2,0(R3,R1)       POINT R2 AT END OF SUB ENTRY                   
         LA    R1,3(R1)          POINT R1 AT FIRST ALLOWED MKT                  
                                                                                
VMKT150  CR    R1,R2             HAVE WE REACHED END OF SUB ENTRY?              
         BNL   MKTNOTOK          YES, NO MATCH FOUND FOR CURRENT MKT            
         CLC   MARKET,0(R1)      NO,  CHK MATCH ON ALLOWED MARKET               
         BE    MKTOKAY                YES                                       
         LA    R1,2(R1)               NO, BUMP TO NEXT ALLOWED MKT              
         B     VMKT150                    AND REPEAT CHECK                      
         DROP  RE                                                               
                                                                                
MKTOKAY  SR    RC,RC                     SET CC CODE TO =                       
MKTNOTOK LTR   RC,RC                     SET CC CODE TO !=                      
         XIT1                                                                   
*  TABLE OF NIELSEN MARKET DATA RESTRICTIONS BY USERID                          
*    TABLE STRUCTURE:                                                           
*    REP ALPHA CODE CL2   ENTRY LENGTH AL2                                      
*        VARIABLE # OF 'LIMIT SUB-ENTRIES' OF TYPE:                             
*                       USER ID                        XL2                      
*                       SIZE OF SUB ENTRY              AL1                      
*                       MARKET CODES OF ALLOWED MKTS   XL2                      
*                       ALLOWS VARIABLE# VALID LOCAL MARKETS FOR USERID         
*                                                                               
USRMKTAB DS    0H                                                               
UV       DC    C'UV',AL2(UVX-UV)                                                
UV001    DC    X'1E91',AL1(UV002-UV001)   KUVSL=NO VALID MKTS                   
UV002    DC    X'1E8E',AL1(UV003-UV002)   KMEXL=NO VALID MKTS                   
UV003    DC    X'1E95',AL1(UV004-UV003)   WLTVL=NO VALID MKTS                   
UV004    DC    X'1E8F',AL1(UV005-UV004)   KTVWL=NO VALID MKTS                   
UV005    DC    X'1E93',AL1(UV006-UV005)   KXLNL=NO VALID MKTS                   
UV006    DC    X'1E8C',AL1(UV007-UV006)   KDTVL=NO VALID MKTS                   
UV007    DC    X'1E96',AL1(UV008-UV007)   WXTVL=NO VALID MKTS                   
UV008    DC    X'1F32',AL1(UV009-UV008)   KABEL=NO VALID MKTS                   
UV009    DC    X'1F64',AL1(UV010-UV009)   KUVIL=NO VALID MKTS                   
UV010    DC    X'1E86',AL1(UV011-UV010)   KFTVL=NO VALID MKTS                   
UV011    DC    X'1F33',AL1(UV012-UV011)   KUVEL=NO VALID MKTS                   
UV012    DC    X'1E90',AL1(UV013-UV012)   KUVNL=NO VALID MKTS                   
UV013    DC    X'1E94',AL1(UV014-UV013)   WGBOL=NO VALID MKTS                   
                                                                                
UV014    DC    X'179F',AL1(UV015-UV014),X'0065019301D2' UNAT=NY,LA,FRS          
UV015    DC    X'0799',AL1(UV016-UV015),X'0065019301D2' UNCH=NY,LA,FRS          
UV016    DC    X'0796',AL1(UV017-UV016),X'0065019301D2' UNDA=NY,LA,FRS          
UV017    DC    X'079A',AL1(UV018-UV017),X'0065019301D2' UNDE=NY,LA,FRS          
UV018    DC    X'0797',AL1(UV019-UV018),X'0065019301D2' UNLA=NY,LA,FRS          
UV019    DC    X'0798',AL1(UV020-UV019),X'0065019301D2' UNMI=NY,LA,FRS          
UV020    DC    X'078C',AL1(UV021-UV020),X'0065019301D2' UNNY=NY,LA,FRS          
UV021    DC    X'174A',AL1(UV022-UV021),X'0065019301D2' UNSA=NY,LA,FRS          
UV022    DC    X'079C',AL1(UVX-UV022),X'0065019301D2'   UNSF=NY,LA,FRS          
UVX      DS    0H                                                               
                                                                                
B3       DC    C'B3',AL2(B3X-B3)                                                
B3001    DC    X'0966',AL1(B3X-B3001),X'0065019301D2'   EJOR=NY,LA,FRS          
B3X      DS    0H                                                               
         DC    X'FF'              END OF TABLE                                  
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DEMAND HOOK FOR EXTRACTING MARKET STATIONS                                    
***********************************************************************         
BOOKHOOK NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   LATESTBK,DB.DBACTBK                                              
*                                                                               
         B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DEMAND HOOK FOR EXTRACTING MARKET STATIONS                                    
***********************************************************************         
LISTHOOK NTR1  BASE=*,LABEL=*                                                   
         L     R4,DB.DBAREC                                                     
         USING MLKEY,R4                                                         
         OC    MLKMKT,MLKMKT       TEST SPILL MARKET                            
         BNZ   LSTHX               YES - IGNORE                                 
*                                                                               
         TM    MLSTAT,X'F0'        TEST STATION NUMERIC                         
         BO    LSTHX               YES - IGNORE                                 
*                                                                               
         L     RE,OVPARMS+4                                                     
         USING VHPARMD,RE                                                       
         CLC   VHPSTA,MLSTAT       TEST STATION = REQUEST STATION               
         BE    LSTHX               YES - IGNORE                                 
         DROP  RE                                                               
*                                                                               
         L     RE,FRSTSTA                                                       
         A     RE,OVPARMS+4                                                     
         ZIC   R0,NUMSTAS                                                       
LSTH010  DS    0H                  IS STATION IN THE REQUEST?                   
         LA    RF,3                                                             
         CLI   4(RE),C' '          ADJUST COMPARE                               
         BNH   *+8                                                              
         LA    RF,1(RF)                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),MLSTAT                                                   
         BE    LSTH020             YES                                          
         LA    RE,5(RE)                                                         
         BCT   R0,LSTH010                                                       
         B     LSTHX               NO - EXIT                                    
*                                                                               
LSTH020  DS    0H                                                               
         L     RE,AIO2                                                          
         ZIC   R1,0(RE)            GET STATION COUNT                            
         LA    R0,1(R1)            BUMP BY ONE                                  
         STC   R0,0(RE)            AND STORE                                    
*                                                                               
         MHI   R1,5                INDEX TO NEXT SPOT                           
         LA    RE,1(R1,RE)            BUMP PAST LENGHT                          
         MVC   0(5,RE),MLSTAT                                                   
         CLI   4(RE),C'T'                                                       
         BNE   *+8                                                              
         MVI   4(RE),C' '                                                       
*                                                                               
LSTHX    DS    0H                                                               
         B     EXIT                                                             
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DEMAND HOOK FOR SIGN ON TO SIGN OFF CUMES FOR THE MARKET                      
*    - P2+ CUME                                                                 
*    - DEMO SPECIFIC CUMES FOR THE REQUEST                                      
***********************************************************************         
MKTHOOK  NTR1  BASE=*,LABEL=*                                                   
         L     RE,DB.DBAQUART                                                   
         CLC   =X'17044C',2(RE)                                                 
         BNE   MKTHX                                                            
*                                                                               
         GOTO1 VDEMOUT,DMCB,(C'L',CUMES),DB.DBLOCK,AIO3,0                       
         CLI   DB.DBERROR,0                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO3                                                          
         ZIC   R0,NUMDEMS                                                       
         ZIC   R6,BYTE2            GET QTR INDEX                                
         MHI   R6,CTQLENQ                                                       
         LA    R6,CUMETAB(R6)      POINT TO THE QUARTER IN THE TABLE            
         USING CUMETAB,R6                                                       
*                                                                               
MKH110   DS    0H                                                               
         MVC   CTMKTC,0(R5)        COPY DEMO VALUE FOR MARKET CUME              
         LA    R5,4(R5)            NEXT DEMO VALUE                              
         LA    R6,CTDLENQ(R6)      NEXT CUME TABLE ENTRY                        
         BCT   R0,MKH110                                                        
         DROP  R6                                                               
*                                                                               
         GOTO1 VDEMOUT,DMCB,(C'D',=X'00C37F'),DB.DBLOCK,AIO3,0                  
         CLI   DB.DBERROR,0                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO3                                                          
         ZIC   R0,NUMDEMS                                                       
         ZIC   R6,BYTE2            GET QTR INDEX                                
         MHI   R6,4                                                             
         LA    R6,MKTBASIS(R6)     POINT TO THE QUARTER                         
         MVC   0(4,R6),0(R5)                                                    
*                                                                               
MKTHX    DS    0H                                                               
         B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DEMAND HOOK FOR EXTRACTING STATION CUME DATA FOR ALL DEMOS                    
*   SIGN ON TO SIGN OFF CUME (6A-2A)                                            
*   13 HARRIS DAYPARTS BASED ON TIME ZONE                                       
*                                                                               
***********************************************************************         
STAHOOK  NTR1  BASE=*,LABEL=*                                                   
         CLI   ISCMT,0                                                          
         BNE   LSTH004                                                          
*                                                                               
         MVI   ISCMT,C'N'          SET TO EASTER/PACIFIC                        
         L     RE,DB.DBAREC                                                     
         AH    RE,DB.DBDTADSP                                                   
LSTH000  CLI   0(RE),X'00'         FIND DIELEM                                  
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(RE),X'03'                                                      
         BE    LSTH002                                                          
         ZIC   R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     LSTH000                                                          
*                                                                               
LSTH002  DS    0H                                                               
         USING DIELEM,RE                                                        
         CLI   DITZ,C'1'           EASTERN?                                     
         BE    LSTH004                                                          
         CLI   DITZ,C'4'           PACIFIC?                                     
         BE    LSTH004                                                          
         DROP  RE                                                               
*                                                                               
         MVI   ISCMT,C'Y'          NO                                           
*                                                                               
LSTH004  DS    0H                                                               
         LA    R1,EPTDPTS                                                       
         CLI   ISCMT,C'Y'                                                       
         BNE   *+8                                                              
         LA    R1,CMTDPTS                                                       
         L     RE,DB.DBAQUART                                                   
STH010   DS    0H                                                               
         CLI   0(R1),0                                                          
         BE    STH100                                                           
         CLC   2(LENDTABQ,RE),0(R1)                                             
         BE    STH020                                                           
         LA    R1,LENDTABQ(R1)                                                  
         B     STH010                                                           
*                                                                               
STH020   DS    0H                                                               
         LA    R0,EPTDPTS                                                       
         CLI   ISCMT,C'Y'                                                       
         BNE   *+8                                                              
         LA    R0,CMTDPTS                                                       
         SR    R1,R0                                                            
         SR    R0,R0                                                            
         LA    RF,LENDTABQ                                                      
         DR    R0,RF                                                            
         LR    R3,R1               INDEX INTO DAYPARTS                          
         MHI   R3,4                                                             
*                                                                               
**TEST                                                                          
*        CLI   BYTE2,1                                                          
*        BNE   STH020X                                                          
*        L     RF,DB.DBAQUART                                                   
*        LA    RF,2(RF)                                                         
*        GOTO1 VHEXOUT,DMCB,(RF),WORK,LENDTABQ,0                                
*        GOTO1 ASETELEM,DMCB,AFABLK,INVDATA,0                                   
*        OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*        GOTO1 AADDDATA,DMCB,AFABLK,INVNUMEL,WORK,LENDTABQ*2                    
*TH020X  DS    0H                                                               
**TEST                                                                          
*                                                                               
         GOTO1 VDEMOUT,DMCB,(C'L',CUMES),DB.DBLOCK,AIO3,0                       
         CLI   DB.DBERROR,0                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO3                                                          
         ZIC   R0,NUMDEMS                                                       
         ZIC   R6,BYTE2            GET QTR INDEX                                
         MHI   R6,CTQLENQ                                                       
         LA    R6,CUMETAB(R6)      POINT TO THE QUARTER IN THE TABLE            
         USING CUMETAB,R6                                                       
*                                                                               
STH030   DS    0H                                                               
         LA    RE,CTDPTC(R3)       INDEX TO CORRECT DAYPART                     
         MVC   0(4,RE),0(R5)       COPY DEMO VALUE                              
*                                                                               
         CHI   R3,DUPLIQ*4         CHECK IF ITS THE DUPLICATE                   
         BNE   *+10                                                             
         MVC   OFFSETQ(4,RE),0(R5)                                              
*                                                                               
         LA    R5,4(R5)            NEXT DEMO VALUE                              
         LA    R6,CTDLENQ(R6)      NEXT CUME TABLE ENTRY                        
         BCT   R0,STH030                                                        
         DROP  R6                                                               
*                                                                               
STH100   DS    0H                                                               
         L     RE,DB.DBAQUART                                                   
         CLC   =X'17044C',2(RE)                                                 
         BNE   STAHX                                                            
*                                                                               
         GOTO1 VDEMOUT,DMCB,(C'L',CUMES),DB.DBLOCK,AIO3,0                       
         CLI   DB.DBERROR,0                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO3                                                          
         ZIC   R0,NUMDEMS                                                       
         ZIC   R6,BYTE2            GET QTR INDEX                                
         MHI   R6,CTQLENQ                                                       
         LA    R6,CUMETAB(R6)      POINT TO THE QUARTER IN THE TABLE            
         USING CUMETAB,R6                                                       
*                                                                               
STH110   DS    0H                                                               
         MVC   CTSTAC,0(R5)        COPY DEMO VALUE FOR STATION CUME             
         LA    R5,4(R5)            NEXT DEMO VALUE                              
         LA    R6,CTDLENQ(R6)      NEXT CUME TABLE ENTRY                        
         BCT   R0,STH110                                                        
         DROP  R6                                                               
*                                                                               
         GOTO1 VDEMOUT,DMCB,(C'L',DEMOS),DB.DBLOCK,AIO3,0                       
         CLI   DB.DBERROR,0                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO3                                                          
         ZIC   R0,NUMDEMS                                                       
         ZIC   R6,BYTE2            GET QTR INDEX                                
         MHI   R6,CTQLENQ                                                       
         LA    R6,CUMETAB(R6)      POINT TO THE QUARTER IN THE TABLE            
         USING CUMETAB,R6                                                       
*                                                                               
STH120   DS    0H                                                               
         MVC   CTSTAR,0(R5)        COPY DEMO VALUE FOR STATION CUME             
         LA    R5,4(R5)            NEXT DEMO VALUE                              
         LA    R6,CTDLENQ(R6)      NEXT CUME TABLE ENTRY                        
         BCT   R0,STH120                                                        
         DROP  R6                                                               
*                                                                               
STAHX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DAYPART TABLE FOR HARRIS MODEL                                                
***********************************************************************         
EPTDPTS  DS    0D                          EASTERN/PACIFIC TIME                 
         DC    X'15000C'                                                        
LENDTABQ EQU   *-EPTDPTS                                                        
         DC    X'150C18'                                                        
         DC    X'151828'                                                        
         DC    X'152830'                                                        
         DC    X'153038'                                                        
         DC    X'173845'                                                        
         DC    X'174446'                                                        
         DC    X'15464C'                                                        
         DC    X'FFFFFF'           N/A                                          
DUPLIQ   EQU   (*-EPTDPTS)/LENDTABQ                                             
         DC    X'600418'                                                        
         DC    X'60182C'                                                        
OFFSETQ  EQU   (((*-EPTDPTS)/LENDTABQ)-DUPLIQ)*4                                
         DC    X'600418'                                                        
         DC    X'701C30'                                                        
         DC    X'00'                                                            
CMTDPTS  DS    0D                          CENTRAL/MOUNTAIN TIME                
         DC    X'15000C'                                                        
         DC    X'150C18'                                                        
         DC    X'151824'                                                        
         DC    X'15242C'                                                        
         DC    X'152C34'                                                        
         DC    X'173441'                                                        
         DC    X'174042'                                                        
         DC    X'154248'                                                        
         DC    X'FFFFFF'           N/A                                          
         DC    X'600418'                                                        
         DC    X'60182C'                                                        
         DC    X'600418'                                                        
         DC    X'701C30'                                                        
         DC    X'00'                                                            
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* OVERLAY WORKING STORAGE                                                       
***********************************************************************         
OVERWRKD DSECT                                                                  
VDEMAND  DS    A                                                                
VDEMOUT  DS    A                                                                
*                                                                               
ACURPARM DS    F                                                                
*                                                                               
FRSTSTA  DS    F                                                                
CURSTA   DS    F                                                                
FRSTDEM  DS    F                                                                
CURDEM   DS    F                                                                
*                                                                               
NUMSTAS  DS    X                                                                
NUMDEMS  DS    X                                                                
NUMQTRS  DS    X                                                                
*                                                                               
REMSTAS  DS    X                                                                
REMDEMS  DS    X                                                                
*                                                                               
STYEAR   DS    X                                                                
ENYEAR   DS    X                                                                
STQTR    DS    X                                                                
ENQTR    DS    X                                                                
*                                                                               
ISCMT    DS    CL1                 Y/N IS MARKET CENTRAL/MOUNTAIN TIME          
*                                                                               
BOOKS    DS    XL(2*MAXQTRS)                                                    
DEMOS    DS    XL(3*(MAXDEMOS+1))                                               
CUMES    DS    XL(3*(MAXDEMOS+1))                                               
*                                                                               
MARKET   DS    XL(L'DBSELRMK)                                                   
LATESTBK DS    XL(L'DBSELBK)                                                    
*                                                                               
MKTBASIS DS    XL(MAXQTRS*4)       BASIS CUME FOR THE MARKET(5 QTRS)            
*                                                                               
MYDBLOCK DS    CL(L'DBLOCK)                                                     
*                                                                               
         DS    CL20                DBLOCK IS BIGGER THAN ADVERTISED             
*                                                                               
*  AREA TO TABLE UP CUMES(R/F INFOS) FOR A STATION                              
*  5 QUARTERS                                                                   
*      24 DEMOS                                                                 
*          1  MARKET CUME                                                       
*          1  STATION CUME                                                      
*          1  STATION RATING                                                    
*          13 DAYPART CUMES                                                     
*                                                                               
MAXQTRS  EQU   5                                                                
MAXDEMOS EQU   24                                                               
*                                                                               
CUMETAB  DS    XL(MAXQTRS*MAXDEMOS*(13+3)*4)                                    
         ORG   CUMETAB                                                          
CTMKTC   DS    XL4                                                              
CTSTAC   DS    XL4                                                              
CTSTAR   DS    XL4                                                              
CTDPTC   DS    13XL4                                                            
CTDLENQ  EQU   *-CUMETAB           LENGTH OF DEMO ENTRY                         
CTSLENQ  EQU   *-CTSTAC            LENGTH OF STATION PORTION                    
CTQLENQ  EQU   MAXDEMOS*CTDLENQ    LENGTH OF QUARTER                            
         ORG                                                                    
*                                                                               
OVERWRKQ EQU   *-OVERWRKD          LENGTH OF WORKING STORAGE                    
         EJECT                                                                  
       ++INCLUDE REPRPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE REGENCUMO                                                      
         EJECT                                                                  
MAPTABD  DSECT                                                                  
       ++INCLUDE REPRPMAP                                                       
*DEDEMFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'211REPRX30X  08/31/00'                                      
         END                                                                    
*                                                                               
