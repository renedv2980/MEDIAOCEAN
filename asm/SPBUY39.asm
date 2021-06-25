*          DATA SET SPBUY39    AT LEVEL 045 AS OF 11/03/20                      
*PHASE T21139C                                                                  
*                                                                               
*=====================================================================*         
*                                                                     *         
* HISTORY                                                             *         
* -------                                                             *         
*                                                                     *         
* WHEN    LEV WHAT                                                    *         
* ----    --- ----                                                    *         
* 04NOV20 045 DS 20.3 RELEASE                                                   
*         045 SPEC-47771|AUTOMG PROCESS FLAG                                    
*         045 SPEC-49122|DELETE OPTIONAL PURPOSE CODES                          
* X20.2XX 044 SPEC-39434|MULTI-COMMENT MKGD REJECTION                           
* X19.1XX 042 SPEC-23940|2-BYTE BUYLINES                                        
* 14NOV18 041 SPEC-24483|SEPARATE SPOT FUNCTIONALITY                            
* 11JUN18 040 SPEC-24234|FIX ISSUE WITH TRADE MAKEGOOD FOR CABLE                
*             BUYS CAUSING INVALD CHECKSUM ERRORS                               
* 31MAY18 039 FIX LIOBAREC OVERFLOW ISSUES                                      
* 17JAN18 038 DON'T ALLOW UPDATE IF READONLY                                    
* 01NOV16 037 COMSCORE SUPPORT                                                  
* 26OCT15 036 FORCE DUMP IF TWA IS CLOBBERED CAUSED BY LARGE BUY      *         
* 02DEC14 035 FIX LIOBWORK OVERFLOW WHEN APPLYING LARGE MAKEGOOD      *         
*         035 FIX AGAIN CKSM ERRORS W/X-NTWK MKGDS MISSING SAME LINE  *         
* 13OCT14 034 FIX CKSM ERRORS W/X-NETWORK MKGDS MISSING SAME LINE     *         
* 16SEP14 033 REPLACE FASYSFACS WITH FASYSFAC                         *         
* 09JUL14 032 ADD CROSS NETWORK MAKEGOOD SUPPORT                      *         
* 18JAN11 029 REMOVE CODE THAT UPDATES SSB TO COUNT CHKSUM ERRORS     *         
* 04JAN11 028 CLEAR BUBKTYPE & BUDLUSW TO FIX BAD BOOK LOOKUP BUG     *         
* 19AUG08 025 2 BYTE LINE NUMBERS                                     *         
* 13NOV07 023 IGNORE 'SPOTS OUTSIDE OF BUY PERIOD' ERROR              *         
* 29OCT07 022 FIX BOUNDARY CONDITION IN SENDING CHANGED MGAPP BUYS    *         
*             (SAME AS LEVEL 18 FIX)                                  *         
* 27SEP07 021 SEND RETURN CODE ON DELETE                              *         
* 24MAY07 020 MORE ERROR HANDLING PROBLEMS                            *         
*         --- REMOVE ZIC/IC INSTRUCTIONS                              *         
* 06APR07 019 SET ADBLOCK FLAG TO RE-BUILD DEMUP BLOCK FOR EACH BUY   *         
*         --- RE-CALCULATE SVTIA ADDR (CAN'T USE AHI)                 *         
*         --- FIX TO NOT SEND BLANK 141 (AND MAKE SURE TO SEND THEM   *         
*             WITH ERRORS)                                            *         
* 28NOV06 018 FIX BOUNDARY CONDITION IN SENDING CHANGED MG BUYS       *         
* 28JUN06 017 CODE TO COUNT NUMBER OF CHECKSUM HITS IN SSB            *         
* 19JUN06 016 MOVE CODE TO CLEAR CHECKSUM TO RIGHT PLACE              *         
* 12JUN06 015 NEED BETTER ERROR CODE IN PACKAGES AND ORBITS TO DEAL   *         
*             WITH LOCKED ORDERS.                                     *         
* 13MAR06 013 UPCASE DATA BEFORE CALLING VALIDATION RTNS FOR ORBITS   *         
*         --- SUPPORT PACKAGES                                        *         
* 19JAN06 012 SUPPORT 2 DECIMAL ORBITS                                *         
* 07NOV05 011 SUPPORT ADDING PB BUYS WITH UN-EVEN SPLITS              *         
*         --- CLEAR CHECKSUM TABLE ON STATION CHANGE                  *         
*         --- BETTER TO CALL PUTRDA *AFTER* SETTING BUYHDDA ON ADDBUY *         
*         --- SUPPORT ORBITS                                          *         
* 12DEC05 010 SUPPORT CHANGING PURPOSE CODES                          *         
* 21JUN05 009 DON'T SEND DUPLICATE D/A'S                              *         
*         --- FIX ADDING BUYS WITH PIGGYS                             *         
* 06JUN05 008 FIX BUG CHANGING PROGRAM NAME TO BLANK                  *         
* 24MAY05 007 DEAL WITH MG APPLY ERROR                                *         
* 28APR05 006 NEW REPLY FOR DESKTOP/OM                                *         
*         --- CODE TO UN-SET PRE VALID BIT (FORCE HEADLINE EDIT)      *         
*         --- MAINTENANCE                                             *         
* 16FEB05 005 FIX BUY ID CODE                                         *         
*         --- SEND ORDER FOR OM ACTIONS                               *         
*         --- ADD CODE TO ALLOW BT= (NO DATA)                         *         
*         --- SUPPORT LIOTERU CALLS (END RUN COMMAND)                 *         
* 29NOV04 004 APPLY/SELF-APPLY/APPROVE/REJECT MAKEGOODS               *         
* 13SEP04 003 MANY FOR MANY MAKEGOODS                                 *         
*         --- DAILY SKED                                              *         
* 13AUG04 002 PROPER ERROR EXIT FROM CHABUY                           *         
*                                                                     *         
*=====================================================================*         
*                                                                               
SPBUY39  TITLE '- SPOTPAK BUY PROGRAM - DESKTOP UPLOAD'                         
SPBUY39  CSECT                                                                  
         LKSVR TYPE=UR,BLOCKS=(LIOBSB1Q,T211FFD,LIOBSB2Q,WORKD)                 
         PRINT NOGEN                                                            
         NMOD1 WORKL,**SB39**,CLEAR=YES                                         
         LR    R9,RC                                                            
         USING WORKD,R9            R9=A(LOCAL W/S)                              
         ST    RB,ASPBUY39         SAVE ENTRY POINT                             
         L     RC,0(R1)                                                         
         USING SPBUYWKD,RC         RC=A(BUY PROGRAM GLOBAL W/S)                 
         L     R7,VBUYTWA                                                       
         USING T211FFD,R7          R7=A(TWA)                                    
         BASR  RA,0                                                             
         AHI   RA,GLOBALS-*                                                     
         USING GLOBALS,RA          RA=A(GLOBAL LITERAL POOL)                    
                                                                                
         L     RE,AREC5            COPY LIOB INTO LOCAL W/S                     
         LA    R0,LIOBAREA                                                      
         LHI   R1,LIOBX-LIOBD                                                   
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         LA    R8,LIOBAREA         AND POINT TO IT                              
         USING LIOBD,R8            R8=A(LOCAL LINKIO CONTROL BLOCK)             
         USING FAWSSVRD,WSSVAREA                                                
                                                                                
         LA    R0,T211FFD                                                       
         ST    R0,LIOBASB1         SET A(TWA)                                   
         LA    R0,WORKD                                                         
         ST    R0,LIOBASB2         SET A(WORKD)                                 
         LA    R0,LINKMAP                                                       
         ST    R0,LIOBAMAP         SET A(RECORD MAP)                            
         MVI   LIOBINDS,LIOBIRET+LIOBISUB+LIOBIGTR+LIOBINRM+LIOBIMRA            
**NOP    MVI   LIOBINDS,LIOBIRET+LIOBISUB+LIOBIGTR                              
                                                                                
         L     RF,VCOMFACS                                                      
         MVC   LINKIO,CLINKIO-COMFACSD(RF)                                      
         MVC   HEXOUT,CHEXOUT-COMFACSD(RF)                                      
         MVC   WSSVR,CWSSVR-COMFACSD(RF)                                        
                                                                                
         L     RF,VBUYSAVE         RESET DRAFT ADD MODE!                        
         NI    SVSPOMAK-BUYSAVE(RF),X'FF'-SVSPOMAK_NOBUY                        
*                                                                               
         XC    BUYHDSTA,BUYHDSTA                                                
*                                                                               
         B     NXTREC                                                           
                                                                                
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* GET NEXT UPLOAD RECORD - ESTABLISH ACTION AND GO TO APPROPRIATE     *         
* PROCESSING ROUTINE - EXIT WHEN ALL RECORDS HAVE BEEN READ           *         
*                                                                     *         
* NOTE THAT BUY HEADER FIELDS (MEDIA THRU STATION) ARE MOVED TO THE   *         
* BUY INPUT SCREEN DIRECTLY DURING THE GET CALL TO LINKIO             *         
***********************************************************************         
                                                                                
NXTREC   GOTOR LINKIO,DMCB,('LIOAGET',LIOBD)                                    
         JH    EXITY               RETURN ON END OF FILE                        
         JE    NXTREC10            PROCESS INPUT RECORD BREAK                   
*                                                                               
***********************************************************************         
* HERE ON A BREAK - NO FIELDS HAVE BEEN PROCESSED YET, ONLY THE       *         
* MAP CODE HAS BEEN ESTABLISHED                                       *         
***********************************************************************         
         XC    A$LINE,A$LINE       ON BREAK CLEAR A$LINE                        
         XC    BUYHDDA,BUYHDDA                                                  
         NI    BY39FLAG,X'FF'-BY39HDDA  TURN OFF D/A SENT FLAG                  
         J     NXTREC                                                           
                                                                                
NXTREC10 DS    0H                                                               
***********************************************************************         
* TRIED TO FORCE A HEADLINE VALIDATION HERE, BUT THAT RESULTED IN DUMPS         
* IN DDLINK, AT DEATH048. HAD TO REMOVE GOBASE CALL.     -HWON 10/28/16         
***********************************************************************         
****NOP  GOTOR GOBASE                                                           
*                                                                               
         BRAS  RE,CHKTWA           CHECK IF TWA IS CLOBBERED                    
                                                                                
         MVI   ADBLOCK,0           MOTHER !@#$%^                                
         L     R1,ADBLOCK                                                       
         XC    0(L'BUDBLOCK,R1),0(R1)                                           
         TM    BUYSTH+4,X'20'      HAS THE STATION CHANGED?                     
         JNZ   NXTREC20             NO                                          
*                                                                               
* FOR CABLE, ONLY CLEAR CKSMTAB IF THE SYSCODE CHANGES                          
*                                                 -HWON 12/02/2014              
         CLI   BUYST,C'0'          ARE WE DOING CABLE?                          
         JL    NXTREC15             NO                                          
         CLC   BUYST(4),BUYHDSTA   YES, MATCH ON SYSCODE?                       
         JE    NXTREC20             YES, DON'T CLEAR CKSM TABLE                 
*                                                                               
NXTREC15 LA    RE,CKSMTAB           YES - CLEAR CHECKSUM TABLE                  
         LHI   RF,L'CKSMTAB*CKSMTABQ                                            
         XR    R0,R0                                                            
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
         MVC   BUYHDSTA,BUYST      SAVE THE HEADER STATION                      
                                                                                
NXTREC20 OC    LIOBSUB#,LIOBSUB#   ENSURE WE HAVE SUB-RECORD MAP NUMBER         
         JNZ   NXTREC30                                                         
         GOTOR GOBASE              MAY JUST WANT TO CHANGE BUYER                
         J     NXTREC                                                           
                                                                                
NXTREC30 LA    RF,ACTTAB           LOOK UP SUB-RECORD IN ACTION TABLE           
         USING ACTTABD,RF                                                       
         LHI   R0,ACTTABN                                                       
         BASR  RE,0                                                             
         CLC   ACTTMAP#,LIOBSUB#   MATCH SUB-RECORD TO ACTION TABLE             
         JE    NXTREC40                                                         
         AHI   RF,ACTTABLQ                                                      
         BCTR  R0,RE                                                            
         DC    H'0'                INVALID SUB-RECORD MAP CODE                  
***********************************************************************         
* BE CAREFUL!! HEADLINES ARE NOT VALIDATED IMMEDIATELY.                         
* THIS MEANS, SVNTDMS IS NOT PROPERLY INITIALIZED.     -HWON 10/28/16           
***********************************************************************         
*&&DO                                                                           
NXTREC40 CLC   LIOBPCV1,=AL1(4,6,0,050)  IF VERSION LESS V4.6.0.50              
         JNL   NXTREC50                                                         
         LAY   RE,SVNTDMS                                                       
         CLC   0(L'SPACES,RE),SPACES        AND HAVE COMSCORE DEMO              
         JH    NXTREC45                                                         
         CLC   L'SPACES(L'SPACES,RE),SPACES                                     
         JNH   NXTREC50                                                         
NXTREC45 GOTOR PUTERR,662                DON'T ALLOW ANY ACTION                 
         J     NXTREC                    MUST UPGRADE TO CHANGE                 
*&&                                                                             
NXTREC40 SR    RE,RE                                                            
         ICM   RE,3,ACTROUT                                                     
         A     RE,ASPBUY39         RE=A(ACTION ROUTINE)                         
         BR    RE                                                               
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* ADD A BUY RECORD - IF SUCCESSFUL RETURN A 'RUN SINGLE BUY DOWNLOAD' *         
* COMMAND AND PASS THE DISK ADDRESS OF THE BUY RECORD JUST ADDED OR   *         
* THE WSSVR TOKEN AND BUY RECORD FOR A SIMULATED BUY ADD. ALSO CREATE *         
* A DUMMY REQUEST ELEMENT CONTAINING THE LINE NUMBER OF THE BUY JUST  *         
* ADDED FOR THE MAKEGOOD ROUTINE TO RECALL THIS BUY                   *         
***********************************************************************         
                                                                                
ADDBUY   CLI   SIMULATE,C'R'       TEST SIMULATE BUY ADD WITH ROUTING           
         JE    *+12                                                             
         CLI   SIMULATE,C'Y'       TEST SIMULATE BUY ADD                        
         JNE   ADDBUY05                                                         
         L     RF,VBUYSAVE                                                      
         OI    SVSPOMAK-BUYSAVE(RF),SVSPOMAK_NOBUY                              
         J     ADDBUY10                                                         
                                                                                
***********************************************************************         
* AFTER A DRAFT ADD, WE ARE GUARANTEED, HEADLINES ARE VALIDATED                 
* WHICH MEANS WE CAN SAFELY CHECK SVNTDMOS         -HWON 10/28/16               
***********************************************************************         
ADDBUY05 BRAS  RE,CHKCOMSC                                                      
         JNE   NXTREC                                                           
         BRAS  RE,CKRDONLY                                                      
         JNE   NXTREC                                                           
*                                                                               
* THESE TWO LINES IS A TEMPORARY FIX FOR THE REAL PROBLEM                       
*                                                                               
ADDBUY10 DS    0H                                                               
         XC    BUBKTYPE,BUBKTYPE                                                
         XC    BUDLUSW,BUDLUSW                                                  
*                                                                               
         GOTOR BLDFLD,DMCB,('BFGOBASE',ADDBUYT),(4,BUYINP1H)                    
         JH    NXTREC              EXIT IF ERROR                                
         JL    ADDBUYMI            ERROR IF NO INPUT DATA                       
                                                                                
         OC    MARKET,MARKET       DID WE GET A MKT TO COMPARE?                 
         JZ    ADDBUY20             NO                                          
         CLC   MARKET,BUYREC+(BUYKMKTN-BUYREC)                                  
         JE    ADDBUY20                                                         
         GOTOR PUTERR,MKTERR                                                    
         J     NXTREC                                                           
                                                                                
ADDBUY20 SR    R2,R2                                                            
         ICM   R2,7,I$NTDEMO+1     IF THERE IS A NON-TRAD DEMO?                 
         JZ    ADDBUY40                                                         
         CLI   LQ_TYPE-LQ_D(R2),LQ_TLSTQ  ENSURE THIS IS AN ARRAY               
         JNE   *+2                                                              
********************************************                                    
* THE BUY PROGRAM WILL SOMETIMES ALTER THE BUY RECORD IN                        
* AREC, SO WE SHOULD ALWAYS REREAD THE BUY RECORD BEFORE A PUTREC               
* BUT DON'T REREAD IF THE ACTION IS SIMULATE                                    
********************************************                                    
         CLI   SIMULATE,C'R'       TEST SIMULATED ADD WITH ROUTING              
         JE    ADDBUY31                                                         
         CLI   SIMULATE,C'Y'       TEST SIMULATED ADD                           
         JE    ADDBUY31                                                         
         GOTOR HIGH                                                             
         JNE   *+2                                                              
         GOTOR GETREC                                                           
         JNE   *+2                                                              
                                                                                
ADDBUY30 GOTOR UPNTDEM,(R2)                                                     
ADDBUY31 XC    I$NTDEMO,I$NTDEMO                                                
                                                                                
ADDBUY40 CLI   SIMULATE,C'R'       TEST SIMULATED ADD WITH ROUTING              
         JE    ADDBUY50                                                         
         CLI   SIMULATE,C'Y'       TEST SIMULATED ADD                           
         JE    ADDBUY50                                                         
                                                                                
ADD$     USING LQ_D,A$LINE         RE=A(DUMMY LINE ENTRY FOR MAKEGOOD)          
         MVI   ADD$.LQ_EL,LQ_RQSTQ                                              
         LHI   R0,L'A$LINE                                                      
         STCM  R0,3,ADD$.LQ_LN                                                  
         MVI   ADD$.LQ_TYPE,LQ_TSINQ                                            
                                                                                
**NOP    LLC   R0,BUYREC+(BUYKBUY-BUYREC)                                       
         ICM   R0,3,BUYREC+(BUYKBUY-BUYREC)   TWO BYTE LINE NUMBERS             
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
                                                                                
         UNPK  ADD$.LQ_VALUE(#$LINE),DUB                                        
         DROP  ADD$                                                             
                                                                                
         GOTOR DOMULT,COMTAB       PROCESS COMMENT INDEX TABLE                  
         OC    BUYHDDA,BUYHDDA     SAVE D/A OF BUY HEADER (BUY ADDED)           
         JNZ   *+2                 BETTER NOT HAVE ONE!                         
                                                                                
         MVC   BUYHDDA,SVBUYDA                                                  
         GOTOR PUTRDA,SVBUYDA      SEND RUN REQUEST AND DISK ADDRESS            
         J     NXTREC                                                           
                                                                                
ADDBUY50 GOTOR PUTRWS              SEND RUN REQUEST AND WSSVR BUFFER            
         J     NXTREC                                                           
                                                                                
ADDBUYMI GOTOR PUTERR,1            SEND MISSING INPUT FIELD IF NO DATA          
         J     NXTREC                                                           
         EJECT                                                                  
***********************************************************************         
* CHANGE A BUY RECORD - IF SUCCESSFUL RETURN A 'RUN SINGLE BUY        *         
* DOWNLOAD' COMMAND AND PASS BACK THE DISK ADDRESS OF THE BUY JUST    *         
* CHANGED                                                             *         
***********************************************************************         
                                                                                
CHABUY   GOTOR RECALL,DMCB,RECNDXT,CHECKSUM,0                                   
         JNE   NXTREC                                                           
                                                                                
***********************************************************************         
* AFTER CALLING RECALL, THE HEADLINES SHOULD BE VALIDATED, WHICH MEANS          
* WE CAN SAFELY CHECK SVNTDMS.                        -HWON 10/28/16            
***********************************************************************         
         BRAS  RE,CHKCOMSC                                                      
         JNE   NXTREC                                                           
         BRAS  RE,CKRDONLY                                                      
         JNE   NXTREC                                                           
                                                                                
         GOTOR DOMULT,CHATAB       PROCESS CHANGE INDEX TABLE                   
         JH    CHABUY50            (RETURN ERROR & DOWNLOAD BUY)                
                                                                                
         CLI   IDEL,C'Y'                                                        
         JNE   CHABUY30                                                         
         GOTOR BLDFLD,DMCB,('BFGOBASE+BFNOINPA',IDELT),(4,BUYINP1H)             
         JNE   NXTREC                                                           
                                                                                
CHABUY30 CLI   CSPILL,C'Y'                                                      
         JNE   CHABUY40                                                         
         GOTOR BLDFLD,DMCB,('BFGOBASE+BFNOINPA',CSPILLT),(4,BUYINP1H)           
         JNE   NXTREC                                                           
                                                                                
CHABUY40 GOTOR DOMULT,COMTAB       PROCESS COMMENT INDEX TABLE                  
                                                                                
         SR    R2,R2                                                            
         ICM   R2,7,I$NTDEMO+1     IF THERE IS A NON-TRAD DEMO?                 
         JZ    CHABUY50                                                         
         CLI   LQ_TYPE-LQ_D(R2),LQ_TLSTQ  ENSURE THIS IS AN ARRAY               
         JNE   *+2                                                              
********************************************                                    
* THE BUY PROGRAM WILL SOMETIMES ALTER THE BUY RECORD IN                        
* AREC, SO WE SHOULD ALWAYS REREAD THE BUY RECORD BEFORE A PUTREC               
********************************************                                    
         GOTOR HIGH                RESTORE THE RECORD                           
         JNE   *+2                                                              
         GOTOR GETREC                                                           
         JNE   *+2                                                              
*                                                                               
         GOTOR UPNTDEM,(R2)                                                     
         XC    I$NTDEMO,I$NTDEMO                                                
                                                                                
CHABUY50 GOTOR PUTRDA,SVBUYDA      SEND RUN REQUEST AND DISK ADDRESS            
         J     NXTREC                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* CHANGE DEMOS ON A BUY RECORD. IF SUCCESSFUL, RETURN A 'RUN SINGLE   *         
* BUY DOWNLOAD' COMMAND AND PASS BACK THE DISK ADDRESS OF THE BUY     *         
* RECORD JUST CHANGED                                                 *         
***********************************************************************         
                                                                                
CHADEM   GOTOR RECALL,DMCB,RECNDXT,CHECKSUM,0                                   
         JNE   NXTREC                                                           
                                                                                
***********************************************************************         
* AFTER CALLING RECALL, THE HEADLINES SHOULD BE VALIDATED, WHICH MEANS          
* WE CAN SAFELY CHECK SVNTDMS.                        -HWON 10/28/16            
***********************************************************************         
         BRAS  RE,CHKCOMSC                                                      
         JNE   NXTREC                                                           
         BRAS  RE,CKRDONLY                                                      
         JNE   NXTREC                                                           
                                                                                
         CLC   LIOBSUB#,=Y(I#SDBBYD)   CHANGE DEMOS OR POST BUY DEMOS?          
         JNE   CHADEM10                 POST BUY                                
         GOTOR BLDFLD,DMCB,('BFGOBASE',CHADEMT),(4,BUYINP1H)                    
         J     CHADEM20                                                         
                                                                                
CHADEM10 GOTOR BLDFLD,DMCB,('BFGOBASE',CHAPBDT),(4,BUYINP1H)                    
                                                                                
CHADEM20 SR    R2,R2                                                            
         ICM   R2,7,I$NTDEMO+1     IF THERE IS A NON-TRAD DEMO?                 
         JZ    CHADEM30                                                         
         CLI   LQ_TYPE-LQ_D(R2),LQ_TLSTQ  ENSURE THIS IS AN ARRAY               
         JNE   *+2                                                              
********************************************                                    
* THE BUY PROGRAM WILL SOMETIMES ALTER THE BUY RECORD IN                        
* AREC, SO WE SHOULD ALWAYS REREAD THE BUY RECORD BEFORE A PUTREC               
* EG. WHEN UPDATING SPILL, SPBUY08 @CHA50 CALS CALLDSP FOUND IN SPBUY22         
* THAT ROUTINE @LD501X OVERWRITE THE SPILL ELEM NDAGYMKT WITH NDPROG            
********************************************                                    
         GOTOR HIGH                                                             
         JNE   *+2                                                              
         GOTOR GETREC                                                           
         JNE   *+2                                                              
*                                                                               
         GOTOR UPNTDEM,(R2)                                                     
         XC    I$NTDEMO,I$NTDEMO                                                
                                                                                
CHADEM30 GOTOR PUTRDA,SVBUYDA      SEND RUN REQUEST AND DISK ADDRESS            
         J     NXTREC                                                           
         EJECT                                                                  
***********************************************************************         
* SPLIT A BUY RECORD -OR- SEPARATE SPOT                                         
* - IF SUCCESSFUL RETURN A 'RUN SINGLE BUY DOWNLOAD' COMMAND AND PASS           
* BACK THE DISK ADDRESS OF THE BUY JUST CHANGED AND THE BUY JUST ADDED          
***********************************************************************         
                                                                                
SPLBUY   GOTOR RECALL,DMCB,RECNDXT,CHECKSUM,0                                   
         JNE   NXTREC                                                           
                                                                                
***********************************************************************         
* AFTER CALLING RECALL, THE HEADLINES SHOULD BE VALIDATED, WHICH MEANS          
* WE CAN SAFELY CHECK SVNTDMS.                        -HWON 10/28/16            
***********************************************************************         
         BRAS  RE,CHKCOMSC                                                      
         JNE   NXTREC                                                           
         BRAS  RE,CKRDONLY                                                      
         JNE   NXTREC                                                           
                                                                                
         MVC   SAVEDA,SVBUYDA                                                   
*                                                                               
         CLC   LIOBSUB#,=Y(I#SDBSPS)   SEPARATE SPOT?                           
         JNE   SPLBUY10                 NO                                      
*                                                                               
         USING LQ_D,RE                                                          
         ICM   RE,7,I$WEEK+1       RE=A(SPOT DATE ARRAY)                        
         JZ    *+2                                                              
         CLI   LQ_TYPE,LQ_TLSTQ    ENSURE THIS IS AN ARRAY                      
         JNE   *+2                                                              
         CLHHSI LQ_VALUE,7         MORE THAN 7 ENTRIES?                         
         JH    SPLBUYER             YES, SEND ERROR                             
         GOTOR BLDFLD,DMCB,('BFGOBASE',SEPSPTT),(4,BUYINP1H)                    
         J     SPLBUY20                                                         
*                                                                               
SPLBUY10 GOTOR BLDFLD,DMCB,('BFGOBASE',SPLBUYT),(4,BUYINP1H)                    
SPLBUY20 JNE   NXTREC                                                           
         GOTOR PUTRDA,SAVEDA       SEND RUN REQUEST AND DISK ADDRESSES          
         GOTOR PUTRDA,SVBUYDA      FOR ORIGINAL LINE AND SPLIT LINE             
*&&DO                                                                           
         GOTOR LINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRUN',I#SDBUYR)               
         GOTOR (RF),(R1),('LIOAPUT',LIOBD),('LIOTRAW',D#DA),           *        
               ('LD_HEXDQ',SVBUYDA),(L'SVBUYDA,0)                               
         GOTOR (RF),(R1),('LIOAPUT',LIOBD),('LIOTERU',0),0,0                    
*&&                                                                             
         J     NXTREC                                                           
*                                                                               
SPLBUYER GOTOR PUTERR,23                                                        
         J     NXTREC                                                           
         EJECT                                                                  
***********************************************************************         
* CHANGE SPOTS ON A BUY RECORD (+/- OTO).  IF SUCCESSFUL, RETURN A    *         
* 'RUN SINGLE BUY DOWNLOAD' COMMAND AND PASS BACK THE DISK ADDRESS OF *         
* THE BUY JUST CHANGED                                                *         
***********************************************************************         
                                                                                
OTOSPT   GOTOR RECALL,DMCB,RECNDXT,CHECKSUM,0                                   
         JNE   NXTREC                                                           
                                                                                
***********************************************************************         
* AFTER CALLING RECALL, THE HEADLINES SHOULD BE VALIDATED, WHICH MEANS          
* WE CAN SAFELY CHECK SVNTDMS.                        -HWON 10/28/16            
***********************************************************************         
         BRAS  RE,CHKCOMSC                                                      
         JNE   NXTREC                                                           
         BRAS  RE,CKRDONLY                                                      
         JNE   NXTREC                                                           
                                                                                
         GOTOR BLDFLD,DMCB,('BFGOBASE',OTOSPTT),(4,BUYINP1H)                    
         GOTOR PUTRDA,SVBUYDA      SEND RUN REQUEST AND DISK ADDRESS            
         J     NXTREC                                                           
         EJECT                                                                  
***********************************************************************         
* ALLOCATE SPOTS ON A BUY RECORD. IF SUCCESSFUL, RETURN A 'RUN SINGLE *         
* BUY DOWNLOAD' COMMAND AND PASS BACK THE DISK ADDRESS OF THE BUY     *         
* RECORD JUST CHANGED                                                 *         
***********************************************************************         
                                                                                
ALCSPT   GOTOR RECALL,DMCB,RECNDXT,CHECKSUM,0                                   
         JNE   NXTREC                                                           
                                                                                
***********************************************************************         
* AFTER CALLING RECALL, THE HEADLINES SHOULD BE VALIDATED, WHICH MEANS          
* WE CAN SAFELY CHECK SVNTDMS.                        -HWON 10/28/16            
***********************************************************************         
         BRAS  RE,CHKCOMSC                                                      
         JNE   NXTREC                                                           
         BRAS  RE,CKRDONLY                                                      
         JNE   NXTREC                                                           
                                                                                
         GOTOR BLDFLD,DMCB,('BFGOBASE',ALCSPTT),(4,BUYINP1H)                    
         GOTOR PUTRDA,SVBUYDA      SEND RUN REQUEST AND DISK ADDRESS            
         J     NXTREC                                                           
         EJECT                                                                  
***********************************************************************         
* ADD/CHANGE MAKEGOODS. RECALL HEADLINES FOR ALL LINES REFERENCED     *         
* TO COMPUTE CHECKSUMS AND SAVE D/A'S.  THEN PERFORM CHANGE ON        *         
* NEW/PASSED LINE TO REFERENCE MADEGOOD LINES, RETURN D/A OF PASSED   *         
* LINE AND RETURN A 'RUN SINGLE BUY DOWNLOAD' COMMAND FOR IT (IF NOT  *         
* JUST ADDED THE MAKEGOOD BUY) AND ALL MADEGOOD BUY LINES             *         
***********************************************************************         
                                                                                
MGDBUY   GOTOR RECALL,DMCB,RECNDXT,CHECKSUM,0                                   
         JNE   NXTREC                                                           
                                                                                
***********************************************************************         
* AFTER CALLING RECALL, THE HEADLINES SHOULD BE VALIDATED, WHICH MEANS          
* WE CAN SAFELY CHECK SVNTDMS.                        -HWON 10/28/16            
***********************************************************************         
         BRAS  RE,CHKCOMSC                                                      
         JNE   NXTREC                                                           
         BRAS  RE,CKRDONLY                                                      
         JNE   NXTREC                                                           
                                                                                
         LA    RE,MGDATAB          CLEAR DA STORAGE TABLE                       
         LHI   RF,MGDATABL                                                      
         XR    R0,R0                                                            
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
         MVC   0(4,RE),=X'FFFFFFFF'                                             
                                                                                
         SR    RE,RE                                                            
         ICM   RE,7,I$MGDCOD+1     IF THERE IS A MG CODE,                       
         JNZ   MGDBY150             IT'S JUST A SIMPLE CHANGE                   
                                                                                
         MVC   S$MGDARY,I$MGDARY                                                
         ICM   RE,7,S$MGDARY+1     RE=A(MAKEGOOD ARRAY)                         
         JNZ   *+6                                                              
         DC    H'0'                                                             
         USING LQ_D,RE                                                          
         CLI   LQ_TYPE,LQ_TLSTQ    ENSURE THIS IS AN ARRAY                      
         JE    *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         ICM   R0,3,LQ_VALUE       N'ARRAY ENTRIES (ROWS)                       
         CHI   R0,MGDATABQ         MAKE SURE D/A TABLE BIG ENOUGH               
         JNH   *+6                                                              
         DC    H'0'                                                             
         LA    R5,LQ_VALUE+2       A(FIRST ARRAY ENTRY)                         
         DROP  RE                                                               
         LA    R2,MGDATAB          R2=A(LIST OF MADEGOOD BUY D/A'S)             
                                                                                
MGD$     USING LQ_D,M$LINE         BUILD DUMMY ARRAY LINE                       
         MVI   MGD$.LQ_EL,LQ_RQSTQ                                              
         LHI   RF,M$LINEL                                                       
         STCM  RF,3,MGD$.LQ_LN                                                  
         MVI   MGD$.LQ_TYPE,LQ_TLSTQ                                            
         LHI   RF,1                                                             
         STCM  RF,3,MGD$.LQ_VALUE                                               
         LA    RF,MGD$.LQ_D                                                     
         ST    RF,I$MGDARY         POINT INDEX TO DUMMY ARRAY LINE              
         DROP  MGD$                                                             
                                                                                
MGDBY010 MVC   M$DATA(M$DATAL),0(R5)                                            
         MVC   M$CKSM,M$DATAL(R5)                                               
         MVI   BYTE,X'80'          SET FLAG FOR RECALL                          
                                                                                
         CLC   LIOBPCV1,=AL1(4,5,0,000)    VERSION 4.5.0.0                      
         JL    MGDBY070                                                         
         OC    M$NETWK,M$NETWK     NETWORK PASSED?                              
         JZ    MGDBY070                                                         
         MVI   BYTE,X'C0'          SET FLAG FOR RECALL                          
MGDK     USING BUYKEY,KEY          BUILD MISSED CABLE BUY KEY                   
         XC    KEY,KEY                                                          
         MVC   MGDK.BUYKEY(10),BUYREC SET AM/C/P/M/E FROM OFFER LINE            
         MVC   DUB(5),BUYST        GET SYS CODE FROM SCREEN,                    
         MVC   DUB+5(3),M$NETWK     MISSED NTWK FROM INPUT, AND                 
         LA    R6,BUYSTXP          MARKET FROM SCREEN                           
*                                  AND PACK IT INTO BUY MKT/STA KEY             
         GOTO1 STAPACK,DMCB,(C'P',(R6)),DUB,MGDK.BUYMSTA                        
                                                                                
         LA    RF,M$LINE#          GET MISSED LINE NUMBER FROM INPUT            
         LA    RE,M$LINE#+2                                                     
MGDBY040 CR    RE,RF               ADJUST FOR TRAILING SPACES                   
         JL    *+2                                                              
         CLI   0(RE),X'40'         SPACE?                                       
         JH    MGDBY050                                                         
         JCT   RE,MGDBY040         YES                                          
MGDBY050 SR    RE,RF                                                            
         LAY   R1,MGDBY060                                                      
         EX    RE,0(R1)                                                         
         J     *+10                                                             
MGDBY060 PACK  DUB,M$LINE#(0)      GET LINE NUMBER                              
         CVB   R1,DUB                                                           
         STCM  R1,3,MGDK.BUYKLIN   SET 2-BYTE LINE #                            
         DROP  MGDK                                                             
                                                                                
MGDBY070 GOTOR RECALL,DMCB,(BYTE,RECFLDT),M$CKSM,0                              
         JNE   NXTREC                                                           
         MVC   0(L'BUYKDA,R2),SVBUYDA                                           
         AHI   R5,M$DATAL+L'M$CKSM  BUMP TO NEXT ARRAY LINE                     
         AHI   R2,L'MGDATAB        BUMP DISK ADDRESS TABLE POINTER              
         BRCT  R0,MGDBY010                                                      
                                                                                
         LA    R0,CHECKSUM         SET CHECKSUM TEST REQUIRED                   
         CLI   A$LINE,0            TEST JUST ADDED A MAKEGOOD LINE              
         JE    MGDBY080            NO                                           
         SR    R0,R0               YES - SET NO CHECKNUM REQUIRED               
         LA    RF,A$LINE           AND POINT TO LINE JUST ADDED                 
         ST    RF,I$LINE                                                        
                                                                                
MGDBY080 GOTOR RECALL,DMCB,(X'80',RECNDXT),(R0),0                               
         JE    *+6                                                              
         DC    H'0'                                                             
         CLI   A$LINE,0            TEST JUST ADDED MAKEGOOD LINE                
         JNE   MGDBY090            YES - DISK ADDRESS ALREADY SENT              
         GOTOR PUTRDA,SVBUYDA                                                   
                                                                                
MGDBY090 SR    RE,RE               DO MAKEGOODS                                 
         ICM   RE,7,S$MGDARY+1     RE=A(MAKEGOOD ARRAY)                         
         USING LQ_D,RE                                                          
         SR    R0,R0                                                            
         ICM   R0,3,LQ_VALUE       N'ARRAY ENTRIES (ROWS)                       
         LA    R5,LQ_VALUE+2       A(FIRST ARRAY ENTRY)                         
         DROP  RE                                                               
         LA    R2,MGDATAB          R2=A(LIST OF MADEGOOD BUY D/A'S)             
         XC    SVBUMGCD,SVBUMGCD                                                
         XC    SAVEDA,SAVEDA                                                    
                                                                                
MGDBY100 MVC   M$DATA(M$DATAL),0(R5)                                            
         GOTOR BLDFLD,DMCB,('BFGOBASE',MGDTABT),(4,BUYINP1H)                    
         JNE   MGDBY140                                                         
         OC    SVBUMGCD,SVBUMGCD   FIRST CHANGE?                                
         JNZ   *+10                 NO                                          
         MVC   SVBUMGCD,BUMGCODE    YES - SAVE MAKEGOOD CODE                    
                                                                                
         CLC   SAVEDA,0(R2)        SAME AS LAST D/A?                            
         JE    MGDBY130             YES - DON'T SEND AGAIN                      
         MVC   SAVEDA,0(R2)                                                     
                                                                                
         LA    RF,MGDATAB                                                       
         CR    RF,R2               FIRST TIME IN?                               
         JE    MGDBY120             YES - SKIP TEST!                            
                                                                                
MGDBY110 CLC   0(L'BUYKDA,RF),0(R2)                                             
         JE    MGDBY130            WE'VE ALREADY SENT THIS D/A                  
         AHI   RF,L'MGDATAB                                                     
         CR    RF,R2                                                            
         JNL   MGDBY110                                                         
                                                                                
MGDBY120 GOTOR PUTRDA,(R2)         SEND RUN REQUEST AND DISK ADDRESS            
                                                                                
MGDBY130 AHI   R5,M$DATAL+L'M$CKSM   BUMP TO NEXT ARRAY LINE                    
         AHI   R2,L'MGDATAB        BUMP DISK ADDRESS TABLE POINTER              
         BRCT  R0,MGDBY100                                                      
         J     NXTREC                                                           
                                                                                
MGDBY140 GOTOR PUTRDA,(R2)         SEND RUN REQUEST AND DISK ADDRESS            
         J     NXTREC              OF OFFENDING BUY AND ON TO NEXT              
                                                                                
         USING LQ_D,RE                                                          
MGDBY150 CLI   LQ_TYPE,LQ_TSINQ    ENSURE THIS IS A SINGLE VALUE                
         JE    *+6                                                              
         DC    H'0'                                                             
         CLC   =C'##',LQ_VALUE     PART OF MANY FOR MANY MG?                    
         JNE   *+10                                                             
         MVC   LQ_VALUE(L'BUMGCODE),SVBUMGCD                                    
                                                                                
         LA    RF,MGDATAB                                                       
         ST    RF,SVDATABP         BUY00 SAVES DA'S OF CHANGED BUYS             
                                                                                
         GOTOR BLDFLD,DMCB,('BFGOBASE',MGDTABT),(4,BUYINP1H)                    
         JNE   MGDBY200                                                         
                                                                                
         LA    R2,MGDATAB          R2=A(LIST OF MADEGOOD BUY D/A'S)             
         XC    SAVEDA,SAVEDA                                                    
                                                                                
MGDBY160 CLC   SAVEDA,0(R2)        SAME AS LAST D/A?                            
         JE    MGDBY190             YES - DON'T SEND AGAIN                      
         MVC   SAVEDA,0(R2)                                                     
                                                                                
         LA    RF,MGDATAB                                                       
         CR    RF,R2               FIRST TIME IN?                               
         JE    MGDBY180             YES - SKIP THIS TEST!                       
                                                                                
MGDBY170 CLC   0(L'BUYKDA,RF),0(R2)                                             
         JE    MGDBY190            WE'VE ALREADY SENT THIS D/A                  
         AHI   RF,L'MGDATAB                                                     
         CR    RF,R2                                                            
         JL    MGDBY170                                                         
                                                                                
MGDBY180 GOTOR PUTRDA,(R2)         SEND RUN REQUEST AND DISK ADDRESS            
                                                                                
MGDBY190 AHI   R2,L'MGDATAB        BUMP DISK ADDRESS TABLE POINTER              
         OC    0(L'MGDATAB,R2),0(R2)                                            
         JZ    MGDBY200                                                         
         CLC   =X'FFFFFFFF',0(R2)                                               
         JNE   MGDBY160                                                         
                                                                                
MGDBY200 XC    SVDATABP,SVDATABP                                                
         J     NXTREC                                                           
         EJECT                                                                  
***********************************************************************         
* APPLY/SELF-APPLY/APPROVE/REJECT MAKEGOODS (OM)                      *         
***********************************************************************         
                                                                                
MGDAPP   LA    RE,MGDATAB          CLEAR DA STORAGE TABLE                       
         ST    RE,SVDATABP         BUY00 SAVES DA'S OF ADD/CHGD BUYS            
         LHI   RF,MGDATABL                                                      
         XR    R0,R0                                                            
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
         MVC   0(4,RE),=X'FFFFFFFF'                                             
                                                                                
*                                                                               
         GOTOR SAVETIA             SAVE TIA                                     
         GOTOR BLDFLD,DMCB,('BFGOBASE',MGETABT),(4,BUYINP1H)                    
         JH    MGDAP90                                                          
         GOTOR RESTTIA             RESTORE TIA                                  
                                                                                
***********************************************************************         
* AFTER CALLING BLDFLD, MGE=XXXXXX AND HEADLINE ARE VALIDATED, WHICH            
* MEANS WE CAN SAFELY CHECK SVNTDMS.                   -HWON 10/28/16           
***********************************************************************         
         BRAS  RE,CHKCOMSC                                                      
         JNE   NXTREC                                                           
         BRAS  RE,CKRDONLY                                                      
         JNE   NXTREC                                                           
*                                                                               
MGDAP10  L     RE,VBUYSAVE                                                      
         LAY   RE,SVMGSAVE-BUYSAVE(RE)                                          
         NI    MGMSCFLG-MGSAVE(RE),X'FF'-MGAUTOMG   TURN OFF X'20'              
         ICM   RF,15,I$MGAUTO         AUTOMG APP/REJ?                           
         JZ    MGDAP20                 NO                                       
         CLI   LQ_VALUE-LQ_D(RF),C'Y'                                           
         JNE   MGDAP20                 NO                                       
         OI    MGMSCFLG-MGSAVE(RE),MGAUTOMG         TURN ON X'20'               
                                                                                
MGDAP20  CLC   LIOBSUB#,=Y(I#SDMGRJ)  MKGD REJECTION?                           
         JNE   MGDAP30                 NO                                       
         ICM   RF,15,I$MGCMT          HAVE REJ COMMENT?                         
         JZ    MGDAP30                 NO                                       
*                                                                               
         LLH   RE,LQ_LN-LQ_D(RF)      GET LENGTH OF LQ_D ELEM                   
         AHI   RE,-(LQ_VALUE-LQ_D)-1  ISOLATE L'COMMENT MINUS 1 FOR EX          
         CLC   LQ_VALUE-LQ_D(0,RF),SPACES                                       
         EXRL  RE,*-6                 DID THEY ENTER SPACES?                    
         JH    MGDAP25                 NO                                       
         XC    I$MGCMT,I$MGCMT         YES, CLEAR ADDRESS                       
         J     MGDAP30                                                          
*                                                                               
MGDAP25  L     RF,VBUYSAVE                                                      
         LAY   RF,SVMGSAVE-BUYSAVE(RF)                                          
         MVC   MGREJCOM-MGSAVE(4,RF),=X'FFFFFFFF'                               
         MVC   MGREJCOM-MGSAVE+4(4,RF),I$MGCMT                                  
         MVC   MGREJCOM-MGSAVE+8(4,RF),I$MGCMT2                                 
         MVC   MGREJCOM-MGSAVE+12(4,RF),I$MGCMT3                                
         MVC   MGREJCOM-MGSAVE+16(4,RF),I$MGCMT4                                
         MVC   MGREJCOM-MGSAVE+20(4,RF),I$MGCMT5                                
*                                                                               
MGR$     USING LQ_D,RJ$LINE        RE=A(DUMMY LINE ENTRY FOR MAKEGOOD)          
         XC    RJ$LINE,RJ$LINE     CLEAR RJ$LINE                                
         MVI   MGR$.LQ_EL,LQ_RQSTQ                                              
         LHI   R0,L'RJ$LINE                                                     
         STCM  R0,3,MGR$.LQ_LN                                                  
         MVI   MGR$.LQ_TYPE,LQ_TSINQ                                            
         MVC   MGR$.LQ_VALUE(#$LINE),=C'###'                                    
         DROP  MGR$                                                             
         LA    RE,RJ$LINE                                                       
         ST    RE,I$MGCMT                                                       
*                                                                               
MGDAP30  XR    RF,RF                                                            
         LA    R2,MGATAB           R2=A(INDEX TABLE)                            
                                                                                
MGDAP35  CLC   LIOBSUB#,2(R2)                                                   
         JNE   MGDAP40                                                          
         ICM   RF,3,0(R2)          RF=DISPLACEMENT TO FIELD TABLE               
         JZ    MGDAP90             EXIT IF ALL DONE                             
         AR    RF,R2               RF=A(LIOF)                                   
         GOTOR SAVETIA             SAVE TIA                                     
         GOTOR BLDFLD,DMCB,('BFGOBASE',(RF)),(4,BUYINP1H)                       
         JH    MGDAP90                                                          
         J     MGDAP50                                                          
                                                                                
MGDAP40  AHI   R2,L'MGATAB         BUMP TO NEXT TABLE ENTRY                     
         OC    0(6,R2),0(R2)                                                    
         JNZ   MGDAP35                                                          
         J     MGDAP90                                                          
                                                                                
MGDAP50  GOTOR RESTTIA             RESTORE TIA                                  
                                                                                
         OC    ORDER,ORDER         IF THEY DON'T SEND AN ORDER NUMBER,          
         JZ    MGDAP60              DON'T TRY TO RETURN ORDER DATA              
         SR    R0,R0                                                            
         ICM   R0,3,4(R2)                                                       
         GOTOR LINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRUN',(R0))                   
         GOTOR (RF),(R1),('LIOAPUT',LIOBD),('LIOTRAW',D#MEDIA),        *        
               ('LD_CHARQ',BUYMD),(L'BUYMD,0)                                   
         GOTOR (RF),(R1),('LIOAPUT',LIOBD),('LIOTRAW',D#ORDNO),        *        
               ('LD_CHARQ',ORDER),(L'ORDER,0)                                   
         GOTOR (RF),(R1),('LIOAPUT',LIOBD),('LIOTERU',0),0,0                    
                                                                                
MGDAP60  LA    R2,MGDATAB          R2=A(LIST OF MADEGOOD BUY D/A'S)             
         XC    SAVEDA,SAVEDA                                                    
                                                                                
MGDAP70  CLC   SAVEDA,0(R2)        SAME AS LAST D/A?                            
         JE    MGDAP85              YES - DON'T SEND AGAIN                      
         MVC   SAVEDA,0(R2)                                                     
                                                                                
         LA    RF,MGDATAB                                                       
         CR    RF,R2               FIRST TIME IN?                               
         JE    MGDAP80              YES - SKIP THIS TEST!                       
                                                                                
MGDAP75  CLC   0(L'BUYKDA,RF),0(R2)                                             
         JE    MGDAP85             WE'VE ALREADY SENT THIS D/A                  
         AHI   RF,L'MGDATAB                                                     
         CR    RF,R2                                                            
         JL    MGDAP75                                                          
                                                                                
MGDAP80  GOTOR PUTRDA,(R2)         SEND RUN REQUEST AND DISK ADDRESS            
                                                                                
MGDAP85  AHI   R2,L'MGDATAB        BUMP DISK ADDRESS TABLE POINTER              
         OC    0(L'MGDATAB,R2),0(R2)                                            
         JZ    MGDAP90                                                          
         CLC   =X'FFFFFFFF',0(R2)                                               
         JNE   MGDAP70                                                          
                                                                                
MGDAP90  XC    SVDATABP,SVDATABP                                                
         J     NXTREC                                                           
         EJECT                                                                  
***********************************************************************         
* DELETE A BUY RECORD                                                 *         
***********************************************************************         
                                                                                
DELBUY   GOTOR RECALL,DMCB,RECNDXT,CHECKSUM,0                                   
         JNE   NXTREC                                                           
                                                                                
***********************************************************************         
* AFTER CALLING RECALL, THE HEADLINES SHOULD BE VALIDATED, WHICH MEANS          
* WE CAN SAFELY CHECK SVNTDMS.                        -HWON 10/28/16            
***********************************************************************         
         BRAS  RE,CHKCOMSC                                                      
         JNE   NXTREC                                                           
         BRAS  RE,CKRDONLY                                                      
         JNE   NXTREC                                                           
                                                                                
         GOTOR BLDFLD,DMCB,('BFGOBASE+BFNOINPA',DELBUYT),(4,BUYINP1H)           
         JH    NXTREC                                                           
         CLC   LIOBPCV1,=X'0300004E' VERSION 3.0.0.78                           
         JL    DELBUYX                                                          
         GOTOR LINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',O#SDBUYR)               
         GOTOR (RF),(R1),('LIOAPUT',LIOBD),('LIOTRAW',D#PCKEY),        *        
               ('LD_VSTRQ',PCKEY),('#$PCKEY',0)                                 
DELBUYX  J     NXTREC                                                           
         EJECT                                                                  
***********************************************************************         
* UPDATE THE X'50' NON-TRADITIONAL DEMO ELEMENT AND PUTREC BUY REC              
* ON ENTRY :   R1      == I$NTDEMO                                              
***********************************************************************         
UPNTDEM  NTR1                                                                   
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,3,LQ_VALUE-LQ_D(R1) R4 = N'ARRAY ENTRIES                      
         LA    R5,LQ_VALUE+2-LQ_D(R1) R5 = A(FIRST ARRAY ENTRY)                 
         USING UPNTDEMD,R5                                                      
*                                                                               
         XC    ELEM,ELEM           INIT ELEM                                    
         MVI   ELCDLO,NTDELCDQ     X'50' - NON-TRAD DEMO ELEMENT                
         MVI   ELCDHI,NTDELCDQ                                                  
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL           DO WE HAVE ONE?                              
         JNE   UPND005              NO                                          
         XC    ELEM,ELEM            YES, LETS SAVE IT IN ELEM                   
         LLC   RE,1(R6)                                                         
         BCTR  RE,0                                                             
         MVC   ELEM(0),0(R6)                                                    
         LAY   RF,*-6                                                           
         EX    RE,0(RF)                                                         
         BRAS  RE,DELEL            DELETE THE OLD X'50' NON-TRAD ELEM           
*                                                                               
         USING NTDELEM,R2                                                       
UPND005  LA    R2,ELEM             R2 = A(ELEM)                                 
         MVI   NTDELCD,NTDELCDQ    X'50' - NON-TRAD DEMO ELEMENT                
         MVI   NTDELLN,NTDOVHDQ                                                 
                                                                                
UPND010  LAY   RE,SVNTDMS                                                       
         LAY   RF,SVNTDMS+L'SVNTDMS                                             
UPND020  CR    RE,RF               END OF LIST?                                 
         JE    *+2                  THAT NOT GOOD                               
         CLC   UPNTDEMN,0(RE)      FOUND A MATCH?                               
         JE    UPND030              YES                                         
         LA    RE,L'UPNTDEMN(RE)    NO, BUMP TO NEXT ENTRY                      
         J     UPND020                                                          
*                                                                               
UPND030  LAY   RF,SVNTDMS-L'NTDDMONM  LETS CALCULATE THE INDEX                  
         SR    RE,RF               GET THE DISPLACEMENT                         
         JM    *+2                 MAKE SURE IT NOT NEGATIVE                    
         SRL   RE,3                DIVIDE BY 8 TO GET INDEX                     
                                                                                
         MHI   RE,L'NTDDMONM+L'NTDDMOFL   MULT INDEX BY L'ENTRY                 
         LA    RE,NTDOVHDQ(RE)     ADD OVRD TO CALCULATE NEW L'ELEMENT          
                                                                                
         CLM   RE,1,NTDELLN        IS NEW L'ELEMENT GT WHAT IS SET?             
         JL    *+8                 NO                                           
         STC   RE,NTDELLN          YES, SAVE NEW L'ELEMENT                      
*                                                                               
         LAY   RE,ELEM-L'NTDDMONM-L'NTDDMOFL(RE) RE=A(ARRAY ENTRY LOC)          
NT       USING NTDDMONM,RE                                                      
         MVC   NT.NTDDMONM,UPNTDEMN                                             
*                                                                               
         CLC   LIOBSUB#,=Y(I#SDBBYD)   CHANGE DEMOS OR POST BUY DEMOS?          
         JE    UPND040                                                          
         MVC   NT.NTDDMOFL,UPNTFLAG                                             
         J     UPND045                                                          
*                                                                               
UPND040  MVI   BYTE,NTDDFDLK+NTDDLKNA  ORIG DEMO, ONLY PROC X'80'&X'20'         
         OC    I$MARKET,I$MARKET   SPILL?                                       
         JZ    UPND040A             NO                                          
         MVI   BYTE,NTDDFSDL        YES, SPIL DEMO, ONLY PROC X'40'             
*                                                                               
UPND040A NC    UPNTFLAG,BYTE       RESET BITS EXCEPT OUR CHANGE BITS            
         XI    BYTE,X'FF'          FLIP BITS SO WE CAN                          
         NC    NT.NTDDMOFL,BYTE    RESET REC BITS EXCEPT UNCHANGED BITS         
         OC    NT.NTDDMOFL,UPNTFLAG LASTLY, SET THE BIT(S)                      
*                                                                               
UPND045  AHI   R5,UPNTDEML         GET NEXT ENTRY                               
         JCT   R4,UPND010                                                       
         DROP  NT,R2,R5                                                         
                                                                                
         MVI   ELCDLO,NTDELCDQ     X'50' - NON-TRAD DEMO ELEMENT                
         MVI   ELCDHI,NTDELCDQ                                                  
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL           DO WE HAVE ONE?                              
         JE    *+2                  SOMETHING IS WRONG                          
         BRAS  RE,ADDEL            AND ADD NEW ONE                              
*                                                                               
         CLI   SIMULATE,C'R'       TEST SIMULATED ADD WITH ROUTING              
         JE    EXITY                                                            
         CLI   SIMULATE,C'Y'       TEST SIMULATED ADD                           
         JE    EXITY                                                            
                                                                                
         GOTO1 PUTREC                                                           
         J     EXITY                                                            
                                                                                
UPNTDEMD DSECT                 ** DSECT TO COVER NON-TRAD DEMO ARRAY **         
UPNTDEMN DS    CL8                DEMO NAME                                     
UPNTFLAG DS    C                  DEMO NAME FLAG                                
UPNTDEML EQU   *-UPNTDEMD                                                       
                                                                                
SPBUY39  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* CHANGE ORBITS ON A BUY RECORD.  IF SUCCESSFUL, RETURN A             *         
* 'RUN SINGLE BUY DOWNLOAD' COMMAND AND PASS BACK THE DISK ADDRESS OF *         
* THE BUY JUST CHANGED                                                *         
***********************************************************************         
                                                                                
ORBIT    GOTOR RECALL,DMCB,RECNDXT,CHECKSUM,0                                   
         JNE   NXTREC                                                           
                                                                                
***********************************************************************         
* AFTER CALLING RECALL, THE HEADLINES SHOULD BE VALIDATED, WHICH MEANS          
* WE CAN SAFELY CHECK SVNTDMS.                        -HWON 10/28/16            
***********************************************************************         
         BRAS  RE,CHKCOMSC                                                      
         JNE   NXTREC                                                           
         BRAS  RE,CKRDONLY                                                      
         JNE   NXTREC                                                           
                                                                                
         MVI   ELCDLO,ACCODEQ      MAKE SURE NO X'10' AFFIDS!                   
         MVI   ELCDHI,ACCODEQ                          (PC SHOULD CK)           
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         JNE   *+6                                                              
         DCHO                                                                   
                                                                                
         MVI   ELCDLO,X'67'        FIND AND DELETE OLD ORBIT                    
         MVI   ELCDHI,X'67'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         JNE   ORB10                                                            
**NOP    GOTO1 VRECUP,DMCB,BUYREC,(R6)                                          
         BRAS  RE,DELEL                                                         
         ICM   R2,3,BMGEERR        TEST FOR ERRORS                              
         JZ    ORB10                NONE                                        
         GOTOR BASERR                                                           
         J     ORBX20                                                           
                                                                                
         USING LQ_D,RE                                                          
ORB10    ST    R6,FULL             FULL IS WHERE TO ADD NEW ELEM                
         ICM   RE,7,I$ORBIT+1      RE=A(ORBIT ARRAY)                            
         JZ    ORBITX              MUST BE DELETING ALL ORBITS                  
         CLI   LQ_TYPE,LQ_TLSTQ    ENSURE THIS IS AN ARRAY                      
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         SR    R0,R0                                                            
         ICM   R0,3,LQ_VALUE       N'ARRAY ENTRIES (ROWS)                       
         JZ    ORBITX                                                           
         LA    R5,LQ_VALUE+2       A(FIRST ARRAY ENTRY)                         
         DROP  RE                                                               
                                                                                
         USING ORBARYD,R5                                                       
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'67'                                                       
         LR    RF,R0               COMPUTE ELEM LENGTH (4+16*N'SHOWS)           
         SLL   RF,4                                                             
         AHI   RF,4                                                             
         STC   RF,ELEM+1                                                        
         LA    R6,ELEM+4                                                        
                                                                                
ORB20    LA    R2,WORK             DUMMY FLD HDR                                
         XC    WORK,WORK                                                        
         MVI   WORK+5,L'ORB$DAY    DUMMY INPUT LENGTH                           
         MVC   WORK+8(L'ORB$DAY),ORB$DAY                                        
         OC    WORK+8(L'ORB$DAY),SPACES                                         
         LA    RF,8(R2)                                                         
         ST    RF,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVI   EDTVAL,DAYEDT                                                    
         GOTO1 CALLEDT                                                          
         ICM   R2,3,BMGEERR        TEST FOR ERRORS                              
         JZ    ORB40                NONE                                        
         GOTOR BASERR                                                           
         J     ORBX20                                                           
                                                                                
ORB40    LA    R2,WORK                                                          
         XC    WORK,WORK                                                        
         MVI   WORK+5,L'ORB$TIME                                                
         MVC   WORK+8(L'ORB$TIME),ORB$TIME                                      
         OC    WORK+8(L'ORB$TIME),SPACES                                        
         LA    RF,8(R2)                                                         
         ST    RF,FADDR                                                         
         XC    FLEN,FLEN                                                        
         MVI   EDTVAL,TIMEDT                                                    
         GOTO1 CALLEDT                                                          
         ICM   R2,3,BMGEERR        TEST FOR ERRORS                              
         JZ    ORB50                NONE                                        
         GOTOR BASERR                                                           
         J     ORBX20                                                           
                                                                                
ORB50    LA    R2,WORK                                                          
         XC    WORK,WORK                                                        
         MVI   WORK+5,L'ORB$DEM                                                 
         MVC   WORK+8(L'ORB$DEM),ORB$DEM                                        
         LA    RF,8(R2)                                                         
         ST    RF,FADDR                                                         
         XC    FLEN,FLEN                                                        
         L     RF,VBUYSAVE         SET DEM CATEGORY FOR DEMEDT                  
         USING BUYSAVE,RF                                                       
         LA    R1,SVDEMOS                                                       
         OC    SVBRDEMS,SVBRDEMS                                                
         JZ    *+8                                                              
         LA    R1,SVBRDEMS                                                      
         DROP  RF                                                               
                                                                                
         MVC   SPDEMTYP,1(R1)                                                   
         MVI   EDTVAL,DEMEDT                                                    
         GOTO1 CALLEDT                                                          
         ICM   R2,3,BMGEERR        TEST FOR ERRORS                              
         JZ    ORB60                NONE                                        
         GOTOR BASERR                                                           
         J     ORBX20                                                           
                                                                                
         USING ORBDAY,R6                                                        
ORB60    MVC   ORBDAY,BUDAYS                                                    
         MVC   ORBTIME,BUTIME                                                   
         MVC   ORBDESC,ORB$PRG                                                  
         MVC   ORBDEM,BUNEWDEM+2                                                
         TM    BUNEWDEM,X'40'      TEST 2-DEC VALUE RET FROM DEMEDT             
         JZ    *+8                                                              
         OI    ORBDEM,X'40'         YES - SET 2 DEC ORBIT                       
                                                                                
         LA    R6,16(R6)                                                        
         LA    R5,ORBARYL(R5)                                                   
         JCT   R0,ORB20                                                         
         DROP  R6                                                               
                                                                                
         L     R6,FULL                                                          
**NOP    GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)  ADD THE NEW ELEM                   
         BRAS  RE,ADDEL                                                         
         ICM   R2,3,BMGEERR        TEST FOR ERRORS                              
         JZ    ORB70                NONE                                        
         GOTOR BASERR                                                           
         J     ORBX20                                                           
                                                                                
* THE REST OF THIS CODE COMES FROM SPBUY07 AROUND ORB25                         
* IF THERE IS A BUG, IT MAY BE THERE TOO!                                       
                                                                                
ORB70    GOTO1 DEMLKUP                                                          
         ICM   R2,3,BMGEERR        TEST FOR ERRORS                              
         JZ    ORB80                NONE                                        
         GOTOR BASERR                                                           
         J     ORBX20                                                           
                                                                                
* CALC OVERRIDE TO DEMO 1 IF ESTIMATES INPUT                                    
* BUT FIRST NEED TO FIND UPGRADE ELEMENT AGAIN                                  
ORB80    MVI   ELCDLO,X'67'                                                     
         MVI   ELCDHI,X'67'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         JE    *+6                                                              
         DCHO                                                                   
                                                                                
         LLC   R0,1(R6)            GET ORBIT ELEM LEN                           
         AHI   R0,-4               REMOVE OVERHEAD (DOESN'T MATTER)             
         SRL   R0,4                GET NUMBER OF POSITIONS                      
         ST    R0,FULL             SAVE IT                                      
         XR    R1,R1               CLEAR ACCUM                                  
         LA    R6,4(R6)                                                         
         USING ORBDAY,R6                                                        
                                                                                
         MVI   BYTE,C'1'           ASSUME 1-DECIMAL VALUES                      
         XR    RE,RE                                                            
                                                                                
ORB100   ICM   RE,3,ORBDEM                                                      
                                                                                
         TM    ORBDEM,X'40'        TEST 2-DEC FLAG                              
         JZ    *+8                                                              
         MVI   BYTE,C'2'           SET 2-DECIMAL VALUES                         
         N     RE,=X'00003FFF'     DROP 2-DEC FLAG                              
                                                                                
         AR    R1,RE               SUM DEMO VALUES                              
         LA    R6,16(R6)                                                        
         JCT   R0,ORB100                                                        
         DROP  R6                                                               
                                                                                
         AR    R1,R1               DOUBLE                                       
         D     R0,FULL                                                          
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         LTR   R1,R1                                                            
         JZ    ORBITX                                                           
         ST    R1,FULL                                                          
* FIND DEMO ELEMENT                                                             
         MVI   ELCDLO,NDCORGQ      X'02' - ORIGINAL DEMO ELEM                   
         MVI   ELCDHI,NDCORGQ                                                   
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         JE    *+6                                                              
         DCHO                                                                   
* CONVERTED DEMO PROCESSING *                                                   
         SPACE 1                                                                
         USING NDELEM,R6                                                        
         MVC   NDEMRAW,FULL        SAVE AVG OF ORB DEMOS AS 1ST DEMO            
         OI    NDEMRAW,X'80'       SET RATE OVERRIDE                            
         MVI   NDSVI,100           SET HUT ADJUSTMENT                           
         CLI   BYTE,C'2'           TEST 2-DEC VALUES                            
         JNE   *+8                                                              
         OI    NDEMRAW,X'40'       SET 2-DECIMAL FLAG                           
         DROP  R6                                                               
                                                                                
ORBITX   MVI   BUWHY2,X'02'                                                     
         GOTO1 SETCHGDT                                                         
         ICM   R2,3,BMGEERR        TEST FOR ERRORS                              
         JZ    ORBX10               NONE                                        
         GOTOR BASERR                                                           
         J     ORBX20                                                           
                                                                                
ORBX10   L     RF,VBUYSAVE         RESET MG TABLE REBUILD                       
         MVI   SVMGINIT-BUYSAVE(RF),0                                           
                                                                                
         USING SVDARED,R2                                                       
         L     R2,ASVDARE                                                       
         OI    SVDRFLG2,SVDRFLG2_ROE   TELL TSTDARE TO RETURN TO ME             
         GOTO1 ATESTDAR                                                         
         CLI   SVDRFLG2,SVDRFLG2_ERR   ANY TSTDARE ERROR?                       
         JNE   ORBX12                   NO                                      
         DROP  R2                                                               
         ICM   R2,3,NERRCD                                                      
         GOTOR BASERR                                                           
         J     ORBX20                                                           
                                                                                
ORBX12   GOTO1 PUTREC                                                           
         ICM   R2,3,BMGEERR        TEST FOR ERRORS                              
         JZ    ORBX20               NONE                                        
         GOTOR BASERR                                                           
                                                                                
ORBX20   GOTOR PUTRDA,SVBUYDA      SEND RUN REQUEST AND DISK ADDRESS            
         J     NXTREC                                                           
         EJECT                                                                  
                                                                                
ORBARYD  DSECT                     ** DSECT TO COVER ORBIT ARRAY **             
ORB$DAY  DS    CL18                ORBIT ROT                                    
ORB$TIME DS    CL15                ORBIT TIME                                   
ORB$PRG  DS    CL7                 ORBIT DESC                                   
ORB$DEM  DS    CL6                 ORBIT ESTIMATED DEMO                         
ORBARYL  EQU   *-ORBARYD                                                        
                                                                                
SPBUY39  CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ADD/CHANGE PACKAGES. RECALL HEADLINES FOR ALL LINES REFERENCED      *         
* TO COMPUTE CHECKSUMS AND SAVE SLAVE LINES. THEN DELETE PKG ELEM ON  *         
* MASTER (PASSED) AND ADD NEW PKG ELEM. THEN CHANGE ALL SLAVE LINES   *         
* TO REFERENCE MASTER LINE (OR REMOVE REFERENCE IF DELETE).  FINALLY, *         
* RETURN A 'RUN SINGLE BUY DOWNLOAD' COMMAND FOR THE MASTER AND ALL   *         
* LINES CHANGED.                                                      *         
*                                                                     *         
* IF THERE ARE NO SLAVE LINES SENT, THEN THIS IS A PACKAGE DELETE.    *         
* SAVE THE LIST OF SLAVE LINES, DELETE THE PKG MASTER ELEM FROM THE   *         
* MASTER LINE, THEN THE PKG SLAVE ELEMS FROM THE SLAVE LINES.         *         
*                                                                     *         
***********************************************************************         
                                                                                
PACKAGE  GOTOR RECALL,DMCB,RECNDXT,CHECKSUM,0                                   
         JNE   NXTREC                                                           
                                                                                
***********************************************************************         
* AFTER CALLING RECALL, THE HEADLINES SHOULD BE VALIDATED, WHICH MEANS          
* WE CAN SAFELY CHECK SVNTDMS.                        -HWON 10/28/16            
***********************************************************************         
         BRAS  RE,CHKCOMSC                                                      
         JNE   NXTREC                                                           
         BRAS  RE,CKRDONLY                                                      
         JNE   NXTREC                                                           
                                                                                
         ICM   RE,7,I$PKGARY+1     RE=A(PACKAGE ARRAY)                          
         JNZ   *+6                                                              
         DC    H'0'                CODE HERE TO DELETE ENTIRE PKG               
         USING LQ_D,RE                                                          
         CLI   LQ_TYPE,LQ_TLSTQ    ENSURE THIS IS AN ARRAY                      
         JE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         SR    R0,R0                                                            
         ICM   R0,3,LQ_VALUE       N'ARRAY ENTRIES (ROWS)                       
         LA    R5,LQ_VALUE+2       A(FIRST ARRAY ENTRY)                         
         DROP  RE                                                               
                                                                                
PKG$     USING LQ_D,P$PKG          BUILD DUMMY ARRAY LINE                       
         MVI   PKG$.LQ_EL,LQ_RQSTQ                                              
         LHI   RF,P$PKGL                                                        
         STCM  RF,3,PKG$.LQ_LN                                                  
         MVI   PKG$.LQ_TYPE,LQ_TLSTQ                                            
         LHI   RF,1                                                             
         STCM  RF,3,PKG$.LQ_VALUE                                               
         DROP  PKG$                                                             
                                                                                
         XC    MYELEM,MYELEM       BUILD PKG MASTER ELEM IN MYELEM              
         LA    R6,MYELEM+PKGLINES-PKGELEM                                       
                                                                                
PKG10    MVC   P$DATA(P$DATAL),0(R5)                                            
         GOTOR RECALL,DMCB,(X'80',RECPKGT),P$CKSM,0                             
         JNE   NXTREC                                                           
         CLI   P$DEL,C'Y'          DELETING THIS LINE FROM PKG?                 
         JE    *+14                 YES- DON'T ADD TO MASTER SLAVE LIST         
**NOP    MVC   0(1,R6),BUYREC+(BUYKBUY-BUYREC)  SAVE SLAVE LINE IN ELEM         
         MVC   0(2,R6),BUYREC+(BUYKBUY-BUYREC)  SAVE SLAVE LINE IN ELEM         
         AHI   R6,2                                                             
         AHI   R5,P$DATAL          BUMP TO NEXT ARRAY LINE                      
         BRCT  R0,PKG10                                                         
                                                                                
*                                                                               
* PUT REFERENCES TO SLAVE LINES ON MASTER LINE                                  
         GOTOR RECALL,DMCB,(X'80',RECNDXT),CHECKSUM,0 GET MASTER LINE           
         JNE   NXTREC                                                           
                                                                                
         LA    RF,MYELEM           BUILD NEW PKG MASTER ELEM                    
         USING PKGELEM,RF                                                       
         MVI   PKGCODE,PKGCODEQ                                                 
         LA    RE,MYELEM                                                        
         SR    R6,RE                                                            
         STC   R6,PKGLEN                                                        
         MVI   PKGIND,PKGMASTQ+X'10'   X'10' = 2 BYTE LINE NUMBER               
         DROP  RF                                                               
                                                                                
         MVI   PKGTYP,PKGMASTQ                                                  
         BRAS  RE,REMPKG                                                        
         JNE   PKGERR                                                           
                                                                                
         CLI   MYELEM+PKGLEN-PKGELEM,3  ANY SLAVE LINES?                        
         JNH   PKG20                     NO - DON'T ADD THE ELEM                
         GOTO1 VRECUP,DMCB,BUYREC,MYELEM,(R6) ADD THE NEW ELEM                  
         ICM   R2,3,BMGEERR        TEST FOR ERRORS                              
         JZ    PKG20                NONE                                        
         GOTOR BASERR                                                           
         J     PKGERR                                                           
                                                                                
PKG20    BRAS  RE,PUTPKG           PUT PACKAGE REC                              
         JNE   PKGERR                                                           
                                                                                
*                                                                               
* NOW PUT REFERENCES TO MASTER LINE ON SLAVE LINES                              
         XC    MYELEM,MYELEM                                                    
         LA    RF,MYELEM                                                        
         USING PKGELEM,RF                                                       
         MVI   PKGCODE,PKGCODEQ    X'05' PACKAGE ELEM                           
         MVI   PKGLEN,5                                                         
         MVI   PKGIND,PKGSLAVQ+X'10'   X'10' = 2 BYTE LINE NUMBER               
**NOP    MVC   PKGLINES,BUYREC+(BUYKBUY-BUYREC)                                 
         MVC   PKGLINES(2),BUYREC+(BUYKBUY-BUYREC)                              
         DROP  RF                                                               
                                                                                
         MVI   PKGTYP,PKGSLAVQ     SET FOR CALLS TO REMPKG                      
         MVC   PKGMAS,MYELEM+(PKGLINES-PKGELEM)   MASTER LINE                   
                                                                                
         ICM   RE,7,I$PKGARY+1     RE=A(PACKAGE ARRAY)                          
         USING LQ_D,RE                                                          
         XR    R0,R0                                                            
         ICM   R0,3,LQ_VALUE       N'ARRAY ENTRIES (ROWS)                       
         LA    R5,LQ_VALUE+2       A(FIRST ARRAY ENTRY)                         
         DROP  RE                                                               
                                                                                
PKG30    MVC   P$DATA(P$DATAL),0(R5)                                            
         GOTOR RECALL,DMCB,(X'80',RECPKGT),P$CKSM,0                             
         JE    *+6                                                              
         DC    H'0'                                                             
         BRAS  RE,REMPKG                                                        
         JNE   PKGERR                                                           
         CLI   P$DEL,C'Y'          DELETING THIS LINE FROM PKG?                 
         JE    PKG40                YES- DON'T ADD PKG ELEM                     
         GOTO1 VRECUP,DMCB,BUYREC,MYELEM,(R6) ADD THE NEW ELEM                  
                                                                                
PKG40    BRAS  RE,PUTPKG           PUT PACKAGE REC                              
         JNE   PKGERR                                                           
         AHI   R5,P$DATAL          BUMP TO NEXT ARRAY LINE                      
         BRCT  R0,PKG30                                                         
         J     NXTREC                                                           
                                                                                
PKGERR   GOTOR PUTRDA,SVBUYDA                                                   
         J     NXTREC                                                           
         EJECT                                                                  
***********************************************************************         
* REMPKG - FIND AND REMOVE PKG ELEMS                                  *         
*  PARMS: PKGTYP = PACKAGE TYPE (PKGMASTQ OR PKGSLAVQ)                *         
*         PKGMAS = PKG MASTER TO DELETE (IF PKGTYP=PKGSLAVQ)          *         
*                                                                     *         
*  RETURNS R6=A(INSERTION ADRESS FOR NEW PKG ELEM)                    *         
***********************************************************************         
                                                                                
         USING PKGELEM,R6                                                       
REMPKG   NTR1                                                                   
                                                                                
         LA    R6,BDELEM                                                        
REMPK2   BRAS  RE,GETPKGEL                                                      
         JNE   REMPK10                                                          
         CLI   PKGTYP,PKGSLAVQ     SLAVE LINE?                                  
         JNE   *+14                 NO - JUST DELETE IT                         
         CLC   PKGLINES(2),PKGMAS  POINTING TO THE RIGHT MASTER?                
         JNE   REMPK2               NO - LOOK FOR ANOTHER                       
         BRAS  RE,DELEL                                                         
**NOP    GOTO1 VRECUP,DMCB,BUYREC,(R6)                                          
         ICM   R2,3,BMGEERR        TEST FOR ERRORS                              
         JZ    REMPKX               NONE                                        
         GOTOR BASERR                                                           
         J     EXITN                                                            
         DROP  R6                                                               
                                                                                
* FIND PLACE IN REC TO INSERT                                                   
REMPK10  LA    R6,BDELEM                                                        
                                                                                
REMPK20  LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         JE    REMPKX                                                           
         CLI   0(R6),6                                                          
         JL    REMPK20                                                          
* NEVER PUT PKGEL BETWEEN 02 AND 22 OR 03 AND 23 !!!                            
         CLI   0(R6),X'22'                                                      
         JE    REMPK20                                                          
         CLI   0(R6),X'23'                                                      
         JE    REMPK20                                                          
                                                                                
REMPKX   CR    R6,R6               SET CC EQ                                    
         XIT1  REGS=(R6)                                                        
                                                                                
***********************************************************************         
* PUTPKG - SET BUWHY AND ACTIVITY ELEMS, WRITE REC AND RUN A SINGLE   *         
*          BUY DOWNLOAD                                               *         
***********************************************************************         
                                                                                
PUTPKG   NTR1                                                                   
         MVI   BUWHY,X'41'                                                      
         GOTO1 SETCHGDT                                                         
         ICM   R2,3,BMGEERR        TEST FOR ERRORS                              
         JZ    PUTPK10              NONE                                        
         GOTOR BASERR                                                           
         J     EXITN                                                            
                                                                                
         USING SVDARED,R2                                                       
PUTPK10  L     R2,ASVDARE                                                       
         OI    SVDRFLG2,SVDRFLG2_ROE   TELL TSTDARE TO RETURN TO ME             
         GOTO1 ATESTDAR                                                         
         CLI   SVDRFLG2,SVDRFLG2_ERR   ANY TSTDARE ERROR?                       
         JNE   PUTPK20                  NO                                      
         DROP  R2                                                               
         ICM   R2,3,NERRCD                                                      
         GOTOR BASERR                                                           
         J     EXITN                                                            
                                                                                
PUTPK20  BRAS  RE,SETPKGEL                                                      
         GOTO1 PUTREC                                                           
         GOTOR PUTRDA,SVBUYDA                                                   
                                                                                
PUTPKX   J     EXITY                                                            
         EJECT                                                                  
                                                                                
*          DATA SET SPBUY13    AT LEVEL 074 AS OF 02/21/08                      
*====================================================================           
* FIND PACKAGE ELEMENT AND CONVERT TO 2-BYTE FORMAT IF NECESSARY                
* ON ENTRY, R6 = A(START ELEM SEARCH)                                           
* IF FOUND R6 HAS A(PKGEL) ON EXIT                                              
*====================================================================           
                                                                                
GETPKGEL NTR1                                                                   
                                                                                
         MVI   ELCDLO,PKGCODEQ     FIND AND DELETE OLD PACKAGE (X'05')          
         MVI   ELCDHI,PKGCODEQ                                                  
                                                                                
GETPKG1  BRAS  RE,NEXTEL                                                        
         JNE   EXITN                                                            
                                                                                
         USING PKGELEM,R6                                                       
         CLC   PKGIND,PKGTYP                                                    
         JNE   GETPKG1                                                          
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
         JNZ   *+6                                                              
         DC    H'0'                                                             
         LA    RE,PKGLINES         AND POINT TO FIRST                           
         LA    RF,X.PKGLINES                                                    
                                                                                
GETPKG2  MVC   1(1,RF),0(RE)       MOVE 1-BYTE LINE TO 2-BYTE FIELD             
         LA    RE,1(RE)                                                         
         LA    RF,2(RF)                                                         
         BRCT  R0,GETPKG2                                                       
                                                                                
         LA    R0,ELEM                                                          
         SR    RF,R0               GET NEW ELEMENT LENGTH                       
         STC   RF,X.PKGLEN                                                      
                                                                                
         BRAS  RE,DELEL            DELETE OLD PKGEL                             
         BRAS  RE,ADDEL            INSERT NEW PKGEL                             
                                                                                
GETPKGX  CR    R6,R6               SET CC EQ                                    
         XIT1  REGS=(R6)                                                        
         DROP  X,R6                                                             
         EJECT                                                                  
                                                                                
*====================================================================           
* PROGRAM HAS BUILT 2-BYTE LINE NUMBERS IN NEW PKGEL                            
* IF NECESSARY, CONVERT BACK TO 1-BYTE LINE NUMBERS AND CHANGE IND              
*====================================================================           
                                                                                
SETPKGEL NTR1                                                                   
                                                                                
         CLI   SV1OR2,2                                                         
         JE    EXITY                                                            
                                                                                
         MVI   ELCDLO,PKGCODEQ     X'05' PACKAGE ELEM                           
         MVI   ELCDHI,PKGCODEQ                                                  
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         JNE   EXITN                                                            
                                                                                
         USING PKGELEM,R6                                                       
X        USING PKGELEM,ELEM                                                     
                                                                                
         XC    ELEM,ELEM                                                        
         MVC   ELEM(3),0(R6)         MOVE ELEM CODE/LEN/IND                     
         NI    X.PKGIND,X'FF'-X'10'  UNSET 2-BYTE FLAG                          
         LLC   R0,PKGLEN                                                        
         AHI   R0,-3                                                            
         SRL   R0,1                SET R0 FOR NUMBER OF LINES                   
         LA    RE,PKGLINES         AND POINT TO FIRST                           
         LA    RF,X.PKGLINES                                                    
                                                                                
SETPKG2  MVC   0(1,RF),1(RE)       MOVE 2-BYTE LINE TO 1-BYTE FIELD             
         LA    RF,1(RF)                                                         
         LA    RE,2(RE)                                                         
         BRCT  R0,SETPKG2                                                       
                                                                                
         LA    R0,ELEM                                                          
         SR    RF,R0               GET NEW ELEMENT LENGTH                       
         STC   RF,X.PKGLEN                                                      
                                                                                
         BRAS  RE,DELEL            DELETE OLD PKGEL                             
         BRAS  RE,ADDEL            INSERT NEW PKGEL                             
         J     EXITY                                                            
         DROP  X,R6                                                             
         EJECT                                                                  
                                                                                
ADDEL    NTR1                                                                   
         GOTO1 VRECUP,DMCB,BUYREC,ELEM,(R6)                                     
         J     EXIT                                                             
                                                                                
DELEL    NTR1                                                                   
         GOTO1 VRECUP,DMCB,BUYREC,(0,(R6))                                      
         J     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* RECALL A BUY RECORD AND IF A CHECKSUM WAS PASSED VERIFY THAT THE    *         
* BUY HASN'T BEEN CHANGED SINCE IT WAS ORIGINALLY DOWNLOADED - RETURN *         
* CHECKSUM ERROR IN DOWNLOAD STREAM IF IT HAS                         *         
*                                                                     *         
* NTRY:- R1=POINTS TO PARAMETER LIST AS FOLLOWS:-                     *         
*                                                                     *         
*        P1 -  AL1(X'80'=ALWAYS RECALL)                               *         
*                 (X'40'=READ HI OF KEY AND GETREC)                   *         
*           -  AL3(FIELD TABLE TO BE USED)                            *         
*        P2 -  A(CHECKSUM VALUE) OR ZERO (IF NO CHECKING TO BE DONE)  *         
*        P3 -  AL1,AL3(OVERRIDE KEY)                                            
***********************************************************************         
                                                                                
RECALL   NTR1  ,                                                                
         TM    0(R1),X'80'         ALWAYS RECALL?                               
         JNZ   *+12                                                             
         CLI   A$LINE,0            TEST JUST ADDED A NEW LINE                   
         JNE   EXITY                YES - DON'T NEED TO RECALL                  
                                                                                
         L     RF,0(R1)            RF=A(FIELD TABLE)                            
         MVC   CHECKTST,4(R1)      SET A(CHECKSUM TEST VALUE)                   
*                                                                               
         TM    0(R1),X'40'         READHI FOR WHATS IN KEY?                     
         JZ    RECALL30                                                         
         GOTOR HIGH                                                             
         JNE   *+2                                                              
         GOTOR GETREC                                                           
         JE    RECALL35                                                         
         DC    H'0'                                                             
*                                                                               
RECALL30 GOTOR BLDFLD,DMCB,('BFGOBASE',(RF)),(4,BUYINP1H)                       
         JNE   EXIT                                                             
                                                                                
RECALL35 OC    BUYHDDA,BUYHDDA     SAVE D/A OF 1ST LINE RECALLED                
         JNZ   *+10                                                             
         MVC   BUYHDDA,SVBUYDA                                                  
                                                                                
         ICM   R1,15,CHECKTST      TEST CHECKSUM TO BE PERFORMED                
         JZ    EXIT                NO                                           
         ICM   R1,15,0(R1)         SET/TEST CHECKSUM VALUE                      
         JZ    EXIT                EXIT IF TESTING (CHECKSUM NOT SENT)          
                                                                                
         LA    RF,CKSMTAB          TEST IF WE ALREADY CHECKED THIS BUY          
         LA    R0,CKSMTABQ                                                      
RECALL40 OC    0(L'BUYKDA,RF),0(RF)   END OF D/A TABLE                          
         JZ    RECALL60                                                         
         CLC   0(L'BUYKDA,RF),SVBUYDA TEST CHECKED THIS BUY ALREADY             
         JE    EXIT                                                             
         AHI   RF,L'CKSMTAB           BUMP TO NEXT D/A ENTRY                    
         BRCT  R0,RECALL40                                                      
         DC    H'0'                   499 MAX!        -HWON 1/25/2019           
                                                                                
RECALL60 MVC   0(L'BUYKDA,RF),SVBUYDA   SAVE D/A OF CHECKED BUY                 
         LA    RE,BUYREC                                                        
         SR    RF,RF                                                            
         ICM   RF,3,BUYRLEN                                                     
         SR    R0,R0                                                            
         CKSM  R0,RE               CALCULATE CHECK SUM                          
         JO    *-4                                                              
         CR    R0,R1               TEST IF BUY RECORD CHANGED                   
         JE    EXIT                                                             
         GOTOR PUTERR,255          IF SO, SEND CHECKSUM ERROR                   
         GOTOR PUTRDA,SVBUYDA      SEND RUN REQUEST AND DISK ADDRESS            
                                                                                
*******************************                                                 
* CODE REMOVED DUE TO STORAGE PROTECTION DUMPS                                  
*******************************                                                 
*&&DO                                                                           
* UPDATE SSB COUNTER FOR CKSM ERROR                                             
         L     RF,VCOMFACS                                                      
         ICM   RF,15,(CSWITCH-COMFACSD)(RF)                                     
         JNZ   *+6                                                              
         DCHO                                                                   
         XC    DUB,DUB                                                          
         MVC   DUB(4),=XL4'FEFFFFFF'  GET V(SYSFACS)                            
         GOTO1 (RF),DUB                                                         
         ICM   RF,15,0(R1)         RF=V(SYSFACS)                                
         JNZ   *+6                                                              
         DCHO                                                                   
         ICM   R1,15,BY$VSSB-BY$SYSFACD(RF)                                     
         JNZ   *+6                                                              
         DCHO                                                                   
                                                                                
RECALL90 ICM   RE,15,SSBCKSM-SSBD(R1)                                           
         LA    RF,1(RE)                                                         
         CS    RE,RF,SSBCKSM-SSBD(R1)                                           
         JNE   RECALL90            IF CS INSTRUCTION FAILS                      
*&&                                                                             
         J     EXITH                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* PROCESS MULTI-FIELD INDEX TABLES (BUY CHANGES, COMMENTS ETC.)       *         
*                                                                     *         
* NTRY:- R1=A(MULTI-FIELD INDEX TABLE)                                *         
***********************************************************************         
                                                                                
DOMULT   NTR1  ,                                                                
         LR    R2,R1               R2=A(INDEX TABLE)                            
DOMULT02 MVC   HALF,0(R2)                                                       
         LH    RF,HALF             RF=DISPLACEMENT TO FIELD TABLE               
         LTR   RF,RF                                                            
         JZ    EXITY               EXIT IF ALL DONE                             
         AR    RF,R2               RF=A(LIOF)                                   
         GOTOR BLDFLD,DMCB,('BFGOBASE',(RF)),(4,BUYINP1H)                       
         JH    EXIT                                                             
         AHI   R2,2                BUMP TO NEXT TABLE ENTRY                     
         J     DOMULT02                                                         
         EJECT                                                                  
***********************************************************************         
* CALL LINKIO TO BUILD INPUT FIELDS AND OPTIONALLY CALL THE BASE      *         
* PROGRAM TO PROCESS THE INPUT DATA                                   *         
*                                                                     *         
* NTRY:- R1=PARAMETER LIST AS FOLLOWS:-                               *         
*                                                                     *         
*        P1 -  AL1(CALL OPTIONS - SEE BELOW),AL3(FIELD TABLE)         *         
*        P2 -  AL1(N'TWA FIELDS),AL3(FIRST TWA OUTPUT FIELD HEADER)   *         
***********************************************************************         
                                                                                
BFGOBASE EQU   X'80'               CALL BASE PROGRAM TO PROCESS ACTION          
BFNOINPA EQU   X'40'               NO INPUT ACCEPTABLE                          
                                                                                
BLDFLD   NTR1  ,                                                                
         MVC   BFCALL,0(R1)        SAVE CALLING FLAG                            
         LM    RF,R0,0(R1)         RF=A(FIELD MAP),R0=N/A(TWA FIELD(S))         
         LA    RF,0(,RF)           CLEAR HOB OF RF TO BE TIDY                   
         LR    R2,RF                                                            
         GOTOR LINKIO,DMCB,('LIOABLD',LIOBD),(RF),(R0)                          
         JH    BLDFLD80            CC HIGH MEANS INPUT ERROR                    
         JE    BLDFLD70            CC EQUAL IS GOOD                             
                                                                                
         TM    BFCALL,BFNOINPA     NO INPUT ACCEPTABLE?                         
         JNZ   BLDFLD70                                                         
                                                                                
         LAY   RF,CHAPGMT          IF THIS WAS A PGM CHANGE REQ                 
         CR    R2,RF                BUILD THE FIELD IF THERE WAS NO INP         
         JNE   BLDFLD02                                                         
         OC    I$PROG,I$PROG       WAS THERE A MAP CODE SENT?                   
         JZ    EXITL                NO - DON'T BUILD IT                         
         J     BLDFLD70                                                         
                                                                                
BLDFLD02 LAY   RF,CHABTYT          IF THIS WAS A BOOK TYPE CHANGE REQ           
         CR    R2,RF                BUILD THE FIELD IF THERE WAS NO INP         
         JNE   BLDFLD04                                                         
         OC    I$BKTY,I$BKTY       WAS THERE A MAP CODE SENT?                   
         JZ    EXITL                NO - DON'T BUILD IT                         
         J     BLDFLD70                                                         
*                                                                               
BLDFLD04 LAY   RF,CHABIDT          TEST BUY ID CHANGE REQ                       
         CR    R2,RF                BUILD THE FIELD IF THERE WAS NO INP         
         JNE   BLDFLD06                                                         
         OC    I$BUYID,I$BUYID     WAS THERE A MAP CODE SENT?                   
         JZ    EXITL                NO - DON'T BUILD IT                         
         LR    RF,R0                                                            
         MVC   8(11,RF),=C'C,ID=DELETE'                                         
         MVI   5(RF),11                                                         
         J     BLDFLD70                                                         
*                                                                               
BLDFLD06 LA    RF,CHAPURT          TEST PURPOSE CODE CHANGE REQ                 
         CR    R2,RF                BUILD THE FIELD IF THERE WAS NO INP         
         JNE   EXITL                                                            
         OC    I$PURP,I$PURP       WAS THERE A MAP CODE SENT?                   
         JZ    EXITL                NO - DON'T BUILD IT                         
         J     BLDFLD70                                                         
                                                                                
BLDFLD70 TM    BFCALL,BFGOBASE     TEST WANT TO CALL BASE PROGRAM               
         JZ    EXITY                                                            
         GOTOR GOBASE              YES - GO DO IT                               
         JE    EXITY                                                            
         J     EXITH                                                            
                                                                                
BLDFLD80 OC    LIOBERR#,LIOBERR#   TEST ERROR FIELD MAP NUMBER KNOWN            
         JZ    BLDFLD90                                                         
         GOTOR HEXOUT,DMCB,LIOBERR#,XTRATEXT,L'LIOBERR#,0                       
                                                                                
BLDFLD90 LHI   R1,INVERR           SET INVALID INPUT FIELD                      
         CLI   DMCB,LIOCMISS                                                    
         JNE   *+8                                                              
         LHI   R1,MISSINP          SET MISSING INPUT FIELD                      
                                                                                
         CLI   DMCB,LIOCTOOL                                                    
         JNE   *+8                                                              
         LHI   R1,DATAOFLO         DATA BUFFER OVERFLOW                         
                                                                                
         GOTOR PUTERR,(R1)                                                      
         J     EXITH                                                            
         EJECT                                                                  
***********************************************************************         
* PUT ERROR TO DOWNLOAD STREAM                                        *         
*                                                                     *         
* NTRY:- R1=ERROR MESSAGE NUMBER                                      *         
***********************************************************************         
                                                                                
PUTERR   NTR1  ,                                                                
         STCM  R1,3,ERROR#                                                      
         GOTOR LINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTMAP',O#SDBUYR)               
         GOTOR (RF),(R1),('LIOAPUT',LIOBD),('LIOTERR',D#UPLERR),       *        
               ERROR#,(L'XTRATEXT,XTRATEXT)                                     
                                                                                
         XC    XTRATEXT,XTRATEXT                                                
         OI    LIOBINDS,LIOBINXT   ON ERROR FORCE NEXT INPUT RECORD             
         J     EXITH                                                            
                                                                                
***********************************************************************         
* SEND RUN BUY DOWNLOAD REQUEST AND DISK ADDRESS OF BUY TO DDLINK     *         
*                                                                     *         
* NTRY:- R1=A(RECORD DISK ADDRESS)                                    *         
***********************************************************************         
                                                                                
PUTRDA   NTR1  ,                                                                
         LR    R2,R1               R2=A(RECORD DISK ADDRESS)                    
         OC    0(4,R2),0(R2)                                                    
         JNZ   *+6                                                              
         DCHO                                                                   
                                                                                
         CLC   BUYHDDA,0(R2)       IS THIS THE BUY HEADER D/A?                  
         JNE   PUTRDA10             NO                                          
         TM    BY39FLAG,BY39HDDA   HAS IT BEEN SENT ALREADY?                    
         JNZ   EXITY                YES, DON'T SEND IT AGAIN                    
         OI    BY39FLAG,BY39HDDA   SET D/A SENT                                 
                                                                                
PUTRDA10 L     RE,LIOBENDI                                                      
         USING LQ_D,RE                                                          
                                                                                
         SR    R0,R0                                                            
PUTRDA20 CLI   LQ_EL,0             DID WE ALREADY SEND THIS D/A?                
         JE    PUTRDA30             - NOPE, HIT EOF, LETS SEND IT               
         CLI   LQ_EL,LQ_RAWDQ      HAVE A RAW DATA DOWNLOAD X'0A'?              
         JNE   PUTRDA25             NOPE, SKIP THIS                             
         CLC   LQ_DCODE,=AL2(D#DA) MAPCODE MATCHES X'0001'?                     
         JNE   PUTRDA25             NOPE, SKIP THIS                             
         CLI   LQ_TYPE,X'08'       TYPE MATCHES?  ** DO WE NEED THIS **         
*        JNE   PUTRDA25             NOPE, SKIP THIS ** COMMENTED OUT **         
         CLC   0(L'BUYKDA,R2),LQ_VALUE  MATCH ON D/A?                           
         JE    EXITY                IF YES, DON'T SEND IT AGAIN                 
PUTRDA25 ICM   R0,3,1(RE)          BUMP TO NEXT REQUEST                         
         AR    RE,R0                                                            
         J     PUTRDA20                                                         
*                                                                               
PUTRDA30 GOTOR LINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRUN',I#SDBUYR)               
*                                                                               
* FIX LIOBWORK OVERFLOW WHEN APPLYING LARGE MAKEGOOD                            
*  THIS IS DONE BY REMOVING TRAILING SPACES, WHICH DECREASES THE ELEM           
*  SIZE, WHICH THEN ALLOWS US TO PUT MORE ELEMENTS                              
*                                                 - HWON 12/02/2014             
         OC    PCKEY,PCKEY         HAVE PCKEY TO SEND?                          
         JZ    PUTRDA50             NO, SKIP SENDING IT                         
         LA    R5,PCKEY+L'PCKEY-1  GET THE ACTUAL LENGTH OF PCKEY               
PUTRDA35 CLI   0(R5),C' '                                                       
         JH    PUTRDA40                                                         
         JCT   R5,PUTRDA35                                                      
PUTRDA40 LA    RE,PCKEY-1                                                       
         SR    R5,RE               R5 = ACTUAL LENGTH OF PCKEY                  
         GOTOR (RF),(R1),('LIOAPUT',LIOBD),('LIOTRAW',D#PCKEY),        *        
               ('LD_VSTRQ',PCKEY),((R5),0)                                      
*                                                                               
PUTRDA50 GOTOR (RF),(R1),('LIOAPUT',LIOBD),('LIOTRAW',D#DA),           *        
               ('LD_HEXDQ',(R2)),(L'BUYKDA,0)                                   
         GOTOR (RF),(R1),('LIOAPUT',LIOBD),('LIOTERU',0),0,0                    
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* PUT MAKEGOOD REJECTION COMMENTS IN WSSVR                                      
***********************************************************************         
PUTMGWS  NTR1  LABEL=*                                                          
         XC    FAWSSVRD(FAWSSVRL),FAWSSVRD                                      
***      LLC   R0,SIMCOUNT         BUILD UNIQUE WSSVR TOKEN                     
***      AHI   R0,1                                                             
***      STC   R0,SIMCOUNT                                                      
***      CVD   R0,DUB                                                           
***      OI    DUB+L'DUB-1,X'0F'                                                
         MVC   FAWSTOKN(L'SIMTOKEN),SIMTOKEN                                    
         UNPK  FAWSTOKN+L'SIMTOKEN(L'FAWSTOKN-L'SIMTOKEN),DUB                   
         MVI   FAWSACTN,FAWSUDEL   DELETE IF ALREADY IN BUFFER                  
         GOTOR WSSVR,FAWSSVRD                                                   
         LA    RE,BUYREC           SET A(BUY RECORD) TO SAVE                    
         ST    RE,FAWSADR                                                       
*                                                                               
         MVC   FAWSLEN,BUYRLEN     SET L'BUY RERCORD TO SAVE                    
         MVI   FAWSACTN,FAWSUSVE                                                
         GOTOR WSSVR,FAWSSVRD                                                   
         CLI   FAWSRTN,0           CALL WSSVR TO SAVE THE BUY RECORD            
         JE    *+6                                                              
         DC    H'0'                DIE IF CAN'T SAVE RECORD BUFFER              
***********************************************************************         
* SEND RUN BUY DOWNLOAD REQUEST, WSSVR TOKEN AND BUY RECORD TO DDLINK *         
***********************************************************************         
                                                                                
PUTRWS   NTR1  ,                                                                
         XC    FAWSSVRD(FAWSSVRL),FAWSSVRD                                      
         LLC   R0,SIMCOUNT         BUILD UNIQUE WSSVR TOKEN                     
         AHI   R0,1                                                             
         STC   R0,SIMCOUNT                                                      
         CVD   R0,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         MVC   FAWSTOKN(L'SIMTOKEN),SIMTOKEN                                    
         UNPK  FAWSTOKN+L'SIMTOKEN(L'FAWSTOKN-L'SIMTOKEN),DUB                   
         MVI   FAWSACTN,FAWSUDEL   DELETE IF ALREADY IN BUFFER                  
         GOTOR WSSVR,FAWSSVRD                                                   
         LA    RE,BUYREC           SET A(BUY RECORD) TO SAVE                    
         ST    RE,FAWSADR                                                       
*                                                                               
         MVC   FAWSLEN,BUYRLEN     SET L'BUY RERCORD TO SAVE                    
         MVI   FAWSACTN,FAWSUSVE                                                
         GOTOR WSSVR,FAWSSVRD                                                   
         CLI   FAWSRTN,0           CALL WSSVR TO SAVE THE BUY RECORD            
         JE    *+6                                                              
         DC    H'0'                DIE IF CAN'T SAVE RECORD BUFFER              
         GOTOR LINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRUN',I#SDBUYR)               
         GOTOR (RF),(R1),('LIOAPUT',LIOBD),('LIOTRAW',D#PCKEY),        *        
               ('LD_VSTRQ',PCKEY),('#$PCKEY',0)                                 
         GOTOR (RF),(R1),('LIOAPUT',LIOBD),('LIOTRAW',D#TOKEN),        *        
               ('LD_CHARQ',FAWSTOKN),(L'FAWSTOKN,0)                             
         GOTOR (RF),(R1),('LIOAPUT',LIOBD),('LIOTERU',0),0,0                    
                                                                                
         CLI   SIMULATE,C'R'       SEND ROUTING?                                
         JNE   PUTRWSX              NO                                          
         GOTOR (RF),(R1),('LIOAPUT',LIOBD),('LIOTRUN',I#SDRD4D)                 
         GOTOR (RF),(R1),('LIOAPUT',LIOBD),('LIOTRAW',1),              *        
               ('LD_HEXDQ',BUYKAM),(L'BUYKAM,0)                                 
         GOTOR (RF),(R1),('LIOAPUT',LIOBD),('LIOTRAW',2),              *        
               ('LD_HEXDQ',BUYKCLT),(L'BUYKCLT,0)                               
         GOTOR (RF),(R1),('LIOAPUT',LIOBD),('LIOTRAW',3),              *        
               ('LD_HEXDQ',BUYKSTA),(L'BUYKSTA,0)                               
         GOTOR (RF),(R1),('LIOAPUT',LIOBD),('LIOTRAW',7),              *        
               ('LD_VSTRQ',BUYST),(L'BUYST,0)                                   
         GOTOR (RF),(R1),('LIOAPUT',LIOBD),('LIOTERU',0),0,0                    
                                                                                
PUTRWSX  J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* CALL BASE PROGRAM TO PROCESS INPUT SCREEN                           *         
***********************************************************************         
                                                                                
GOBASE   NTR1  ,                                                                
         CLI   REVAL,C'Y'          UNSET PRE VALID BIT? (FORCE HL EDIT)         
         JNE   GOBASE02             NO                                          
         NI    BUYMDH+4,X'FF'-X'20'                                             
         MVI   REVAL,C'N'          AND DON'T DO IT AGAIN                        
                                                                                
GOBASE02 GOTOR VCALLBAS,=C'*T21139*' CALL BASE TO PROCESS INPUT                 
         SR    R2,R2                                                            
         ICM   R2,3,BMGEERR        TEST FOR ERRORS                              
         JZ    EXIT                                                             
         CHI   R2,DSPCMNT          RETRY 'MUST DISPLAY COMMENT' ERROR           
         JE    GOBASE02                                                         
         CHI   R2,NOELEMS          IGNORE 'NO SPOTS IN PERIOD' ERROR            
         JE    GOBASE10                                                         
         CHI   R2,SOFBDP           IGNORE 'SPOTS OUTSIDE OF BUY' ERROR          
         JE    GOBASE10                                                         
         GOTOR BASERR                                                           
         J     EXITN                                                            
                                                                                
GOBASE10 XC    BMGEERR,BMGEERR                                                  
         J     EXITY                                                            
         EJECT                                                                  
***********************************************************************         
* ERROR ROUTINE FOR BASE PROGRAM ERRORS                               *         
***********************************************************************         
                                                                                
BASERR   NTR1                                                                   
         LA    R5,XTRATEXT                                                      
         CLI   ERRTEXT,C' '                                                     
         JNH   BASERR10                                                         
         AHI   R5,L'ERRTEXT                                                     
         MVC   XTRATEXT(L'ERRTEXT),ERRTEXT                                      
         CLI   0(R5),C' '                                                       
         JH    *+8                                                              
         JCT   R5,*-8                                                           
         AHI   R5,2                                                             
                                                                                
BASERR10 MVI   0(R5),C'@'          BUILD RECORD#/DATA MAP# STRING               
         AHI   R5,1                                                             
         GOTOR HEXOUT,DMCB,LIOBSUB#,0(R5),L'LIOBSUB#,0                          
         AHI   R5,4                                                             
         GOTOR HEXOUT,DMCB,LIOBDTA#,0(R5),L'LIOBDTA#,0                          
         AHI   R5,4                                                             
         MVI   0(R5),C'@'                                                       
         GOTOR PUTERR,(R2)         RETURN BUY PROGRAM ERROR MESSAGE             
                                                                                
**NOP    CHI   R2,DARELOCK                                                      
         CLM   R2,3,=Y(AUTORCL)                                                 
         JNE   EXIT                                                             
         GOTOR LINKIO,DMCB,('LIOAPUT',LIOBD),('LIOTRAW',D#OLOCK),      X        
               ('LD_CHARQ',LOCKEDL),(L'LOCKEDL,0)                               
         J     EXITN                                                            
                                                                                
***********************************************************************         
* SAVE TIA                                                            *         
***********************************************************************         
                                                                                
SAVETIA  NTR1  ,                                                                
         L     RE,VTIA             SAVE TIA                                     
         LAY   R0,SVTIA                                                         
         C     R0,=X'00012000'                                                  
         JNL    *+6                                                             
         DC    H'00'                                                            
         LHI   R1,L'SVTIA                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         J     EXIT                                                             
                                                                                
***********************************************************************         
* RESTORE TIA                                                         *         
***********************************************************************         
                                                                                
RESTTIA  NTR1  ,                                                                
         L     RE,VTIA             SAVE TIA                                     
         LAY   R0,SVTIA                                                         
         C     R0,=X'00012000'                                                  
         JNL    *+6                                                             
         DC    H'00'                                                            
         LHI   R1,L'SVTIA                                                       
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
         J     EXIT                                                             
         EJECT                                                                  
                                                                                
***********************************************************************         
* CHECK TWA IS INTACT                                                 *         
*                                                                               
*   LAST 4K OF TIA IS PASSED TO LINKIO FOR LIOBAREC (I/O AREA)                  
*   THE PROBLEM IS SOMETIMES THE WORKER FILE IS LARGER THAN 4K,                 
*   WHICH RESULTS IN TWA (AREA FOLLOWING TIA) GET CLOBBERED                     
*   SO WE NEED TO PREVENT THE PROGRAM FROM CONTINUING AND POSSSIBLY             
*   WRITING OUT BAD RECORDS.                                                    
*                                                                               
* NOTE:AS LONG AS WE USE THE LAST 4K OF TIA, WE SHOULD ALWAYS CHECK TWA         
*                                                                               
***********************************************************************         
                                                                                
CHKTWA   NTR1                                                                   
                                                                                
         XC    DUB,DUB                                                          
         LHI   R0,-1                                                            
         ST    R0,DUB                                                           
         L     RF,VCOMFACS                                                      
         L     RF,(CSWITCH-COMFACSD)(RF)                                        
         GOTO1 (RF),DUB                                                         
         L     RF,0(R1)                                                         
         USING UTLD,RF                                                          
*                                                                               
         L     RE,VBUYTWA                                                       
         CLC   TOFFCODE,1(RE)      DOES TOFFCODE MATCH TWAOFFC?                 
         JNE   *+2                  NO,                                         
         CLC   TUSER,10(RE)        DOES TUSER MATCH TWAUSRID?                   
         JNE   *+2                  NO,                                         
         CLC   TAGY,14(RE)         DOES TAGY MATCH TWAAGY?                      
         JNE   *+2                  NO,                                         
         CLI   64(RE),0            CHK LEN 1ST FIELD HEADER AT TWA+64?          
         JE    *+2                  LENGTH IS 0, NOT GOOD                       
         J     EXITY                                                            
         DROP  RF                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* CHECK SBTK VERSION SUPPORTS COMSCORE, IF NOT CHECK EST HAS COMSCORE           
*  ON ENTRY:  (REQ) PC VERSION MUST BE SET                                      
*             (REQ) SVNTDMS MUST BE SET    (AFTER BUY HDLN VALIDATION)          
* NOTE:                                                                         
* IT APPEARS THE VERY FIRST TIME WE GLOBBER TO BUY, HEADLINES ARE NOT           
* VALIDATED YET, WHICH MEANS, SVNTDMOS IS NOT SET, AND CHECKING IT              
* WOULD ALWAYS RETURN NO COMSCORE DEMOS, WHICH IS NOT CORRECT.                  
*                                                      -=HWON 10/28/16          
***********************************************************************         
CHKCOMSC NTR1                                                                   
         CLC   LIOBPCV1,=AL1(4,6,0,050)  IF VERSION LESS V4.6.0.50              
         JNL   EXITY                                                            
         LAY   RE,SVNTDMS                                                       
         CLC   0(L'SPACES,RE),SPACES        AND HAVE COMSCORE DEMO              
         JH    CHKC010                                                          
         CLC   L'SPACES(L'SPACES,RE),SPACES                                     
         JNH   EXITY                                                            
CHKC010  GOTOR PUTERR,662                DON'T ALLOW ANY ACTION                 
         J     EXITN                                                            
         EJECT                                                                  
***********************************************************************         
* CHECK IF READONLY SYSTEM OR MODE                                              
***********************************************************************         
CKRDONLY NTR1                                                                   
         L     RF,VCOMFACS                                                      
         ICM   RF,15,CXTRAINF-COMFACSD(RF)                                      
         JZ    *+2                                                              
*        BZ    *+12                                                             
*                                  *** TEST CONNECTED WITH U=N ***              
         TM    XIFLAG2-XTRAINFD(RF),XICTUEN                                     
         JNZ   CKRDO010                                                         
*                   *** TEST READONLY SYSTEM OR MODE, WRONG FACKPAK ***         
         TM    XIFLAG1-XTRAINFD(RF),XIROSYS+XIROMODE+XIWRONGF                   
         JZ    EXITY                                                            
CKRDO010 GOTOR PUTERR,686                DON'T ALLOW ANY ACTION                 
         J     EXITN                                                            
***********************************************************************         
* NEXTEL                                                              *         
***********************************************************************         
                                                                                
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         LLC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
                                                                                
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
                                                                                
EXITL    DS    0H                  CC=LOW EXIT                                  
EXITN    LHI   RE,0                CC=NOT EQUAL EXIT                            
         J     EXITCC                                                           
EXITY    LHI   RE,1                CC=EQUAL EXIT                                
         J     EXITCC                                                           
EXITH    LHI   RE,2                CC=HIGH EXIT                                 
EXITCC   CHI   RE,1                SET CONDITION CODE                           
                                                                                
EXIT     XIT1  ,                                                                
         EJECT                                                                  
                                                                                
GLOBALS  DS    0D                  ** GLOBAL LITERALS **                        
                                                                                
ACTTAB   DS    0XL(ACTTABLQ)       ** SUB-RECORD ACTION TABLE **                
         DC    AL2(I#SDBADD,ADDBUY-SPBUY39)        MAP 0160                     
         DC    AL2(I#SDBCHA,CHABUY-SPBUY39)        MAP 0161                     
         DC    AL2(I#SDBSKD,CHABUY-SPBUY39)        MAP 0162                     
         DC    AL2(I#SDBSD,CHABUY-SPBUY39)         MAP 0166                     
         DC    AL2(I#SDBSPL,SPLBUY-SPBUY39)        MAP 0164                     
         DC    AL2(I#SDBSPC,OTOSPT-SPBUY39)        MAP 0165                     
         DC    AL2(I#SDBDEL,DELBUY-SPBUY39)        MAP 0167                     
         DC    AL2(I#SDBALC,ALCSPT-SPBUY39)        MAP 0169                     
         DC    AL2(I#SDBMGD,MGDBUY-SPBUY39)        MAP 0168                     
         DC    AL2(I#SDBBYD,CHADEM-SPBUY39)        MAP 016A                     
         DC    AL2(I#SDBPBD,CHADEM-SPBUY39)        MAP 016B                     
         DC    AL2(I#SDMGPP,MGDAPP-SPBUY39)        MAP 017A                     
         DC    AL2(I#SDMGSP,MGDAPP-SPBUY39)        MAP 0179                     
         DC    AL2(I#SDMGAP,MGDAPP-SPBUY39)        MAP 0177                     
         DC    AL2(I#SDMGRJ,MGDAPP-SPBUY39)        MAP 0178                     
         DC    AL2(I#SDBORB,ORBIT-SPBUY39)         MAP 016C                     
         DC    AL2(I#SDBPKG,PACKAGE-SPBUY39)       MAP 016D                     
         DC    AL2(I#SDBSPS,SPLBUY-SPBUY39)        MAP 016E                     
ACTTABN  EQU   (*-ACTTAB)/ACTTABLQ                                              
                                                                                
ACTTABD  DSECT                     ** DSECT TO COVER ACTION TABLE **            
ACTTMAP# DS    AL2                 UPLOAD RECORD MAP NUMBER                     
ACTROUT  DS    AL2                 DISPLACEMENT TO ACTION ROUTINE               
ACTTABLQ EQU   *-ACTTABD                                                        
                                                                                
SPBUY39  CSECT                                                                  
         EJECT                                                                  
         LTORG                                                                  
                                                                                
SIMTOKEN DC    C'B#'               TOKEN PREFIX FOR SIMULATED BUY I/O           
MGRJTOKN DC    C'M#'               TOKEN PREFIX FOR SIMULATED BUY I/O           
                                                                                
LOCKEDL  DC    C'Y'                ORDER LOCKED REPLY                           
DOLLARL  DC    C'$'                DOLLAR SIGN                                  
HYPHENL  DC    C'-'                HYPHEN                                       
COMMAL   DC    C','                COMMAL                                       
EQUALL   DC    C'='                EQUAL                                        
SLASHL   DC    C'/'                SLASH                                        
LPARENL  DC    C'('                LEFT PARENTHESIS                             
RPARENL  DC    C')'                RIGHT PARENTHESIS                            
                                                                                
ADDBUYL  DC    C'B'                ADD BUY LINE ACTION                          
RECALLL  DC    C'R'                RECALL BUY LINE ACTION                       
CHABUYL  DC    C'C'                CHANGE BUY LINE ACTION                       
INVDETL  DC    C'I'                INVOICE DETAIL LINE ACTION                   
OTOSPTL  DC    C'O'                OTO SPOT ACTION                              
ALCSPTL  DC    C'A'                ALLOCATE SPOT ACTION                         
DELBUYL  DC    C'DEL'              DELETE BUY LINE ACTION                       
                                                                                
CHAPERL  DC    C'PER='             CHANGE PERIOD KEYWORD                        
CHALENL  DC    C'LEN='             CHANGE LENGTH KEYWORD                        
CHADPTL  DC    C'DPT='             CHANGE DAYPART KEYWORD                       
CHAPGML  DC    C'PGM='             CHANGE PROGRAM KEYWORD                       
CHATIML  DC    C'TIM='             CHANGE TIME KEYWORD                          
CHANPWL  DC    C'NPW='             CHANGE NUMBER PER WEEK KEYWORD               
CHACSTL  DC    C'COST='            CHANGE COST KEYWORD                          
CHACS2L  DC    C'COS2='            CHANGE COST 2 KEYWORD                        
CHASKDL  DC    C'SK'               SKED SPOTS KEYWORD                           
CHASDL   DC    C'SD'               DAILY SKED SPOTS KEYWORD                     
CHAADJL  DC    C'ADJ='             CHANGE ADJACENCY CODE KEYWORD                
CHARBKL  DC    C'BK='              CHANGE BOOK KEYWORD                          
CHABTYL  DC    C'BT='              CHANGE BOOK TYPE KEYWORD                     
CHAUPGL  DC    C'UPT='             CHANGE UPGRADE FORMULA KEYWORD               
CHADEML  DC    C'DEM'              CHANGE BUY DEMO VALUES KEYWORD               
CHAPBDL  DC    C'PBD'              CHANGE POST BUY DEMO VALUES KEYWORD          
CHAMGDL  DC    C'MG='              CHANGE MAKEGOOD KEYWORD                      
CHATAXL  DC    C'TAX='             CHANGE TAX KEYWORD                           
CHABIDL  DC    C'ID='              CHANGE ID KEYWORD                            
CSPILLL  DC    C'SPILL'            SPILL KEYWORD                                
CHAPRDL  DC    C'M='               CHANGE MASTER PRODUCT KEYWORD                
MALLOCL  DC    C',M='              ALLOCATE MASTER PROD KWD (ON ADD)            
CHAREPL  DC    C'REP='             CHANGE REP KEYWORD                           
CHAPURL  DC    C'PUR='             CHANGE PURPOSE CODE KEYWORD                  
SPLITBL  DC    C'SPLIT='           SPLIT A BUYLINE                              
MGEL     DC    C'MGE'              MGE                                          
MGEAPPL  DC    C'MGEZZT'           APPLY MAKEGOOD                               
MGESAPL  DC    C'MGESAP'           SELF APPLY MAKEGOOD                          
MGEAPPRL DC    C'MGEAPP'           APPROVE A MAKEGOOD                           
MGEREJL  DC    C'MGEREJ'           REJECT A MAKEGOOD                            
COMNT1L  DC    C'COM=1-'           CHANGE COMMENT 1 KEYWORD                     
COMNT2L  DC    C'COM=2-'           CHANGE COMMENT 2 KEYWORD                     
COMNT3L  DC    C'COM=3-'           CHANGE COMMENT 3 KEYWORD                     
COMNT4L  DC    C'COM=4-'           CHANGE COMMENT 4 KEYWORD                     
COMNT5L  DC    C'COM=5-'           CHANGE COMMENT 5 KEYWORD                     
SEPSPOTL DC    C'SEPSPOT='         SEPARATE SPOT                                
         EJECT                                                                  
***********************************************************************         
* FIELD TABLES (SEE LIOFD)                                            *         
***********************************************************************         
                                                                                
ADDBUYT  DS    0X                  ** ADD BUY FIELD TABLE **                    
                                                                                
         DC    AL1(LIOFILIT+LIOFICOM,L'ADDBUYL)                                 
         DC    AL2(ADDBUYL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIREQ+LIOFICOM,0)                                
         DC    AL2(F$PER-*)                                                     
                                                                                
         DC    AL1(LIOFIMAP+LIOFIREQ+LIOFICOM,0)                                
         DC    AL2(F$DAYS-*)                                                    
                                                                                
         DC    AL1(LIOFIMAP+LIOFIREQ+LIOFICOM,0)                                
         DC    AL2(F$NPW-*)                                                     
                                                                                
         DC    AL1(LIOFIMAP+LIOFIREQ+LIOFICOM,0)                                
         DC    AL2(F$TIMES-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIREQ+LIOFICOM,0)                                
         DC    AL2(F$DPT-*)                                                     
                                                                                
         DC    AL1(LIOFIMAP+LIOFIREQ+LIOFICOM,0)                                
         DC    AL2(F$SLN-*)                                                     
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$PROG-*)                                                    
                                                                                
         DC    AL1(LIOFILIT,L'COMMAL)                                           
         DC    AL2(COMMAL-*)                                                    
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT+LIOFICOM,0)                                
         DC    AL2(F$ADJCY-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$RTYPE-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIREQ+LIOFICOM,0)                                
         DC    AL2(F$COST-*)                                                    
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT+LIOFISLS,LIOFIFST+LIOFILST)                
         DC    AL2(F$DEMOS-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'MALLOCL)                                 
         DC    AL2(MALLOCL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$MPRD-*)                                                    
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'HYPHENL)                                 
         DC    AL2(HYPHENL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$PIGGY-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'SLASHL)                                  
         DC    AL2(SLASHL-*)                                                    
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,LIOFIDSH)                                  
         DC    AL2(F$PBSL1-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$PBSL2-*)                                                   
                                                                                
ADDBUYTX DC    AL1(LIOFIEOT)                                                    
                                                                                
RECNDXT  DS    0X                  ** RECALL BUY FIELD TABLE 1 **               
                                                                                
         DC    AL1(LIOFIMAP+LIOFIREQ,0)                                         
         DC    AL2(F$LINE-*)                                                    
                                                                                
         DC    AL1(LIOFILIT,L'RECALLL)                                          
         DC    AL2(RECALLL-*)                                                   
                                                                                
RECNDXTX DC    AL1(LIOFIEOT)                                                    
                                                                                
RECFLDT  DS    0X                  ** RECALL BUY FIELD TABLE 2 **               
                                                                                
         DC    AL1(LIOFIMAP+LIOFIREQ,0)                                         
         DC    AL2(D$MGLINE-*)                                                  
                                                                                
         DC    AL1(LIOFILIT,L'RECALLL)                                          
         DC    AL2(RECALLL-*)                                                   
                                                                                
RECFLDTX DC    AL1(LIOFIEOT)                                                    
                                                                                
RECPKGT  DS    0X                  ** RECALL BUY FIELD TABLE 3 **               
                                                                                
         DC    AL1(LIOFIMAP+LIOFIREQ,0)                                         
         DC    AL2(D$PKGLIN-*)                                                  
                                                                                
         DC    AL1(LIOFILIT,L'RECALLL)                                          
         DC    AL2(RECALLL-*)                                                   
                                                                                
RECPKGTX DC    AL1(LIOFIEOT)                                                    
                                                                                
MGDTABT  DS    0X                  ** MAKEGOOD FIELD TABLE **                   
                                                                                
         DC    AL1(LIOFILIT+LIOFICOM,L'CHABUYL)                                 
         DC    AL2(CHABUYL-*)                                                   
                                                                                
         DC    AL1(LIOFILIT,L'CHAMGDL)                                          
         DC    AL2(CHAMGDL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$MGCODE-*)                                                  
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,LIOFIFST)                                  
         DC    AL2(F$MGMIN-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$MGLINE-*)                                                  
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,LIOFIDSH)                                  
         DC    AL2(F$MGWEEK-*)                                                  
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$MGSPOT-*)                                                  
                                                                                
         DC    AL1(LIOFILIT+LIOFIOPT+LIOFIPFX,L'SLASHL)                         
         DC    AL2(SLASHL-*)                                                    
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,LIOFILST)                                  
         DC    AL2(F$MGNET-*)                                                   
                                                                                
MGDTABTX DC    AL1(LIOFIEOT)                                                    
                                                                                
MGETABT  DS    0X                  ** MGE FIELD TABLE **                        
                                                                                
         DC    AL1(LIOFILIT,L'MGEL)                                             
         DC    AL2(MGEL-*)                                                      
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$MGFLT-*)                                                   
                                                                                
         DC    AL1(LIOFILIT,L'EQUALL)                                           
         DC    AL2(EQUALL-*)                                                    
                                                                                
         DC    AL1(LIOFIMAP+LIOFIREQ,0)                                         
         DC    AL2(F$MGGRP-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'SLASHL)                                  
         DC    AL2(SLASHL-*)                                                    
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$MGTRD-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'COMMAL)                                  
         DC    AL2(COMMAL-*)                                                    
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$MGPRD-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'HYPHENL)                                 
         DC    AL2(HYPHENL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$MGPR2-*)                                                   
                                                                                
MGETABTX DC    AL1(LIOFIEOT)                                                    
                                                                                
         DS    0D                                                               
MGATAB   DS    0XL6                ** MAKEGOOD INDEX TABLE **                   
         DC    AL2(MAPTABT-*),AL2(I#SDMGPP),AL2(I#SDOAPY)                       
         DC    AL2(MSATABT-*),AL2(I#SDMGSP),AL2(I#SDOSAP)                       
         DC    AL2(APPTABT-*),AL2(I#SDMGAP),AL2(I#SDOAPR)                       
         DC    AL2(REJTABT-*),AL2(I#SDMGRJ),AL2(I#SDOREJ)                       
MGATABX  DC    XL6'0'                                                           
                                                                                
MAPTABT  DS    0X                  ** APPLY MAKEGOOD FIELD TABLE **             
                                                                                
         DC    AL1(LIOFILIT,L'MGEAPPL)                                          
         DC    AL2(MGEAPPL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$MGFLT-*)                                                   
                                                                                
         DC    AL1(LIOFILIT,L'EQUALL)                                           
         DC    AL2(EQUALL-*)                                                    
                                                                                
         DC    AL1(LIOFIMAP+LIOFIREQ,0)                                         
         DC    AL2(F$MGGRP-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'SLASHL)                                  
         DC    AL2(SLASHL-*)                                                    
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$MGTRD-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'COMMAL)                                  
         DC    AL2(COMMAL-*)                                                    
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$MGPRD-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'HYPHENL)                                 
         DC    AL2(HYPHENL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$MGPR2-*)                                                   
                                                                                
MAPTABTX DC    AL1(LIOFIEOT)                                                    
                                                                                
MSATABT  DS    0X                  ** SELF APPLY MKGD FIELD TABLE **            
                                                                                
         DC    AL1(LIOFILIT,L'MGESAPL)                                          
         DC    AL2(MGESAPL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$MGFLT-*)                                                   
                                                                                
         DC    AL1(LIOFILIT,L'EQUALL)                                           
         DC    AL2(EQUALL-*)                                                    
                                                                                
         DC    AL1(LIOFIMAP+LIOFIREQ,0)                                         
         DC    AL2(F$MGGRP-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'SLASHL)                                  
         DC    AL2(SLASHL-*)                                                    
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$MGTRD-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'COMMAL)                                  
         DC    AL2(COMMAL-*)                                                    
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$MGPRD-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'HYPHENL)                                 
         DC    AL2(HYPHENL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$MGPR2-*)                                                   
                                                                                
MSATABTX DC    AL1(LIOFIEOT)                                                    
                                                                                
APPTABT  DS    0X                  ** APPROVE MAKEGOOD FIELD TABLE **           
                                   ** MGEAPP00=XX/T,PRD-PR2                     
         DC    AL1(LIOFILIT,L'MGEAPPRL)                                         
         DC    AL2(MGEAPPRL-*)                                                  
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$MGFLT-*)                                                   
                                                                                
         DC    AL1(LIOFILIT,L'EQUALL)                                           
         DC    AL2(EQUALL-*)                                                    
                                                                                
         DC    AL1(LIOFIMAP+LIOFIREQ,0)                                         
         DC    AL2(F$MGGRP-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'SLASHL)                                  
         DC    AL2(SLASHL-*)                                                    
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$MGTRD-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'COMMAL)                                  
         DC    AL2(COMMAL-*)                                                    
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$MGPRD-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'HYPHENL)                                 
         DC    AL2(HYPHENL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$MGPR2-*)                                                   
                                                                                
APPTABTX DC    AL1(LIOFIEOT)                                                    
                                                                                
REJTABT  DS    0X                  ** REJECT MKGD FIELD TABLE **                
                                   ** MKGREJ00=XX/T,PRD-PR2                     
         DC    AL1(LIOFILIT,L'MGEREJL)                                          
         DC    AL2(MGEREJL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$MGFLT-*)                                                   
                                                                                
         DC    AL1(LIOFILIT,L'EQUALL)                                           
         DC    AL2(EQUALL-*)                                                    
                                                                                
         DC    AL1(LIOFIMAP+LIOFIREQ,0)                                         
         DC    AL2(F$MGGRP-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'SLASHL)                                  
         DC    AL2(SLASHL-*)                                                    
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$MGTRD-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'COMMAL)                                  
         DC    AL2(COMMAL-*)                                                    
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$MGPRD-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'HYPHENL)                                 
         DC    AL2(HYPHENL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$MGPR2-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'COMMAL)                                  
         DC    AL2(COMMAL-*)                                                    
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$MGCMT-*)                                                   
                                                                                
REJTABTX DC    AL1(LIOFIEOT)                                                    
                                                                                
CHATAB   DS    0AL2                ** CHANGE BUY INDEX TABLE **                 
         DC    AL2(CHAPERT-*)                                                   
         DC    AL2(CHANPWT-*)                                                   
         DC    AL2(CHATIMT-*)                                                   
         DC    AL2(CHADPTT-*)                                                   
         DC    AL2(CHASLNT-*)                                                   
         DC    AL2(CHAPGMT-*)                                                   
         DC    AL2(CHACSTT-*)                                                   
         DC    AL2(CHACS2T-*)                                                   
         DC    AL2(CHASKDT-*)                                                   
         DC    AL2(CHASDT-*)                                                    
         DC    AL2(CHAADJT-*)                                                   
         DC    AL2(CHARBKT-*)                                                   
         DC    AL2(CHABTYT-*)                                                   
         DC    AL2(CHAUPGT-*)                                                   
         DC    AL2(CHAPRDT-*)                                                   
         DC    AL2(CHATAXT-*)                                                   
         DC    AL2(CHAREPT-*)                                                   
         DC    AL2(CHAPURT-*)                                                   
         DC    AL2(CHABIDT-*)                                                   
CHATABX  DC    AL2(0)                                                           
                                                                                
CHAPERT  DS    0X                  ** CHANGE PERIOD FIELD TABLE **              
                                                                                
         DC    AL1(LIOFILIT+LIOFICOM,L'CHABUYL)                                 
         DC    AL2(CHABUYL-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'CHAPERL)                                 
         DC    AL2(CHAPERL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT+LIOFICOM,0)                                
         DC    AL2(F$PER-*)                                                     
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$DAYS-*)                                                    
                                                                                
CHAPERTX DC    AL1(LIOFIEOT)                                                    
                                                                                
CHANPWT  DS    0X                  ** CHANGE NPW FIELD TABLE **                 
                                                                                
         DC    AL1(LIOFILIT+LIOFICOM,L'CHABUYL)                                 
         DC    AL2(CHABUYL-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'CHANPWL)                                 
         DC    AL2(CHANPWL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$NPW-*)                                                     
                                                                                
CHANPWTX DC    AL1(LIOFIEOT)                                                    
                                                                                
CHATIMT  DS    0X                  ** CHANGE TIME FIELD TABLE **                
                                                                                
         DC    AL1(LIOFILIT+LIOFICOM,L'CHABUYL)                                 
         DC    AL2(CHABUYL-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'CHATIML)                                 
         DC    AL2(CHATIML-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$TIMES-*)                                                   
                                                                                
CHATIMTX DC    AL1(LIOFIEOT)                                                    
                                                                                
CHADPTT  DS    0X                  ** CHANGE DAYPART FIELD TABLE **             
                                                                                
         DC    AL1(LIOFILIT+LIOFICOM,L'CHABUYL)                                 
         DC    AL2(CHABUYL-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'CHADPTL)                                 
         DC    AL2(CHADPTL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$DPT-*)                                                     
                                                                                
CHADPTTX DC    AL1(LIOFIEOT)                                                    
                                                                                
CHASLNT  DS    0X                  ** CHANGE LENGTH FIELD TABLE **              
                                                                                
         DC    AL1(LIOFILIT+LIOFICOM,L'CHABUYL)                                 
         DC    AL2(CHABUYL-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'CHALENL)                                 
         DC    AL2(CHALENL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$SLN-*)                                                     
                                                                                
CHASLNTX DC    AL1(LIOFIEOT)                                                    
                                                                                
CHAPGMT  DS    0X                  ** CHANGE PROGRAM FIELD TABLE **             
                                                                                
         DC    AL1(LIOFILIT+LIOFICOM,L'CHABUYL)                                 
         DC    AL2(CHABUYL-*)                                                   
                                                                                
         DC    AL1(LIOFILIT,L'CHAPGML)                                          
         DC    AL2(CHAPGML-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$PROG-*)                                                    
                                                                                
CHAPGMTX DC    AL1(LIOFIEOT)                                                    
                                                                                
CHACSTT  DS    0X                  ** CHANGE COST FIELD TABLE **                
                                                                                
         DC    AL1(LIOFILIT+LIOFICOM,L'CHABUYL)                                 
         DC    AL2(CHABUYL-*)                                                   
                                                                                
         DC    AL1(LIOFILIT,L'CHACSTL)                                          
         DC    AL2(CHACSTL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$RTYPE-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$COST-*)                                                    
                                                                                
CHACSTTX DC    AL1(LIOFIEOT)                                                    
                                                                                
CHACS2T  DS    0X                  ** CHANGE COST 2 FIELD TABLE **              
                                                                                
         DC    AL1(LIOFILIT+LIOFICOM,L'CHABUYL)                                 
         DC    AL2(CHABUYL-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'CHACS2L)                                 
         DC    AL2(CHACS2L-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$COST2-*)                                                   
                                                                                
CHACS2TX DC    AL1(LIOFIEOT)                                                    
                                                                                
CHASKDT  DS    0X                  ** SKED SPOTS FIELD TABLE **                 
                                                                                
         DC    AL1(LIOFILIT+LIOFICOM,L'CHABUYL)                                 
         DC    AL2(CHABUYL-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'CHASKDL)                                 
         DC    AL2(CHASKDL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT+LIOFIEQU+LIOFIPFX,0)                       
         DC    AL2(F$WEEK-*)                                                    
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$SPOTS-*)                                                   
                                                                                
CHASKDTX DC    AL1(LIOFIEOT)                                                    
                                                                                
CHASDT   DS    0X                  ** DAILY SKED SPOTS FIELD TABLE **           
                                                                                
         DC    AL1(LIOFILIT+LIOFICOM,L'CHABUYL)                                 
         DC    AL2(CHABUYL-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'CHASDL)                                  
         DC    AL2(CHASDL-*)                                                    
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT+LIOFIEQU+LIOFIPFX,0)                       
         DC    AL2(F$SDWEEK-*)                                                  
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$SDSPTS-*)                                                  
                                                                                
CHASDTX  DC    AL1(LIOFIEOT)                                                    
                                                                                
CHAADJT  DS    0X                  ** CHANGE ADJACENCY FIELD TABLE **           
                                                                                
         DC    AL1(LIOFILIT+LIOFICOM,L'CHABUYL)                                 
         DC    AL2(CHABUYL-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'CHAADJL)                                 
         DC    AL2(CHAADJL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$ADJCY-*)                                                   
                                                                                
CHAADJTX DC    AL1(LIOFIEOT)                                                    
                                                                                
CHARBKT  DS    0X                  ** CHANGE BOOK FIELD TABLE **                
                                                                                
         DC    AL1(LIOFILIT+LIOFICOM,L'CHABUYL)                                 
         DC    AL2(CHABUYL-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'CHARBKL)                                 
         DC    AL2(CHARBKL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$BOOK-*)                                                    
                                                                                
CHARBKTX DC    AL1(LIOFIEOT)                                                    
                                                                                
CHABTYT  DS    0X                  ** CHANGE BOOK TYPE FIELD TABLE **           
                                                                                
         DC    AL1(LIOFILIT+LIOFICOM,L'CHABUYL)                                 
         DC    AL2(CHABUYL-*)                                                   
                                                                                
         DC    AL1(LIOFILIT,L'CHABTYL)                                          
         DC    AL2(CHABTYL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$BKTY-*)                                                    
                                                                                
CHABTYTX DC    AL1(LIOFIEOT)                                                    
                                                                                
CHAUPGT  DS    0X                  ** CHANGE UPGRADE FORMULA TABLE **           
                                                                                
         DC    AL1(LIOFILIT+LIOFICOM,L'CHABUYL)                                 
         DC    AL2(CHABUYL-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'CHAUPGL)                                 
         DC    AL2(CHAUPGL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$UPGRD-*)                                                   
                                                                                
CHAUPGTX DC    AL1(LIOFIEOT)                                                    
                                                                                
CHADEMT  DS    0X                  ** CHANGE DEMO VALUES FIELD TABLE **         
                                                                                
         DC    AL1(LIOFILIT+LIOFICOM,L'CHABUYL)                                 
         DC    AL2(CHABUYL-*)                                                   
                                                                                
         DC    AL1(LIOFILIT,L'CHADEML)                                          
         DC    AL2(CHADEML-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'SLASHL)                                  
         DC    AL2(SLASHL-*)                                                    
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$MKTDEM-*)                                                  
                                                                                
         DC    AL1(LIOFILIT,L'EQUALL)                                           
         DC    AL2(EQUALL-*)                                                    
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT+LIOFISLS,LIOFIFST+LIOFILST)                
         DC    AL2(F$BYDEM-*)                                                   
                                                                                
CHADEMTX DC    AL1(LIOFIEOT)                                                    
                                                                                
CHAPBDT  DS    0X               ** CHANGE PB DEMO VALUES FIELD TABLE **         
                                                                                
         DC    AL1(LIOFILIT+LIOFICOM,L'CHABUYL)                                 
         DC    AL2(CHABUYL-*)                                                   
                                                                                
         DC    AL1(LIOFILIT,L'CHAPBDL)                                          
         DC    AL2(CHAPBDL-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'SLASHL)                                  
         DC    AL2(SLASHL-*)                                                    
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$MKTDEM-*)                                                  
                                                                                
         DC    AL1(LIOFILIT,L'EQUALL)                                           
         DC    AL2(EQUALL-*)                                                    
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT+LIOFISLS,LIOFIFST+LIOFILST)                
         DC    AL2(F$BYDEM-*)                                                   
                                                                                
CHAPBDTX DC    AL1(LIOFIEOT)                                                    
                                                                                
CHAPRDT  DS    0X                  ** CHANGE MASTER PRD FIELD TABLE **          
                                                                                
         DC    AL1(LIOFILIT+LIOFICOM,L'CHABUYL)                                 
         DC    AL2(CHABUYL-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'CHAPRDL)                                 
         DC    AL2(CHAPRDL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$MPRD-*)                                                    
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'HYPHENL)                                 
         DC    AL2(HYPHENL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$PIGGY-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'SLASHL)                                  
         DC    AL2(SLASHL-*)                                                    
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,LIOFIDSH)                                  
         DC    AL2(F$PBSL1-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$PBSL2-*)                                                   
                                                                                
CHAPRDTX DC    AL1(LIOFIEOT)                                                    
                                                                                
CHATAXT  DS    0X                  ** CHANGE TAX FIELD TABLE **                 
                                                                                
         DC    AL1(LIOFILIT+LIOFICOM,L'CHABUYL)                                 
         DC    AL2(CHABUYL-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'CHATAXL)                                 
         DC    AL2(CHATAXL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$TAX-*)                                                     
                                                                                
CHATAXTX DC    AL1(LIOFIEOT)                                                    
                                                                                
CHAREPT  DS    0X                  ** CHANGE REP FIELD TABLE **                 
                                                                                
         DC    AL1(LIOFILIT+LIOFICOM,L'CHABUYL)                                 
         DC    AL2(CHABUYL-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'CHAREPL)                                 
         DC    AL2(CHAREPL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$REPCD-*)                                                   
                                                                                
CHAREPTX DC    AL1(LIOFIEOT)                                                    
                                                                                
CHAPURT  DS    0X                  ** CHANGE PURPOSE CODE FIELD TAB **          
                                                                                
         DC    AL1(LIOFILIT+LIOFICOM,L'CHABUYL)                                 
         DC    AL2(CHABUYL-*)                                                   
                                                                                
         DC    AL1(LIOFILIT,L'CHAPURL)                                          
         DC    AL2(CHAPURL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$PURP-*)                                                    
                                                                                
CHAPURTX DC    AL1(LIOFIEOT)                                                    
                                                                                
CHABIDT  DS    0X                  ** CHANGE BUY ID FIELD TABLE **              
                                                                                
         DC    AL1(LIOFILIT+LIOFICOM,L'CHABUYL)                                 
         DC    AL2(CHABUYL-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'CHABIDL)                                 
         DC    AL2(CHABIDL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$BUYID-*)                                                   
                                                                                
CHABIDTX DC    AL1(LIOFIEOT)                                                    
                                                                                
IDELT    DS    0X                  ** I,DEL FIELD TABLE **                      
                                                                                
         DC    AL1(LIOFILIT+LIOFICOM,L'INVDETL)                                 
         DC    AL2(INVDETL-*)                                                   
                                                                                
         DC    AL1(LIOFILIT,L'DELBUYL)                                          
         DC    AL2(DELBUYL-*)                                                   
                                                                                
IDELTX   DC    AL1(LIOFIEOT)                                                    
                                                                                
CSPILLT  DS    0X                  ** C,SPILL FIELD TABLE **                    
                                                                                
         DC    AL1(LIOFILIT+LIOFICOM,L'CHABUYL)                                 
         DC    AL2(CHABUYL-*)                                                   
                                                                                
         DC    AL1(LIOFILIT,L'CSPILLL)                                          
         DC    AL2(CSPILLL-*)                                                   
                                                                                
CSPILLTX DC    AL1(LIOFIEOT)                                                    
                                                                                
OTOSPTT  DS    0X                  ** OTO SPOTS FIELD TABLE **                  
                                                                                
         DC    AL1(LIOFILIT+LIOFICOM,L'OTOSPTL)                                 
         DC    AL2(OTOSPTL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIREQ+LIOFICOM,LIOFIFST)                         
         DC    AL2(F$OTO-*)                                                     
                                                                                
         DC    AL1(LIOFIMAP+LIOFIREQ,0)                                         
         DC    AL2(F$OTOWK-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'DOLLARL)                                 
         DC    AL2(DOLLARL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$OTODOL-*)                                                  
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'HYPHENL)                                 
         DC    AL2(HYPHENL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$OTOSPT-*)                                                  
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$OTOPRD-*)                                                  
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'HYPHENL)                                 
         DC    AL2(HYPHENL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,LIOFILST)                                  
         DC    AL2(F$OTOPIG-*)                                                  
                                                                                
OTOSPTTX DC    AL1(LIOFIEOT)                                                    
                                                                                
ALCSPTT  DS    0X                  ** ALLOCATE SPOTS FIELD TABLE **             
                                                                                
         DC    AL1(LIOFILIT+LIOFICOM,L'ALCSPTL)                                 
         DC    AL2(ALCSPTL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIREQ+LIOFICOM,LIOFIFST)                         
         DC    AL2(F$ALCWK-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'HYPHENL)                                 
         DC    AL2(HYPHENL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$ALCSPT-*)                                                  
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'DOLLARL)                                 
         DC    AL2(DOLLARL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$ALCDOL-*)                                                  
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$ALCPRD-*)                                                  
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'HYPHENL)                                 
         DC    AL2(HYPHENL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$ALCPIG-*)                                                  
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'SLASHL)                                  
         DC    AL2(SLASHL-*)                                                    
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,LIOFIDSH)                                  
         DC    AL2(F$ALCSL1-*)                                                  
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,LIOFILST)                                  
         DC    AL2(F$ALCSL2-*)                                                  
                                                                                
ALCSPTTX DC    AL1(LIOFIEOT)                                                    
                                                                                
DELBUYT  DS    0X                  ** BUY DELETE FIELD TABLE **                 
                                                                                
         DC    AL1(LIOFILIT,L'DELBUYL)                                          
         DC    AL2(DELBUYL-*)                                                   
                                                                                
DELBUYTX DC    AL1(LIOFIEOT)                                                    
                                                                                
SPLBUYT  DS    0X                  ** SPLIT BUYLINE FIELD TABLE **              
                                                                                
         DC    AL1(LIOFILIT+LIOFICOM,L'CHABUYL)                                 
         DC    AL2(CHABUYL-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'SPLITBL)                                 
         DC    AL2(SPLITBL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIREQ,LIOFIDSH)                                  
         DC    AL2(F$WEEK-*)                                                    
                                                                                
         DC    AL1(LIOFIMAP+LIOFIREQ,0)                                         
         DC    AL2(F$SPOTS-*)                                                   
                                                                                
SPLBUYTX DC    AL1(LIOFIEOT)                                                    
                                                                                
SEPSPTT  DS    0X                  ** SEPARETE SPOT FIELD TABLE **              
                                                                                
         DC    AL1(LIOFILIT+LIOFICOM,L'CHABUYL)                                 
         DC    AL2(CHABUYL-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'SEPSPOTL)                                
         DC    AL2(SEPSPOTL-*)                                                  
                                                                                
         DC    AL1(LIOFIMAP+LIOFIREQ+LIOFICOM,LIOFIFST)                         
         DC    AL2(F$SWEEK-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'HYPHENL)                                 
         DC    AL2(HYPHENL-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIREQ,LIOFILST)                                  
         DC    AL2(F$SSPOTS-*)                                                  
                                                                                
SEPSPTTX DC    AL1(LIOFIEOT)                                                    
*                                                                               
COMTAB   DS    0AL2                ** BUY COMMENT INDEX TABLE **                
         DC    AL2(COMNT1T-*)                                                   
         DC    AL2(COMNT2T-*)                                                   
         DC    AL2(COMNT3T-*)                                                   
         DC    AL2(COMNT4T-*)                                                   
         DC    AL2(COMNT5T-*)                                                   
COMTABX  DC    AL2(0)                                                           
                                                                                
COMNT1T  DS    0X                  ** COMMENT LINE 1 FIELD TABLE **             
                                                                                
         DC    AL1(LIOFILIT+LIOFICOM,L'CHABUYL)                                 
         DC    AL2(CHABUYL-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'COMNT1L)                                 
         DC    AL2(COMNT1L-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$COM1-*)                                                    
                                                                                
COMNT1TX DC    AL1(LIOFIEOT)                                                    
                                                                                
COMNT2T  DS    0X                  ** COMMENT LINE 2 FIELD TABLE **             
                                                                                
         DC    AL1(LIOFILIT+LIOFICOM,L'CHABUYL)                                 
         DC    AL2(CHABUYL-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'COMNT2L)                                 
         DC    AL2(COMNT2L-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$COM2-*)                                                    
                                                                                
COMNT2TX DC    AL1(LIOFIEOT)                                                    
                                                                                
COMNT3T  DS    0X                  ** COMMENT LINE 3 FIELD TABLE **             
                                                                                
         DC    AL1(LIOFILIT+LIOFICOM,L'CHABUYL)                                 
         DC    AL2(CHABUYL-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'COMNT3L)                                 
         DC    AL2(COMNT3L-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$COM3-*)                                                    
                                                                                
COMNT3TX DC    AL1(LIOFIEOT)                                                    
                                                                                
COMNT4T  DS    0X                  ** COMMENT LINE 4 FIELD TABLE **             
                                                                                
         DC    AL1(LIOFILIT+LIOFICOM,L'CHABUYL)                                 
         DC    AL2(CHABUYL-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'COMNT4L)                                 
         DC    AL2(COMNT4L-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$COM4-*)                                                    
                                                                                
COMNT4TX DC    AL1(LIOFIEOT)                                                    
                                                                                
COMNT5T  DS    0X                  ** COMMENT LINE 5 FIELD TABLE **             
                                                                                
         DC    AL1(LIOFILIT+LIOFICOM,L'CHABUYL)                                 
         DC    AL2(CHABUYL-*)                                                   
                                                                                
         DC    AL1(LIOFILIT+LIOFIPFX,L'COMNT5L)                                 
         DC    AL2(COMNT5L-*)                                                   
                                                                                
         DC    AL1(LIOFIMAP+LIOFIOPT,0)                                         
         DC    AL2(F$COM5-*)                                                    
                                                                                
COMNT5TX DC    AL1(LIOFIEOT)                                                    
                                                                                
LINKMAP  DS    0XL(LIORL)          ** RECORD MAP TABLE **                       
         DC    AL2(I#SDBYHD,O#SDBUYR,BUYHDR-LINKMAP)                            
         DC    AL2(I#SDMGBH,O#SDBUYR,BUYMGHD-LINKMAP)                           
         DC    AL2(I#SDBADD,I#SDBADD,BUYADD-LINKMAP)                            
         DC    AL2(I#SDBCHA,I#SDBCHA,BUYCHA-LINKMAP)                            
         DC    AL2(I#SDBSKD,I#SDBSKD,BUYSKD-LINKMAP)                            
         DC    AL2(I#SDBSD,I#SDBSD,BUYSD-LINKMAP)                               
         DC    AL2(I#SDBSPL,I#SDBSPL,BUYSPL-LINKMAP)                            
         DC    AL2(I#SDBSPC,I#SDBSPC,BUYSPT-LINKMAP)                            
         DC    AL2(I#SDBALC,I#SDBALC,BUYALC-LINKMAP)                            
         DC    AL2(I#SDBMGD,I#SDBMGD,BUYMGD-LINKMAP)                            
         DC    AL2(I#SDBDEL,I#SDBDEL,BUYDEL-LINKMAP)                            
         DC    AL2(I#SDBBYD,I#SDBBYD,BUYDEM-LINKMAP)                            
         DC    AL2(I#SDBPBD,I#SDBPBD,BUYPBD-LINKMAP)                            
         DC    AL2(I#SDMGSP,I#SDMGSP,BUYMGSA-LINKMAP)                           
         DC    AL2(I#SDMGPP,I#SDMGPP,BUYMGPP-LINKMAP)                           
         DC    AL2(I#SDMGAP,I#SDMGAP,BUYMGAP-LINKMAP)                           
         DC    AL2(I#SDMGRJ,I#SDMGRJ,BUYMGRJ-LINKMAP)                           
         DC    AL2(I#SDBORB,I#SDBORB,BUYORB-LINKMAP)                            
         DC    AL2(I#SDBPKG,I#SDBPKG,BUYPKG-LINKMAP)                            
         DC    AL2(I#SDBSPS,I#SDBSPS,BUYSPS-LINKMAP)                            
LINKMAPX DC    AL2(0)                                                           
         EJECT                                                                  
D$MGLINE DC    AL2(D#MGREF)        DUMMY ENTRY FOR MAKEGOOD LINE RECALL         
         DC    AL1(LIOBSB2Q)                                                    
         DC    AL2(M$LINE#-WORKD)                                               
         DC    AL1(L'M$LINE#)                                                   
         DC    AL1(LIODISFF)                                                    
         DC    AL1(0)                                                           
                                                                                
D$PKGLIN DC    AL2(D#LINE)         DUMMY ENTRY FOR PACKAGE LINE RECALL          
         DC    AL1(LIOBSB2Q)                                                    
         DC    AL2(P$PKGSLV-WORKD)                                              
         DC    AL1(L'P$PKGSLV)                                                  
         DC    AL1(LIODISFF)                                                    
         DC    AL1(0)                                                           
                                                                                
       ++INCLUDE SPMAPBUY                                                       
         EJECT                                                                  
WORKD    DSECT                     ** LOCAL WORKING STORAGE **                  
                                                                                
ASPBUY39 DS    A                   A(SPBUY39)                                   
HEXOUT   DS    A                   A(HEXOUT)                                    
LINKIO   DS    A                   A(LINKIO)                                    
WSSVR    DS    A                   A(WSSVR)                                     
                                                                                
BY39FLAG DS    X                   VARIOUS FLAGS                                
BY39HDDA EQU   X'80'               BUY HEADER D/A SENT                          
                                                                                
MYELEM   DS    XL(L'ELEM)                                                       
SIMCOUNT DS    X                   NUMBER OF SIMULATED BUYS ADDED               
ERROR#   DS    AL2                 ERROR MESSAGE NUMBER                         
XTRATEXT DS    CL20                EXTRA ERROR MESSAGE TEXT                     
BFCALL   DS    X                   BLDFLD CALLING FLAGS                         
SAVEDA   DS    XL(L'BUYKDA)        SAVED BUY RECORD D/A (SPLIT & MKGD)          
BUYHDDA  DS    XL(L'BUYKDA)        D/A OF BUY HEADER SENT                       
BUYHDSTA DS    CL(L'BUYST)         STA OF BUY HEADER SENT                       
CHECKTST DS    AL4                 A(CHECKSUM VALUE TO BE TESTED)               
SVBUMGCD DS    XL(L'BUMGCODE)      SAVED MAKEGOOD CODE                          
S$MGDARY DS    AL(L'I$MGDARY)      SAVED I$MGDARY VALUE                         
                                                                                
CHECKSUM DS    XL(#$CKSUM)         BUY RECORD CHECKSUM                          
PCKEY    DS    XL(#$PCKEY)         BUY RECORD PC KEY                            
SIMULATE DS    CL(#$SIMADD)        SIMULATE BUY ADD                             
MARKET   DS    XL(#$MARKET)        BUY SHEET'S MARKET                           
IDEL     DS    CL(#$IDEL)          DELETE AFFIDS                                
CSPILL   DS    CL(#$CSPILL)        SPILL LOOKUP                                 
ORDER    DS    CL8                 ORDER NUMBER                                 
REVAL    DS    C                   RE-VALIDATE OPTION                           
PKGTYP   DS    X                   PKG TYPE FOR REMPKG                          
PKGMAS   DS    XL2                 PKG MASTER LINE FOR REMPKG                   
                                                                                
         LKNDX GEN                 FIELD INDEX LIST                             
                                                                                
RJ$LINE  DS    0XL(A$LINEL)        DUMMY REQUEST ELEMENT FOR REJECT COM         
A$LINE   DS    XL(A$LINEL)         DUMMY REQUEST ELEMENT FOR ADDED LINE         
A$LINEL  EQU   LQ_VALUE+#$LINE-LQ_D                                             
                                                                                
M$LINE   DS    XL(LQ_VALUE+2-LQ_D) DUMMY REQUEST ELEMENT FOR MAKEGOOD           
M$DATA   DS    0C                  ** MAKEGOOD ARRAY LINE **                    
M$MGMIN  DS    C                   UNREFERENCE INDICATOR                        
M$LINE#  DS    CL(#$LINE)          BUY LINE NUMBER                              
M$DATE   DS    CL(#$WEEK)          WEEK                                         
M$REF    DS    CL(#$SPTS)          SPOT REFERENCE NUMBER                        
M$NETWK  DS    CL(L'QCBLNET)       MAKEGOOD NETWORK                             
M$DATAL  EQU   *-M$DATA            WIDTH OF ARRAY LINE                          
M$LINEL  EQU   *-M$LINE            LENGTH OF DUMMY ELEMENT                      
M$CKSM   DS    XL(#$CKSUM)         BUY RECORD CHECKSUM VALUE                    
                                                                                
P$PKG    DS    XL(LQ_VALUE+2-LQ_D) DUMMY REQUEST ELEMENT FOR PACKAGE            
P$DATA   DS    0C                  ** PACKAGE ARRAY LINE **                     
P$PKGSLV DS    CL(#$LINE)          PACKAGE SLAVE LINE NUMBER                    
P$CKSM   DS    XL(#$CKSUM)         BUY RECORD CHECKSUM VALUE                    
P$DEL    DS    C                   DELETE INDICATOR                             
P$DATAL  EQU   *-P$DATA            WIDTH OF ARRAY LINE                          
P$PKGL   EQU   *-P$PKG             LENGTH OF DUMMY ELEMENT                      
                                                                                
CKSMTAB  DS    0XL(L'BUYKDA)       ** TABLE OF BUYS CHECKED **                  
CKSMTABQ EQU   499                 499             -HWON 1/25/2019              
         DS    (CKSMTABQ)XL(L'BUYKDA)                                           
                                                                                
MGDATAB  DS    0XL(L'BUYKDA)       ** TABLE OF MADEGOOD BUY D/A'S **            
MGDATABQ EQU   255                                                              
         DS    (MGDATABQ)XL(L'BUYKDA)                                           
MGDATABL EQU   *-MGDATAB                                                        
         DS    XL4                 EOT MARKER FOR BUY00                         
***      DC    X'FFFFFFFF'         EOT MARKER FOR BUY00                         
                                                                                
         DS    0F                                                               
WSSVAREA DS    XL(FAWSSVRL)        ** WSSVR CONTROL BLOCK **                    
                                                                                
         DS    0F                                                               
LIOBAREA DS    XL(L'LIOB)          ** LINKIO CONTROL BLOCK **                   
                                                                                
WORKL    EQU   *-WORKD                                                          
                                                                                
* INCLUDED BOOKS FOLLOW                                                         
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDLINKD                                                        
LIOBD    DSECT                                                                  
       ++INCLUDE DDLINKIOD                                                      
LIOBD    DSECT                                                                  
         ORG   LIOBWORK                                                         
LIOBSAVE DS    AL2                 SAVED MAP NUMBER                             
LIOBMAPN DS    AL2                 CURRENT ELEMENT MAP NUMBER                   
                                                                                
LIOBTOKN DS    0XL6                ** WSSVR TOKEN/LENGTH **                     
LIOBTVAL DS    CL4                 DDLINK TOKEN VALUE                           
LIOBTLEN DS    AL2                 BUFFER LENGTH                                
                                                                                
LIOBWREC DS    XL(L'W_RECS)        NUMBER OF RECORDS ON WORKER FILE             
LIOBACUR DS    0AL4                A(CURRENT RECORD IN BUFFER)                  
LIOBCREC DS    XL(L'LIOBWREC)      CURRENT RECORD NUMBER                        
LIOBRMEA DS    AL4                 A(RETURN MAP ELEMENT)                        
LIOBENDI DS    AL4                 A(END OF INPUT DATA)                         
LIOBRECA DS    AL4                 A(FIRST DATA ELEMENT)                        
LIOBNXTA DS    AL4                 A(NEXT RETURN DATA ELEMENT)                  
LIOBENDA DS    AL4                 A(END OF CURRENT RECORD)                     
LIOBCURA DS    AL4                 A(CURRENT ELEMENT)                           
LIOBMAPA DS    AL4                 A(DATA MAP TABLE)                            
LIOBOMAP DS    XL(L'LIOROMAP)      RETURN MAP CODE                              
                                                                                
       ++INCLUDE FAWSSVRD                                                       
       ++INCLUDE SPBUYWORK                                                      
       ++INCLUDE SPMAPEQUS                                                      
       ++INCLUDE DMWRKFD                                                        
       ++INCLUDE FAUTL                                                          
       ++INCLUDE FAXTRAINF                                                      
       ++INCLUDE SPBUY31WRK                                                     
*&&DO                                                                           
       ++INCLUDE FASSB                                                          
*PREFIX=BY$                                                                     
       ++INCLUDE FASYSFAC                                                       
*PREFIX=                                                                        
*&&                                                                             
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045SPBUY39   11/03/20'                                      
         END                                                                    
