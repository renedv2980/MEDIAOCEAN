* THESE ARE HLASM SYSADATA FILE RECORD LAYOUTS.                         00001   
*                                                                       00002   
* THIS MEMBER CONTAINS DFSORT SYMBOL DEFINITION STATEMENTS. IT IS       00003   
* INTENDED AS INPUT TO ANY DFSORT APPLICATION THAT PROCESSES THE        00004   
* SYSADATA FILE.                                                        00005   
*                                                                       00006   
* THESE STATEMENTS WERE GENERATED *PROGRAMMATICALLY* BY A UTILITY       00007   
* (WRITTEN BY DEIS) WHICH TRANSFORMS ASSEMBLER DSECTS INTO DFSORT       00008   
* SYMBOLS. SEE DDBLDSYMS FOR DETAILS.                                   00009   
*                                                                       00010   
************************  VERY IMPORTANT ****************************** 00011   
*                                                                     * 00012   
* AS PER THE IBM DOCUMENTATION, THE STRUCTURE OF THE SYSADATA FILE    * 00013   
* IS SUBJECT TO CHANGE. WHENEVER WE UPGRADE TO A NEW VERSION OF THE   * 00014   
* ASSEMBLER, WE MUST CHECK THE "ARCHITECTURE LEVEL" AND CONFIRM       * 00015   
* WHETHER OR NOT WE NEED TO CHANGE ANY CODE THAT READS THE SYSADATA   * 00016   
* FILE.                                                               * 00017   
*                                                                     * 00018   
* SEE PAN MEMBER DDASMADATA FOR THE ASSEMBLER MACRO SOURCE WHICH      * 00019   
* GENERATES THE ASSEMBLY LANGUAGE DSECTS EQUIVALENT TO THE STATEMENTS * 00020   
* BELOW (AND FROM WHICH THE DFSORT SYMBOLS WERE GENERATED).           * 00021   
*                                                                     * 00022   
*********************************************************************** 00023   
*                                                                       00024   
*                                                                       00025   
*============> START OF PROGRAMMATICALLY-GENERATED SYMBOLS <=========== 00026   
*                                                                       00027   
* The following DFSORT symbols were generated **programmatically** by   00028   
* the DDBLDSYMS utility. This utility interprets DS/DC/EQU statements   00029   
* in an assembler DSECT member, and constructs equivalent DFSORT        00030   
* symbols. See DDBLDSYMS for details.                                   00031   
*                                                                       00032   
DT02,1                                                        EQU       00033**4
DT02X,2                                                       EQU       00034**4
DT02N,5                                                       EQU       00035**4
DT14,3                                                        EQU       00036**4
DT14X,4                                                       EQU       00037**4
DT14N,6                                                       EQU       00038**4
DTSPCL,X'40000000'                                            EQU       00039**4
DTIGNOR,X'20000000'                                           EQU       00040**5
DTNOCNV,X'20000000'                                           EQU       00041**4
PTYPE_ELCODE,512                                              EQU       00042**4
PTYPE_RELATED_ELEMENT,513                                     EQU       00043**4
PTYPE_ACTIVE,514                                              EQU       00044**4
PTYPE_PASSIVE,515                                             EQU       00045**4
ADATA_LEN,1,2,FI                                              DS/DC     00046   
ADATA_VERSION,5,1,FI                                          DS/DC     00047   
ADATA_VERASM,16                                               EQU       00048   
ADATA_RECTYPE,6,2,BI                                          DS/DC     00049   
ADATA_LEVEL,8,1,FI                                            DS/DC     00050   
ADATA_L1,1                                                    EQU       00051   
ADATA_L2,2                                                    EQU       00052   
ADATA_L3,3                                                    EQU       00053   
ADATA_LVL,3                                                   EQU       00054   
ADATA_CONT,9,1,BI                                             DS/DC     00055   
ADATA_NOTCONT,0                                               EQU       00056   
ADATA_ISCONT,1                                                EQU       00057   
ADATA_EDITION,10,1,FI                                         DS/DC     00058   
ADATA_DATA_LEN,15,2,FI                                        DS/DC     00059   
ADATA_ORG,17,2,FI                                             DS/DC     00060   
ADATA_EQU_LEN,16                                              EQU       00061   
ADATA_RECJID,0                                                EQU       00062   
ADATA_EDNJID,1                                                EQU       00063   
ADJID_DATE,17,8,CH                                            DS/DC     00064   
ADJID_TIME,25,4,CH                                            DS/DC     00065   
ADJID_PROD_NUMBER,29,8,CH                                     DS/DC     00066   
ADJID_PROD_VERSION,37,8,CH                                    DS/DC     00067   
ADJID_PROD_LEVEL,45,2,FI                                      DS/DC     00068   
ADJID_PROD_LEVEL_CURRENT,6                                    EQU       00069   
ADJID_PTF,47,8,CH                                             DS/DC     00070   
ADJID_SYSTEM_ID,55,24,CH                                      DS/DC     00071   
ADJID_JOBNAME,79,8,CH                                         DS/DC     00072   
ADJID_STEPNAME,87,8,CH                                        DS/DC     00073   
ADJID_PROCSTEP,95,8,CH                                        DS/DC     00074   
ADJID_FILES_NUM,103,4,FI                                      DS/DC     00075   
ADJID_FILES_OFF,107,4,FI                                      DS/DC     00076   
ADJID_EQU_LEN,110                                             EQU       00077   
ADATA_RECAID,1                                                EQU       00078   
ADATA_EDNAID,0                                                EQU       00079   
ADAID_TIME,17,8,BI                                            DS/DC     00080   
ADAID_CCSID,25,2,BI                                           DS/DC     00081   
ADAID_EQU_LEN,26                                              EQU       00082   
ADATA_RECCOMPUNIT,2                                           EQU       00083   
ADATA_EDNCOMPUNIT,0                                           EQU       00084   
ADCOMP_IND,17,2,BI                                            DS/DC     00085   
ADCOMP_IND_START,0                                            EQU       00086   
ADCOMP_IND_END,1                                              EQU       00087   
ADCOMP_RCOUNT,21,4,FI                                         DS/DC     00088   
ADCOMP_EQU_LEN,24                                             EQU       00089   
ADATA_RECOUTPUT,10                                            EQU       00090   
ADATA_EDNOUTPUT,1                                             EQU       00091   
ADOUT_OBJECT_NUM,17,4,FI                                      DS/DC     00092   
ADOUT_OBJECT_OFF,21,4,FI                                      DS/DC     00093   
ADOUT_PUNCH_NUM,25,4,FI                                       DS/DC     00094   
ADOUT_PUNCH_OFF,29,4,FI                                       DS/DC     00095   
ADOUT_PRINT_NUM,33,4,FI                                       DS/DC     00096   
ADOUT_PRINT_OFF,37,4,FI                                       DS/DC     00097   
ADOUT_TERM_NUM,41,4,FI                                        DS/DC     00098   
ADOUT_TERM_OFF,45,4,FI                                        DS/DC     00099   
ADOUT_ADATA_NUM,49,4,FI                                       DS/DC     00100   
ADOUT_ADATA_OFF,53,4,FI                                       DS/DC     00101   
ADOUT_EQU_LEN,56                                              EQU       00102   
ADATA_RECAOPT,11                                              EQU       00103   
ADATA_EDNAOPT,1                                               EQU       00104   
ADAOPT_FILES_NUM,17,4,FI                                      DS/DC     00105   
ADAOPT_FILES_OFF,21,4,FI                                      DS/DC     00106   
ADAOPT_EQU_LEN,24                                             EQU       00107   
ADATA_RECOPT,16                                               EQU       00108   
ADATA_EDNOPT,3                                                EQU       00109   
ADOPT_DECK,16                                                 EQU       00110   
ADOPT_OBJECT,16                                               EQU       00111   
ADOPT_LIST,16                                                 EQU       00112   
ADOPT_XREF,16                                                 EQU       00113   
ADOPT_RENT,16                                                 EQU       00114   
ADOPT_TEST,16                                                 EQU       00115   
ADOPT_BATCH,16                                                EQU       00116   
ADOPT_ALIGN,16                                                EQU       00117   
ADOPT_BYTE1,17,1,BI                                           DS/DC     00118   
ADOPT_ESD,17                                                  EQU       00119   
ADOPT_RLD,17                                                  EQU       00120   
ADOPT_XREF_SHORT,17                                           EQU       00121   
ADOPT_TRACE,17                                                EQU       00122   
ADOPT_XREF_FULL,17                                            EQU       00123   
ADOPT_SIZE_MAX,17                                             EQU       00124   
ADOPT_XREF_UNREFS,17                                          EQU       00125   
ADOPT_RXREF,17                                                EQU       00126   
ADOPT_BYTE2,18,1,BI                                           DS/DC     00127   
ADOPT_TERM,18                                                 EQU       00128   
ADOPT_TERM_NARROW,18                                          EQU       00129   
ADOPT_DBCS,18                                                 EQU       00130   
ADOPT_DXREF,18                                                EQU       00131   
ADOPT_FOLD,18                                                 EQU       00132   
ADOPT_SIZE,18                                                 EQU       00133   
ADOPT_FLAG_PUSH,18                                            EQU       00134   
ADOPT_THREAD,18                                               EQU       00135   
ADOPT_BYTE3,19,1,BI                                           DS/DC     00136   
ADOPT_PCONT_ON,19                                             EQU       00137   
ADOPT_PCONT_GEN,19                                            EQU       00138   
ADOPT_PCONT_DATA,19                                           EQU       00139   
ADOPT_PCONT_UHEAD,19                                          EQU       00140   
ADOPT_PCONT_MSOURCE,19                                        EQU       00141   
ADOPT_SECTALGN,19                                             EQU       00142   
ADOPT_DATDF,19                                                EQU       00143**2
ADOPT_BYTE4,20,1,BI                                           DS/DC     00144   
ADOPT_ASA,20                                                  EQU       00145   
ADOPT_USING_WARN,20                                           EQU       00146   
ADOPT_USING_LIMIT,20                                          EQU       00147   
ADOPT_USING_MAP,20                                            EQU       00148   
ADOPT_INEXIT,20                                               EQU       00149   
ADOPT_LIBEXIT,20                                              EQU       00150   
ADOPT_PRTEXIT,20                                              EQU       00151   
ADOPT_OBJEXIT,20                                              EQU       00152   
ADOPT_BYTE5,21,1,BI                                           DS/DC     00153   
ADOPT_SYSPARM,21                                              EQU       00154   
ADOPT_FLAG,21                                                 EQU       00155   
ADOPT_LANGUAGE,21                                             EQU       00156   
ADOPT_LINECOUNT,21                                            EQU       00157   
ADOPT_OPTABLE,21                                              EQU       00158   
ADOPT_ADATA,21                                                EQU       00159   
ADOPT_ADEXIT,21                                               EQU       00160   
ADOPT_TRMEXIT,21                                              EQU       00161   
ADOPT_BYTE6,22,1,BI                                           DS/DC     00162   
ADOPT_LIST_121,22                                             EQU       00163   
ADOPT_LIST_133,22                                             EQU       00164   
ADOPT_LIST_MAX,22                                             EQU       00165   
ADOPT_BYTE7,23,1,BI                                           DS/DC     00166   
ADOPT_MXREF,23                                                EQU       00167   
ADOPT_MXREF_FULL,23                                           EQU       00168   
ADOPT_MXREF_SOURCE,23                                         EQU       00169   
ADOPT_MXREF_XREF,23                                           EQU       00170   
ADOPT_TRANSLATE,23                                            EQU       00171   
ADOPT_GOFF,23                                                 EQU       00172   
ADOPT_GOFF_ADATA,23                                           EQU       00173   
ADOPT_PROFILE,23                                              EQU       00174   
ADOPT_BYTE8,24,1,BI                                           DS/DC     00175   
ADOPT_FLAG_RECORD,24                                          EQU       00176   
ADOPT_PCONT_MCALL,24                                          EQU       00177   
ADOPT_PCONT_OFF,24                                            EQU       00178   
ADOPT_PCONT_NODATA,24                                         EQU       00179   
ADOPT_PCONT_NOGEN,24                                          EQU       00180   
ADOPT_PCONT_NOUHEAD,24                                        EQU       00181   
ADOPT_PCONT_NOMSOURCE,24                                      EQU       00182   
ADOPT_PCONT_NOMCALL,24                                        EQU       00183   
ADOPT_BYTE9,25,1,BI                                           DS/DC     00184   
ADOPT_SERVICE,25                                              EQU       00185   
ADOPT_WORKFILE,25                                             EQU       00186   
ADOPT_OPTABLE_LIST,25                                         EQU       00187   
ADOPT_CODEPAGE,25                                             EQU       00188   
ADOPT_OPTION_ERRORS,25                                        EQU       00189   
ADOPT_INFO,25                                                 EQU       00190   
ADOPT_BYTE10,26,1,BI                                          DS/DC     00191   
ADOPT_FLAG_EXLITW,26                                          EQU       00192   
ADOPT_TYPECHECK_MAG,26                                        EQU       00193   
ADOPT_TYPECHECK_REG,26                                        EQU       00194   
ADOPT_COMPAT_CASE,26                                          EQU       00195   
ADOPT_COMPAT_SYSLIST,26                                       EQU       00196   
ADOPT_COMPAT_LITTYPE,26                                       EQU       00197   
ADOPT_COMPAT_MACROCASE,26                                     EQU       00198   
ADOPT_BYTE11,27,1,BI                                          DS/DC     00199   
ADOPT_FLAG_USING0,27                                          EQU       00200   
ADOPT_LIBMAC,27                                               EQU       00201   
ADOPT_RA2,27                                                  EQU       00202   
ADOPT_FLAG_ALIGN,27                                           EQU       00203   
ADOPT_FLAG_CONT,27                                            EQU       00204   
ADOPT_FLAG_SUBSTR,27                                          EQU       00205   
ADOPT_FLAG_IMPLEN,27                                          EQU       00206   
ADOPT_FLAG_PAGE0,27                                           EQU       00207   
ADOPT_BYTE12,28,1,BI                                          DS/DC     00208   
ADOPT_SUPRWARN,28                                             EQU       00209   
ADOPT_FAILMSG,28                                              EQU       00210   
ADOPT_FAILMNOTE,28                                            EQU       00211   
ADOPT_FAILMAXERRS,28                                          EQU       00212   
ADOPT_ILMA,28                                                 EQU       00213**2
ADOPT_FLAG_TRUNC,28                                           EQU       00214**4
ADOPT_FLAG_LONGER,28                                          EQU       00215**4
ADOPT_FLAG_SIGNED,28                                          EQU       00216**4
ADOPT_BYTE13,29,1,BI                                          DS/DC     00217   
ADOPT_BYTE14,30,1,BI                                          DS/DC     00218   
ADOPT_COMPAT,34                                               EQU       00219   
ADOPT_EXIT,34                                                 EQU       00220   
ADOPT_PCONT,34                                                EQU       00221   
ADOPT_PESTOP,34                                               EQU       00222   
ADOPT_SUBLIB_DF,34                                            EQU       00223   
ADOPT_EXTRA_BYTE1,35,1,BI                                     DS/DC     00224   
ADOPT_CODEPAGE_VAL,40,4,BI                                    DS/DC     00225   
ADOPT_FLAG_VAL,44,1,FI                                        DS/DC     00226   
ADOPT_LANGUAGE_VAL,45,3,CH                                    DS/DC     00227   
ADOPT_LINECOUNT_VAL,48,2,FI                                   DS/DC     00228   
ADOPT_OPTABLE_VAL,50,3,CH                                     DS/DC     00229   
ADOPT_PROFILE_VAL,53,8,CH                                     DS/DC     00230   
ADOPT_SECTALGN_VAL,61,4,FI                                    DS/DC     00231   
ADOPT_TRANSLATE_VAL,65,2,CH                                   DS/DC     00232   
ADOPT_USING_LIMIT_VAL,67,2,FI                                 DS/DC     00233   
ADOPT_USING_WARN_VAL,69,1,FI                                  DS/DC     00234   
ADOPT_FAILMSG_VAL,70,1,FI                                     DS/DC     00235   
ADOPT_FAILMNOTE_VAL,71,1,FI                                   DS/DC     00236   
ADOPT_FAILMAXERRS_VAL,72,2,FI                                 DS/DC     00237   
ADOPT_PARM_STR_OFF,102,4,FI                                   DS/DC     00238   
ADOPT_PARM_STR_LEN,106,4,FI                                   DS/DC     00239   
ADOPT_SYSPARM_STR_OFF,110,4,FI                                DS/DC     00240   
ADOPT_SYSPARM_STR_LEN,114,4,FI                                DS/DC     00241   
ADOPT_INEXIT_PROG_OFF,118,4,FI                                DS/DC     00242   
ADOPT_INEXIT_PROG_LEN,122,4,FI                                DS/DC     00243   
ADOPT_INEXIT_STR_OFF,126,4,FI                                 DS/DC     00244   
ADOPT_INEXIT_STR_LEN,130,4,FI                                 DS/DC     00245   
ADOPT_LIBEXIT_PROG_OFF,134,4,FI                               DS/DC     00246   
ADOPT_LIBEXIT_PROG_LEN,138,4,FI                               DS/DC     00247   
ADOPT_LIBEXIT_STR_OFF,142,4,FI                                DS/DC     00248   
ADOPT_LIBEXIT_STR_LEN,146,4,FI                                DS/DC     00249   
ADOPT_PRTEXIT_PROG_OFF,150,4,FI                               DS/DC     00250   
ADOPT_PRTEXIT_PROG_LEN,154,4,FI                               DS/DC     00251   
ADOPT_PRTEXIT_STR_OFF,158,4,FI                                DS/DC     00252   
ADOPT_PRTEXIT_STR_LEN,162,4,FI                                DS/DC     00253   
ADOPT_OBJEXIT_PROG_OFF,166,4,FI                               DS/DC     00254   
ADOPT_OBJEXIT_PROG_LEN,170,4,FI                               DS/DC     00255   
ADOPT_OBJEXIT_STR_OFF,174,4,FI                                DS/DC     00256   
ADOPT_OBJEXIT_STR_LEN,178,4,FI                                DS/DC     00257   
ADOPT_ADEXIT_PROG_OFF,182,4,FI                                DS/DC     00258   
ADOPT_ADEXIT_PROG_LEN,186,4,FI                                DS/DC     00259   
ADOPT_ADEXIT_STR_OFF,190,4,FI                                 DS/DC     00260   
ADOPT_ADEXIT_STR_LEN,194,4,FI                                 DS/DC     00261   
ADOPT_TRMEXIT_PROG_OFF,198,4,FI                               DS/DC     00262   
ADOPT_TRMEXIT_PROG_LEN,202,4,FI                               DS/DC     00263   
ADOPT_TRMEXIT_STR_OFF,206,4,FI                                DS/DC     00264   
ADOPT_TRMEXIT_STR_LEN,210,4,FI                                DS/DC     00265   
ADOPT_EQU_LEN,213                                             EQU       00266   
ADOPT_PARM_STR,214,255,CH                                     DS/DC     00267   
ADOPT_SYSPARM_STR,214,255,CH                                  DS/DC     00268   
ADOPT_INEXIT_PROG,214,8,CH                                    DS/DC     00269   
ADOPT_INEXIT_STR,214,64,CH                                    DS/DC     00270   
ADOPT_LIBEXIT_PROG,214,8,CH                                   DS/DC     00271   
ADOPT_LIBEXIT_STR,214,64,CH                                   DS/DC     00272   
ADOPT_PRTEXIT_PROG,214,8,CH                                   DS/DC     00273   
ADOPT_PRTEXIT_STR,214,64,CH                                   DS/DC     00274   
ADOPT_OBJEXIT_PROG,214,8,CH                                   DS/DC     00275   
ADOPT_OBJEXIT_STR,214,64,CH                                   DS/DC     00276   
ADOPT_ADEXIT_PROG,214,8,CH                                    DS/DC     00277   
ADOPT_ADEXIT_STR,214,64,CH                                    DS/DC     00278   
ADOPT_TRMEXIT_PROG,214,8,CH                                   DS/DC     00279   
ADOPT_TRMEXIT_STR,214,64,CH                                   DS/DC     00280   
ADATA_RECESD,32                                               EQU       00281   
ADATA_EDNESD,1                                                EQU       00282   
ADESD_TYPE,17,1,BI                                            DS/DC     00283   
ADESD_TYPE_SECTION,0                                          EQU       00284   
ADESD_TYPE_LABEL,1                                            EQU       00285   
ADESD_TYPE_EXTREF,2                                           EQU       00286   
ADESD_TYPE_ELEMENT,3                                          EQU       00287   
ADESD_TYPE_PRIVATE,4                                          EQU       00288   
ADESD_TYPE_COMMON,5                                           EQU       00289   
ADESD_TYPE_EXTDUMMY,6                                         EQU       00290   
ADESD_TYPE_PART,7                                             EQU       00291   
ADESD_TYPE_EXTWEAK,10                                         EQU       00292   
ADESD_TYPE_DUMMY,255                                          EQU       00293   
ADESD_FLAG_RSECT,17                                           EQU       00294   
ADESD_FLAG_RANY,17                                            EQU       00295   
ADESD_FLAG_AANY,17                                            EQU       00296   
ADESD_FLAG_A31,17                                             EQU       00297   
ADESD_FLAG_A24,17                                             EQU       00298   
ADESD_FLAGS,18,1,BI                                           DS/DC     00299   
ADESD_ESDID,21,4,FI                                           DS/DC     00300   
ADESD_ADDR,29,4,BI                                            DS/DC     00301   
ADESD_LEN,37,4,FI                                             DS/DC     00302   
ADESD_OWNERID,41,4,FI                                         DS/DC     00303   
ADESD_NAME_OFF,53,4,FI                                        DS/DC     00304   
ADESD_NAME_LEN,57,4,FI                                        DS/DC     00305   
ADESD_ALIAS_OFF,61,4,FI                                       DS/DC     00306   
ADESD_ALIAS_LEN,65,4,FI                                       DS/DC     00307   
ADESD_EQU_LEN,68                                              EQU       00308   
ADESD_NAME,69,63,CH                                           DS/DC     00309   
ADESD_ALIAS,69,63,CH                                          DS/DC     00310   
ADATA_RECSOURCE,48                                            EQU       00311   
ADATA_EDNSOURCE,1                                             EQU       00312   
ADSRC_ESDID,17,4,FI                                           DS/DC     00313   
ADSRC_STMT,21,4,FI                                            DS/DC     00314   
ADSRC_INPUT_REC,25,4,FI                                       DS/DC     00315   
ADSRC_PARENT_REC,29,4,FI                                      DS/DC     00316   
ADSRC_INPUT_NUM,33,4,FI                                       DS/DC     00317   
ADSRC_PARENT_NUM,37,4,FI                                      DS/DC     00318   
ADSRC_LOCTR,41,4,FI                                           DS/DC     00319   
ADSRC_REC_ORIGIN,45,1,BI                                      DS/DC     00320   
ADSRC_REC_PINP,1                                              EQU       00321   
ADSRC_REC_MACGEN,2                                            EQU       00322   
ADSRC_REC_MAC,3                                               EQU       00323   
ADSRC_REC_AINSERT,5                                           EQU       00324   
ADSRC_PARENT_ORIGIN,46,1,BI                                   DS/DC     00325   
ADSRC_PRINT_GEN,46                                            EQU       00326   
ADSRC_PRINT_DATA,46                                           EQU       00327   
ADSRC_PRINT_ON,46                                             EQU       00328   
ADSRC_PRINT_NOMSOURCE,46                                      EQU       00329   
ADSRC_PRINT_UHEAD,46                                          EQU       00330   
ADSRC_PRINT_MCALL,46                                          EQU       00331   
ADSRC_PRINT_FLAGS,47,1,BI                                     DS/DC     00332   
ADSRC_REC_TYPE,50,1,BI                                        DS/DC     00333   
ADSRC_REC_COMM,1                                              EQU       00334   
ADSRC_REC_MACH,2                                              EQU       00335   
ADSRC_REC_ASM,3                                               EQU       00336   
ADSRC_REC_MACR,4                                              EQU       00337   
ADSRC_REC_MACD,5                                              EQU       00338   
ADSRC_ASM_OPCODE,51,1,BI                                      DS/DC     00339   
ADSRC_ASM_GBLA,0                                              EQU       00340   
ADSRC_ASM_GBLB,1                                              EQU       00341   
ADSRC_ASM_GBLC,2                                              EQU       00342   
ADSRC_ASM_LCLA,3                                              EQU       00343   
ADSRC_ASM_LCLB,4                                              EQU       00344   
ADSRC_ASM_LCLC,5                                              EQU       00345   
ADSRC_ASM_SETA,6                                              EQU       00346   
ADSRC_ASM_SETB,7                                              EQU       00347   
ADSRC_ASM_SETC,8                                              EQU       00348   
ADSRC_ASM_AIF,9                                               EQU       00349   
ADSRC_ASM_AGO,10                                              EQU       00350   
ADSRC_ASM_ANOP,11                                             EQU       00351   
ADSRC_ASM_COPY,12                                             EQU       00352   
ADSRC_ASM_MACRO,13                                            EQU       00353   
ADSRC_ASM_MNOTE,14                                            EQU       00354   
ADSRC_ASM_MEXIT,15                                            EQU       00355   
ADSRC_ASM_MEND,16                                             EQU       00356   
ADSRC_ASM_ICTL,17                                             EQU       00357   
ADSRC_ASM_ISEQ,18                                             EQU       00358   
ADSRC_ASM_PRINT,19                                            EQU       00359   
ADSRC_ASM_SPACE,20                                            EQU       00360   
ADSRC_ASM_EJECT,21                                            EQU       00361   
ADSRC_ASM_PUNCH,22                                            EQU       00362   
ADSRC_ASM_REPRO,23                                            EQU       00363   
ADSRC_ASM_TITLE,24                                            EQU       00364   
ADSRC_ASM_ENTRY,25                                            EQU       00365   
ADSRC_ASM_EXTRN,26                                            EQU       00366   
ADSRC_ASM_START,27                                            EQU       00367   
ADSRC_ASM_CSECT,28                                            EQU       00368   
ADSRC_ASM_DSECT,29                                            EQU       00369   
ADSRC_ASM_COM,30                                              EQU       00370   
ADSRC_ASM_EQU,31                                              EQU       00371   
ADSRC_ASM_ORG,32                                              EQU       00372   
ADSRC_ASM_END,33                                              EQU       00373   
ADSRC_ASM_LTORG,34                                            EQU       00374   
ADSRC_ASM_USING,35                                            EQU       00375   
ADSRC_ASM_DROP,36                                             EQU       00376   
ADSRC_ASM_ACTR,37                                             EQU       00377   
ADSRC_ASM_DC,38                                               EQU       00378   
ADSRC_ASM_DS,39                                               EQU       00379   
ADSRC_ASM_CCW,40                                              EQU       00380   
ADSRC_ASM_CNOP,41                                             EQU       00381   
ADSRC_ASM_LOCTR,42                                            EQU       00382   
ADSRC_ASM_DXD,43                                              EQU       00383   
ADSRC_ASM_CXD,44                                              EQU       00384   
ADSRC_ASM_OPSYN,46                                            EQU       00385   
ADSRC_ASM_PUSH,47                                             EQU       00386   
ADSRC_ASM_POP,48                                              EQU       00387   
ADSRC_ASM_LITR,51                                             EQU       00388   
ADSRC_ASM_MHELP,55                                            EQU       00389   
ADSRC_ASM_AREAD,56                                            EQU       00390   
ADSRC_ASM_WXTRN,59                                            EQU       00391   
ADSRC_ASM_AMODE,61                                            EQU       00392   
ADSRC_ASM_RMODE,62                                            EQU       00393   
ADSRC_ASM_RSECT,63                                            EQU       00394   
ADSRC_ASM_CCW0,64                                             EQU       00395   
ADSRC_ASM_CCW1,65                                             EQU       00396   
ADSRC_ASM_EXITCTL,66                                          EQU       00397   
ADSRC_ASM_ASPACE,67                                           EQU       00398   
ADSRC_ASM_AEJECT,68                                           EQU       00399   
ADSRC_ASM_ALIAS,69                                            EQU       00400   
ADSRC_ASM_CEJECT,70                                           EQU       00401   
ADSRC_ASM_ADATA,71                                            EQU       00402   
ADSRC_ASM_SETAF,72                                            EQU       00403   
ADSRC_ASM_SETCF,73                                            EQU       00404   
ADSRC_ASM_CATTR,74                                            EQU       00405   
ADSRC_ASM_ACONTROL,75                                         EQU       00406   
ADSRC_ASM_XATTR,76                                            EQU       00407   
ADSRC_ASM_AINSERT,77                                          EQU       00408   
ADSRC_ASM_HIGHEST,77                                          EQU       00409   
ADSRC_FLAGA1,51                                               EQU       00410   
ADSRC_FLAGA2,51                                               EQU       00411   
ADSRC_FLAGS,52,1,BI                                           DS/DC     00412   
ADSRC_ADDRESS1,57,4,BI                                        DS/DC     00413   
ADSRC_ADDRESS2,65,4,BI                                        DS/DC     00414   
ADSRC_NAME_OFF,69,4,FI                                        DS/DC     00415   
ADSRC_NAME_LEN,73,4,FI                                        DS/DC     00416   
ADSRC_OP_ENT_OFF,77,4,FI                                      DS/DC     00417   
ADSRC_OP_ENT_LEN,81,4,FI                                      DS/DC     00418   
ADSRC_OPND_ENT_OFF,85,4,FI                                    DS/DC     00419   
ADSRC_OPND_ENT_LEN,89,4,FI                                    DS/DC     00420   
ADSRC_REM_ENT_OFF,93,4,FI                                     DS/DC     00421   
ADSRC_REM_ENT_LEN,97,4,FI                                     DS/DC     00422   
ADSRC_CONT_IND_OFF,101,4,FI                                   DS/DC     00423   
ADSRC_MEMBER_OFF,109,4,FI                                     DS/DC     00424   
ADSRC_MEMBER_LEN,113,4,FI                                     DS/DC     00425   
ADSRC_PARENT_OFF,117,4,FI                                     DS/DC     00426   
ADSRC_PARENT_LEN,121,4,FI                                     DS/DC     00427   
ADSRC_RECORD_OFF,125,4,FI                                     DS/DC     00428   
ADSRC_RECORD_LEN,129,4,FI                                     DS/DC     00429   
ADSRC_EQU_LEN,140                                             EQU       00430   
ADSRC_MEMBER_NAME,141,256,CH                                  DS/DC     00431   
ADSRC_PARENT_NAME,141,256,CH                                  DS/DC     00432   
ADSRC_RECORD,141,80,CH                                        DS/DC     00433   
ADATA_RECSRCERR,50                                            EQU       00434   
ADATA_EDNSRCERR,1                                             EQU       00435   
ADSRCERR_STMT,17,4,FI                                         DS/DC     00436   
ADSRCERR_ID,21,16,CH                                          DS/DC     00437   
ADSRCERR_SEV,37,2,FI                                          DS/DC     00438   
ADSRCERR_FLAG_SUPR,38                                         EQU       00439   
ADSRCERR_FLAG1,39,1,BI                                        DS/DC     00440   
ADSRCERR_MSG_OFF,40,4,FI                                      DS/DC     00441   
ADSRCERR_MSG_LEN,44,4,FI                                      DS/DC     00442   
ADSRCERR_EQU_LEN,55                                           EQU       00443   
ADSRCERR_MSG,56,80,CH                                         DS/DC     00444   
ADATA_RECDCDS,52                                              EQU       00445   
ADATA_EDNDCDS,1                                               EQU       00446   
ADDCDS_ESDID,17,4,FI                                          DS/DC     00447   
ADDCDS_TYPE_DC,20                                             EQU       00448   
ADDCDS_TYPE_CXD,20                                            EQU       00449   
ADDCDS_TYPE_CCW,20                                            EQU       00450   
ADDCDS_TYPE_DXD,20                                            EQU       00451   
ADDCDS_TEXT_REC,20                                            EQU       00452   
ADDCDS_TYPE_FLAG,21,1,BI                                      DS/DC     00453   
ADDCDS_STMT,27,4,FI                                           DS/DC     00454   
ADDCDS_OPS_NUM,31,4,FI                                        DS/DC     00455   
ADDCDS_OPS_OFF,35,4,FI                                        DS/DC     00456   
ADDCDS_EQU_LEN,38                                             EQU       00457   
ADDCDS_NEXT_OP_OFF,1,4,FI                                     DS/DC     00458   
ADDCDS_LOCTR,5,4,FI                                           DS/DC     00459   
ADDCDS_DUP,9,4,FI                                             DS/DC     00460   
ADDCDS_BIT_OFFSET,13,1,BI                                     DS/DC     00461   
ADDCDS_TYPE_ATTR,14,1,BI                                      DS/DC     00462   
ADDCDS_TYPE_EXT,15,1,CH                                       DS/DC     00463   
ADDCDS_PROGRAM_TYPE,16,4,BI                                   DS/DC     00464   
ADDCDS_VALUES_NUM,24,4,FI                                     DS/DC     00465   
ADDCDS_VALUES_OFF,28,4,FI                                     DS/DC     00466   
ADDCDS_OPERAND_EQU_LEN,31                                     EQU       00467   
ADDCDS_NEXT_VAL_OFF,1,4,FI                                    DS/DC     00468   
ADDCDS_OBJECT_OFF,5,4,FI                                      DS/DC     00469   
ADDCDS_BYTE_LEN,9,4,FI                                        DS/DC     00470   
ADDCDS_BIT_LEN,13,4,FI                                        DS/DC     00471   
ADDCDS_VALUE_EQU_LEN,16                                       EQU       00472   
ADDCDS_OBJECT_VL,17                                           VL start  00473   
ADDCDS_OBJECT,17,1,BI                                         DS/DC     00474   
ADATA_RECDCX,53                                               EQU       00475   
ADATA_EDNDCX,1                                                EQU       00476   
ADDCX_ESDID,17,4,FI                                           DS/DC     00477   
ADDCX_STMT,21,4,FI                                            DS/DC     00478   
ADDCX_LOCTR,25,4,FI                                           DS/DC     00479   
ADDCX_OBJECT_OFF,37,4,FI                                      DS/DC     00480   
ADDCX_OBJECT_LEN,41,4,FI                                      DS/DC     00481   
ADDCX_EQU_LEN,44                                              EQU       00482   
ADDCX_OBJECT_VL,45                                            VL start  00483   
ADDCX_OBJECT,45,1,BI                                          DS/DC     00484   
ADATA_RECMACH,54                                              EQU       00485   
ADATA_EDNMACH,1                                               EQU       00486   
ADMACH_ESDID,17,4,FI                                          DS/DC     00487   
ADMACH_STMT,21,4,FI                                           DS/DC     00488   
ADMACH_LOCTR,25,4,FI                                          DS/DC     00489   
ADMACH_INST_OFF,37,4,FI                                       DS/DC     00490   
ADMACH_INST_LEN,41,4,FI                                       DS/DC     00491   
ADMACH_EQU_LEN,44                                             EQU       00492   
ADMACH_INST_VAL_VL,45                                         VL start  00493   
ADMACH_INST_VAL,45,1,BI                                       DS/DC     00494   
ADATA_RECRLD,64                                               EQU       00495   
ADATA_EDNRLD,1                                                EQU       00496   
ADRLD_POSID,17,4,FI                                           DS/DC     00497   
ADRLD_RELID,21,4,FI                                           DS/DC     00498   
ADRLD_ADDRESS,29,4,BI                                         DS/DC     00499   
ADRLD_FLAG_UNREF,32                                           EQU       00500   
ADRLD_FLAG_LEN4,32                                            EQU       00501   
ADRLD_FLAG_CXD,32                                             EQU       00502   
ADRLD_FLAG_Q,32                                               EQU       00503   
ADRLD_FLAG_V,32                                               EQU       00504   
ADRLD_FLAG_LENGTH,32                                          EQU       00505   
ADRLD_FLAG_SIGN,32                                            EQU       00506   
ADRLD_FLAG_SAME_RP,32                                         EQU       00507   
ADRLD_FLAGS,33,1,BI                                           DS/DC     00508   
ADRLD_EQU_LEN,33                                              EQU       00509   
ADATA_RECSYM,66                                               EQU       00510   
ADATA_EDNSYM,1                                                EQU       00511   
ADSYM_ESDID,17,4,FI                                           DS/DC     00512   
ADSYM_STMT,21,4,FI                                            DS/DC     00513   
ADSYM_LOCTR,25,4,FI                                           DS/DC     00514   
ADSYM_TYPE,29,1,BI                                            DS/DC     00515   
ADSYM_TYPE_UNDEF,0                                            EQU       00516   
ADSYM_TYPE_SECTION,1                                          EQU       00517   
ADSYM_TYPE_DSECT,2                                            EQU       00518   
ADSYM_TYPE_COMMON,3                                           EQU       00519   
ADSYM_TYPE_DXD,4                                              EQU       00520   
ADSYM_TYPE_VCON,5                                             EQU       00521   
ADSYM_TYPE_QUAL,6                                             EQU       00522   
ADSYM_TYPE_XTRN,7                                             EQU       00523   
ADSYM_TYPE_LOCTR,8                                            EQU       00524   
ADSYM_TYPE_DUP,9                                              EQU       00525   
ADSYM_TYPE_LITERAL,10                                         EQU       00526   
ADSYM_TYPE_ASTLIT,11                                          EQU       00527   
ADSYM_TYPE_EQU,12                                             EQU       00528   
ADSYM_TYPE_ORDINARY,13                                        EQU       00529   
ADSYM_TYPE_UNRES,14                                           EQU       00530   
ADSYM_DUP,30,4,FI                                             DS/DC     00531   
ADSYM_ATTR,34,1,BI                                            DS/DC     00532   
ADSYM_ASM_TYPE,35,4,CH                                        DS/DC     00533   
ADSYM_PROGRAM_TYPE,39,4,BI                                    DS/DC     00534   
ADSYM_BYTE_LEN,43,4,FI                                        DS/DC     00535   
ADSYM_INT,47,2,FI                                             DS/DC     00536   
ADSYM_SCALE,49,2,FI                                           DS/DC     00537   
ADSYM_RELOC,50                                                EQU       00538   
ADSYM_COMPLEX,50                                              EQU       00539   
ADSYM_RELOC_FLAG,51,1,BI                                      DS/DC     00540   
ADSYM_NAME_OFF,59,4,FI                                        DS/DC     00541   
ADSYM_NAME_LEN,63,4,FI                                        DS/DC     00542   
ADSYM_EQU_LEN,66                                              EQU       00543   
ADSYM_NAME,67,63,CH                                           DS/DC     00544   
ADATA_RECXREF,68                                              EQU       00545   
ADATA_EDNXREF,1                                               EQU       00546   
ADXREF_STMT,17,4,FI                                           DS/DC     00547   
ADXREF_REL_TYPE,21,1,CH                                       DS/DC     00548   
ADXREF_REL_SIMPLE,64                                          EQU       00549   
ADXREF_REL_ABSOLUTE,193                                       EQU       00550   
ADXREF_REL_COMPLEX,195                                        EQU       00551   
ADXREF_SYM_OFF,29,4,FI                                        DS/DC     00552   
ADXREF_SYM_LEN,33,4,FI                                        DS/DC     00553   
ADXREF_TOTAL_REFS,37,4,FI                                     DS/DC     00554   
ADXREF_REFS_NUM,41,4,FI                                       DS/DC     00555   
ADXREF_REFS_OFF,45,4,FI                                       DS/DC     00556   
ADXREF_EQU_LEN,48                                             EQU       00557   
ADXREF_SYM_NAME,49,63,CH                                      DS/DC     00558   
ADXREF_REF_STMT,1,4,FI                                        DS/DC     00559   
ADXREF_REF_FLAG,5,1,CH                                        DS/DC     00560   
ADXREF_REF_NOBRMOD,64                                         EQU       00561   
ADXREF_REF_MODIFY,212                                         EQU       00562   
ADXREF_REF_BRANCH,194                                         EQU       00563   
ADXREF_REF_USING,228                                          EQU       00564   
ADXREF_REF_DROP,196                                           EQU       00565   
ADXREF_REF_EXECUTE,231                                        EQU       00566   
ADXREF_REF_EQU_LEN,5                                          EQU       00567   
ADATA_RECRXREF,69                                             EQU       00568   
ADATA_EDNRXREF,1                                              EQU       00569   
ADRXREF_REG_NUMBER,17,1,BI                                    DS/DC     00570   
ADRXREF_REG_TYPE,18,1,CH                                      DS/DC     00571   
ADRXREF_TOTAL_REFS,21,4,FI                                    DS/DC     00572   
ADRXREF_REFS_NUM,25,4,FI                                      DS/DC     00573   
ADRXREF_REFS_OFF,29,4,FI                                      DS/DC     00574   
ADRXREF_EQU_LEN,32                                            EQU       00575   
ADRXREF_REF_STMT,1,4,FI                                       DS/DC     00576   
ADRXREF_REF_FLAG,5,1,CH                                       DS/DC     00577   
ADRXREF_REF_NOBRMOD,64                                        EQU       00578   
ADRXREF_REF_MODIFY,212                                        EQU       00579   
ADRXREF_REF_BRANCH,194                                        EQU       00580   
ADRXREF_REF_INDEX,213                                         EQU       00581   
ADRXREF_REF_USING,228                                         EQU       00582   
ADRXREF_REF_DROP,196                                          EQU       00583   
ADRXREF_REF_EQU_LEN,5                                         EQU       00584   
ADATA_RECMXREF,96                                             EQU       00585   
ADATA_EDNMXREF,1                                              EQU       00586   
ADMXREF_CONCAT,17,4,FI                                        DS/DC     00587   
ADMXREF_DAT_OFF,21,4,FI                                       DS/DC     00588   
ADMXREF_DAT_LEN,25,4,FI                                       DS/DC     00589   
ADMXREF_VOL_OFF,29,4,FI                                       DS/DC     00590   
ADMXREF_VOL_LEN,33,4,FI                                       DS/DC     00591   
ADMXREF_DD_OFF,37,4,FI                                        DS/DC     00592   
ADMXREF_DD_LEN,41,4,FI                                        DS/DC     00593   
ADMXREF_MACROS_NUM,45,4,FI                                    DS/DC     00594   
ADMXREF_MACROS_OFF,49,4,FI                                    DS/DC     00595   
ADMXREF_EQU_LEN,52                                            EQU       00596   
ADMXREF_DATASET,53,256,CH                                     DS/DC     00597   
ADMXREF_VOL,53,256,CH                                         DS/DC     00598   
ADMXREF_DDNAME,53,256,CH                                      DS/DC     00599   
ADMXREF_MACRO_NEXT_OFF,1,4,FI                                 DS/DC     00600   
ADMXREF_MACRO_NAME_OFF,5,4,FI                                 DS/DC     00601   
ADMXREF_MACRO_NAME_LEN,9,4,FI                                 DS/DC     00602   
ADMXREF_MACRO_EQU_LEN,12                                      EQU       00603   
ADMXREF_MACRO_NAME,13,256,CH                                  DS/DC     00604   
ADATA_RECMXREFX,98                                            EQU       00605   
ADATA_EDNMXREFX,1                                             EQU       00606   
ADMXRFX_CNUM,17,4,FI                                          DS/DC     00607   
ADMXRFX_DEFN,21,4,FI                                          DS/DC     00608   
ADMXRFX_CTYPE,25,1,CH                                         DS/DC     00609   
ADMXRFX_CTYPE_L,211                                           EQU       00610   
ADMXRFX_CTYPE_P,215                                           EQU       00611   
ADMXRFX_DEFN_FLAG,26,1,CH                                     DS/DC     00612   
ADMXRFX_DEFN_NORMAL,64                                        EQU       00613   
ADMXRFX_DEFN_IMBED,231                                        EQU       00614   
ADMXRFX_MEM_OFF,35,4,FI                                       DS/DC     00615   
ADMXRFX_MEM_LEN,39,4,FI                                       DS/DC     00616   
ADMXRFX_CALL_OFF,43,4,FI                                      DS/DC     00617   
ADMXRFX_CALL_LEN,47,4,FI                                      DS/DC     00618   
ADMXRFX_TOTAL_REFS,51,4,FI                                    DS/DC     00619   
ADMXRFX_REFS_NUM,55,4,FI                                      DS/DC     00620   
ADMXRFX_REFS_OFF,59,4,FI                                      DS/DC     00621   
ADMXRFX_EQU_LEN,62                                            EQU       00622   
ADMXRFX_MEM_NAME,63,64,CH                                     DS/DC     00623   
ADMXRFX_CALL_NAME,63,64,CH                                    DS/DC     00624   
ADMXRFX_REF_STMT,1,4,FI                                       DS/DC     00625   
ADMXRFX_REF_FLAG,5,1,CH                                       DS/DC     00626   
ADMXRFX_REF_MACRO,64                                          EQU       00627   
ADMXRFX_REF_COPY,195                                          EQU       00628   
ADMXRFX_REF_EQU_LEN,5                                         EQU       00629   
ADATA_RECUSER,112                                             EQU       00630   
ADATA_EDNUSER,1                                               EQU       00631   
ADUSER_FIELD1,17,4,BI                                         DS/DC     00632   
ADUSER_FIELD2,21,4,BI                                         DS/DC     00633   
ADUSER_FIELD3,25,4,BI                                         DS/DC     00634   
ADUSER_FIELD4,29,4,BI                                         DS/DC     00635   
ADUSER_DATA_OFF,33,4,FI                                       DS/DC     00636   
ADUSER_DATA_LEN,37,4,FI                                       DS/DC     00637   
ADUSER_EQU_LEN,40                                             EQU       00638   
ADUSER_DATA_VL,41                                             VL start  00639   
ADUSER_DATA,41,1,CH                                           DS/DC     00640**3
ADATA_RECUSING,128                                            EQU       00641   
ADATA_EDNUSING,1                                              EQU       00642   
ADUSING_TYPE_DROP,16                                          EQU       00643   
ADUSING_TYPE_PUSH,16                                          EQU       00644   
ADUSING_TYPE_POP,16                                           EQU       00645   
ADUSING_TYPE_USING,16                                         EQU       00646   
ADUSING_TYPE,17,1,BI                                          DS/DC     00647   
ADUSING_FLAG_LABDEP,17                                        EQU       00648   
ADUSING_FLAG_DEP,17                                           EQU       00649   
ADUSING_FLAG_LAB,17                                           EQU       00650   
ADUSING_FLAG_ORD,17                                           EQU       00651   
ADUSING_FLAG,18,1,BI                                          DS/DC     00652   
ADUSING_LOCTR_ESDID,19,4,FI                                   DS/DC     00653   
ADUSING_STMT,23,4,FI                                          DS/DC     00654   
ADUSING_LOCTR,27,4,FI                                         DS/DC     00655   
ADUSING_VALUE,31,4,FI                                         DS/DC     00656   
ADUSING_LAST_STMT,35,4,FI                                     DS/DC     00657   
ADUSING_ID,39,4,FI                                            DS/DC     00658   
ADUSING_REG,43,1,BI                                           DS/DC     00659   
ADUSING_DISP,44,2,BI                                          DS/DC     00660   
ADUSING_RANGE,47,4,FI                                         DS/DC     00661   
ADUSING_LABEL_OFF,53,4,FI                                     DS/DC     00662   
ADUSING_LABEL_LEN,57,4,FI                                     DS/DC     00663   
ADUSING_EQU_LEN,60                                            EQU       00664   
ADUSING_LABEL,61,63,CH                                        DS/DC     00665   
ADATA_RECSTATS,144                                            EQU       00666   
ADATA_EDNSTATS,2                                              EQU       00667   
ADSTATS_BUFFP,17,4,FI                                         DS/DC     00668   
ADSTATS_REQ_INSTOR,21,4,FI                                    DS/DC     00669   
ADSTATS_P_INPUT,25,4,FI                                       DS/DC     00670   
ADSTATS_LIB_INPUT,29,4,FI                                     DS/DC     00671   
ADSTATS_WORK_FILE,33,4,FI                                     DS/DC     00672   
ADSTATS_P_PRINT,37,4,FI                                       DS/DC     00673   
ADSTATS_P_PUNCH,41,4,FI                                       DS/DC     00674   
ADSTATS_WORK_WRITE,45,4,FI                                    DS/DC     00675   
ADSTATS_ADATA,49,4,FI                                         DS/DC     00676   
ADSTATS_ADATA_CALLS,53,4,FI                                   DS/DC     00677   
ADSTATS_ADATA_ADDED,57,4,FI                                   DS/DC     00678   
ADSTATS_ADATA_DELETE,61,4,FI                                  DS/DC     00679   
ADSTATS_ADATA_DIAG,65,4,FI                                    DS/DC     00680   
ADSTATS_LIB_CALLS,69,4,FI                                     DS/DC     00681   
ADSTATS_LIB_ADDED,73,4,FI                                     DS/DC     00682   
ADSTATS_LIB_DELETE,77,4,FI                                    DS/DC     00683   
ADSTATS_LIB_DIAG,81,4,FI                                      DS/DC     00684   
ADSTATS_LIST_CALLS,85,4,FI                                    DS/DC     00685   
ADSTATS_LIST_ADDED,89,4,FI                                    DS/DC     00686   
ADSTATS_LIST_DELETE,93,4,FI                                   DS/DC     00687   
ADSTATS_LIST_DIAG,97,4,FI                                     DS/DC     00688   
ADSTATS_OBJ_CALLS,101,4,FI                                    DS/DC     00689   
ADSTATS_OBJ_ADDED,105,4,FI                                    DS/DC     00690   
ADSTATS_OBJ_DELETE,109,4,FI                                   DS/DC     00691   
ADSTATS_OBJ_DIAG,113,4,FI                                     DS/DC     00692   
ADSTATS_INPUT_CALLS,117,4,FI                                  DS/DC     00693   
ADSTATS_INPUT_ADDED,121,4,FI                                  DS/DC     00694   
ADSTATS_INPUT_DELETE,125,4,FI                                 DS/DC     00695   
ADSTATS_INPUT_DIAG,129,4,FI                                   DS/DC     00696   
ADSTATS_PUNCH_CALLS,133,4,FI                                  DS/DC     00697   
ADSTATS_PUNCH_ADDED,137,4,FI                                  DS/DC     00698   
ADSTATS_PUNCH_DELETE,141,4,FI                                 DS/DC     00699   
ADSTATS_PUNCH_DIAG,145,4,FI                                   DS/DC     00700   
ADSTATS_TERM_CALLS,149,4,FI                                   DS/DC     00701   
ADSTATS_TERM_ADDED,153,4,FI                                   DS/DC     00702   
ADSTATS_TERM_DELETE,157,4,FI                                  DS/DC     00703   
ADSTATS_TERM_DIAG,161,4,FI                                    DS/DC     00704   
ADSTATS_START_TIME,165,4,FI                                   DS/DC     00705   
ADSTATS_STOP_TIME,169,4,FI                                    DS/DC     00706   
ADSTATS_PROC_TIME,173,4,FI                                    DS/DC     00707   
ADSTATS_ASMAOPT_READ,177,4,FI                                 DS/DC     00708   
ADSTATS_XFUNCS_NUM,185,4,FI                                   DS/DC     00709   
ADSTATS_XFUNCS_OFF,189,4,FI                                   DS/DC     00710   
ADSTATS_SUPR_NUM,193,4,FI                                     DS/DC     00711   
ADSTATS_SUPR_OFF,197,4,FI                                     DS/DC     00712   
ADSTATS_EQU_LEN,200                                           EQU       00713   
ADSTATS_XFUNC_NEXT_OFF,1,4,FI                                 DS/DC     00714   
ADSTATS_XFUNC_SETAF,9,4,FI                                    DS/DC     00715   
ADSTATS_XFUNC_SETCF,13,4,FI                                   DS/DC     00716   
ADSTATS_XFUNC_MSG,17,4,FI                                     DS/DC     00717   
ADSTATS_XFUNC_MSEV,21,2,FI                                    DS/DC     00718   
ADSTATS_XFUNC_NAME_OFF,23,4,FI                                DS/DC     00719   
ADSTATS_XFUNC_NAME_LEN,27,4,FI                                DS/DC     00720   
ADSTATS_XFUNC_EQU_LEN,30                                      EQU       00721   
ADSTATS_XFUNC_NAME,31,256,CH                                  DS/DC     00722   
ADSTATS_SUPR_MSGNO,1,2,FI                                     DS/DC     00723   
ADSTATS_SUPR_COUNT,3,4,FI                                     DS/DC     00724   
ADSTATS_SUPR_EQU_LEN,6                                        EQU       00725   
ADFBLOCK_NEXT_OFF,1,4,FI                                      DS/DC     00726   
ADFBLOCK_FILENUMBER,5,4,FI                                    DS/DC     00727   
ADFBLOCK_FILE_OFF,9,4,FI                                      DS/DC     00728   
ADFBLOCK_FILE_LEN,13,4,FI                                     DS/DC     00729   
ADFBLOCK_VOL_OFF,17,4,FI                                      DS/DC     00730   
ADFBLOCK_VOL_LEN,21,4,FI                                      DS/DC     00731   
ADFBLOCK_MEM_OFF,25,4,FI                                      DS/DC     00732   
ADFBLOCK_MEM_LEN,29,4,FI                                      DS/DC     00733   
ADFBLOCK_EQU_LEN,32                                           EQU       00734   
ADFBLOCK_FILENAME,33,256,CH                                   DS/DC     00735   
ADFBLOCK_VOLNAME,33,256,CH                                    DS/DC     00736   
ADFBLOCK_MEMNAME,33,256,CH                                    DS/DC     00737   
ASMADATA_RDW,1,4,BI                                           DS/DC     00738**4
ASMADATA_COMMON_HEADER,5,12,BI                                DS/DC     00739**4
ADDCDS_NEXT_OP_OFF_FIRST,39,4,FI                              DS/DC     00740   
ADDCDS_LOCTR_FIRST,43,4,FI                                    DS/DC     00741   
ADDCDS_DUP_FIRST,47,4,FI                                      DS/DC     00742   
ADDCDS_BIT_OFFSET_FIRST,51,1,BI                               DS/DC     00743   
ADDCDS_TYPE_ATTR_FIRST,52,1,CH                                DS/DC     00744   
ADDCDS_TYPE_EXT_FIRST,53,1,CH                                 DS/DC     00745   
ADDCDS_PROGRAM_TYPE_FIRST,54,4,BI                             DS/DC     00746   
ADDCDS_VALUES_NUM_FIRST,62,4,FI                               DS/DC     00747   
ADDCDS_VALUES_OFF_FIRST,66,4,FI                               DS/DC     00748   
ADDCDS_NEXT_VAL_OFF_FIRST,70,4,FI                             DS/DC     00749   
ADDCDS_OBJECT_OFF_FIRST,74,4,FI                               DS/DC     00750   
ADDCDS_BYTE_LEN_FIRST,78,4,FI                                 DS/DC     00751   
ADDCDS_BIT_LEN_FIRST,82,4,FI                                  DS/DC     00752   
ADDCDS_OBJECT_FIRST,86,1,BI                                   DS/DC     00753   
ADMACH_INST_VAL_BYTE1,45,1,BI                                 DS/DC     00754   
ADMACH_INST_VAL_BYTE2,46,1,BI                                 DS/DC     00755   
ADMACH_INST_VAL_BYTES3_4,47,2,BI                              DS/DC     00756   
ADMACH_INST_VAL_BYTE3,47,1,BI                                 DS/DC     00757   
ADMACH_INST_VAL_BYTE4,48,1,BI                                 DS/DC     00758   
ADMACH_INST_VAL_BYTE5,49,1,BI                                 DS/DC     00759   
ADMACH_INST_VAL_BYTE6,50,1,BI                                 DS/DC     00760   
ADSRC_MEMBER_NAME_CL8,141,8,CH                                DS/DC     00761**2
ADSRC_RECORD_START_VL,141                                     VL start  00762**3
ADSRC_RECORD_START,141,1,CH                                   DS/DC     00763**3
ADSRC_RECORD_STMT,141,72,CH                                   DS/DC     00764**3
ADSRC_RECORD_STMT_WITHOUT_CONT,141,71,CH                      DS/DC     00765**3
ADSRC_RECORD_STMT_CONT_COLUMN,212,1,CH                        DS/DC     00766**3
ADSRC_RECORD_STMT_SEQUENCE_FIELD,213,8,CH                     DS/DC     00767**3
ADSRC_RECORD_STMT_PAN_STMT_#,213,5,CH                         DS/DC     00768**3
ADSRC_RECORD_STMT_PAN_LEVEL_#,218,3,CH                        DS/DC     00769**3
ADSRC_RECORD_PAN_SPECIAL_COMMENT_DATA_SET,141,20,CH           DS/DC     00770**4
ADSRC_RECORD_PAN_MEMBER_NAME,161,10,CH                        DS/DC     00771**4
ADSRC_RECORD_PAN_SPECIAL_COMMENT_AT_LEVEL,171,10,CH           DS/DC     00772**4
ADSRC_RECORD_PAN_LEVEL_NUMBER,181,3,CH                        DS/DC     00773**4
ADSRC_RECORD_PAN_SPECIAL_COMMENT_AS_OF,184,7,CH               DS/DC     00774**4
ADSRC_RECORD_PAN_UPDATE_DATE,191,8,CH                         DS/DC     00775**4
ADSRC_RECORD_PAN_SPECIAL_COMMENT_BLANKS,199,22,CH             DS/DC     00776**4
ADSRC_RECORD_WITHOUT_SEQ_FLD,5,208,CH                         DS/DC     00777**3
LEN_ADATA_LEN,2                                               abs. len. 00778   
LEN_ADATA_VERSION,1                                           abs. len. 00779   
LEN_ADATA_RECTYPE,2                                           abs. len. 00780   
LEN_ADATA_LEVEL,1                                             abs. len. 00781   
LEN_ADATA_CONT,1                                              abs. len. 00782   
LEN_ADATA_EDITION,1                                           abs. len. 00783   
LEN_ADATA_DATA_LEN,2                                          abs. len. 00784   
LEN_ADATA_ORG,2                                               abs. len. 00785   
LEN_ADJID_DATE,8                                              abs. len. 00786   
LEN_ADJID_TIME,4                                              abs. len. 00787   
LEN_ADJID_PROD_NUMBER,8                                       abs. len. 00788   
LEN_ADJID_PROD_VERSION,8                                      abs. len. 00789   
LEN_ADJID_PROD_LEVEL,2                                        abs. len. 00790   
LEN_ADJID_PTF,8                                               abs. len. 00791   
LEN_ADJID_SYSTEM_ID,24                                        abs. len. 00792   
LEN_ADJID_JOBNAME,8                                           abs. len. 00793   
LEN_ADJID_STEPNAME,8                                          abs. len. 00794   
LEN_ADJID_PROCSTEP,8                                          abs. len. 00795   
LEN_ADJID_FILES_NUM,4                                         abs. len. 00796   
LEN_ADJID_FILES_OFF,4                                         abs. len. 00797   
LEN_ADAID_TIME,8                                              abs. len. 00798   
LEN_ADAID_CCSID,2                                             abs. len. 00799   
LEN_ADCOMP_IND,2                                              abs. len. 00800   
LEN_ADCOMP_RCOUNT,4                                           abs. len. 00801   
LEN_ADOUT_OBJECT_NUM,4                                        abs. len. 00802   
LEN_ADOUT_OBJECT_OFF,4                                        abs. len. 00803   
LEN_ADOUT_PUNCH_NUM,4                                         abs. len. 00804   
LEN_ADOUT_PUNCH_OFF,4                                         abs. len. 00805   
LEN_ADOUT_PRINT_NUM,4                                         abs. len. 00806   
LEN_ADOUT_PRINT_OFF,4                                         abs. len. 00807   
LEN_ADOUT_TERM_NUM,4                                          abs. len. 00808   
LEN_ADOUT_TERM_OFF,4                                          abs. len. 00809   
LEN_ADOUT_ADATA_NUM,4                                         abs. len. 00810   
LEN_ADOUT_ADATA_OFF,4                                         abs. len. 00811   
LEN_ADAOPT_FILES_NUM,4                                        abs. len. 00812   
LEN_ADAOPT_FILES_OFF,4                                        abs. len. 00813   
LEN_ADOPT_BYTE1,1                                             abs. len. 00814   
LEN_ADOPT_BYTE2,1                                             abs. len. 00815   
LEN_ADOPT_BYTE3,1                                             abs. len. 00816   
LEN_ADOPT_BYTE4,1                                             abs. len. 00817   
LEN_ADOPT_BYTE5,1                                             abs. len. 00818   
LEN_ADOPT_BYTE6,1                                             abs. len. 00819   
LEN_ADOPT_BYTE7,1                                             abs. len. 00820   
LEN_ADOPT_BYTE8,1                                             abs. len. 00821   
LEN_ADOPT_BYTE9,1                                             abs. len. 00822   
LEN_ADOPT_BYTE10,1                                            abs. len. 00823   
LEN_ADOPT_BYTE11,1                                            abs. len. 00824   
LEN_ADOPT_BYTE12,1                                            abs. len. 00825   
LEN_ADOPT_BYTE13,1                                            abs. len. 00826   
LEN_ADOPT_BYTE14,1                                            abs. len. 00827   
LEN_ADOPT_EXTRA_BYTE1,1                                       abs. len. 00828   
LEN_ADOPT_CODEPAGE_VAL,4                                      abs. len. 00829   
LEN_ADOPT_FLAG_VAL,1                                          abs. len. 00830   
LEN_ADOPT_LANGUAGE_VAL,3                                      abs. len. 00831   
LEN_ADOPT_LINECOUNT_VAL,2                                     abs. len. 00832   
LEN_ADOPT_OPTABLE_VAL,3                                       abs. len. 00833   
LEN_ADOPT_PROFILE_VAL,8                                       abs. len. 00834   
LEN_ADOPT_SECTALGN_VAL,4                                      abs. len. 00835   
LEN_ADOPT_TRANSLATE_VAL,2                                     abs. len. 00836   
LEN_ADOPT_USING_LIMIT_VAL,2                                   abs. len. 00837   
LEN_ADOPT_USING_WARN_VAL,1                                    abs. len. 00838   
LEN_ADOPT_FAILMSG_VAL,1                                       abs. len. 00839   
LEN_ADOPT_FAILMNOTE_VAL,1                                     abs. len. 00840   
LEN_ADOPT_FAILMAXERRS_VAL,2                                   abs. len. 00841   
LEN_ADOPT_PARM_STR_OFF,4                                      abs. len. 00842   
LEN_ADOPT_PARM_STR_LEN,4                                      abs. len. 00843   
LEN_ADOPT_SYSPARM_STR_OFF,4                                   abs. len. 00844   
LEN_ADOPT_SYSPARM_STR_LEN,4                                   abs. len. 00845   
LEN_ADOPT_INEXIT_PROG_OFF,4                                   abs. len. 00846   
LEN_ADOPT_INEXIT_PROG_LEN,4                                   abs. len. 00847   
LEN_ADOPT_INEXIT_STR_OFF,4                                    abs. len. 00848   
LEN_ADOPT_INEXIT_STR_LEN,4                                    abs. len. 00849   
LEN_ADOPT_LIBEXIT_PROG_OFF,4                                  abs. len. 00850   
LEN_ADOPT_LIBEXIT_PROG_LEN,4                                  abs. len. 00851   
LEN_ADOPT_LIBEXIT_STR_OFF,4                                   abs. len. 00852   
LEN_ADOPT_LIBEXIT_STR_LEN,4                                   abs. len. 00853   
LEN_ADOPT_PRTEXIT_PROG_OFF,4                                  abs. len. 00854   
LEN_ADOPT_PRTEXIT_PROG_LEN,4                                  abs. len. 00855   
LEN_ADOPT_PRTEXIT_STR_OFF,4                                   abs. len. 00856   
LEN_ADOPT_PRTEXIT_STR_LEN,4                                   abs. len. 00857   
LEN_ADOPT_OBJEXIT_PROG_OFF,4                                  abs. len. 00858   
LEN_ADOPT_OBJEXIT_PROG_LEN,4                                  abs. len. 00859   
LEN_ADOPT_OBJEXIT_STR_OFF,4                                   abs. len. 00860   
LEN_ADOPT_OBJEXIT_STR_LEN,4                                   abs. len. 00861   
LEN_ADOPT_ADEXIT_PROG_OFF,4                                   abs. len. 00862   
LEN_ADOPT_ADEXIT_PROG_LEN,4                                   abs. len. 00863   
LEN_ADOPT_ADEXIT_STR_OFF,4                                    abs. len. 00864   
LEN_ADOPT_ADEXIT_STR_LEN,4                                    abs. len. 00865   
LEN_ADOPT_TRMEXIT_PROG_OFF,4                                  abs. len. 00866   
LEN_ADOPT_TRMEXIT_PROG_LEN,4                                  abs. len. 00867   
LEN_ADOPT_TRMEXIT_STR_OFF,4                                   abs. len. 00868   
LEN_ADOPT_TRMEXIT_STR_LEN,4                                   abs. len. 00869   
LEN_ADOPT_PARM_STR,255                                        abs. len. 00870   
LEN_ADOPT_SYSPARM_STR,255                                     abs. len. 00871   
LEN_ADOPT_INEXIT_PROG,8                                       abs. len. 00872   
LEN_ADOPT_INEXIT_STR,64                                       abs. len. 00873   
LEN_ADOPT_LIBEXIT_PROG,8                                      abs. len. 00874   
LEN_ADOPT_LIBEXIT_STR,64                                      abs. len. 00875   
LEN_ADOPT_PRTEXIT_PROG,8                                      abs. len. 00876   
LEN_ADOPT_PRTEXIT_STR,64                                      abs. len. 00877   
LEN_ADOPT_OBJEXIT_PROG,8                                      abs. len. 00878   
LEN_ADOPT_OBJEXIT_STR,64                                      abs. len. 00879   
LEN_ADOPT_ADEXIT_PROG,8                                       abs. len. 00880   
LEN_ADOPT_ADEXIT_STR,64                                       abs. len. 00881   
LEN_ADOPT_TRMEXIT_PROG,8                                      abs. len. 00882   
LEN_ADOPT_TRMEXIT_STR,64                                      abs. len. 00883   
LEN_ADESD_TYPE,1                                              abs. len. 00884   
LEN_ADESD_FLAGS,1                                             abs. len. 00885   
LEN_ADESD_ESDID,4                                             abs. len. 00886   
LEN_ADESD_ADDR,4                                              abs. len. 00887   
LEN_ADESD_LEN,4                                               abs. len. 00888   
LEN_ADESD_OWNERID,4                                           abs. len. 00889   
LEN_ADESD_NAME_OFF,4                                          abs. len. 00890   
LEN_ADESD_NAME_LEN,4                                          abs. len. 00891   
LEN_ADESD_ALIAS_OFF,4                                         abs. len. 00892   
LEN_ADESD_ALIAS_LEN,4                                         abs. len. 00893   
LEN_ADESD_NAME,63                                             abs. len. 00894   
LEN_ADESD_ALIAS,63                                            abs. len. 00895   
LEN_ADSRC_ESDID,4                                             abs. len. 00896   
LEN_ADSRC_STMT,4                                              abs. len. 00897   
LEN_ADSRC_INPUT_REC,4                                         abs. len. 00898   
LEN_ADSRC_PARENT_REC,4                                        abs. len. 00899   
LEN_ADSRC_INPUT_NUM,4                                         abs. len. 00900   
LEN_ADSRC_PARENT_NUM,4                                        abs. len. 00901   
LEN_ADSRC_LOCTR,4                                             abs. len. 00902   
LEN_ADSRC_REC_ORIGIN,1                                        abs. len. 00903   
LEN_ADSRC_PARENT_ORIGIN,1                                     abs. len. 00904   
LEN_ADSRC_PRINT_FLAGS,1                                       abs. len. 00905   
LEN_ADSRC_REC_TYPE,1                                          abs. len. 00906   
LEN_ADSRC_ASM_OPCODE,1                                        abs. len. 00907   
LEN_ADSRC_FLAGS,1                                             abs. len. 00908   
LEN_ADSRC_ADDRESS1,4                                          abs. len. 00909   
LEN_ADSRC_ADDRESS2,4                                          abs. len. 00910   
LEN_ADSRC_NAME_OFF,4                                          abs. len. 00911   
LEN_ADSRC_NAME_LEN,4                                          abs. len. 00912   
LEN_ADSRC_OP_ENT_OFF,4                                        abs. len. 00913   
LEN_ADSRC_OP_ENT_LEN,4                                        abs. len. 00914   
LEN_ADSRC_OPND_ENT_OFF,4                                      abs. len. 00915   
LEN_ADSRC_OPND_ENT_LEN,4                                      abs. len. 00916   
LEN_ADSRC_REM_ENT_OFF,4                                       abs. len. 00917   
LEN_ADSRC_REM_ENT_LEN,4                                       abs. len. 00918   
LEN_ADSRC_CONT_IND_OFF,4                                      abs. len. 00919   
LEN_ADSRC_MEMBER_OFF,4                                        abs. len. 00920   
LEN_ADSRC_MEMBER_LEN,4                                        abs. len. 00921   
LEN_ADSRC_PARENT_OFF,4                                        abs. len. 00922   
LEN_ADSRC_PARENT_LEN,4                                        abs. len. 00923   
LEN_ADSRC_RECORD_OFF,4                                        abs. len. 00924   
LEN_ADSRC_RECORD_LEN,4                                        abs. len. 00925   
LEN_ADSRC_MEMBER_NAME,256                                     abs. len. 00926   
LEN_ADSRC_PARENT_NAME,256                                     abs. len. 00927   
LEN_ADSRC_RECORD,80                                           abs. len. 00928   
LEN_ADSRCERR_STMT,4                                           abs. len. 00929   
LEN_ADSRCERR_ID,16                                            abs. len. 00930   
LEN_ADSRCERR_SEV,2                                            abs. len. 00931   
LEN_ADSRCERR_FLAG1,1                                          abs. len. 00932   
LEN_ADSRCERR_MSG_OFF,4                                        abs. len. 00933   
LEN_ADSRCERR_MSG_LEN,4                                        abs. len. 00934   
LEN_ADSRCERR_MSG,80                                           abs. len. 00935   
LEN_ADDCDS_ESDID,4                                            abs. len. 00936   
LEN_ADDCDS_TYPE_FLAG,1                                        abs. len. 00937   
LEN_ADDCDS_STMT,4                                             abs. len. 00938   
LEN_ADDCDS_OPS_NUM,4                                          abs. len. 00939   
LEN_ADDCDS_OPS_OFF,4                                          abs. len. 00940   
LEN_ADDCDS_NEXT_OP_OFF,4                                      abs. len. 00941   
LEN_ADDCDS_LOCTR,4                                            abs. len. 00942   
LEN_ADDCDS_DUP,4                                              abs. len. 00943   
LEN_ADDCDS_BIT_OFFSET,1                                       abs. len. 00944   
LEN_ADDCDS_TYPE_ATTR,1                                        abs. len. 00945   
LEN_ADDCDS_TYPE_EXT,1                                         abs. len. 00946   
LEN_ADDCDS_PROGRAM_TYPE,4                                     abs. len. 00947   
LEN_ADDCDS_VALUES_NUM,4                                       abs. len. 00948   
LEN_ADDCDS_VALUES_OFF,4                                       abs. len. 00949   
LEN_ADDCDS_NEXT_VAL_OFF,4                                     abs. len. 00950   
LEN_ADDCDS_OBJECT_OFF,4                                       abs. len. 00951   
LEN_ADDCDS_BYTE_LEN,4                                         abs. len. 00952   
LEN_ADDCDS_BIT_LEN,4                                          abs. len. 00953   
LEN_ADDCX_ESDID,4                                             abs. len. 00954   
LEN_ADDCX_STMT,4                                              abs. len. 00955   
LEN_ADDCX_LOCTR,4                                             abs. len. 00956   
LEN_ADDCX_OBJECT_OFF,4                                        abs. len. 00957   
LEN_ADDCX_OBJECT_LEN,4                                        abs. len. 00958   
LEN_ADMACH_ESDID,4                                            abs. len. 00959   
LEN_ADMACH_STMT,4                                             abs. len. 00960   
LEN_ADMACH_LOCTR,4                                            abs. len. 00961   
LEN_ADMACH_INST_OFF,4                                         abs. len. 00962   
LEN_ADMACH_INST_LEN,4                                         abs. len. 00963   
LEN_ADRLD_POSID,4                                             abs. len. 00964   
LEN_ADRLD_RELID,4                                             abs. len. 00965   
LEN_ADRLD_ADDRESS,4                                           abs. len. 00966   
LEN_ADRLD_FLAGS,1                                             abs. len. 00967   
LEN_ADSYM_ESDID,4                                             abs. len. 00968   
LEN_ADSYM_STMT,4                                              abs. len. 00969   
LEN_ADSYM_LOCTR,4                                             abs. len. 00970   
LEN_ADSYM_TYPE,1                                              abs. len. 00971   
LEN_ADSYM_DUP,4                                               abs. len. 00972   
LEN_ADSYM_ATTR,1                                              abs. len. 00973   
LEN_ADSYM_ASM_TYPE,4                                          abs. len. 00974   
LEN_ADSYM_PROGRAM_TYPE,4                                      abs. len. 00975   
LEN_ADSYM_BYTE_LEN,4                                          abs. len. 00976   
LEN_ADSYM_INT,2                                               abs. len. 00977   
LEN_ADSYM_SCALE,2                                             abs. len. 00978   
LEN_ADSYM_RELOC_FLAG,1                                        abs. len. 00979   
LEN_ADSYM_NAME_OFF,4                                          abs. len. 00980   
LEN_ADSYM_NAME_LEN,4                                          abs. len. 00981   
LEN_ADSYM_NAME,63                                             abs. len. 00982   
LEN_ADXREF_STMT,4                                             abs. len. 00983   
LEN_ADXREF_REL_TYPE,1                                         abs. len. 00984   
LEN_ADXREF_SYM_OFF,4                                          abs. len. 00985   
LEN_ADXREF_SYM_LEN,4                                          abs. len. 00986   
LEN_ADXREF_TOTAL_REFS,4                                       abs. len. 00987   
LEN_ADXREF_REFS_NUM,4                                         abs. len. 00988   
LEN_ADXREF_REFS_OFF,4                                         abs. len. 00989   
LEN_ADXREF_SYM_NAME,63                                        abs. len. 00990   
LEN_ADXREF_REF_STMT,4                                         abs. len. 00991   
LEN_ADXREF_REF_FLAG,1                                         abs. len. 00992   
LEN_ADRXREF_REG_NUMBER,1                                      abs. len. 00993   
LEN_ADRXREF_REG_TYPE,1                                        abs. len. 00994   
LEN_ADRXREF_TOTAL_REFS,4                                      abs. len. 00995   
LEN_ADRXREF_REFS_NUM,4                                        abs. len. 00996   
LEN_ADRXREF_REFS_OFF,4                                        abs. len. 00997   
LEN_ADRXREF_REF_STMT,4                                        abs. len. 00998   
LEN_ADRXREF_REF_FLAG,1                                        abs. len. 00999   
LEN_ADMXREF_CONCAT,4                                          abs. len. 01000   
LEN_ADMXREF_DAT_OFF,4                                         abs. len. 01001   
LEN_ADMXREF_DAT_LEN,4                                         abs. len. 01002   
LEN_ADMXREF_VOL_OFF,4                                         abs. len. 01003   
LEN_ADMXREF_VOL_LEN,4                                         abs. len. 01004   
LEN_ADMXREF_DD_OFF,4                                          abs. len. 01005   
LEN_ADMXREF_DD_LEN,4                                          abs. len. 01006   
LEN_ADMXREF_MACROS_NUM,4                                      abs. len. 01007   
LEN_ADMXREF_MACROS_OFF,4                                      abs. len. 01008   
LEN_ADMXREF_DATASET,256                                       abs. len. 01009   
LEN_ADMXREF_VOL,256                                           abs. len. 01010   
LEN_ADMXREF_DDNAME,256                                        abs. len. 01011   
LEN_ADMXREF_MACRO_NEXT_OFF,4                                  abs. len. 01012   
LEN_ADMXREF_MACRO_NAME_OFF,4                                  abs. len. 01013   
LEN_ADMXREF_MACRO_NAME_LEN,4                                  abs. len. 01014   
LEN_ADMXREF_MACRO_NAME,256                                    abs. len. 01015   
LEN_ADMXRFX_CNUM,4                                            abs. len. 01016   
LEN_ADMXRFX_DEFN,4                                            abs. len. 01017   
LEN_ADMXRFX_CTYPE,1                                           abs. len. 01018   
LEN_ADMXRFX_DEFN_FLAG,1                                       abs. len. 01019   
LEN_ADMXRFX_MEM_OFF,4                                         abs. len. 01020   
LEN_ADMXRFX_MEM_LEN,4                                         abs. len. 01021   
LEN_ADMXRFX_CALL_OFF,4                                        abs. len. 01022   
LEN_ADMXRFX_CALL_LEN,4                                        abs. len. 01023   
LEN_ADMXRFX_TOTAL_REFS,4                                      abs. len. 01024   
LEN_ADMXRFX_REFS_NUM,4                                        abs. len. 01025   
LEN_ADMXRFX_REFS_OFF,4                                        abs. len. 01026   
LEN_ADMXRFX_MEM_NAME,64                                       abs. len. 01027   
LEN_ADMXRFX_CALL_NAME,64                                      abs. len. 01028   
LEN_ADMXRFX_REF_STMT,4                                        abs. len. 01029   
LEN_ADMXRFX_REF_FLAG,1                                        abs. len. 01030   
LEN_ADUSER_FIELD1,4                                           abs. len. 01031   
LEN_ADUSER_FIELD2,4                                           abs. len. 01032   
LEN_ADUSER_FIELD3,4                                           abs. len. 01033   
LEN_ADUSER_FIELD4,4                                           abs. len. 01034   
LEN_ADUSER_DATA_OFF,4                                         abs. len. 01035   
LEN_ADUSER_DATA_LEN,4                                         abs. len. 01036   
LEN_ADUSING_TYPE,1                                            abs. len. 01037   
LEN_ADUSING_FLAG,1                                            abs. len. 01038   
LEN_ADUSING_LOCTR_ESDID,4                                     abs. len. 01039   
LEN_ADUSING_STMT,4                                            abs. len. 01040   
LEN_ADUSING_LOCTR,4                                           abs. len. 01041   
LEN_ADUSING_VALUE,4                                           abs. len. 01042   
LEN_ADUSING_LAST_STMT,4                                       abs. len. 01043   
LEN_ADUSING_ID,4                                              abs. len. 01044   
LEN_ADUSING_REG,1                                             abs. len. 01045   
LEN_ADUSING_DISP,2                                            abs. len. 01046   
LEN_ADUSING_RANGE,4                                           abs. len. 01047   
LEN_ADUSING_LABEL_OFF,4                                       abs. len. 01048   
LEN_ADUSING_LABEL_LEN,4                                       abs. len. 01049   
LEN_ADUSING_LABEL,63                                          abs. len. 01050   
LEN_ADSTATS_BUFFP,4                                           abs. len. 01051   
LEN_ADSTATS_REQ_INSTOR,4                                      abs. len. 01052   
LEN_ADSTATS_P_INPUT,4                                         abs. len. 01053   
LEN_ADSTATS_LIB_INPUT,4                                       abs. len. 01054   
LEN_ADSTATS_WORK_FILE,4                                       abs. len. 01055   
LEN_ADSTATS_P_PRINT,4                                         abs. len. 01056   
LEN_ADSTATS_P_PUNCH,4                                         abs. len. 01057   
LEN_ADSTATS_WORK_WRITE,4                                      abs. len. 01058   
LEN_ADSTATS_ADATA,4                                           abs. len. 01059   
LEN_ADSTATS_ADATA_CALLS,4                                     abs. len. 01060   
LEN_ADSTATS_ADATA_ADDED,4                                     abs. len. 01061   
LEN_ADSTATS_ADATA_DELETE,4                                    abs. len. 01062   
LEN_ADSTATS_ADATA_DIAG,4                                      abs. len. 01063   
LEN_ADSTATS_LIB_CALLS,4                                       abs. len. 01064   
LEN_ADSTATS_LIB_ADDED,4                                       abs. len. 01065   
LEN_ADSTATS_LIB_DELETE,4                                      abs. len. 01066   
LEN_ADSTATS_LIB_DIAG,4                                        abs. len. 01067   
LEN_ADSTATS_LIST_CALLS,4                                      abs. len. 01068   
LEN_ADSTATS_LIST_ADDED,4                                      abs. len. 01069   
LEN_ADSTATS_LIST_DELETE,4                                     abs. len. 01070   
LEN_ADSTATS_LIST_DIAG,4                                       abs. len. 01071   
LEN_ADSTATS_OBJ_CALLS,4                                       abs. len. 01072   
LEN_ADSTATS_OBJ_ADDED,4                                       abs. len. 01073   
LEN_ADSTATS_OBJ_DELETE,4                                      abs. len. 01074   
LEN_ADSTATS_OBJ_DIAG,4                                        abs. len. 01075   
LEN_ADSTATS_INPUT_CALLS,4                                     abs. len. 01076   
LEN_ADSTATS_INPUT_ADDED,4                                     abs. len. 01077   
LEN_ADSTATS_INPUT_DELETE,4                                    abs. len. 01078   
LEN_ADSTATS_INPUT_DIAG,4                                      abs. len. 01079   
LEN_ADSTATS_PUNCH_CALLS,4                                     abs. len. 01080   
LEN_ADSTATS_PUNCH_ADDED,4                                     abs. len. 01081   
LEN_ADSTATS_PUNCH_DELETE,4                                    abs. len. 01082   
LEN_ADSTATS_PUNCH_DIAG,4                                      abs. len. 01083   
LEN_ADSTATS_TERM_CALLS,4                                      abs. len. 01084   
LEN_ADSTATS_TERM_ADDED,4                                      abs. len. 01085   
LEN_ADSTATS_TERM_DELETE,4                                     abs. len. 01086   
LEN_ADSTATS_TERM_DIAG,4                                       abs. len. 01087   
LEN_ADSTATS_START_TIME,4                                      abs. len. 01088   
LEN_ADSTATS_STOP_TIME,4                                       abs. len. 01089   
LEN_ADSTATS_PROC_TIME,4                                       abs. len. 01090   
LEN_ADSTATS_ASMAOPT_READ,4                                    abs. len. 01091   
LEN_ADSTATS_XFUNCS_NUM,4                                      abs. len. 01092   
LEN_ADSTATS_XFUNCS_OFF,4                                      abs. len. 01093   
LEN_ADSTATS_SUPR_NUM,4                                        abs. len. 01094   
LEN_ADSTATS_SUPR_OFF,4                                        abs. len. 01095   
LEN_ADSTATS_XFUNC_NEXT_OFF,4                                  abs. len. 01096   
LEN_ADSTATS_XFUNC_SETAF,4                                     abs. len. 01097   
LEN_ADSTATS_XFUNC_SETCF,4                                     abs. len. 01098   
LEN_ADSTATS_XFUNC_MSG,4                                       abs. len. 01099   
LEN_ADSTATS_XFUNC_MSEV,2                                      abs. len. 01100   
LEN_ADSTATS_XFUNC_NAME_OFF,4                                  abs. len. 01101   
LEN_ADSTATS_XFUNC_NAME_LEN,4                                  abs. len. 01102   
LEN_ADSTATS_XFUNC_NAME,256                                    abs. len. 01103   
LEN_ADSTATS_SUPR_MSGNO,2                                      abs. len. 01104   
LEN_ADSTATS_SUPR_COUNT,4                                      abs. len. 01105   
LEN_ADFBLOCK_NEXT_OFF,4                                       abs. len. 01106   
LEN_ADFBLOCK_FILENUMBER,4                                     abs. len. 01107   
LEN_ADFBLOCK_FILE_OFF,4                                       abs. len. 01108   
LEN_ADFBLOCK_FILE_LEN,4                                       abs. len. 01109   
LEN_ADFBLOCK_VOL_OFF,4                                        abs. len. 01110   
LEN_ADFBLOCK_VOL_LEN,4                                        abs. len. 01111   
LEN_ADFBLOCK_MEM_OFF,4                                        abs. len. 01112   
LEN_ADFBLOCK_MEM_LEN,4                                        abs. len. 01113   
LEN_ADFBLOCK_FILENAME,256                                     abs. len. 01114   
LEN_ADFBLOCK_VOLNAME,256                                      abs. len. 01115   
LEN_ADFBLOCK_MEMNAME,256                                      abs. len. 01116   
LEN_ASMADATA_RDW,4                                            abs. len. 01117**3
LEN_ASMADATA_COMMON_HEADER,12                                 abs. len. 01118**3
LEN_ADDCDS_NEXT_OP_OFF_FIRST,4                                abs. len. 01119   
LEN_ADDCDS_LOCTR_FIRST,4                                      abs. len. 01120   
LEN_ADDCDS_DUP_FIRST,4                                        abs. len. 01121   
LEN_ADDCDS_BIT_OFFSET_FIRST,1                                 abs. len. 01122   
LEN_ADDCDS_TYPE_ATTR_FIRST,1                                  abs. len. 01123   
LEN_ADDCDS_TYPE_EXT_FIRST,1                                   abs. len. 01124   
LEN_ADDCDS_PROGRAM_TYPE_FIRST,4                               abs. len. 01125   
LEN_ADDCDS_VALUES_NUM_FIRST,4                                 abs. len. 01126   
LEN_ADDCDS_VALUES_OFF_FIRST,4                                 abs. len. 01127   
LEN_ADDCDS_NEXT_VAL_OFF_FIRST,4                               abs. len. 01128   
LEN_ADDCDS_OBJECT_OFF_FIRST,4                                 abs. len. 01129   
LEN_ADDCDS_BYTE_LEN_FIRST,4                                   abs. len. 01130   
LEN_ADDCDS_BIT_LEN_FIRST,4                                    abs. len. 01131   
LEN_ADDCDS_OBJECT_FIRST,1                                     abs. len. 01132   
LEN_ADMACH_INST_VAL_BYTE1,1                                   abs. len. 01133   
LEN_ADMACH_INST_VAL_BYTE2,1                                   abs. len. 01134   
LEN_ADMACH_INST_VAL_BYTES3_4,2                                abs. len. 01135   
LEN_ADMACH_INST_VAL_BYTE3,1                                   abs. len. 01136   
LEN_ADMACH_INST_VAL_BYTE4,1                                   abs. len. 01137   
LEN_ADMACH_INST_VAL_BYTE5,1                                   abs. len. 01138   
LEN_ADMACH_INST_VAL_BYTE6,1                                   abs. len. 01139   
LEN_ADSRC_MEMBER_NAME_CL8,8                                   abs. len. 01140**2
LEN_ADSRC_RECORD_STMT,72                                      abs. len. 01141**3
LEN_ADSRC_RECORD_STMT_WITHOUT_CONT,71                         abs. len. 01142**3
LEN_ADSRC_RECORD_STMT_CONT_COLUMN,1                           abs. len. 01143**3
LEN_ADSRC_RECORD_STMT_SEQUENCE_FIELD,8                        abs. len. 01144**3
LEN_ADSRC_RECORD_STMT_PAN_STMT_#,5                            abs. len. 01145**3
LEN_ADSRC_RECORD_STMT_PAN_LEVEL_#,3                           abs. len. 01146**3
LEN_ADSRC_RECORD_PAN_SPECIAL_COMMENT_DATA_SET,20              abs. len. 01147**4
LEN_ADSRC_RECORD_PAN_MEMBER_NAME,10                           abs. len. 01148**4
LEN_ADSRC_RECORD_PAN_SPECIAL_COMMENT_AT_LEVEL,10              abs. len. 01149**4
LEN_ADSRC_RECORD_PAN_LEVEL_NUMBER,3                           abs. len. 01150**4
LEN_ADSRC_RECORD_PAN_SPECIAL_COMMENT_AS_OF,7                  abs. len. 01151**4
LEN_ADSRC_RECORD_PAN_UPDATE_DATE,8                            abs. len. 01152**4
LEN_ADSRC_RECORD_PAN_SPECIAL_COMMENT_BLANKS,22                abs. len. 01153**4
LEN_ADSRC_RECORD_WITHOUT_SEQ_FLD,208                          abs. len. 01154**3
*                                                                       01155   
*=============> END OF PROGRAMMATICALLY-GENERATED SYMBOLS <============ 01156   
